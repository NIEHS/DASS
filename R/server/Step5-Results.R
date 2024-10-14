# Generate and Display Results =====
dass_results <- reactiveVal()
dass_res_template <- reactive({
  switch(
    input$selected_da,
    da_2o3 = list(
      ke1_call = NULL,
      ke2_call = NULL,
      ke3_call = NULL,
      hazard = NULL
    ),
    da_its = list(
      ke1_score = NULL,
      ke3_score = NULL,
      insil_score = NULL,
      total_score = NULL,
      hazard = NULL,
      potency = NULL
    ),
    da_ke31 = list(
      hazard = NULL,
      potency = NULL
    )
  )
})

all_out <- reactiveValues(
  result_df = NULL,
  user_selected = NULL,
  dass_input = NULL,
  dass_results = NULL,
  to_hide = NULL
)

# Confirm Run -----
observeEvent(input$run_dass, {
  if (!blr()) {
    req(dt_review())
    if (any(dt_review()$Flagged)) {
      toggleModal(session = session, modalId = "confirm_run_with_flag", toggle = "open")
    } else {
      run_dass()
    }
    
  } else if (blr()) {
    
    any_flag <- any(sapply(dt_ke_blr_data()$flags, function(x) any(x$Flagged)))
    if (any_flag) {
      toggleModal(session = session, modalId = "confirm_run_with_flag", toggle = "open")
    } else {
      run_borderline()
    }
  }
})

observeEvent(input$run_with_flags, {
  if (!blr()) {
    run_dass()
  } else if (blr()) {
    run_borderline()
  }
  toggleModal(session = session, modalId = "confirm_run_with_flag", toggle = "close")
})

observeEvent(input$cancel_run, {
  toggleModal(session = session, modalId = "confirm_run_with_flag", toggle = "close")
})


# Run -----
run_dass <- reactive({
  data_select <- data_select()
  dass_res <- dass_res_template()
  
  if (ke1_calc_mean()) {
    data_select$ke1_mean_c_l_dep_col$col_name <- "ke1_mean_calculated"
    data_select$ke1_mean_c_l_dep_col$display_name <- paste(
      data_select$ke1_mean_c_l_dep_col$display_name, "(Calculated)"
    )
    data_select$ke1_mean_c_l_dep_col$converted_values <-
      (data_select$ke1_c_dep_col$converted_values +
         data_select$ke1_l_dep_col$converted_values)/2
  }
  
  if (ke1_get_call()) {
    data_select$ke1_call_col$col_name <- "ke1_call_calculated"
    data_select$ke1_call_col$display_name <- paste(data_select$ke1_call_col$display_name, "(Calculated)")

    tmp <- ke1Call(
      assay = input$ke1_assay_name,
      mean_c_l_dep = data_select$ke1_mean_c_l_dep_col$converted_values,
      c_dep = data_select$ke1_c_dep_col$converted_values
    )$outcome
    
    data_select$ke1_call_col$converted_values <- as.numeric(tmp == "Positive")
  }
  
  if (input$selected_da == "da_2o3") {
  dass_res$hazard <- da2o3(
    assayA_call = data_select$ke1_call_col$converted_values,
    assayB_call = data_select$ke2_call_col$converted_values,
    assayC_call = data_select$ke3_call_col$converted_values
  )$hazard
  dass_res <- data.frame(do.call("cbind", dass_res), check.names = F)
  }
  
  if (input$selected_da == "da_its") {
    dass_res <- daITS(
      ke1_assay = input$ke1_assay_name,
      ke1_mean_c_l_dep = data_select$ke1_mean_c_l_dep_col$converted_values,
      ke1_c_dep = data_select$ke1_c_dep_col$converted_values,
      ke3_assay = input$ke3_assay_name,
      ke3_value = data_select$ke3_val_col$converted_values,
      insil_prediction = data_select$insil_call_col$converted_values,
      insil_ad = data_select$insil_ad_col$converted_values
    )[,names(dass_res)]
  }
  
  if (input$selected_da == "da_ke31") {
    dass_res <- daKE31(
      ke1_call = data_select$ke1_call_col$converted_values,
      ke3_value = data_select$ke3_val_col$converted_values
    )[,names(dass_res)]
  }
  
  names(dass_res) <- paste0(abbrev[input$selected_da], ".", names(dass_res))
  
  da_input <- sapply(data_select, function(x) !is.null(x$converted_values))
  da_input <- lapply(data_select[da_input], function(x) {
    out <- list(x$converted_values)
    names(out) <- paste0(x$col_name, "_input")
    if (x$flagged) {
      names(out) <- paste0(names(out), "_FLAG")
    }
    
    
    return(out)
  })
  da_input <- as.data.frame(da_input)

  merged <- do.call("cbind.data.frame", list(dt_analyze(), da_input, dass_res))
  all_out$result_df <- merged

  selected_cols <- unlist(lapply(data_select, function(x) if (!is.null(x$values)) return(x$col_name)))
  all_out$user_selected <- match(selected_cols, names(merged))

  all_out$dass_input <- match(names(da_input), names(merged))
  all_out$dass_results <- match(names(dass_res), names(merged))

  all_out$to_hide <- setdiff(1:ncol(merged), c(1:3, all_out$user_selected, all_out$dass_results))

  names(all_out$result_df)[all_out$user_selected] <- paste0(names(all_out$result_df)[all_out$user_selected], "*")

  showHide(show = c("result_contents", "result_contents_standard", "compare_setup_standard"), hide = c("result_contents_blr"))
  updateTabsetPanel(inputId = "step_set", selected = "Results")
  
  updateRadioButtons(inputId = "perf_pred_col", choices = switch(input$selected_da, da_2o3 = "Hazard", da_its = c("Hazard", "Potency"), da_ke31 = c("Hazard", "Potency")))
  updateSelectInput(inputId = "perf_ref_col", choices = names(dt_analyze()))
  updateSelectInput(inputId = "perf_ice_user_identifier", choices = c("", names(dt_analyze())))
  
})

output$dt_results <- renderDataTable({
  req(all_out$result_df)

  to_hide <- all_out$to_hide - 1
  to_show <- setdiff(0:(ncol(all_out$result_df)-1), to_hide)
  
  dt_results <- datatable(
    all_out$result_df,
    class = "table-bordered stripe",
    rownames = F,
    filter = "top",
    selection = "none",
    extensions = "Buttons",
    callback = JS("$('#dt_results .dataTables_scrollBody').each((i, e) => e.setAttribute('tabIndex', 0))"),
    options = list(
      dom = "Brtp",
      scrollX = T,
      scrollY = T,
      buttons = list(
        list(extend = "colvis", text = "Column Visibility", collectionLayout = "columns", attr = list(id = "resColPicker")),
        list(extend = "colvisGroup", text = "Hide All Columns", hide = ":visible"),
        list(extend = "colvisGroup", text = "Show All Columns", show = ":hidden"),
        list(extend = "colvisGroup", text = "Reset Columns", show = to_show, hide = to_hide)
        )
    )
  )
  dt_results <- formatStyle(dt_results, columns = all_out$user_selected, backgroundColor = "#F0E442")
  dt_results <- formatStyle(dt_results, columns = all_out$dass_input, backgroundColor = "#CC79A7")
  dt_results <- formatStyle(dt_results, columns = all_out$dass_results, backgroundColor = "#56B4E9")

  i <- length(dt_results$x$options$columnDefs)
  dt_results$x$options$columnDefs[[i+1]] <- list(
    targets = to_hide,
    visible = F
  )
  
  # rc <- dt_results$x$options$rowCallback
  # rc <- unlist(strsplit(as.character(rc), '\n'))
  # dt_results$x$options$rowCallback <- JS(append(rc, after = length(rc) - 1, showNA_js))

  dt_results
})

### Save -----
create_xl_file <- reactive({
  wb <- createWorkbook()

  addWorksheet(wb, sheetName = "Key")
  addWorksheet(wb, sheetName = "Selections")
  addWorksheet(wb, sheetName = "Results")

  # Styles
  bold_font <- createStyle(textDecoration = "bold")
  blue_bg <- createStyle(fgFill = "#56B4E9", halign = "right")
  pink_bg <- createStyle(fgFill = "#CC79A7", halign = "right")
  yellow_bg <- createStyle(fgFill = "#F0E442", halign = "right")

  # Create key worksheet
  key_df <- data.frame(
    Color = c("Yellow", "Pink", "Blue"),
    `Column Annotation` = c(
      "Ends with an asterisk.",
      "Ends with '_input'. If calculated by the app, ends with 'calculated'.",
      "Begins with 'da_' and the abbreviation of the DA."
    ),
    Label = c(
      "User-selected columns",
      "Transformed versions of user-selected columns. These are the values used as input in the DASS",
      "Defined approach result"
    ),
    check.names = FALSE
  )

  writeData(wb, sheet = "Key", key_df, headerStyle = bold_font)
  addStyle(wb, sheet = "Key", style = blue_bg, row = 4, col = 1)
  addStyle(wb, sheet = "Key", style = pink_bg, row = 3, col = 1)
  addStyle(wb, sheet = "Key", style = yellow_bg, row = 2, col = 1)

  dt_review <- dt_review()
  colnames(dt_review) <- c("Required Endpoint", "Selection", "Flagged")
  
  summary_df <- data.frame(
    `Required Endpoint` = c("DASS App", "Date Run", "", "Required Endpoint", "DA"),
    `Selection` = c("v2.0", format.Date(Sys.Date()), "", "Selection", abbrev[input$selected_da]),
    Flagged = c(rep("", 3), "Flagged", ""),
    check.names = F
  )
  
  if ((input$selected_da == "da_2o3" & input$ke1_call_interpret) | input$selected_da == "da_its") {
    summary_df <- rbind(summary_df, data.frame(
      `Required Endpoint` = c("KE1 Assay"),
      `Selection` = abbrev[input$ke1_assay_name],
      Flagged = "",
      check.names = F
    ))
  }
  
  if (input$selected_da == "da_its") {
    summary_df <- rbind(summary_df, data.frame(
      `Required Endpoint` = c("KE3 Assay"),
      `Selection` = abbrev[input$ke3_assay_name],
      Flagged = "",
      check.names = F
    ))
  }  
  
  summary_df <- rbind(summary_df, dt_review)

  writeData(wb, "Selections", summary_df, colNames = F, rowNames = F)
  
  writeData(wb, sheet = "Results", x = all_out$result_df, headerStyle = bold_font, keepNA = T, na.string = "NA")
  row_select <- 2:(nrow(all_out$result_df) + 1)
  addStyle(wb, sheet = "Results", style = yellow_bg,
           rows = row_select, cols = all_out$user_selected, gridExpand = T)
  addStyle(wb, sheet = "Results", style = pink_bg,
           rows = row_select, cols = all_out$dass_input, gridExpand = T)
  addStyle(wb, sheet = "Results", style = blue_bg,
           rows = row_select, cols = all_out$dass_results, gridExpand = T)
  activeSheet(wb) <- "Results"

  wb
})

output$downloadres_txt <- downloadHandler(
  filename = function() {
    if (input$use_demo_data) {
      fname <- "DemoData"
    } else {
      fname <- unlist(strsplit(input$fpath$name, "[.]"))
      fname <- paste(fname[-length(fname)], collapse = ".")
    }
    paste0(fname, "_DASSResults_", format.Date(Sys.time(), "%Y%m%d-%H%M%S"), ".txt")
  },
  content = function(con) {
    tmp <- data.frame(t(c("DASS App v2.0", format.Date(Sys.Date()), rep("", ncol(all_out$result_df)-2))))
    colnames(tmp) <- names(all_out$result_df)
    tmp <- rbind(all_out$result_df, tmp)
    write.table(x = tmp, file = con, quote = F, row.names = F, sep = "\t")
  }
)

outputOptions(output, "downloadres_txt", suspendWhenHidden = FALSE)

output$downloadres_xl <- downloadHandler(
  filename = function() {
    if (input$use_demo_data) {
      fname <- "DemoData"
    } else {
      fname <- unlist(strsplit(input$fpath$name, "[.]"))
      fname <- paste(fname[-length(fname)], collapse = ".")
    }
    paste0(fname, "_DASSResults_", format.Date(Sys.time(), "%Y%m%d-%H%M%S"), ".xlsx")
  },
  content = function(con) {
    saveWorkbook(wb = create_xl_file(), file = con)
  }
)

outputOptions(output, "downloadres_xl", suspendWhenHidden = FALSE)

# Borderline -----
all_out_blr <- reactiveValues(
  dass_results = NULL,
  ke1_run_outcome = NULL,
  ke2_run_outcome = NULL,
  ke3_run_outcome = NULL
)

run_borderline <- reactive({
  
  ke1 <- dt_ke_blr_data()$col_data$ke1
  ke1_mean <- (ke1$ke1_blr_c_dep_col$converted_values + ke1$ke1_blr_l_dep_col$converted_values)/2
  ke1_c <- ke1$ke1_blr_c_dep_col$converted_values
  
  ke1_mean[ke1$ke1_blr_c_only_col$values == "y"] <- NA
  ke1_c[ke1$ke1_blr_c_only_col$values == "n"] <- NA
  
  ke1_out <- ke1Call(
    assay = input$ke1_blr_assay_name,
    mean_c_l_dep = ke1_mean,
    c_dep = ke1_c,
    borderline = T
  )
  
  ke1_out <- data.frame(
    cid = ke1$ke1_blr_cid_col$values,
    ke1_out
  )
  
  ke1_overall <- unlist(as.list(by(ke1_out, ke1_out$cid, function(x) {
    if (nrow(x) > 1) {
      out_count <- unlist(table(x[["outcome_bl"]]))
      out <- names(out_count)[out_count == max(out_count)]
      if (length(out) == 1) {
        return(out)
      } else {
        return("Inconclusive")
      }
    }
  })))
  ke1_overall <- data.frame(cid = names(ke1_overall), ke1_overall, row.names = NULL)
  
  ke2 <- dt_ke_blr_data()$col_data$ke2
  if (input$ke2_blr_assay_name == "ks") {
    ke2_out <- NULL
    ke2_overall <- data.frame(
      cid = ke2$ke2_blr_cid_col$values,
      ke2_overall = ke2$ke2_blr_ks_call_col$converted_values
    )
  } else if (input$ke2_blr_assay_name == "lusens") {
    
    lusens <- Map(function(col_id, ke2_list) {
      if (is.null(ke2_list$converted_values)) {
        tmp <- data.frame(x = ke2_list$values)
      } else {
        tmp <- data.frame(x = ke2_list$converted_values)
      }
      names(tmp) <- col_id
      tmp
    }, names(ke2), ke2)
    lusens <- as.data.frame(lusens)
    lusens <- split(lusens, list(lusens$ke2_blr_cid_col, lusens$ke2_blr_run_col), drop = T)
    
    ke2_out <- lapply(lusens, function(df) {
      data.frame(
        compound_id = df[["ke2_blr_cid_col"]][1],
        run_id = df[["ke2_blr_run_col"]][1],
        outcome_bl = lusensCall(
          conc = df[["ke2_blr_conc_col"]],
          fold_induction = df[["ke2_blr_fi_col"]],
          relative_viability = df[["ke2_blr_cv_col"]],
          ttest_p = df[["ke2_blr_p_col"]]
        )
      )
    })
    ke2_out <- do.call("rbind", ke2_out)  

    ke2_overall <- unlist(as.list(by(ke2_out, ke2_out$compound_id, function(x) {
      if (nrow(x) > 1) {
        out_count <- unlist(table(x[["outcome_bl"]]))
        out <- names(out_count)[out_count == max(out_count)]
        if (length(out) == 1) {
          return(out)
        } else {
          return("Inconclusive")
        }
      }
    })))
    ke2_overall <- data.frame(cid = names(ke2_overall), ke2_overall, row.names = NULL)
    
    names(ke2_out) <- c("Compound ID", input$ke2_blr_run_col, "Outcome")
    ke2_out[["Compound ID"]] <- as.factor(ke2_out[["Compound ID"]])
    ke2_out[["Outcome"]] <- factor(ke2_out[["Outcome"]], levels = c("Positive", "Negative", "Borderline", "Inconclusive"))
  }
  
  ke3 <- dt_ke_blr_data()$col_data$ke3
  ke3 <- Map(function(col_id, ke3_list) {
    if (is.null(ke3_list$converted_values)) {
      tmp <- data.frame(x = ke3_list$values)
    } else {
      tmp <- data.frame(x = ke3_list$converted_values)
    }
    names(tmp) <- col_id
    tmp
  }, names(ke3), ke3)
  ke3 <- as.data.frame(ke3)
  if (input$ke3_blr_assay_name == "gard") {
    ke3_out <- data.frame(
      ke3$ke3_blr_cid_col,
      ke3$ke3_blr_gard_meanDV_col,
      gardskinCall(ke3[["ke3_blr_gard_meanDV_col"]])
    )
    names(ke3_out) <- c("Compound ID", input$ke3_blr_gard_meanDV_col, "Outcome")
  } else {
    ke3 <- split(ke3, list(ke3$ke3_blr_run_col, ke3$ke3_blr_cid_col), drop = T)
    if(input$ke3_blr_assay_name == "hclat") {
      ke3_out <- lapply(ke3, function(df) {
        data.frame(
          compound_id = df[["ke3_blr_cid_col"]][1],
          run_id = df[["ke3_blr_run_col"]][1],
          outcome_bl = hclatCall(
            cd54 = df[["ke3_blr_hclat_cd54_col"]],
            cd86 = df[["ke3_blr_hclat_cd86_col"]],
            viability = df[["ke3_blr_hclat_cv_col"]]
          )
        )
      })
    } else if (input$ke3_blr_assay_name == "il8") {
      ws <- lapply(ke3, function(x) unique(x[,c("ke3_blr_cid_col", "ke3_blr_il8_ws_col")]))
      ws <- na.omit(do.call("rbind", ws))
      ke3_out <- lapply(ke3, function(df) {
        data.frame(
          compound_id = df[["ke3_blr_cid_col"]][1],
          run_id = df[["ke3_blr_run_col"]][1],
          outcome_bl = il8Call(
            ind_il8la = df[["ke3_blr_il8_ind_col"]],
            ind_il8la_ll = df[["ke3_blr_il8_indLCL_col"]],
            inh_gapla = df[["ke3_blr_il8_inh_col"]],
            ws = ws$ke3_blr_il8_ws_col[ws$ke3_blr_cid_col == df[["ke3_blr_cid_col"]][1]]
          )
        )
      })
    } else if (input$ke3_blr_assay_name == "usens") {
      ke3_out <- lapply(ke3, function(df) {
        data.frame(
          compound_id = df[["ke3_blr_cid_col"]][1],
          run_id = df[["ke3_blr_run_col"]][1],
          outcome_bl = usensCall(
            conc = df[["ke3_blr_conc_col"]],
            viability = df[["ke3_blr_usens_cv_col"]],
            si_cd86 = df[["ke3_blr_usens_cd86_col"]]
          )
        )
      })
    }
    
    ke3_out <- do.call("rbind", ke3_out)  
    names(ke3_out) <- c("Compound ID", input$ke3_blr_run_col, "Outcome")
  }
  ke3_overall <- unlist(as.list(by(ke3_out, ke3_out[["Compound ID"]], function(x) {
    if (nrow(x) > 1) {
      out_count <- unlist(table(x[["Outcome"]]))
      out <- names(out_count)[out_count == max(out_count)]
      if (length(out) == 1) {
        return(out)
      } else {
        return("Inconclusive")
      }
    }
  })))
  ke3_overall <- data.frame(cid = names(ke3_overall), ke3_overall, row.names = NULL)
  
  data_in <- merge(merge(ke1_overall, ke2_overall, by = "cid", all = T), ke3_overall, by = "cid", all = T)

  da_results <- apply(data_in, 1, function(c_row) {
    x <- na.omit(c_row[-1])
    out <- NA
    if (length(x) > 1) {
      out_count <- unlist(table(tolower(x)))
      out <- names(out_count)[out_count == max(out_count)]
      if (length(out) == 1) {
        out <- switch(out, positive = "Positive", negative = "Negative", borderline = "Borderline", inconclusive = "Inconclusive")
      } else {
        out <- "Inconclusive"
      }
    }
    data.frame(cid = c_row[1], da_2o3 = out)
  })

  da_results <- do.call("rbind", da_results)
  da_results <- merge(data_in, da_results, by = "cid", all = T)
  da_results <- as.data.frame(lapply(da_results, as.factor))
  colnames(da_results) <- c(
    "Compound ID", 
    abbrev[input$ke1_blr_assay_name],
    abbrev[input$ke2_blr_assay_name],
    abbrev[input$ke3_blr_assay_name],
    "DA 2o3 Hazard")

  ke1_out <- ke1_out[,c("cid", "mean_c_l_dep", "c_dep", "outcome_bl")]
  names(ke1_out) <- c("Compound ID", "Mean Depletion", "Cys/NAC Depletion", "Outcome")
  ke1_out[["Compound ID"]] <- as.factor(ke1_out[["Compound ID"]])
  ke1_out[["Outcome"]] <- factor(ke1_out[["Outcome"]], levels = c("Positive", "Negative", "Borderline", "Inconclusive"))
  
  ke3_out[["Compound ID"]] <- as.factor(ke3_out[["Compound ID"]])
  ke3_out[["Outcome"]] <- factor(ke3_out[["Outcome"]], levels = c("Positive", "Negative", "Borderline", "Inconclusive"))
  
  all_out_blr$dass_results <- da_results
  all_out_blr$ke1_run_outcome <- ke1_out
  if (!is.null(ke2_out)) {
    all_out_blr$ke2_run_outcome <- ke2_out
    shinyjs::hide("ks_run_text")
  } else {
    shinyjs::show("ks_run_text")
  }
  all_out_blr$ke3_run_outcome <- ke3_out
  
  showHide(show = c("result_contents", "result_contents_blr", "compare_explore_borderline"), hide = c("result_contents_standard", "compare_setup_standard"))
  updateTabsetPanel(inputId = "step_set", selected = "Results")
})

## Tables -----
output$dass_results_blr <- renderDataTable({
  datatable(
    all_out_blr$dass_results,
    class     = "table-bordered stripe",
    rownames  = F,
    filter    = "top",
    selection = "none",
    callback  = JS("$('#dt_results .dataTables_scrollBody').each((i, e) => e.setAttribute('tabIndex', 0))"),
    options   = list(
      dom = "lrtip",
      scrollY = TRUE,
      scrollX = TRUE)
    )
})

output$ke1_blr_indiv <- renderDataTable({
  datatable(
    all_out_blr$ke1_run_outcome,
    class     = "table-bordered table-condensed stripe",
    rownames  = F,
    filter    = "top",
    selection = "none",
    callback  = JS("$('#dt_results .dataTables_scrollBody').each((i, e) => e.setAttribute('tabIndex', 0))"),
    options   = list(
      dom = "lrtip",
      scrollY = TRUE,
      scrollX = TRUE)
  )
})

output$ke2_blr_indiv <- renderDataTable({
  datatable(all_out_blr$ke2_run_outcome,
            class     = "table-bordered table-condensed stripe",
            rownames  = F,
            filter    = "top",
            selection = "none",
            callback  = JS("$('#dt_results .dataTables_scrollBody').each((i, e) => e.setAttribute('tabIndex', 0))"),
            options   = list(
              dom = "lrtip",
              scrollY = TRUE,
              scrollX = TRUE)
  )
})

output$ke3_blr_indiv <- renderDataTable({
  datatable(all_out_blr$ke3_run_outcome,
            class     = "table-bordered table-condensed stripe",
            rownames  = F,
            filter    = "top",
            selection = "none",
            callback  = JS("$('#dt_results .dataTables_scrollBody').each((i, e) => e.setAttribute('tabIndex', 0))"),
            options   = list(
              dom = "lrtip",
              scrollY = TRUE,
              scrollX = TRUE)
  )
})

## Downloads -----
create_blr_xl_file <- reactive({
  wb <- createWorkbook()
  
  flags <- dt_ke_blr_data()$flags
  
  ke1_tmp <- rbind(
    data.frame(
    `Required Endpoint` = c("KE1 Assay", "KE1 Worksheet"),
    `Selected Column` = c(abbrev[input$ke1_blr_assay_name], input$ke1_blr_ws, use.names = F),
    Flagged = NA,
    check.names = F
  ), flags$ke1)

  ke2_tmp <- rbind(data.frame(
    `Required Endpoint` = c("KE2 Assay", "KE2 Worksheet"),
    `Selected Column` = c(abbrev[input$ke2_blr_assay_name], input$ke2_blr_ws, use.names = F),
    Flagged = NA,
    check.names = F
  ), flags$ke2)
  
  ke3_tmp <- rbind(data.frame(
    `Required Endpoint` = c("KE3 Assay", "KE3 Worksheet"),
    `Selected Column` = c(abbrev[input$ke3_blr_assay_name], input$ke3_blr_ws, use.names = F),
    Flagged = NA,
    check.names = F
  ), flags$ke3)

  summary_list <- rbind(rbind(ke1_tmp, ke2_tmp), ke3_tmp)

  summary_list <- rbind(
    data.frame(`Required Endpoint` = "Required Endpoint",
               `Selected Column` = "Selection",
               Flagged = "Flagged",
               check.names = F),
    summary_list
  )
  
  summary_list <- rbind(data.frame(
    `Required Endpoint` = c("DASS App", "Date Run", ""),
    `Selected Column` = c("v2.0", format.Date(Sys.Date()), ""),
    Flagged = NA,
    check.names = F
  ), summary_list)
  
  addWorksheet(wb, sheetName = "Summary")
  writeData(wb, sheet = "Summary", summary_list, colNames = F)
  
  addWorksheet(wb, sheetName = "KE1 Run Outcomes")
  writeData(wb, sheet = "KE1 Run Outcomes", all_out_blr$ke1_run_outcome)
  
  if(!is.null(all_out_blr$ke2_run_outcome)) {
    addWorksheet(wb, sheetName = "KE2 Run Outcomes")
    writeData(wb, sheet = "KE2 Run Outcomes", all_out_blr$ke2_run_outcome)
  }

  addWorksheet(wb, sheetName = "KE3 Run Outcomes")
  writeData(wb, sheet = "KE3 Run Outcomes", all_out_blr$ke3_run_outcome)
  
  addWorksheet(wb, sheetName = "DA Results")
  writeData(wb, sheet = "DA Results", all_out_blr$dass_results)
  
  activeSheet(wb) <- "DA Results"
  
  wb
})


output$downloadres_blr_xl <- downloadHandler(
  filename = function() {
    fname <- unlist(strsplit(input$fpath$name, "[.]"))
    fname <- paste(fname[-length(fname)], collapse = ".")
    paste0(fname, "_DASSResults_", format.Date(Sys.time(), "%Y%m%d-%H%M%S"), ".xlsx")
  },
  content = function(con) {
    saveWorkbook(wb = create_blr_xl_file(), file = con)
  }
)

outputOptions(output, "downloadres_blr_xl", suspendWhenHidden = FALSE)
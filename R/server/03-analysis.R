# Apply DAs =====
# Confirm Run -----
observeEvent(input$run_dass, {
  req(data_input())
  
  flag_check <- switch(
    wf(),
    std = (any(dt_review_std()$Flagged) | any(duplicated(dt_review_std()$`Selected Column`))),
    bl = any(unlist(lapply(data_input(), function(ke) {unlist(lapply(ke, function(cname) {cname$flagged}))})))
  )
  
  if (flag_check) {
    modal_toggle("run_warning", "show")
  } else {
    shinyjs::runjs("$('#confirm_run_dass').click();")
  }
})

observeEvent(input$confirm_run_dass, {
  # keep download list in dom.
  runjs("$('#dl_container').appendTo($('#ui_res_std'));")

  if (wf() == "std") {
    updatePickerInput(inputId = "compare_usr_dt_col", choices = names(data_tables()))
    updatePickerInput(inputId = "compare_ice_cid", choices = names(data_tables()))
    show_hide(show = c(names(tab_names[5:6]), "ui_res_std", "compare_ice_cid"), hide = "div_compare_bl")
  } else if (wf() == "bl") {
    updatePickerInput(inputId = "compare_bl_ws",choices = names(bl_ws_idx()))
    show_hide(show = c(names(tab_names[5:6]), "ui_res_bl", "div_compare_bl"), hide = "compare_ice_cid")
  }
  
  if (da() == "da_2o3") {
    updateRadioButtons(inputId = "compare_type", choices = "Hazard")
  } else {
    updateRadioButtons(inputId = "compare_type", choices = c("Hazard", "Potency"))
  }

  shinyjs::reset(names(tab_names[6]))
  modal_toggle("run_warning", "hide")
  
  tab_change("tabs", tab_names[[5]])
})

dass_res_template <- reactive({
  switch(
    da(),
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

# Standard -----
dass_result <- eventReactive(input$confirm_run_dass, {
  req(wf() == "std")
  data_input <- data_input()
  dass_res <- dass_res_template()

  # Calculate the mean depletion values if needed. 
  ke1_mean_cond1 <- (da() %in% c("da_2o3", "da_ke31")) & input$get_ke1_call & input$get_ke1_mean_dep
  ke1_mean_cond2 <- da() == "da_its" & input$get_ke1_mean_dep
  
  if (ke1_mean_cond1 | ke1_mean_cond2) {
    data_input$ke1$ke1_std_mean_dep$col_name <- "ke1_mean_depl_calculated"
    data_input$ke1$ke1_std_mean_dep$display_name <- paste(
      data_input$ke1$ke1_std_mean_dep$display_name, "(Calculated)"  
    )
    data_input$ke1$ke1_std_mean_dep$converted_values <- 
      (data_input$ke1$ke1_std_c_dep$converted_values + data_input$ke1$ke1_std_k_dep$converted_values)/2
    data_input$ke1$ke1_std_mean_dep$flagged <- (data_input$ke1$ke1_std_c_dep$flagged | data_input$ke1$ke1_std_k_dep$flagged)
  }
  
  # Get KE1 call if needed. 
  ke1_call_cond <- (da() %in% c("da_2o3", "da_ke31")) & input$get_ke1_call
  if (ke1_call_cond) {
    data_input$ke1$ke1_std_call$col_name <- "ke1_call_calculated"
    data_input$ke1$ke1_std_call$display_name <- paste(
      data_input$ke1$ke1_std_call$display_name, "(Calculated)"  
    )
    
    ke1_call_tmp <- ke1Call(
      assay = switch(da(), da_2o3 = input$ke1_std_assay, da_ke31 = "dpra"),
      mean_c_k_dep = data_input$ke1$ke1_std_mean_dep$converted_values,
      c_dep = data_input$ke1$ke1_std_c_dep$converted_values
    )
    
    data_input$ke1$ke1_std_call$converted_values <- as.numeric(ke1_call_tmp$outcome == "Positive")
    if (is.null(data_input$ke1$ke1_std_c_dep$flagged)) {
      data_input$ke1$ke1_std_call$flagged <- data_input$ke1$ke1_std_mean_dep$flagged
    } else {
      data_input$ke1$ke1_std_call$flagged <- (data_input$ke1$ke1_std_mean_dep$flagged) | (data_input$ke1$ke1_std_c_dep$flagged)
    }
  }
  
  if (da() == "da_2o3") {
    dass_res <- list(hazard=da2o3(
      data_input$ke1$ke1_std_call$converted_values,
      data_input$ke2$ke2_std_call$converted_values,
      data_input$ke3$ke3_std_call$converted_values
    )$hazard)
  } else if (da() == "da_its") {
    dass_res <- daITS(
      ke1_assay = input$ke1_std_assay,
      ke1_mean_c_k_dep = data_input$ke1$ke1_std_mean_dep$converted_values,
      ke1_c_dep = data_input$ke1$ke1_std_c_dep$converted_values,
      ke3_assay = input$ke3_std_assay,
      ke3_value = data_input$ke3$ke3_std_val$converted_values,
      insil_prediction = data_input$insil$insil_call$converted_values,
      insil_ad = data_input$insil$insil_ad$converted_values
    )[,names(dass_res)]
  } else if (da() == "da_ke31") {
    dass_res <- daKE31(
      ke1_call = data_input$ke1$ke1_std_call$converted_values,
      ke3_value = data_input$ke3$ke3_std_val$converted_values
    )[,names(dass_res)]
  }
  
  names(dass_res) <- paste0(da_dict[[da()]]$abbrev, ".", names(dass_res))
  
  # Prepare input columns for results table
  da_input_flat <- do.call("c", unname(data_input))
  da_input_idx <- sapply(da_input_flat, function(x) !is.null(x$converted_values))
  da_input <- lapply(da_input_flat[da_input_idx], function(x) {
    if (!is.null(x$converted_values)) {
      out <- data.frame(x$converted_values)
      names(out) <- paste0(x$col_name, "_input")
      if (x$flagged) {
        names(out) <- paste0(x$col_name, "_FLAG")
      }
      return(out)
    }
  })
  da_input <- as.data.frame(da_input)
  
  # Merge tables to create results table
  merged <- do.call("cbind.data.frame", list(data_tables(), da_input, dass_res))
  
  # Get index for user selected columns
  selected_cols <- unlist(sapply(da_input_flat, function(x) if (!is.null(x$values)) return(x$col_name)))
  selected_cols <- match(selected_cols, names(merged))

  # Get index for input columns
  input_cols <- match(names(da_input), names(merged))
  
  # Get index for result columns
  res_cols <- match(names(dass_res), names(merged))
  
  # Set index for columns to hide by default
  to_hide <- setdiff(1:ncol(merged), c(1:3, selected_cols, res_cols))
  
  names(merged)[selected_cols] <- paste0(names(merged)[selected_cols], "*")
  list(
    results_df = merged,
    user_selected = selected_cols,
    dass_input = input_cols,
    dass_results = res_cols,
    to_hide = to_hide
  )
})

output$results_std <- DT::renderDataTable({
  res <- dass_result()
  
  to_hide <- res$to_hide - 1
  to_show <- setdiff(0:(ncol(res$results_df)-1), to_hide)
  
  dt_results <- datatable(
    res$results_df,
    class = "table-data stripe",
    rownames = F,
    filter = "top",
    selection = "none",
    extensions = "Buttons",
    options = list(
      dom = "Brtp",
      initComplete = JS("() => updateDT('results_std', true)"),
      buttons = list(
        list(extend = "", text = "Table Key", action = JS("function(e, dt, node, config) {$('#table_key').modal('show')}")),
        list(
          extend = "collection",
          collectionLayout = "dropdown",
          text = "Column Visibility",
          buttons = list(
              list(extend = "colvis", text = "Select Columns", collectionLayout = "columns", attr = list(id = "resColPicker")),
              list(extend = "colvisGroup", text = "Hide All Columns", hide = ":visible"),
              list(extend = "colvisGroup", text = "Show All Columns", show = ":hidden"),
              list(extend = "colvisGroup", text = "Reset Columns", show = to_show, hide = to_hide)
          )
        )
      )
    ),
    callback = JS("tabBody(table); $('#dl_container').appendTo($('#results_std .dt-buttons'));")
  )
  
  dt_results <- formatStyle(dt_results, columns = res$user_selected, backgroundColor = "#F0E442")
  dt_results <- formatStyle(dt_results, columns = res$dass_input, backgroundColor = "#CC79A7")
  dt_results <- formatStyle(dt_results, columns = res$dass_results, backgroundColor = "#56B4E9")
  
  i <- length(dt_results$x$options$columnDefs)
  dt_results$x$options$columnDefs[[i+1]] <- list(
    targets = to_hide,
    visible = F
  )
  
  dt_results
})

export_std <- reactive({
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
  
  dt_review <- dt_review_std()
  colnames(dt_review) <- c("Required Endpoint", "Selection", "Flagged")
  
  summary_df <- data.frame(
    `Required Endpoint` = c("DASS App", "Date Run", "", "Required Endpoint", "DA"),
    `Selection` = c("v2.0", format.Date(Sys.Date()), "", "Selection", da_dict[[da()]]$abbrev),
    Flagged = c(rep("", 3), "Flagged", ""),
    check.names = F
  )
  
  if ((da() == "da_2o3" & input$get_ke1_call) | da() == "da_its") {
    summary_df <- rbind(summary_df, data.frame(
      `Required Endpoint` = c("KE1 Assay"),
      `Selection` = assay_dict[[input$ke1_std_assay]],
      Flagged = "",
      check.names = F
    ))
  }
  
  if (da() == "da_its") {
    summary_df <- rbind(summary_df, data.frame(
      `Required Endpoint` = c("KE3 Assay"),
      `Selection` = assay_dict[[input$ke3_std_assay]],
      Flagged = "",
      check.names = F
    ))
  }
  
  summary_df <- rbind(summary_df, dt_review)
  list(
    key_df = key_df,
    summary_df = summary_df
  )
})

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
    wb <- createWorkbook()
    
    addWorksheet(wb, sheetName = "Key")
    addWorksheet(wb, sheetName = "Selections")
    addWorksheet(wb, sheetName = "Results")
    
    # Styles
    bold_font <- createStyle(textDecoration = "bold")
    blue_bg <- createStyle(fgFill = "#56B4E9", halign = "right")
    pink_bg <- createStyle(fgFill = "#CC79A7", halign = "right")
    yellow_bg <- createStyle(fgFill = "#F0E442", halign = "right")
    
    writeData(wb, sheet = "Key", export_std()$key_df, headerStyle = bold_font)
    addStyle(wb, sheet = "Key", style = blue_bg, row = 4, col = 1)
    addStyle(wb, sheet = "Key", style = pink_bg, row = 3, col = 1)
    addStyle(wb, sheet = "Key", style = yellow_bg, row = 2, col = 1)
    
    writeData(wb, "Selections", export_std()$summary_df, colNames = F, rowNames = F)
    
    writeData(wb, "Results", x = dass_result()$results_df, headerStyle = bold_font, keepNA = T, na.string = "NA")
    
    row_select <- 2:(nrow(dass_result()$results_df) + 1)
    addStyle(wb, sheet = "Results", style = yellow_bg,
             rows = row_select, cols = dass_result()$user_selected, gridExpand = T)
    addStyle(wb, sheet = "Results", style = pink_bg,
             rows = row_select, cols = dass_result()$dass_input, gridExpand = T)
    addStyle(wb, sheet = "Results", style = blue_bg,
             rows = row_select, cols = dass_result()$dass_results, gridExpand = T)
    activeSheet(wb) <- "Results"
    
    saveWorkbook(wb = wb, file = con)
  },
  contentType = "file/xlsx"
)

outputOptions(output, "downloadres_xl", suspendWhenHidden = FALSE)

output$downloadres_zip <- downloadHandler(
  filename = function() {
    if (input$use_demo_data) {
      fname <- "DemoData"
    } else {
      fname <- unlist(strsplit(input$fpath$name, "[.]"))
      fname <- paste(fname[-length(fname)], collapse = ".")
    }
    paste0(fname, "_DASSResults_", format.Date(Sys.time(), "%Y%m%d-%H%M%S"), ".zip")
  },
  content = function(con) {
    dir <- getwd()
    setwd(tempdir()); on.exit(setwd(dir))
    
    files <- c("table_key.txt", "dass_selections.txt", "dass_results.txt")
    write.table(export_std()$key_df[,-1], files[1], row.names = F, sep = "\t", quote = F)
    write.table(export_std()$summary_df, files[2], row.names = F, sep = "\t", quote = F, col.names = F)
    write.table(dass_result()$results_df, files[3], quote = F, row.names = F, sep = "\t")
    
    zip(con, files)
  },
  contentType = "application/zip"
)

outputOptions(output, "downloadres_zip", suspendWhenHidden = FALSE)

dass_result_bl <- eventReactive(input$confirm_run_dass, {
  # Borderline -----
  req(wf() == "bl")

  data_input <- data_input()
  
  ## KE1 -----
  ke1_c <- data_input$ke1$ke1_bl_c_dep$converted_values
  ke1_k <- data_input$ke1$ke1_bl_k_dep$converted_values
  ke1_c_only <- data_input$ke1$ke1_bl_c_only$converted_values
  ke1_mean <- (ke1_c + ke1_k)/2
  ke1_mean[ke1_c_only == "y"] <- NA
  ke1_c[ke1_c_only == "n"] <- NA
  ke1_out <- ke1Call(
    assay = input$ke1_bl_assay,
    mean_c_k_dep = ke1_mean,
    c_dep = ke1_c,
    borderline = T
  )
  ke1_out <- cbind(compound_id = data_input$ke1$ke1_bl_cid$values, ke1_out)
  
  ke1_overall <- unlist(as.list(by(ke1_out, ke1_out$compound_id, function(x) {
    if(all(is.na(x[["outcome"]]))) {
      return(NA)
    } else {
      if (nrow(x) == 1) {
        if (grepl("^BL|^Borderline", x[["outcome"]])) {
          return("Inconclusive")
        } else {
          return(x[["outcome"]])
        }
      } else {
        outcome_tmp <- gsub("^BL\\s+", "", x[["outcome"]])
        out_count <- unlist(table(outcome_tmp))
        out <- names(out_count)[out_count == max(out_count)]
        if (length(out) == 1) {
          return(out)
        } else {
          return("Inconclusive")
        }
      }
    }
  })))
  
  ke1_overall <- data.frame(compound_id = names(ke1_overall), ke1_overall, row.names = NULL)
  names(ke1_out)
  
  ke1_out <- ke1_out[,c("compound_id", "mean_c_k_dep", "c_dep", "outcome")]
  ke1_out$compound_id <- factor(ke1_out$compound_id)
  ke1_out$outcome <- gsub("^BL\\s+", "", ke1_out$outcome)
  ke1_out$outcome <- factor(ke1_out$outcome, levels = c("Positive", "Borderline", "Negative", "Inconclusive"))
  names(ke1_out) <- c("Compound ID", "Mean Depletion", "Cys/NAC Depletion", "Outcome")

  ## KE2 -----
  if (input$ke2_bl_assay == "ks") {
    ke2_out <- NULL
    ke2_overall <- data.frame(
      compound_id = data_input$ke2$ke2_bl_cid$values,
      ke2_overall = data_input$ke2$ke2_bl_ks_call$converted_values
    )
    
    ke2_overall <- by(ke2_overall, ke2_overall$compound_id, function(x) {
      if (nrow(x) == 1) {
        return(x)
      } else {
        tmp <- x[1,]
        tmp$ke2_overall <- NA
        return(tmp)
      }
    })
    
    ke2_overall <- do.call("rbind", ke2_overall)
    shinyjs::show("ks_run_text")
  } else if (input$ke2_bl_assay == "lusens") {
    lusens <- lapply(data_input$ke2, function(col_list) {
      if (is.null(col_list$converted_values)) {
        col_list$values
      } else {
        col_list$converted_values
      }
    })
    
    lusens <- data.frame(lusens)
    lusens <- split(lusens, list(lusens$ke2_bl_cid, lusens$ke2_bl_lusens_run), drop = T)
    
    ke2_out <- lapply(lusens, function(df) {
      data.frame(
        compound_id = df$ke2_bl_cid[1],
        run_id = df$ke2_bl_lusens_run[1],
        outcome_bl = lusensCall(
          conc = df$ke2_bl_lusens_conc,
          fold_induction = df$ke2_bl_lusens_fi,
          relative_viability = df$ke2_bl_lusens_cv,
          ttest_p = df$ke2_bl_lusens_pval
        )
      )
    })
    ke2_out <- do.call("rbind", ke2_out)
    
    ke2_overall <- unlist(as.list(by(ke2_out, ke2_out$compound_id, function(x) {
      if(all(is.na(x[["outcome_bl"]]))) {
        return(NA)
      } else {
        if (nrow(x) > 1) {
          out_count <- unlist(table(x[["outcome_bl"]]))
          out <- names(out_count)[out_count == max(out_count)]
          if (length(out) == 1) {
            return(out)
          } else {
            return("Inconclusive")
          }
        }
      }
    })))
    ke2_overall <- data.frame(compound_id = names(ke2_overall), ke2_overall, row.names = NULL)
    
    ke2_out$compound_id <- factor(ke2_out$compound_id)
    ke2_out$outcome_bl  <- factor(ke2_out$outcome_bl, levels = c("Positive", "Negative", "Borderline", "Inconclusive", "Invalid"))
    names(ke2_out) <- c("Compound ID", input$ke2_bl_lusens_run, "Outcome")
    shinyjs::hide("ks_run_text")
  }
  
  ## KE3 -----
  ke3 <- lapply(data_input$ke3, function(col_list) {
    if (is.null(col_list$converted_values)) {
      col_list$values
    } else {
      col_list$converted_values
    }
  })
  ke3 <- data.frame(ke3)
  if (input$ke3_bl_assay == "gard") {
    ke3_out <- data.frame(
      ke3$ke3_bl_cid,
      ke3$ke3_bl_gard_meanDV,
      gardskinCall(ke3$ke3_bl_gard_meanDV)
    )
    names(ke3_out) <- c("Compound ID", input$ke3_bl_gard_meanDV, "Outcome")
  } else {
    ke3 <- split(ke3, list(ke3$ke3_bl_run, ke3$ke3_bl_cid), drop = T)
    if(input$ke3_bl_assay == "hclat") {
      ke3_out <- lapply(ke3, function(df) {
        data.frame(
          compound_id = df$ke3_bl_cid[1],
          run_id = df$ke3_bl_run[1],
          outcome_bl = hclatCall(
            cd54 = df$ke3_bl_hclat_cd54,
            cd86 = df$ke3_bl_hclat_cd86,
            viability = df$ke3_bl_hclat_cv
          )
        )
      })
    } else if (input$ke3_bl_assay == "il8") {
      ws <- lapply(ke3, function(x) unique(x[,c("ke3_bl_cid", "ke3_bl_il8_ws")]))
      ws <- na.omit(do.call("rbind", ws))
      ke3_out <- lapply(ke3, function(df) {
        data.frame(
          compound_id = df$ke3_bl_cid[1],
          run_id = df$ke3_bl_run[1],
          outcome_bl = il8Call(
            ind_il8la = df$ke3_bl_il8_ind,
            ind_il8la_ll = df$ke3_bl_il8_indLCL,
            inh_gapla = df$ke3_bl_il8_inh,
            ws = ws$ke3_bl_il8_ws[ws$ke3_bl_cid == df$ke3_bl_cid[1]][1]
          )
        )
      })
    } else if (input$ke3_bl_assay == "usens") {
      ke3_out <- lapply(ke3, function(df) {
        data.frame(
          compound_id = df$ke3_bl_cid[1],
          run_id = df$ke3_bl_run[1],
          outcome_bl = usensCall(
            conc = df$ke3_bl_conc,
            viability = df$ke3_bl_usens_cv,
            si_cd86 = df$ke3_bl_usens_cd86
          )
        )
      })
    }
    
    ke3_out <- do.call("rbind", ke3_out)
    names(ke3_out) <- c("Compound ID", input$ke3_bl_run, "Outcome")
  }
  
  ke3_overall <- unlist(as.list(by(ke3_out, ke3_out[["Compound ID"]], function(x) {
    if(all(is.na(x[["Outcome"]]))) {
      return(NA)
    } else {
      n_req <- ifelse(input$ke3_bl_assay == "gard", 1, 2)
      if (nrow(x) >= n_req) {
        out_count <- unlist(table(x[[3]]))
        out <- names(out_count)[out_count == max(out_count)]
        if (length(out) == 1) {
          return(out)
        } else {
          return("Inconclusive")
        }
      } else {
        return(NA)
      }
    }
  })))
  ke3_overall <- data.frame(compound_id = names(ke3_overall), ke3_overall, row.names = NULL)
  
  ke3_out[["Compound ID"]] <- factor(ke3_out[["Compound ID"]])
  ke3_out[["Outcome"]] <- factor(ke3_out[["Outcome"]], levels = c("Positive", "Negative", "Borderline", "Inconclusive"))
  
  data_in <- merge(merge(ke1_overall, ke2_overall, by = "compound_id", all = T), ke3_overall, by = "compound_id", all = T)
  
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
    data.frame(compound_id = c_row[1], da_2o3 = out)
  })
  
  da_results <- do.call("rbind", da_results)
  da_results <- merge(data_in, da_results, by = "compound_id", all = T)
  da_results <- as.data.frame(lapply(da_results, as.factor))
  
  colnames(da_results) <- c(
    "Compound ID", 
    assay_dict[input$ke1_bl_assay],
    assay_dict[input$ke2_bl_assay],
    assay_dict[input$ke3_bl_assay],
    "DA 2o3 Hazard"
  )
  
  list(
    dass_results = da_results,
    ke1_run_outcome = ke1_out,
    ke2_run_outcome = ke2_out,
    ke3_run_outcome = ke3_out
  )
})

## Tables -----
output$results_bl <- renderDataTable({
  datatable(
    dass_result_bl()$dass_results,
    class     = "table-data stripe",
    rownames  = F,
    filter    = "top",
    selection = "none",
    options   = list(
      dom = "lrtip",
      initComplete = JS("() => updateDT('results_std')")
    ),
    callback = JS("tabBody(table);")
  )
})

output$results_ke1_bl <- renderDataTable({
  datatable(
    dass_result_bl()$ke1_run_outcome,
    class     = "table-data stripe",
    rownames  = F,
    filter    = "top",
    selection = "none",
    options   = list(
      dom = "lrtip",
      initComplete = JS("() => updateDT('results_std')")
    ),
    callback = JS("tabBody(table);")
  )
})

output$results_ke2_bl <- renderDataTable({
  datatable(
    dass_result_bl()$ke2_run_outcome,
    class     = "table-data stripe",
    rownames  = F,
    filter    = "top",
    selection = "none",
    options   = list(
      dom = "lrtip",
      initComplete = JS("() => updateDT('results_std')")
    ),
    callback = JS("tabBody(table);")
  )
})

output$results_ke3_bl <- renderDataTable({
  datatable(
    dass_result_bl()$ke3_run_outcome,
    class     = "table-data stripe",
    rownames  = F,
    filter    = "top",
    selection = "none",
    options   = list(
      dom = "lrtip",
      initComplete = JS("() => updateDT('results_std')")
    ),
    callback = JS("tabBody(table);")
  )
})

output$downloadres_bl_xl <- downloadHandler(
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
    # Selection summary
    ke1_tmp <- rbind(
      data.frame(
        Endpoint = c("KE1 Assay", "KE1 Worksheet"),
        `Selected Column` = c(assay_dict[[input$ke1_bl_assay]], input$ke1_bl_ws, use.names = F),
        Flagged = NA,
        check.names = F
      ), dt_review_bl$ke1)
    
    ke2_tmp <- rbind(data.frame(
      Endpoint = c("KE2 Assay", "KE2 Worksheet"),
      `Selected Column` = c(assay_dict[[input$ke2_bl_assay]], input$ke2_bl_ws, use.names = F),
      Flagged = NA,
      check.names = F
    ), dt_review_bl$ke2)
    
    ke3_tmp <- rbind(data.frame(
      Endpoint = c("KE3 Assay", "KE3 Worksheet"),
      `Selected Column` = c(assay_dict[[input$ke3_bl_assay]], input$ke3_bl_ws, use.names = F),
      Flagged = NA,
      check.names = F
    ), dt_review_bl$ke3)
    
    summary_df <- data.frame(
      Endpoint = c("DASS App", "Date Run", "", "Required Endpoint"),
      `Selected Column` = c("v2.0", format.Date(Sys.Date()), "", "Selection"),
      Flagged = c(rep("", 3), "Flagged"),
      check.names = F
    )
    summary_df <- rbind(rbind(rbind(summary_df, ke1_tmp), ke2_tmp), ke3_tmp)
    
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = "Summary")
    writeData(wb, sheet = "Summary", summary_df, colNames = F)
    
    addWorksheet(wb, sheetName = "KE1 Run Outcomes")
    writeData(wb, sheet = "KE1 Run Outcomes", dass_result_bl()$ke1_run_outcome)
    
    if (!is.null(dass_result_bl()$ke2_run_outcome)) {
      addWorksheet(wb, sheetName = "KE2 Run Outcomes")
      writeData(wb, sheet = "KE2 Run Outcomes", dass_result_bl()$ke2_run_outcome)
    }
    
    addWorksheet(wb, sheetName = "KE3 Run Outcomes")
    writeData(wb, sheet = "KE3 Run Outcomes", dass_result_bl()$ke3_run_outcome)
    
    addWorksheet(wb, sheetName = "DA Results")
    writeData(wb, sheet = "DA Results", dass_result_bl()$dass_results)
    activeSheet(wb) <- "DA Results"
    
    saveWorkbook(wb = wb, file = con)
  }
)

outputOptions(output, "downloadres_bl_xl", suspendWhenHidden = FALSE)

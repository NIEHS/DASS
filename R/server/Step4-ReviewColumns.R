# REVIEW SELECTIONS =====
## STANDARD -----
### Reactives -----
ke1_calc_mean <- reactiveVal(F)
ke1_get_call <- reactiveVal(F)
data_select <- reactiveVal()
dt_review <- reactiveVal()

### Dictionaries -----
# Column IDs by type
check_call_cols <- c("ke1_call_col", "ke2_call_col", "ke3_call_col", "insil_call_col")
check_numeric_cols <- c("ke1_mean_c_l_dep_col", "ke1_c_dep_col", "ke1_l_dep_col", "ke2_val_col")
check_ke3_cols <- "ke3_val_col"
check_ad_cols <- "insil_ad_col"

# Strings for pattern matching
call0_str <- c("0", "i", "inactive", "n", "neg", "negative", "non-sensitizer", "non-sensitiser", "nonsensitizer", "nonsensitiser")
call1_str <- c("1", "a", "active", "p", "pos", "positive", "sensitizer", "sensitiser")
ke3_0_str <- c("NI", "Inf", "i", "inactive", "n", "neg", "negative", "non-sensitizer", "non-sensitiser", "nonsensitizer", "nonsensitiser")

### Review -----
observeEvent(input$review_entries, {

  req(!blr())
  
  # Get required information sources
  col_req <- switch(input$selected_da,
                    da_2o3 = c("ke2_call_col", "ke3_call_col"),
                    da_its = c("ke3_val_col", "insil_call_col", "insil_ad_col"),
                    da_ke31 = c("ke3_val_col"))
  
  if ((input$selected_da %in% c("da_2o3", "da_ke31")) & !input$ke1_call_interpret) {
    ke1_get_call(F)
    col_req <- c(col_req, "ke1_call_col")
  } else if ((input$selected_da %in% c("da_2o3", "da_ke31")) & input$ke1_call_interpret) {
    ke1_get_call(T)
    if (input$ke1_choose_dep) {
      ke1_calc_mean(T)
      col_req <- c(col_req, "ke1_c_dep_col", "ke1_l_dep_col")
    } else if (!input$ke1_choose_dep) {
      ke1_calc_mean(F)
      col_req <- c(col_req, "ke1_mean_c_l_dep_col")
    }
  } else if(input$selected_da == "da_its") {
    if (input$ke1_choose_dep) {
      ke1_calc_mean(T)
      col_req <- c(col_req, "ke1_c_dep_col", "ke1_l_dep_col")
    } else if (!input$ke1_choose_dep) {
      ke1_calc_mean(F)
      col_req <- c(col_req, "ke1_mean_c_l_dep_col")
    }
  }
  
  no_select <- all(sapply(col_req, function(x) input[[x]] != ""))
  if (!no_select) {
    showNotification(
      type = "error",
      ui = "Missing required column selections.",
      duration = 10
    )
  }
  req(no_select)
  
  data_select <- data_select_template
  col_req <- as.character(sort(factor(unique(col_req), levels = names(data_select))))
  
  call_cols <- col_req[col_req %in% check_call_cols]
  if (length(call_cols) > 0) {
    for (i in call_cols) {
      col_name <- input[[i]]
      data_select[[i]][["col_name"]] <- col_name

      vals <- dt_analyze()[[col_name]]
      data_select[[i]][["values"]] <- vals

      converted_values <- rep(NA, length(vals))
      converted_values[grepl_ci(concatOrString(call1_str), vals)] <- 1
      converted_values[grepl_ci(concatOrString(call0_str), vals)] <- 0
      data_select[[i]][["converted_values"]] <- converted_values

      data_select[[i]][["flagged"]] <- !all(is.na(vals[is.na(converted_values)]))
    }
  }
  
  numeric_cols <- col_req[col_req %in% check_numeric_cols]
  if (length(numeric_cols) > 0) {
    for (i in numeric_cols) {
      col_name <- input[[i]]
      data_select[[i]][["col_name"]] <- col_name

      vals <- dt_analyze()[[col_name]]
      data_select[[i]][["values"]] <- vals

      converted_values <- suppressWarnings(as.numeric(vals))
      data_select[[i]][["converted_values"]] <- converted_values

      data_select[[i]][["flagged"]] <- !all(is.na(vals[is.na(converted_values)]))
    }
  }
  
  if (check_ke3_cols %in% col_req) {
    col_name <- input[[check_ke3_cols]]
    data_select[[check_ke3_cols]][["col_name"]] <- col_name

    vals <- dt_analyze()[[col_name]]
    data_select[[check_ke3_cols]][["values"]] <- vals

    converted_values <- rep(NA, length(vals))
    isNeg <- grepl_ci(concatOrString(ke3_0_str), vals)
    converted_values[isNeg] <- Inf
    converted_values[!isNeg] <- suppressWarnings(as.numeric(vals[!isNeg]))
    data_select[[check_ke3_cols]][["converted_values"]] <- converted_values

    data_select[[check_ke3_cols]][["flagged"]] <- !all(is.na(vals[is.na(converted_values)]))
  }
  
  if (check_ad_cols %in% col_req) {
    col_name <- input[[check_ad_cols]]
    data_select[[check_ad_cols]][["col_name"]] <- col_name

    vals <- dt_analyze()[[col_name]]
    data_select[[check_ad_cols]][["values"]] <- vals

    converted_values <- rep(NA, length(vals))
    converted_values[grepl_ci(concatOrString(c("1", "in")), vals)] <- 1
    converted_values[grepl_ci(concatOrString(c("0", "out")), vals)] <- 0
    data_select[[check_ad_cols]][["converted_values"]] <- converted_values

    data_select[[check_ad_cols]][["flagged"]] <- !all(is.na(vals[is.na(converted_values)]))
  }
  
  data_select(data_select)

  dt_review <- lapply(data_select[col_req], function(x) x[c("display_name", "col_name", "flagged")])
  dt_review <- do.call("rbind.data.frame", dt_review)
  names(dt_review) <- c("Endpoint", "Selected Column", "Flagged")

  dt_review(dt_review)

  updateTabsetPanel(inputId = "step_set", selected = "Review Selection")
  showHide(show = c("review_contents_ui", "review_contents"), hide = "review_contents_blr")
})

output$review_contents_standard_ui <- renderUI({
  
  ui <- list(
    select_list = NULL,
    dupe_warn = NULL,
    flag_warn = NULL,
    dt_review = dataTableOutput("dt_review", width = "fit-content"),
    br()
  )

  select_summary <- list(
    tags$li(tags$b("Selected DA: "), abbrev[input$selected_da]),
    tags$li(tags$b("Input File: "), ifelse(input$use_demo_data, "Demo Data", input$fpath$name)),
    ke1_tmp = NULL,
    ke3_tmp = NULL
  )
  
  if ((input$selected_da == "da_2o3" & input$ke1_call_interpret) | input$selected_da == "da_its") {
    select_summary$ke1_tmp <- tags$li(tags$b("KE1 Assay: "), abbrev[input$ke1_assay_name])
  }
  
  if (input$selected_da == "da_its") {
    select_summary$ke3_tmp <- tags$li(tags$b("KE3 Assay: "), abbrev[input$ke3_assay_name])
  }
  
  ui$select_list <- tags$ul(select_summary)

  if (any(duplicated(dt_review()$`Selected Column`))) {
    ui$dupe_warn <- p(strong(class = "warningText", "Warning: Identical column assigned to more than one endpoint."))
  }
  
  if (any(dt_review()$Flagged)) {
    ui$flag_warn <- p(strong(class = "warningText", "Warning: Selected data columns have been flagged for invalid values. Invalid values will not be evaluated in the DASS."))
  }
  tagList(ui)
})

### Table -----
output$dt_review <- DT::renderDataTable({
  datatable(dt_review(),
            class = "table-bordered table-condensed table-plain",
            rownames = F,
            escape = F,
            selection = "none",
            options = list(
              columnDefs = list(list(className = "dt-head-center", targets = "_all")),
              rowCallback = JS("styleWarnRow"),
              dom = "t",
              ordering = F
            ),
            callback = JS("$('#dt_review .dataTables_scrollBody').each((i, e) => e.setAttribute('tabIndex', 0))"))
})

## BORDERLINES -----
### Reactives -----
dt_ke_blr_data <- reactiveVal()

### -----
observeEvent(input$review_entries, {
  req(blr())
  
  # Get required columns
  ke_blr_cols <- list(ke1 = ke1_blr_id[[input$ke1_blr_assay_name]], 
                      ke2 = ke2_blr_id[[input$ke2_blr_assay_name]], 
                      ke3 = ke3_blr_id[[input$ke3_blr_assay_name]])

  # Error
  no_blank <- all(sapply(unlist(ke_blr_cols, use.names = F), function(x) input[[x]] != ""))
  if (!no_blank) {
    showNotification(
      type = "error",
      ui = "Missing required column selections.",
      duration = 10
    )
  }
  req(no_blank)
  
  ke_blr_data <- list(
    worksheetSelection = list(
      fname = input$fpath$name,
      ke1_worksheet = input$ke1_blr_ws,
      ke2_worksheet = input$ke2_blr_ws,
      ke3_worksheet = input$ke3_blr_ws
    )
  )

  ke_blr_data$col_data <- Map(function(kei_cols_list, kei) {
    # Get worksheet identifier
    ws_varname <- sprintf("%s_blr_ws", kei)
    
    # Prepare data by type
    list_by_type <- Map(function(col_vector, ctype) {
      Map(function(cname) {
        col_data <- list(
          col_name = input[[cname]],
          values = blrSheets()[[input[[ws_varname]]]][[input[[cname]]]],
          converted_values = NULL,
          flagged = NULL
        )
        
        if (ctype %in% c("conc_column", "numeric_columns")) {
          
          col_data$converted_values <- suppressWarnings(as.numeric(col_data$values))
          col_data$flagged <- !is.na(col_data$values) & is.na(col_data$converted_values)
          
        } else if (ctype %in% c("yn_nomiss_columns", "yn_miss_columns")) {
          
          converted_values <- rep(NA, length(col_data$values))
          converted_values[grepl_ci(concatOrString(c("y", "yes", "1")), col_data$values)] <- "y"
          converted_values[grepl_ci(concatOrString(c("n", "no", "0")), col_data$values)] <- "n"
          col_data$converted_values <- converted_values
          col_data$flagged <- !is.na(col_data$values) & is.na(converted_values)
          
        } else if (ctype == "ks_pn_column") {
          
          col_data$converted_values <- ifelse(col_data$values %in% c("POSITIVE", "NEGATIVE", "BORDERLINE"), col_data$values, NA)
          col_data$flagged <- !is.na(col_data$values) & is.na(col_data$converted_values)
        }
        
        return(col_data)
      }, col_vector)
    }, kei_cols_list, names(kei_cols_list))
    
    do.call("c", unname(list_by_type))
    
  }, ke_blr_cols, names(ke_blr_cols))

  # FLAGS
  cid <- Map(function(col_data, kei) {
    cid <- col_data[[sprintf("%s_blr_cid_col", kei)]]$values
    tmp <- data.frame(cid = unique(na.omit(cid)))
    tmp[[kei]] <- "x"
    return(tmp)
  }, ke_blr_data$col_data, names(ke_blr_data$col_data))

  cid <- Reduce(function(x, y) merge(x, y, by="cid", all = T), cid)
  cid_ke_sum <- rowSums(cid[,-1] == "x", na.rm = T)
  cid$flagged <- cid_ke_sum < 2
  names(cid) <- c("Chemical Identifier", "KE1 Worksheet", "KE2 Worksheet", "KE3 Worksheet", "Flagged")
  
  ke_blr_data$col_data <- lapply(ke_blr_data$col_data, function(kei) {
    kei[[1]]$flagged <- kei[[1]]$values %in% cid$`Chemical Identifier`[cid$Flagged]
    return(kei)
  })

  flags <- lapply(ke_blr_data$col_data, function(kei) {
    tmp <- Map(function(ke_col, ke_col_name) {
      list(
        required_endpoint = column_text_labels[ke_col_name],
        selected_column = ke_col$col_name,
        flagged = any(ke_col$flagged)
      )
    }, kei, names(kei))
    tmp <- do.call("rbind.data.frame", tmp)

    rownames(tmp) <- NULL
    colnames(tmp) <- c("Required Endpoint", "Selected Column", "Flagged")
    
    return(tmp)
  })
  
  flags$ke1$Flagged[flags$ke1$`Required Endpoint` == "Chemical Identifier"]
  
  
  
  flags$cid <- cid
  ke_blr_data$flags <- flags
  
  dt_ke_blr_data(ke_blr_data)
  
  updateTabsetPanel(inputId = "step_set", selected = "Review Selection")
  showHide(show = c("review_contents_ui", "review_contents_blr"), hide = "review_contents")
})

output$review_contents_blr_ui <- renderUI({
  ke_blr_data <- dt_ke_blr_data()
  
  tagList(
    tags$details(
      open = "open",
      tags$summary("General"),
      tags$ul(
        tags$li(tags$b("Selected DA: "), "2o3"),
        tags$li(tags$b("Analysis Type: "), "Borderline"),
        tags$li(tags$b("Input File: "), ke_blr_data$worksheetSelection$fname)
      )
    ),
    tags$details(
      open = "open",
      tags$summary("KE1 Assay"),
      tags$ul(
        tags$li(tags$b("Selected Assay: "), abbrev[input$ke1_blr_assay_name]),
        tags$li(tags$b("Selected KE1 Worksheet: "), ke_blr_data$worksheetSelection$ke1_worksheet)
      ),
      dataTableOutput("dt_review_blr_ke1", width = "fit-content")
    ),
    tags$details(
      open = "open",
      tags$summary("KE2 Assay"),
      tags$ul(
        tags$li(tags$b("Selected Assay: "), abbrev[input$ke2_blr_assay_name]),
        tags$li(tags$b("Selected KE2 Worksheet: "), ke_blr_data$worksheetSelection$ke2_worksheet)
      ),
      dataTableOutput("dt_review_blr_ke2", width = "fit-content")
    ),
    tags$details(
      open = "open",
      tags$summary("KE3 Assay"),
      tags$ul(
        tags$li(tags$b("Selected Assay: "), abbrev[input$ke3_blr_assay_name]),
        tags$li(tags$b("Selected KE3 Worksheet: "), ke_blr_data$worksheetSelection$ke3_worksheet)
      ),
      dataTableOutput("dt_review_blr_ke3", width = "fit-content")
    ),
    tags$details(
      open = "open",
      tags$summary("Chemical Identifiers"),
      tags$ul(
        tags$li(tags$b("Number of Unique Identifiers: "), nrow(ke_blr_data$flags$cid)),
        tags$li(tags$b("Number of Flagged Identifiers: "),sum(ke_blr_data$flags$cid$Flagged))
      ),
      dataTableOutput("dt_review_blr_cid", width = "fit-content")
    )
  )
})

output$dt_review_blr_cid <- renderDataTable({
  datatable(dt_ke_blr_data()$flags$cid,
            class = "table-bordered table-condensed table-plain",
            rownames = F,
            escape = F,
            selection = "none",
            options = list(
              columnDefs = list(list(className = "dt-head-center", targets = "_all")),
              rowCallback = JS("styleWarnRow"),
              dom = "t",
              ordering = F
            ))
})

output$dt_review_blr_ke1 <- renderDataTable({
  datatable(dt_ke_blr_data()$flags$ke1,
            class = "table-bordered table-condensed table-plain",
            rownames = F,
            escape = F,
            selection = "none",
            options = list(
              columnDefs = list(list(className = "dt-head-center", targets = "_all")),
              rowCallback = JS("styleWarnRow"),
              dom = "t",
              ordering = F
            ))
})

output$dt_review_blr_ke2 <- renderDataTable({
  datatable(dt_ke_blr_data()$flags$ke2,
            class = "table-bordered table-condensed table-plain",
            rownames = F,
            escape = F,
            selection = "none",
            options = list(
              columnDefs = list(list(className = "dt-head-center", targets = "_all")),
              rowCallback = JS("styleWarnRow"),
              dom = "t",
              ordering = F
            ))
})

output$dt_review_blr_ke3 <- renderDataTable({
  datatable(dt_ke_blr_data()$flags$ke3,
            class = "table-bordered table-condensed table-plain",
            rownames = F,
            escape = F,
            selection = "none",
            options = list(
              columnDefs = list(list(className = "dt-head-center", targets = "_all")),
              rowCallback = JS("styleWarnRow"),
              dom = "t",
              ordering = F
            ))
})
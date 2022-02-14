# =============================================================================#
# File Name: Step3.R                                                           #
# Original Creator: ktto                                                       #
# Contact Information: comptox@ils-inc.com                                     #
# Date Created: 2021-02-10                                                     #
# License: MIT                                                                 #
# Description: server object for Step 3 of  DASS app.                          #
# Required Packages:                                                           #
# - data.table, DT                                                             #
# - openxlsx                                                                   #
# - readxl                                                                     #
# - shiny shinyBS shinyjs                                                      #
# =============================================================================#


# Step 3: Review Selections -----
# Labels for displayed table
end_labels <- list(
  dpra_call = "DPRA Hazard Identification",
  dpra_pC = "DPRA %C-Depletion",
  dpra_pK = "DPRA %K-Depletion",
  hclat_call = "h-CLAT Hazard Identification",
  hclat_mit = "h-CLAT MIT",
  ks_call = "KeratinoSens&trade; Hazard Identification",
  ks_imax = "KeratinoSens&trade; iMax",
  oecd_tb_call = "In Silico Hazard Identification",
  oecd_tb_ad = "In Silico Applicability Domain"
)

# Flags for review
end_flags <- list(
  dpra_call = "Must be '0', 'n', 'neg', or 'negative' to indicate negative outcomes and '1', 'p', 'pos', or 'positive' to indicate positive outcomes.",
  dpra_pC = "Must be numeric",
  dpra_pK = "Must be numeric",
  hclat_call = "Must be '0', 'n', 'neg', or 'negative' to indicate negative outcomes and '1', 'p', 'pos', or 'positive' to indicate positive outcomes.",
  hclat_mit = "Must be 'n', 'neg', 'negative', or 'Inf' to indicate negative outcomes and numeric for positive outcomes.",
  ks_call = "Must be '0', 'n', 'neg', or 'negative' to indicate negative outcomes and '1', 'p', 'pos', or 'positive' to indicate positive outcomes.",
  ks_imax = "Must be numeric",
  oecd_tb_call = "Must be '0', 'n', 'neg', or 'negative' to indicate negative outcomes and '1', 'p', 'pos', or 'positive' to indicate positive outcomes.",
  oecd_tb_ad = "Must be '0' or 'out' for chemicals outside the applicability domain and '1' or 'in' for chemicals in the applicability domain."
)

observeEvent(input$review_entries, {
  
  # Get selected columns
  col_summary <- reactiveValuesToList(dt_col_select)
  
  # Check that all variables have a column assigned
  cols_to_check <- check_cols(
    dass = dass_choice(),
    ks_call_method = input$ks_choice,
    dpra_call_method = input$dpra_call_choice
  )
  cols_to_check <- unlist(col_summary[cols_to_check])
  col_blank <- any(cols_to_check == "")
  if (col_blank) showNotification(type = "error", ui = "Missing required columns.", duration = 10)
  req(!col_blank)
  
  # List of formatted data
  dt_list <- list()
  
  # Get data from selected columns
  col_dict <- dat_for_anlz$col_dict <- col_summary[names(cols_to_check)]
  col_vec <- unlist(col_dict, use.names = F)
  col_data <- usr_dt()[, .SD, .SDcols = col_vec]
  setnames(col_data, old = col_vec, new = names(col_dict))
  
  col_flags <- vector(mode = "list", length = ncol(col_data))
  names(col_flags) <- names(col_data)
  
  # Check call columns
  call_cols <- c("ks_call", "dpra_call", "hclat_call", "oecd_tb_call")
  if (any(names(col_data) %in% call_cols)) {
    call_col_names <- names(col_data)[names(col_data) %in% call_cols]
    # Data can be entered as:
    # Positive: 1, Pos, Positive, P
    # Negative: 0, Neg, Negative, N
    #  Count number of entries per column that have invalid values
    call_check <- col_data[, lapply(.SD, function(x) {
      !(grepl_ci("^1$|^0$|^p$|^n$|^pos$|^neg$|^positive$|^negative$", x) | is.na(x))
    }), .SDcols = call_col_names][, lapply(.SD, sum), .SDcols = call_col_names]
    call_check_id <- which(call_check > 0)
    if (length(call_check_id) > 0) {
      col_flags[names(call_check)[call_check_id]] <- 1
    }
    # Replace positive with 1 and negative with 0
    dt_list$call_cols <- col_data[, lapply(.SD, function(x) {
      fcase(
        grepl_ci("^1$|^p$|^pos$|^positive$", x), 1,
        grepl_ci("^0$|^n$|^neg$|^negative$", x), 0
      )
    }), .SDcols = call_col_names]
  }
  
  # Check numeric columns
  num_cols <- c("dpra_pC", "dpra_pK", "ks_imax")
  if (any(names(col_data) %in% num_cols)) {
    num_col_names <- names(col_data)[names(col_data) %in% num_cols]
    # Values must be numeric
    num_check <- col_data[, lapply(.SD, function(x) {
      # Value provided, but it is not numeric
      any(!is.na(x) & is.na(suppressWarnings(as.numeric(x))))
    }), .SDcols = num_col_names]
    num_check <- unlist(num_check)
    
    num_check_id <- which(num_check)
    if (length(num_check_id) > 0) {
      col_flags[num_col_names[num_check_id]] <- 1
    }
    
    dt_list$num_cols <- col_data[, lapply(.SD, function(x) suppressWarnings(as.numeric(x))),
                                 .SDcols = num_col_names
    ]
  }
  
  # Check h-CLAT MIT
  if (any(names(col_data) == "hclat_mit")) {
    mit_check <- col_data[, .(hclat_mit, hclat_mit_num = suppressWarnings(as.numeric(hclat_mit)), flag = T)]
    
    # Can have numeric or 'negative'
    # Remove flags for valid values
    mit_check[is.na(hclat_mit), flag := F]
    mit_check[grepl_ci("^n$|^neg$|^negative$|^Inf$", hclat_mit), flag := F]
    mit_check[!is.na(hclat_mit_num), flag := F]
    
    if (any(mit_check[, flag])) {
      col_flags$hclat_mit <- 1
    }
    
    mit_check <- mit_check[,.(hclat_mit = fcase(
      !is.na(hclat_mit_num), hclat_mit_num,
      grepl_ci("^n$|^neg$|^negative$|^Inf$", hclat_mit), Inf,
      is.na(hclat_mit_num), as.numeric(NA)
    ))]
    
    dt_list$mit_col <- mit_check
  }
  
  # Check applicability domain
  if (any(names(col_data) == "oecd_tb_ad")) {
    ad_check <- col_data[, oecd_tb_ad]
    # Can be 0, 1, in, or out
    ad_check <- !(grepl_ci("^1$|^0$|^in$|^out$", ad_check) | is.na(ad_check))
    
    if (any(ad_check)) {
      col_flags$oecd_tb_ad <- 1
    }
    
    dt_list$ad_col <- col_data[, .SD, .SDcols = "oecd_tb_ad"][, .(oecd_tb_ad = fcase(
      grepl_ci("^1$|^in$", oecd_tb_ad), 1,
      grepl_ci("^0$|^out$", oecd_tb_ad), 0
    ))]
  }
  
  col_flags <- lapply(col_flags, function(x) fifelse(is.null(x), 0, 1))
  names(dt_list) <- NULL
  updateCollapse(session,
                 id = "panels",
                 close = "panel_col_options"
  )
  updateCollapse(session,
                 id = "panels",
                 open = "panel_review"
  )
  show("review_contents")
  dt_anlz <- do.call("cbind", dt_list)
  dat_for_anlz$col_data <- dt_anlz
  dt_review <- data.table(
    Variable = names(col_dict),
    `Selected Column` = unlist(col_dict, use.names = F),
    Flag = unlist(col_flags, use.names = F)
  )
  
  dt_review[, Flag := fcase(
    Flag == 0, "",
    Flag == 1, unlist(end_flags[Variable])
  )]
  dt_review[, Variable := unlist(end_labels[Variable], use.names = F)]
  dt_review(dt_review)
})

output$dt_review <- renderDataTable({
  dt_review <- dt_review()
  flag_row <- which(dt_review$Flag != "")
  dt_review <- datatable(dt_review,
                         class = "table-bordered",
                         rownames = FALSE,
                         escape = FALSE,
                         selection = "none",
                         options = list(
                           dom = "t",
                           ordering = F
                         )
  )
  
  if (length(flag_row) > 0) {
    dt_review <- formatStyle(dt_review, 0, target = "row", color = styleRow(flag_row, rep("#D55E00", length = length(flag_row))))
    review_label(paste(
      "<p style='color:#D55E00; font-weight:bold;'>Warning:",
      "Selected data columns have been flagged for invalid values.</p>",
      "<p>Review the selected columns and flags in the table below.",
      "Upload an updated dataset or select new columns.<br><br>",
      "Click 'Run' to run DASS anyway. Invalid values will be",
      "considered missing (NA) and will <b>not</b> be used to evaluate",
      "skin sensitization hazard identification or potency.</p><br>"
    ))
    flagged(1)
  } else {
    review_label("<p>Review the selected columns and click 'Run' to run DASS.</p><br>")
    flagged(0)
  }
  
  dt_review
})

output$review_label <- renderText({
  review_label()
})

observeEvent(input$run_dass, {
  req(flagged())
  if (flagged() == 1) {
    showModal(modalDialog(
      title = NULL,
      footer = NULL,
      HTML(
        "<p>The selected columns have been flagged for invalid values. Invalid",
        "values will be considered missing (NA) and will <b>not</b> be used",
        "to evaluate skin sensitization hazard identification or potency. Continue?</p>"
      ),
      actionButton(inputId = "run_with_flags", label = "Run"),
      actionButton(inputId = "cancel_run", label = "Cancel")
    ))
  } else if (flagged() == 0) {
    run_dass()
  }
})

observeEvent(input$run_with_flags, {
  run_dass()
  removeModal()
})

observeEvent(input$cancel_run, {
  removeModal()
})
# =============================================================================#
# File Name: Step4-ReviewColumns.R
# Original Creator: ktto
# Contact Information: comptox@ils-inc.com
# Date Created: 2021-02-10
# License: MIT
# Description: server file for module with selection review
# Required Packages:
# - data.table, DT
# - shiny shinyBS shinyjs
# =============================================================================#

# Step 4: Review Selections -----
## Reactive Values -----
# tracks selected column names to prevent duplicate column selection
# col_select_input <- reactiveValues()

# formatted data
dat_for_anlz <- reactiveValues(col_data = NULL,
                               col_dict = NULL)

# table with selected columns for user to review
dt_review <- reactiveVal()

# indicator to trigger popup warning if running with flagged columns
flagged <- reactiveVal()

# text shown during review of selected columns
review_label <- reactiveVal()

# text shown if duplicates selected
dupe_label <- reactiveVal()

## Globals -----
# Labels for displayed table
end_labels <- list(
  dpra_call = "DPRA Hazard Call",
  dpra_pC = "DPRA %C-Depletion",
  dpra_pK = "DPRA %K-Depletion",
  hclat_call = "h-CLAT Hazard Call",
  hclat_mit = "h-CLAT MIT",
  ks_call = "KeratinoSens&trade; Hazard Call",
  ks_imax = "KeratinoSens&trade; iMax",
  insilico_call = "In Silico Hazard Call",
  insilico_ad = "In Silico Applicability Domain"
)

# Flags for review
end_flags <- list(
  dpra_call = "Must be '0', 'i', 'inactive', 'n', 'neg', or 'negative' to indicate inactive calls and '1', 'a', 'active', 'p', 'pos', or 'positive' to indicate active calls.",
  dpra_pC = "Must be numeric",
  dpra_pK = "Must be numeric",
  hclat_call = "Must be '0', 'i', 'inactive', 'n', 'neg', or 'negative' to indicate inactive calls and '1', 'a', 'active', 'p', 'pos', or 'positive' to indicate active calls.",
  hclat_mit = "Must be 'Inf', 'i', 'inactive', 'n', 'neg', or 'negative' to indicate inactive calls and numeric for active calls.",
  ks_call = "Must be '0', 'i', 'inactive', 'n', 'neg', or 'negative' to indicate inactive calls and '1', 'a', 'active', 'p', 'pos', or 'positive' to indicate active calls.",
  ks_imax = "Must be numeric",
  insilico_call = "Must be '0', 'i', 'inactive', 'n', 'neg', or 'negative' to indicate inactive calls and '1', 'a', 'active', 'p', 'pos', or 'positive' to indicate active calls.",
  insilico_ad = "Must be '0' or 'out' for chemicals outside the applicability domain and '1' or 'in' for chemicals in the applicability domain."
)

## Check columns -----
observeEvent(input$review_entries, {
  # Get selected columns
  col_summary <- list(
    dpra_call = input$dpra_call_col,
    dpra_pC = input$dpra_pC_col,
    dpra_pK = input$dpra_pK_col,
    hclat_call = input$hclat_call_col,
    hclat_mit = input$hclat_mit_col,
    ks_call = input$ks_call_col,
    ks_imax = input$ks_imax_col,
    insilico_call = input$insilico_call_col,
    insilico_ad = input$insilico_ad_col
  )
  # Check that all variables have a column assigned
  cols_to_check <- check_cols(
    dass = dass_choice(),
    ks_call_method = input$ks_choice,
    dpra_call_method = input$dpra_call_choice
  )
  # Maintain expected order. Depends on labels used in check_cols
  cols_to_check <- as.character(sort(factor(cols_to_check, levels = names(col_summary))))
  cols_to_check <- unlist(col_summary[cols_to_check])
  col_blank <- any(cols_to_check == "")
  if (col_blank) showNotification(type = "error", ui = "Missing required columns.", duration = 10)
  req(!col_blank)
  
  # List of formatted data
  dt_list <- list()
  # Create named list mapping selected column to new variable name
  col_dict <- dat_for_anlz$col_dict <- col_summary[names(cols_to_check)]
  col_vec <- unlist(col_dict, use.names = F)
  col_data <- usr_dt()[, .SD, .SDcols = col_vec]
  
  # setnames(col_data, old = col_vec, new = names(col_dict))
  # setnames can't handle duplicate selections
  names(col_data) <- names(col_dict)
  
  # Check for duplicate selections
  if (any(duplicated(col_vec))) {
    dupe_label(paste(
      "<p style='color:#D55E00; font-weight:bold;'>Warning:",
      "Single column assigned to more than one variable.",
      "</p>"
    ))
  }

  col_flags <- vector(mode = "list", length = ncol(col_data))
  names(col_flags) <- names(col_data)
  
  # Check call columns
  call_cols <- c("ks_call", "dpra_call", "hclat_call", "insilico_call")
  if (any(names(col_data) %in% call_cols)) {
    call_col_names <- names(col_data)[names(col_data) %in% call_cols]
    # Data can be entered as:
    # Positive: 1, Pos, Positive, P
    # Negative: 0, Neg, Negative, N
    #  Count number of entries per column that have invalid values
    call_check <- col_data[, lapply(.SD, function(x) {
      !(grepl_ci("^1$|^0$|^p$|^n$|^pos$|^neg$|^positive$|^negative$|^a$|^i$|^active$|^inactive$|^sensitizer$|^sensitiser$|^non-sensitizer$|^non-sensitiser$", x) | is.na(x))
    }), .SDcols = call_col_names][, lapply(.SD, sum), .SDcols = call_col_names]
    call_check_id <- which(call_check > 0)
    if (length(call_check_id) > 0) {
      col_flags[names(call_check)[call_check_id]] <- 1
    }
    # Replace positive with 1 and negative with 0
    dt_list$call_cols <- col_data[, lapply(.SD, function(x) {
      fcase(
        grepl_ci("^1$|^p$|^pos$|^positive$|^a$|^active$|^sensitizer$|^sensitiser$", x), 1,
        grepl_ci("^0$|^n$|^neg$|^negative$|^i$|^inactive$|^non-sensitizer$|^non-sensitiser$", x), 0
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
    mit_check[grepl_ci("^n$|^neg$|^negative$|^Inf$|^i$|^inactive$|^non-sensitizer$|^non-sensitiser$", hclat_mit), flag := F]
    mit_check[!is.na(hclat_mit_num), flag := F]
    
    if (any(mit_check[, flag])) {
      col_flags$hclat_mit <- 1
    }
    
    mit_check <- mit_check[,.(hclat_mit = fcase(
      !is.na(hclat_mit_num), hclat_mit_num,
      grepl_ci("^n$|^neg$|^negative$|^Inf$|^i$|^inactive$|^non-sensitizer$|^non-sensitiser$", hclat_mit), Inf,
      is.na(hclat_mit_num), as.numeric(NA)
    ))]
    
    dt_list$mit_col <- mit_check
  }
  
  # Check applicability domain
  if (any(names(col_data) == "insilico_ad")) {
    ad_check <- col_data[, insilico_ad]
    # Can be 0, 1, in, or out
    ad_check <- !(grepl_ci("^1$|^0$|^in$|^out$", ad_check) | is.na(ad_check))
    
    if (any(ad_check)) {
      col_flags$insilico_ad <- 1
    }
    
    dt_list$ad_col <- col_data[, .SD, .SDcols = "insilico_ad"][, .(insilico_ad = fcase(
      grepl_ci("^1$|^in$", insilico_ad), 1,
      grepl_ci("^0$|^out$", insilico_ad), 0
    ))]
  }
  
  col_flags <- lapply(col_flags, function(x) fifelse(is.null(x), 0, 1))
  names(dt_list) <- NULL
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
                         class = "table-bordered stripe",
                         callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
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

output$dupe_label <- renderText({
  dupe_label()
})
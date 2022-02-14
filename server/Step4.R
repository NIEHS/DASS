# =============================================================================#
# File Name: Step4.R                                                           #
# Original Creator: ktto                                                       #
# Contact Information: comptox@ils-inc.com                                     #
# Date Created: 2021-02-10                                                     #
# License: MIT                                                                 #
# Description: server object for Step 4 of  DASS app.                          #
# Required Packages:                                                           #
# - data.table, DT                                                             #
# - openxlsx                                                                   #
# - readxl                                                                     #
# - shiny shinyBS shinyjs                                                      #
# =============================================================================#


# Step 4: Results -----
run_dass <- reactive({
  ####
  # Workaround to give dass_predict the right ks and dpra methods
  # Example of issue:
  # In app - select only KE 3/1 STS. Select a column for DPRA Hazard ID
  # Go back to step 1. Deselect KE3/1 STS. Select ITS. Fill dropdowns
  # and run DASS. dpra_call_choice is not NULL so throws an error
  # within dass_predict
  
  if (any(c("da_2o3", "da_ke31") %in% dass_choice())) {
    dpra_method <- input$dpra_call_choice
  } else {
    dpra_method <- NULL
  }
  
  if ("da_2o3" %in% dass_choice()) {
    ks_method <- input$ks_choice
  } else {
    ks_method <- NULL
  }
  ####
  
  da_out <- dass_predict(
    dt = dat_for_anlz$col_data,
    dass = dass_choice(),
    ks_call_method = ks_method,
    dpra_call_method = dpra_method
  )
  
  # Reorder columns
  col_order <- c("dpra_call", "dpra_pC", "dpra_pK",
                 "dpra_mean_calculated", "dpra_call_calculated",
                 "hclat_call", "hclat_mit", 
                 "ks_imax", "ks_call", "ks_call_calculated", 
                 "oecd_tb_call", "oecd_tb_ad",
                 "ITS_hCLAT_Score", "ITS_DPRA_Score", "ITS_OECDQSARTB_Score", 
                 "ITS_TotalScore", "DA_ITS_Call", "DA_ITS_Potency", 
                 "DA_2o3_Call", 
                 "DA_KE31STS_Call", "DA_KE31STS_Potency")
  
  col_match <- na.omit(match(col_order, names(da_out)))
  da_out <- da_out[,..col_match]
  
  # Convert h-CLAT MIT to character so that Inf renders correctly
  if ("hclat_mit" %in% colnames(da_out)) {
    da_out[,hclat_mit := sprintf("%.2f", hclat_mit)]
  }
  
  new_col_names <- c("DPRA Hazard Id. Input",
                     "DPRA %-C Depletion Input",
                     "DPRA %-K Depletion Input",
                     "DPRA Mean (Calculated)",
                     "DPRA Hazard Id. Input (Calculated)",
                     "h-CLAT Hazard Id. Input",
                     "h-CLAT MIT Input",
                     "Keratinosens(TM) iMax Input",
                     "Keratinosens(TM) Hazard Id. Input",
                     "Keratinosens(TM) Hazard Id. Input (Calculated)",
                     "In Silico Hazard Id. Input",
                     "In Silico Applicability Domain Input",
                     "DA ITS h-CLAT Score",
                     "DA ITS DPRA Score",
                     "DA ITS in Silico Score",
                     "DA ITS Total Score",
                     "DA ITS Hazard Id.",
                     "DA ITS Potency",
                     "DA 2o3 Hazard Id.",
                     "DA KE 3/1 STS Hazard Id.",
                     "DA KE 3/1 STS Potency")
  col_match_new <- na.omit(match(names(da_out), col_order))
  new_col_names <- new_col_names[col_match_new]
  
  setnames(da_out,
           old = colnames(da_out),
           new = new_col_names)
  
  dass_res(da_out)
  updateCollapse(session,
                 id = "panels",
                 close = "panel_review"
  )
  updateCollapse(session,
                 id = "panels",
                 open = "panel_results"
  )
  show("result_contents")
})

output$dt_results <- renderDataTable({
  dass_res <- dass_res()
  if (is.null(dass_res)) {
    datatable(NULL)
  } else {
    res <- cbind(usr_dt(), dass_res)
    # Set up columns to color
    da_sty <- grep("^DA .*", names(dass_res), value = T)
    da_font <- grep("Hazard|Potency", da_sty, value = T)
    in_sty <- grep("^DA .*", names(dass_res), value = T, invert = T)
    col_sty <- dt_review()[,`Selected Column`]
    
    datatable(
      res,
      class = "table-bordered",
      rownames = FALSE,
      selection = "none",
      options = list(
        scrollX = TRUE,
        rowCallback = JS(rowCallback)
      )
    ) %>%
      formatStyle(
        columns = da_sty,
        backgroundColor = "#56B4E9"
      ) %>% 
      formatStyle(
        columns = da_font,
        fontWeight = "bold"
      ) %>% 
      formatStyle(
        columns = in_sty,
        backgroundColor = "#CC79A7"
      ) %>% 
      formatStyle(
        columns = col_sty,
        backgroundColor = "#F0E442"
      )
  }
  
  
})

## Save -----
create_file <- reactive({
  # Create excel workbook
  wb <- createWorkbook()
  
  # Add worksheets
  addWorksheet(wb, sheetName = "Key")
  addWorksheet(wb, sheetName = "Column Selection")
  addWorksheet(wb, sheetName = "Results")
  
  # Styles
  orange_font <- createStyle(fontColour = "#D55E00")
  bold_font <- createStyle(textDecoration = "bold")
  blue_bg <- createStyle(fgFill = "#56B4E9", halign = "right")
  pink_bg <- createStyle(fgFill = "#CC79A7", halign = "right")
  yellow_bg <- createStyle(fgFill = "#F0E442")
  bold_blue <- createStyle(textDecoration = "bold", fgFill = "#56B4E9", halign = "right")
  
  # Create key worksheet
  key_df <- data.frame(
    Color = c("Blue", "Pink", "Yellow"),
    Label = c("Defined approach result",
              "Transformed versions of user-selected columns. These are the values used as input in the DASS",
              "User-selected columns")
  )
  writeData(wb, sheet = "Key", key_df, headerStyle = bold_font)
  addStyle(wb, sheet = "Key", style = blue_bg, row = 2, col = 1)
  addStyle(wb, sheet = "Key", style = pink_bg, row = 3, col = 1)
  addStyle(wb, sheet = "Key", style = yellow_bg, row = 4, col = 1)
  
  # Create column selection worksheet
  # Column review table
  col_select <- dt_review()
  # Replace html
  col_select[,Variable := gsub("&trade;", "(TM)", Variable)]
  writeData(wb, sheet = "Column Selection", x = col_select, headerStyle = bold_font)
  setColWidths(wb, sheet = "Column Selection", cols = 1:ncol(col_select), widths = "auto")
  # Get row IDs to highlight
  flag_row <- col_select[,which(Flag != "")]
  if (length(flag_row) > 0) {
    flag_row <- flag_row + 1
    addStyle(wb, sheet = "Column Selection", style = orange_font,
             rows = flag_row, cols = 1:ncol(col_select), gridExpand = T)
  }
  
  # Create results worksheet
  dass_res <- dass_res()
  res <- cbind(usr_dt(), dass_res)
  writeData(wb, sheet = "Results", x = res, headerStyle = bold_font, keepNA = TRUE, na.string = "NA")
  # setColWidths(wb, sheet = "Results", cols = 1:ncol(res), widths = "auto")
  
  # Highlight any ITS scores
  score_col <- grep("^DA.*Score$", colnames(dass_res), value = T)
  if (length(score_col) > 0) {
    score_col <- na.omit(match(score_col, colnames(res)))
    addStyle(wb, sheet = "Results", style = blue_bg,
             rows = 2:(nrow(res) + 1), cols = score_col, gridExpand = T)
  }
  
  # Highlight columns with DA results
  dass_res_cols <- grep("^DA.*Hazard|^DA.*Potency", colnames(dass_res), value = TRUE)
  dass_cols <- na.omit(match(dass_res_cols, colnames(res)))
  addStyle(wb, sheet = "Results", style = bold_blue,
           rows = 2:(nrow(res) + 1), cols = dass_cols, gridExpand = T)
  
  # Highlight columns used as input
  in_cols <- grep("^DA ", colnames(dass_res), value = TRUE, invert = T)
  in_cols <- na.omit(match(in_cols, colnames(res)))
  
  addStyle(wb, sheet = "Results", style = pink_bg,
           rows = 2:(nrow(res) + 1), cols = in_cols, gridExpand = T)
  
  # Highlight columns that user selected
  usr_cols <- dt_review()[,`Selected Column`]
  usr_cols <- na.omit(match(usr_cols, colnames(res)))
  addStyle(wb, sheet = "Results", style = yellow_bg,
           rows = 2:(nrow(res) + 1), cols = usr_cols, gridExpand = T)
  activeSheet(wb) <- "Results"
  wb
})

output$downloadres <- downloadHandler(
  filename = function() {
    fname <- unlist(strsplit(input$fpath$name, "[.]"))
    fname <- paste(fname[-length(fname)], collapse = ".")
    paste0(fname, "_DASSResults_", Sys.Date(), ".xlsx")
  },
  content = function(con) {
    saveWorkbook(wb = create_file(), file = con)
    # write.csv(x = dass_res(), file = con, quote = F, row.names = F)
  }
)

# =============================================================================#
# File Name: Step5-Results.R
# Original Creator: ktto
# Contact Information: ICE-support@niehs.nih.gov
# Date Created: 2021-02-10
# License: MIT
# Description: server file for module with dass results.
# Required Packages:
# - data.table, DT
# - openxlsx
# - shiny shinyBS shinyjs
# =============================================================================#

# Step 5: Results -----
## Confirm Run -----
observeEvent(input$run_dass, {
  req(flagged())
  if (flagged() == 1) {
    toggleModal(session = session, modalId = "confirm_run_with_flag", toggle = "open")
  } else if (flagged() == 0) {
    run_dass()
  }
})

observeEvent(input$run_with_flags, {
  run_dass()
  toggleModal(session = session, modalId = "confirm_run_with_flag", toggle = "close")
})

observeEvent(input$cancel_run, {
  toggleModal(session = session, modalId = "confirm_run_with_flag", toggle = "close")
})

# dass_results <- reactiveVal()
dass_res_template <- list(
  da_2o3 = list(
    KE1_call_BL = NULL,
    KE2_call_BL = NULL,
    KE3_call_BL = NULL,
    hazard = NULL,
    hazard_BL = NULL
  ),
  da_its = list(
    KE1_score = NULL,
    KE3_score = NULL,
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

all_out <- reactiveValues(
  result_df = NULL,
  user_selected = NULL,
  dass_input = NULL,
  dass_results = NULL,
  to_hide = NULL
)

run_dass <- reactive({
  data_select <- data_select()
  dass_res <- dass_res_template
  
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
    data_select$ke1_call_col$display_name <- paste(
      data_select$ke1_call_col$display_name, "(Calculated)"
    )
    data_select$ke1_call_col$converted_values <- 
      ke1Call(
        assay = input$ke1_assay_name,
        mean_C_L_dep = data_select$ke1_mean_c_l_dep_col$converted_values,
        C_dep = data_select$ke1_c_dep_col$converted_values,
        L_dep = data_select$ke1_l_dep_col$converted_values
      )$hazard
  }

  if (input$do_da_2o3) {
    tmp_2o3 <- da2o3(
      assayA_call = data_select$ke1_call_col$converted_values,
      assayB_call = data_select$ke2_call_col$converted_values,
      assayC_call = data_select$ke3_call_col$converted_values
    )
    
    dass_res$da_2o3$hazard <- tmp_2o3$hazard
    
    # if (input$do_da_2o3_BL) {
    #   dass_res$da_2o3$KE1_call_BL <- ke1BL(input$ke1_assay_name, 
    #                                        call = data_select$ke1_call_col$converted_values,
    #                                        mean_c_l_dep = data_select$ke1_mean_c_l_dep_col$converted_values,
    #                                        c_dep = data_select$ke1_c_dep_col$converted_values)
    #   dass_res$da_2o3$KE2_call_BL <- ke2BL(input$ke2_assay_name,
    #                                        call = data_select$ke2_call_col$converted_values,
    #                                        value = data_select$ke2_val_col$converted_values)
    #   
    #  
    # }
  }
  
  if (input$do_da_its) {
    tmp_its <- daITS(
      ke1_assay = input$ke1_assay_name,
      ke1_mean_c_l_dep = data_select$ke1_mean_c_l_dep_col$converted_values, 
      ke1_c_dep = data_select$ke1_c_dep_col$converted_values,
      ke3_assay = input$ke3_assay_name, 
      ke3_value = data_select$ke3_val_col$converted_values,
      insil_prediction = data_select$insil_call_col$converted_values, 
      insil_ad = data_select$insil_ad_col$converted_values
    )
    dass_res$da_its <- list(
      ke1_score = tmp_its$ke1_score,
      ke3_score = tmp_its$ke3_score,
      insil_score = tmp_its$insil_score,
      total_score = tmp_its$total_score,
      hazard = tmp_its$hazard,
      potency = tmp_its$potency)
  }
  
  if (input$do_da_ke31) {
    tmp_ke31 <- daKE31(
      ke1_call = data_select$ke1_call_col$converted_values,
      ke3_value = data_select$ke3_val_col$converted_values
    )
    dass_res$da_ke31 <- list(
      hazard = tmp_ke31$hazard,
      potency = tmp_ke31$potency
    )
  }
  
  by_da <- sapply(dass_res, function(x) !is.null(x))
  dass_res <- lapply(dass_res[by_da], function(x) {
    by_col <- sapply(x, function(y) !is.null(y))
    as.data.frame(x[by_col])
  })
  dass_res <- dass_res <- do.call("c", dass_res)
  dass_res <- as.data.frame(dass_res)
  # dass_results(dass_res)
  
  da_input <- sapply(data_select, function(x) !is.null(x$converted_values))
  da_input <- lapply(data_select[da_input], function(x) {
    out <- list(x$converted_values)
    names(out) <- paste0(x$col_name, "_input")
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
  
  shinyjs::show("result_contents")
  shinyjs::enable("tab_results")
  updateTabsetPanel(inputId = "step_set", selected = "Results")
  
})

output$dt_results <- renderDataTable({
  req(all_out$result_df)

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
        list(extend = "colvisGroup", text = "Show All Columns", show = ":hidden")
        )
    )
  )
  dt_results <- formatStyle(dt_results, columns = all_out$user_selected, backgroundColor = "#F0E442")
  dt_results <- formatStyle(dt_results, columns = all_out$dass_input, backgroundColor = "#CC79A7")
  dt_results <- formatStyle(dt_results, columns = all_out$dass_results, backgroundColor = "#56B4E9")

  i <- length(dt_results$x$options$columnDefs)
  dt_results$x$options$columnDefs[[i+1]] <- list(
    targets = all_out$to_hide - 1,
    visible = F
  )
  
  rc <- dt_results$x$options$rowCallback
  rc <- unlist(strsplit(as.character(rc), '\n'))
  dt_results$x$options$rowCallback <- JS(append(rc, after = length(rc) - 1, "showNA(row, data);"))
  dt_results
  
})

### Save -----
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
    write.table(x = all_out$result_df, file = con, quote = F, row.names = F, sep = "\t")
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

# ### Run DASS -----
# run_dass <- reactive({
#   ####
#   # Workaround to give dass_predict the right ks and dpra methods
#   # Example of issue:
#   # In app - select only KE 3/1 STS. Select a column for DPRA Hazard ID
#   # Go back to step 1. Deselect KE3/1 STS. Select ITS. Fill dropdowns
#   # and run DASS. dpra_call_choice is not NULL so throws an error
#   # within dass_predict
#   
#   if (any(c("da_2o3", "da_ke31") %in% dass_choice())) {
#     dpra_method <- input$dpra_call_choice
#   } else {
#     dpra_method <- NULL
#   }
#   
#   if ("da_2o3" %in% dass_choice()) {
#     ks_method <- input$ks_choice
#   } else {
#     ks_method <- NULL
#   }
#   ####
#   
#   da_out <- dass_predict(
#     dt = dat_for_anlz$col_data,
#     dass = dass_choice(),
#     # 23/03/07 - Removed KS Imax option. Code needs to be cleaned
#     # to remove imax from any evaluation
#     # ks_call_method = ks_method,
#     ks_call_method = "call",
#     dpra_call_method = dpra_method
#   )
#   # Reorder columns
#   col_order <- c("dpra_call", "dpra_pC", "dpra_pK",
#                  "dpra_mean_calculated", "dpra_call_calculated",
#                  "hclat_call", "hclat_mit", 
#                  "ks_imax", "ks_call", "ks_call_calculated", 
#                  "insilico_call", "insilico_ad",
#                  "ITS_hCLAT_Score", "ITS_DPRA_Score", "ITS_inSilico_Score", 
#                  "ITS_TotalScore", "DA_ITS_Call", "DA_ITS_Potency", 
#                  "DA_2o3_Call", 
#                  "DA_KE31STS_Call", "DA_KE31STS_Potency")
#   
#   col_match <- na.omit(match(col_order, names(da_out)))
#   da_out <- da_out[,..col_match]
#   
#   # Convert h-CLAT MIT to character so that Inf renders correctly
#   if ("hclat_mit" %in% colnames(da_out)) {
#     da_out[,hclat_mit := sprintf("%.2f", hclat_mit)]
#   }
#   
#   new_col_names <- c("DPRA Call Input",
#                      "DPRA %-C Depletion Input",
#                      "DPRA %-K Depletion Input",
#                      "DPRA Mean (Calculated)",
#                      "DPRA Call Input (Calculated)",
#                      "h-CLAT Call Input",
#                      "h-CLAT MIT Input",
#                      "Keratinosens(TM) iMax Input",
#                      "Keratinosens(TM) Call Input",
#                      "Keratinosens(TM) Call Input (Calculated)",
#                      "In Silico Call Input",
#                      "In Silico Applicability Domain Input",
#                      "DA ITS h-CLAT Score",
#                      "DA ITS DPRA Score",
#                      "DA ITS in Silico Score",
#                      "DA ITS Total Score",
#                      "DA ITS Call",
#                      "DA ITS Potency",
#                      "DA 2o3 Call",
#                      "DA KE 3/1 STS Call",
#                      "DA KE 3/1 STS Potency")
#   col_match_new <- na.omit(match(names(da_out), col_order))
#   new_col_names <- new_col_names[col_match_new]
#   
#   setnames(da_out,
#            old = colnames(da_out),
#            new = new_col_names)
#   
#   res_merged <- cbind(dt_analyze(), da_out)
#   # Set up columns to style
#   da_sty <- grep("^DA .*", names(da_out), value = T)
#   da_font <- grep("Call|Potency", da_sty, value = T)
#   in_sty <- grep("^DA .*", names(da_out), value = T, invert = T)
#   col_sty_old <- unique(dt_review()[,`Selected Column`])
#   col_sty <- paste0(col_sty_old, "*")
#   setnames(res_merged, old = col_sty_old, new = col_sty)
# 
#   dass_res$results <- res_merged
#   dass_res$user_select <- col_sty
#   dass_res$da_input <- in_sty
#   dass_res$da_output <- da_sty
# 
#   shinyjs::show("result_contents")
#   shinyjs::show("performanceUI")
#   updateTabsetPanel(inputId = "stepSet", selected = "Results")
#   
#   compareSelects <- c("binaryRefSelect", "binaryIdentifier")
#   
#   updateSelectInput(
#     inputId = "binaryRefSelect",
#     choices = names(dt_analyze())
#   )
#   updateSelectInput(
#     inputId = "binaryDASelect",
#     choices = grep("Call", da_sty, value = T)
#   )
#   
#   if (any(grepl("Potency", da_sty))) {
#     updateSelectInput(
#       inputId = "potencyRefSelect",
#       choices = names(dt_analyze())
#     )
#     updateSelectInput(
#       inputId = "potencyDASelect",
#       choices = grep("Potency", da_sty, value = T)
#     )
#     
#   }
#   
# })

create_xl_file <- reactive({
  wb <- createWorkbook()
  
  addWorksheet(wb, sheetName = "Key")
  addWorksheet(wb, sheetName = "Column Selection")
  addWorksheet(wb, sheetName = "Results")
  
  # Styles
  orange_font <- createStyle(fontColour = "#D55E00")
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
  
  writeData(wb, sheet = "Column Selection", x = dt_review(), headerStyle = bold_font)
  flag_rows <- which(dt_review()$Flagged == "FLAG")
  
  if (length(flag_rows) > 0) {
    flag_rows <- flag_rows + 1
    addStyle(wb, sheet = "Column Selection", style = orange_font,
             rows = flag_rows, cols = 1:ncol(dt_review()), gridExpand = T)
  }
  
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

# ## Save -----
# create_xl_file <- reactive({
# 
#   # Create excel workbook
#   wb <- createWorkbook()
# 
#   # Add worksheets
#   addWorksheet(wb, sheetName = "Key")
#   addWorksheet(wb, sheetName = "Column Selection")
#   addWorksheet(wb, sheetName = "Results")
# 
#   # Styles
#   orange_font <- createStyle(fontColour = "#D55E00")
#   bold_font <- createStyle(textDecoration = "bold")
#   blue_bg <- createStyle(fgFill = "#56B4E9", halign = "right")
#   pink_bg <- createStyle(fgFill = "#CC79A7", halign = "right")
#   yellow_bg <- createStyle(fgFill = "#F0E442")
#   bold_blue <- createStyle(textDecoration = "bold", fgFill = "#56B4E9", halign = "right")
# 

# 
#   writeData(wb, sheet = "Key", key_df, headerStyle = bold_font)
#   addStyle(wb, sheet = "Key", style = blue_bg, row = 4, col = 1)
#   addStyle(wb, sheet = "Key", style = pink_bg, row = 3, col = 1)
#   addStyle(wb, sheet = "Key", style = yellow_bg, row = 2, col = 1)
# 
#   # Create column selection worksheet
#   # Column review table
#   col_select <- dt_review()
#   # Replace html
#   col_select[,Variable := gsub("&trade;", "(TM)", Variable)]
#   writeData(wb, sheet = "Column Selection", x = col_select, headerStyle = bold_font)
#   # causes issues when downloading twice:
#   # setColWidths(wb, sheet = "Column Selection", cols = 1:ncol(col_select), widths = "auto")
#   # Get row IDs to highlight
#   flag_row <- col_select[,which(Flag != "")]
#   if (length(flag_row) > 0) {
#     flag_row <- flag_row + 1
#     addStyle(wb, sheet = "Column Selection", style = orange_font,
#              rows = flag_row, cols = 1:ncol(col_select), gridExpand = T)
#   }
# 
#   # Create results worksheet
#   res <- dass_res$results
#   writeData(wb, sheet = "Results", x = res, headerStyle = bold_font, keepNA = TRUE, na.string = "NA")
# 
#   usr_cols <- na.omit(match(dass_res$user_select, colnames(res)))
#   addStyle(wb, sheet = "Results", style = yellow_bg,
#            rows = 2:(nrow(res) + 1), cols = usr_cols, gridExpand = T)
#   
#   in_cols <- na.omit(match(dass_res$da_input, colnames(res)))
#   addStyle(wb, sheet = "Results", style = pink_bg,
#            rows = 2:(nrow(res) + 1), cols = in_cols, gridExpand = T)
#   
#   dass_cols <- na.omit(match(dass_res$da_output, colnames(res)))
#   addStyle(wb, sheet = "Results", style = blue_bg,
#            rows = 2:(nrow(res) + 1), cols = dass_cols, gridExpand = T)
# 
#   activeSheet(wb) <- "Results"
#   wb
# })
# 


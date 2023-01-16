# =============================================================================#
# File Name: Step2-UploadData.R                                                #
# Original Creator: ktto                                                       #
# Contact Information: comptox@ils-inc.com                                     #
# Date Created: 2021-02-10                                                     #
# License: MIT                                                                 #
# Description: server file that sets up reactive values and loads user data    #
# Required Packages:                                                           #
# - data.table, DT                                                             #
# - openxlsx                                                                   #
# - readxl                                                                     #
# - shiny shinyBS shinyjs                                                      #
# =============================================================================#

### for debugging
# observeEvent(input$browser,{
#   browser()
# })
###

# Selection Names -----
# IDs for column selection drop down menus
si_ids <- c(
  "dpra_call_col",
  "dpra_pC_col",
  "dpra_pK_col",
  "hclat_call_col",
  "hclat_mit_col",
  "ks_call_col",
  "ks_imax_col",
  "oecd_call_col",
  "oecd_ad_col"
)

# Formatting -----
# output tables will show "NA" instead of blanks
rowCallback <- c(
  "function(row, data) {",
  "for (var i=0; i<data.length; i++) {",
  "if(data[i]===null){",
  "$('td:eq('+i+')', row).html('NA')",
  ".css({'color': 'rgb(89,89,89)'});",
  "}",
  "}",
  "}"
)

# Reactive Values -----
# tracks selected column names to prevent duplicate column selection
col_select_input <- reactiveValues()

# selected tests ordered as: 2o3, its, ke 3/1 sts
dass_choice <- reactiveVal()

# DASS results
dass_res <- reactiveVal()

# formatted data
dat_for_anlz <- reactiveValues(col_data = NULL,
                               col_dict = NULL)

# list with user data column names
dt_col_select <- reactiveValues(
  dpra_call = "",
  dpra_pC = "",
  dpra_pK = "",
  hclat_call = "",
  hclat_mit = "",
  ks_call = "",
  ks_imax = "",
  oecd_tb_call = "",
  oecd_tb_ad = ""
)

# table with selected columns for user to review
dt_review <- reactiveVal()

# indicator to trigger popup warning if running with flagged columns
flagged <- reactiveVal()

# text shown during review of selected columns
review_label <- reactiveVal()

# user's uploaded data
usr_dt <- reactiveVal()

# Data Loading -----
# Once the user selects a file, load the data onto the page and
# show the defined approaches menu
observeEvent(input$button_upload, {
  # Check file extension
  ext <- unlist(strsplit(input$fpath$name, "[.]"))
  ext <- ext[length(ext)]
  if (!grepl("^csv$|^tsv$|^txt$|^xls$|^xlsx$", ext)) {
    showNotification(
      type = "error",
      "Incorrect file type. Accepted file extensions: csv, tsv, txt, xlsx",
      duration = 10
    )
  } else {
    # Reset reactive values
    if (!is.null(usr_dt())) {
      for (i in 1:length(si_ids)) {
        updateSelectInput(inputId = si_ids[i], selected = "")
      }
      dat_for_anlz$col_data <- NULL
      dat_for_anlz$col_dict <- NULL
      dt_col_select$dpra_call <- ""
      dt_col_select$dpra_pC <- ""
      dt_col_select$dpra_pK <- ""
      dt_col_select$hclat_call <- ""
      dt_col_select$hclat_mit <- ""
      dt_col_select$ks_call <- ""
      dt_col_select$ks_imax <- ""
      dt_col_select$oecd_tb_call <- ""
      dt_col_select$oecd_tb_ad <- ""
      dt_review(NULL)
      flagged(NULL)
      review_label(NULL)
      dass_res(NULL)
      output$step2ui <- renderUI({})
      hide("review_contents")
      hide("result_contents")
    }
    
    # Read in data
    dt <- read_data(input$fpath$datapath)
    usr_dt(dt)
    
    shinyjs::show("user_data_block")
    shinyjs::show("confirm_data")
  }
})

output$ae_req <- renderDataTable({
  tmp <- data.frame(
    `2o3` = c("X", "X", "X", "", "", "O", "O", "O"),
    ITS = c("", "", "", "X", "X", "X", "X", "X"),
    STS = c("X", "", "", "", "", "O", "O", "X"),
    Assay = c("DPRA", "hCLAT", "KeratinoSens", "In Silico Prediction",
              "In Silico Prediction", "DPRA", "DPRA", "hCLAT"),
    Endpoint = c(rep("Call", 4), "Applicability Domain", "%-Cystine Depletion",
                 "% Lysine Depletion", "Minimum Induction Threshold"),
    `Format Requirements` = c(
      rep("<ul><li>Active assay calls should be indicated by '1', 'p', 'pos', or 'positive'.</li>
      <li>Inactive assay calls should be indicated by '0', 'n', 'neg', or 'negative'.</li></ul>", 4),
      "<ul><li>Predictions within the applicability domain should be indicated by '1' or 'In'.</li><li>Predictions outside the applicability domain should be indicated by '0' or 'Out'. These will be omitted from analysis.</li></ul>",
      "<ul><li>Numeric values only.</li><li>No symbols.</li></ul>",
      "<ul><li>Numeric values only.</li><li>No symbols.</li></ul>",
      "<td><ul><li>For active hCLAT calls, numeric values only. No symbols.</li><li>Indicate inactive hCLAT calls with 'Inf', 'n', 'neg', or 'negative'</li></ul>"),
    check.names = F)
  datatable(tmp,
            class = "cell-border stripe hover compact",
            rownames = FALSE,
            options = list(
              dom = "t",
              autoWidth = TRUE,
              columnDefs = list(
                list(width = "1%", padding = "100px", targets = 0)
                )
              ), 
            escape = F)
})

output$usr_dt <- renderDataTable({
  req(usr_dt())
  datatable(usr_dt(),
            # selectize-input in step 2 won't work if filter argument
            # https://github.com/rstudio/shiny/issues/3125
            # filter = "top",
            class = "cell-border stripe hover",
            options = list(
              scrollY = TRUE,
              scrollX = TRUE,
              rowCallback = JS(rowCallback)
            )
  )
})
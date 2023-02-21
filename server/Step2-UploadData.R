# =============================================================================#
# File Name: Step2-UploadData.R
# Original Creator: ktto
# Contact Information: comptox@ils-inc.com
# Date Created: 2021-02-10
# License: MIT
# Description: server file that sets up reactive values and loads user data
# Required Packages:
# - data.table, DT
# - readxl
# - shiny shinyjs
# =============================================================================#

# Step 2: Upload Data -----
## Formatting -----
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

## Reactive Values -----
# user's uploaded data
usr_dt <- reactiveVal()

## Data Loading -----
# When user selects file using the browse button, it checks the extension.
# If the file is an excel workbook, the sheet names are loaded into a dropdown
# menu for the user to select.
observeEvent(input$fpath, {
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
    if (grepl("^xls$|^xlsx$", ext)) {
      sheets <- readxl::excel_sheets(input$fpath$datapath)
      updateSelectInput(inputId = "xlsheet", choices = sheets)
      shinyjs::show("xlsheet-label")
    }
  }
})

# User confirms file selection, and worksheet if needed.
observeEvent(input$button_upload, {
  req(input$fpath)
  # Check file extension. Same check run when user chooses file via 'browse'
  ext <- unlist(strsplit(input$fpath$name, "[.]"))
  ext <- ext[length(ext)]
  if (!grepl("^csv$|^tsv$|^txt$|^xls$|^xlsx$", ext)) {
    showNotification(
      type = "error",
      "Incorrect file type. Accepted file extensions: csv, tsv, txt, xlsx",
      duration = 10
    )
  } else {
    # Read in data
    dt <- read_data(input$fpath$datapath, sheet = input$xlsheet)
    usr_dt(dt)

    shinyjs::show("user_data_block")
    shinyjs::show("confirm_data")
  }
})

## Tables -----
# Table with data formatting requirements
output$ae_req <- renderDataTable({
  # Create table
  tmp <- data.table(
    `2o3` = c("X", "X", "X", " ", " ", "O", "O", "O", "O"),
    ITS = c(" ", " ", " ", "X", "X", "X", "X", "X", " "),
    STS = c("X", " ", " ", " ", " ", "O", "O", "X", " "),
    Assay = c("DPRA", "hCLAT", "KeratinoSens&trade;", "In Silico Prediction",
              "In Silico Prediction", "DPRA", "DPRA", "hCLAT", "KeratinoSens&trade;"),
    Endpoint = c(rep("Binary Call", 4), "Applicability Domain", "%-Cysteine Depletion",
                 "% Lysine Depletion", "Minimum Induction Threshold", "Imax"),
    `Format Requirements` = c(
      rep(
        "<ul><li>Positive outcomes should be indicated by 'sensitizer', 'active', 'a', 'positive', 'pos', 'p', or '1'. </li><li>Negative outcomes should be indicated by 'non-sensitizer', 'inactive', 'i', 'negative', 'neg', 'n', or '0'.</li></ul>",
        4),
      "<ul><li>Predictions within the applicability domain should be indicated by '1' or 'In'.</li><li>Predictions outside the applicability domain should be indicated by '0' or 'Out'. These will be omitted from analysis.</li></ul>",
      "<ul><li>Numeric values only.</li><li>No symbols.</li></ul>",
      "<ul><li>Numeric values only.</li><li>No symbols.</li></ul>",
      "<td><ul><li>For positive h-CLAT outcomes, numeric values only. No symbols.</li><li>Indicate negative h-CLAT outcomes with 'non-sensitizer', 'Inf', 'i', 'inactive', 'n', 'neg', or 'negative'.</li></ul>",
      "<ul><li>Numeric values only.</li><li>No symbols.</li></ul>"),
    check.names = F)
  tmp <- tmp[order(Assay, Endpoint)]
  coltmp <- c("2o3", "ITS", "STS", "Assay", "Endpoint")
  tmp[,(coltmp) := lapply(.SD, as.factor),.SDcols = coltmp]
  
  datatable(tmp,
            class = "table-bordered stripe",
            filter = "top",
            rownames = FALSE,
            # Callback to remove extra black lines DT prints at bottom of table.
            callback = JS(c("$('.dataTables_scrollBody').css('border-bottom', 'none');", "$('table.no-footer').css('border-bottom', 'none');")),
            caption = tags$caption(
              style = "caption-side: bottom; text.align:left;",
              "Columns 1-3 indicate the DAs that require a given endpoint",
              br(),
              "X = the DA requires the endpoint.",
              br(),
              "O = This endpoint can be used to derive a required call endpoint."
            ),
            selection = "none",
            options = list(
              dom = "t",
              scrollY = TRUE,
              scrollX = TRUE
              ), 
            escape = F)
})

jqui_resizable("#data_req_modal .modal-content")
jqui_draggable("#data_req_modal .modal-content")

# User data
output$usr_dt <- renderDataTable({
  req(usr_dt())
  datatable(usr_dt(),
            # selectize-input in step 2 won't work if filter argument
            # https://github.com/rstudio/shiny/issues/3125
            # filter = "top",
            class = "table-bordered stripe",
            # Callback to remove bottom border printed by DT
            callback = JS("$('.dataTables_scrollBody').css('border-bottom', 'none');"),
            options = list(
              scrollY = TRUE,
              scrollX = TRUE,
              rowCallback = JS(rowCallback)
            )
  )
})
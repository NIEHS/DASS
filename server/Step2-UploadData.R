# =============================================================================#
# File Name: Step2-UploadData.R
# Original Creator: ktto
# Contact Information: ICE-support@niehs.nih.gov
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
xlsheet <- reactiveVal()

## Data Loading -----
observeEvent(input$fpath, {
  # Clear and hide UI objects in case data were previously uploaded.
  usr_dt(NULL)
  shinyjs::hide("user_data_block")
  shinyjs::hide("confirm_data")
  shinyjs::hide("xlsheet_text_ui")
  
  # Check file extension
  ext <- unlist(strsplit(input$fpath$name, "[.]"))
  ext <- ext[length(ext)]
  if (!grepl("^csv$|^tsv$|^txt$|^xls$|^xlsx$", ext)) {
    showNotification(
      type = "error",
      "Incorrect file type. Accepted file extensions: csv, tsv, txt, xlsx",
      duration = 10
    )
  } else if (grepl("^xls$|^xlsx$", ext)) {
    sheets <- readxl::excel_sheets(input$fpath$datapath)
    if (length(sheets) == 1) {
      # Excel workbooks with only 1 sheet are automatically read in
      dt <- read_data(input$fpath$datapath, sheet = 1)
      usr_dt(dt)
      shinyjs::show("user_data_block")
      shinyjs::show("confirm_data")
    } else if (length(sheets) > 1) {
      # Render error (workaround to show error with easyclose)
      sheet_text_ui <- span(
        span(style = "font-weight:bold;", "Error: No Excel worksheet selected!"),
        actionLink(inputId = "button_choose_xlsheet", 
                   label = "Open worksheet selector"),
      )
      output$xlsheet_text_ui <- renderUI({
        sheet_text_ui
      })
      
      shinyjs::show("xlsheet_text_ui")
      
      # Show excel worksheet selector
      updateSelectInput(session, "xl_sheet_list", choices = sheets)
      toggleModal(session, "xl_select_modal", toggle = "open")
    }
  } else if (grepl("^csv$|^tsv$|^txt$", ext)) {
    # Data are automatically read in
    dt <- read_data(input$fpath$datapath, sheet = input$xlsheet)
    usr_dt(dt)
    
    shinyjs::show("user_data_block")
    shinyjs::show("confirm_data")
  }
})

# User confirms a worksheet in the modal
observeEvent(input$confirm_xl_sheet, {
  # Update UI to show selected worksheet
  sheet_text_ui <- span(
    span(style = "font-weight:bold;", "Selected Worksheet:"),
    input$xl_sheet_list,
    "(",
    actionLink(inputId = "button_change_xlsheet", 
                 label = "Change Selected Worksheet"),
    ")"
  )
  output$xlsheet_text_ui <- renderUI({
    sheet_text_ui
  })
  
  # Read in data and show
  dt <- read_data(input$fpath$datapath, sheet = input$xl_sheet_list)
  usr_dt(dt)
  shinyjs::show("user_data_block")
  shinyjs::show("confirm_data")
  shinyjs::show("xlsheet_text_ui")
  toggleModal(session, "xl_select_modal", toggle = "close")
})

# User selected a worksheet, but changes it.
observeEvent(input$button_change_xlsheet, {
  toggleModal(session, "xl_select_modal", toggle = "open")
})

# User does not select a worksheet
observeEvent(input$cancel_xl_sheet, {
  toggleModal(session, "xl_select_modal", toggle = "close")
})

# For case when user initially uploads xl file but doesn't select sheet.
observeEvent(input$button_choose_xlsheet, {
  toggleModal(session, "xl_select_modal", toggle = "open")
})

## Tables -----
# Table with data formatting requirements
output$ae_req <- renderDataTable({
  # Create table
  tmp <- data.table(
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
    `2o3` = c("X", "X", "X", " ", " ", "O", "O", "O", "O"),
    ITS = c(" ", " ", " ", "X", "X", "X", "X", "X", " "),
    STS = c("X", " ", " ", " ", " ", "O", "O", "X", " "),
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
              "Columns 4-6 indicate the DAs that require a given endpoint",
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


jqui_draggable("#data_req_modal .modal-content")
jqui_resizable("#data_req_modal .modal-content", options = list(handles = "e"))

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
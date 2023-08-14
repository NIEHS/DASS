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
## Reactive Values -----
# user's uploaded data
data_trigger <- reactiveVal()
usr_dt <- reactiveVal()
xlsheet <- reactiveVal()

resetApp_newData <- reactive({
  if (!is.null(usr_dt())) {
    # Clear and hide UI objects in case data were previously uploaded.
    usr_dt(NULL)
    data_trigger(NULL)
    xlsheet(NULL)
    dass_choice(NULL)
    dat_for_anlz$col_data <- dat_for_anlz$col_dict <- NULL
    dt_review(NULL)
    flagged(NULL)
    dass_res$results <- dass_res$user_select <- dass_res$da_input <- dass_res$da_output <- NULL
    
    shinyjs::runjs("resetHidden();")
    shinyjs::runjs("rmDPRAListener();")
  }
})

# Data Loading -----
observeEvent(input$fpath, {
  resetApp_newData()
  # Check file extension
  ext <- unlist(strsplit(input$fpath$name, "[.]"))
  ext <- ext[length(ext)]
  extvalid <- grepl("^csv$|^tsv$|^txt$|^xls$|^xlsx$", ext)
  
  if (!extvalid) {
    showNotification(
      type = "error",
      "Incorrect file type. Accepted file extensions: csv, tsv, txt, xlsx",
      duration = 10
    )
    req(extvalid)
  }
  
  if (grepl("^xls$|^xlsx$", ext)) {
    sheets <- readxl::excel_sheets(input$fpath$datapath)
    if (length(sheets) == 1) {
      xlsheet(1)
      data_trigger(1)
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
  }
  
  if (grepl("^csv$|^tsv$|^txt$", ext)) {
    # Data are automatically read in
    data_trigger(1)
  }
})

observeEvent(data_trigger(), {
  usr_dt(read_data(input$fpath$datapath, sheet = xlsheet()))
  shinyjs::runjs(sprintf("showScroll('%s', '%s', '%s', '%s')", "user_data_block_confirm", "label", "id", "fpath-label"))
}, ignoreNULL = T)

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
  
  xlsheet(input$xl_sheet_list)
  data_trigger(1)

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
# User data
output$usr_dt <- DT::renderDataTable({
  req(usr_dt())
  datatable(usr_dt(),
            class = "table-bordered stripe",
            options = list(
              scrollY = TRUE,
              scrollX = TRUE,
              rowCallback = JS("showNA")
            )
  )
})
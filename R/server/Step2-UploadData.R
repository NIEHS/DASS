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
## Tab -----
observeEvent(input$confirmDAs, {
  check_select <- all(!input$do_da_2o3 &
                        !input$do_da_its & !input$do_da_ke31)
  
  if (check_select) {
    showNotification(
      type = "error",
      ui = "No defined approaches selected.",
      duration = 10
    )
  } 
  req(!check_select)
  if (!is.null(dass_choice())) {
    shinyjs::runjs("resetHidden(false);")
    shinyjs::runjs("rmDPRAListener();")
  }

  updateTabsetPanel(inputId = "stepSet", selected = "Upload Data")
  shinyjs::runjs("$('#stepSet')[0].scrollIntoView();")
})

## Reactive Values -----
usr_dt <- reactiveVal()
dt_analyze <- reactiveVal()
demo_data <- reactiveVal()
xlsheet <- reactiveVal()

## Demo -----
observeEvent(input$useDemoData, {
  if (input$useDemoData) {
    demo_data(fread("www/dassAppDemoData-fromGL497Annex2.csv"))
    dt_analyze(demo_data())
    shinyjs::hide("uploadBlock")
  }
  
  if (!input$useDemoData) {
    shinyjs::show("uploadBlock")
    if (is.null(usr_dt())) {
      dt_analyze(NULL)
    } else {
      dt_analyze(usr_dt())
    }
  }
})

## Upload -----
load_data <- reactive({
  usr_dt(read_data(input$fpath$datapath, sheet = xlsheet()))
  dt_analyze(usr_dt())
})

observeEvent(input$fpath, {
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
      load_data()
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
    shinyjs::hide("xlsheet_text_ui")
    # Data are automatically read in
    load_data()
  }
}, ignoreInit = T)

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
  load_data()

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

observeEvent(dt_analyze(), {
  if (is.null(dt_analyze())) {
    shinyjs::hide("data_block")
  } else {
    shinyjs::show("data_block")
  }
  shinyjs::runjs("resetHidden(false);")
  shinyjs::runjs("rmDPRAListener();")
}, ignoreNULL = F)

## Tables -----
output$dt_analyze <- DT::renderDataTable({
  datatable(dt_analyze(),
            class = "table-bordered stripe",
            options = list(
              scrollY = TRUE,
              scrollX = TRUE,
              rowCallback = JS("showNA")
            ),
            callback = JS("$('#dt_analyze .dataTables_scrollBody').each((i, e) => e.setAttribute('tabIndex', 0))")
  )
})
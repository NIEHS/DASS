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
## Reactive Values -----
usr_dt <- reactiveVal()
dt_analyze <- reactiveVal()
demo_data <- reactiveVal()
xl_sheet <- reactiveVal()
numeric_data <- reactiveVal()

## Update Tab -----
observeEvent(input$confirm_da, {

  # if (!is.null(dass_choice())) {
  #   shinyjs::runjs("resetHidden(false);")
  #   shinyjs::runjs("rmDPRAListener();")
  # }

  updateTabsetPanel(inputId = "step_set", selected = "Upload Data")
  shinyjs::runjs("$('#step_set')[0].scrollIntoView();")
})

## Load Demo Data -----
observeEvent(input$use_demo_data, {
  if (input$use_demo_data) {
    demo_data(fread("www/dassAppDemoData-fromGL497Annex2.csv", na.strings = c("", "na", "NA")))
    dt_analyze(demo_data())

    numeric_data(lapply(dt_analyze(), function(x) {
      if (is.numeric(x)) x
    }))

    shinyjs::hide("upload_block")
  }

  if (!input$use_demo_data) {
    shinyjs::show("upload_block")
    if (is.null(usr_dt())) {
      dt_analyze(NULL)
      numeric_data(NULL)
    } else {
      dt_analyze(usr_dt())
      numeric_data(lapply(dt_analyze(), function(x) {
        check_num <- sum(grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}[eE]{0,1}[-]{0,1}[0-9]{0,1}$", x), na.rm = T)
        if (check_num >= 5) x
      }))
    }
  }
})

## Upload User Data -----
load_data <- reactive({
  usr_dt(read_data(input$fpath$datapath, sheet = xl_sheet()))
  dt_analyze(usr_dt())
})

observeEvent(input$fpath, {
  # Check file extension
  ext <- unlist(strsplit(input$fpath$name, "[.]"))
  ext <- ext[length(ext)]
  ext_valid <- grepl("^csv$|^tsv$|^txt$|^xls$|^xlsx$", ext)

  if (!ext_valid) {
    showNotification(
      type = "error",
      "Incorrect file type. Accepted file extensions: csv, tsv, txt, xlsx",
      duration = 10
    )
    req(ext_valid)
  }

  if (grepl("^xls$|^xlsx$", ext)) {
    sheets <- readxl::excel_sheets(input$fpath$datapath)
    if (length(sheets) == 1) {
      xl_sheet(1)
      load_data()
    } else if (length(sheets) > 1) {
      # Render error (workaround to show error with easyclose)
      sheet_text_ui <- span(
        span(style = "font-weight:bold;", "Error: No Excel worksheet selected!"),
        actionLink(inputId = "button_choose_xl_sheet",
                   label = "Open worksheet selector"),
      )
      output$xl_sheet_text_ui <- renderUI({
        sheet_text_ui
      })

      shinyjs::show("xl_sheet_text_ui")

      # Show excel worksheet selector
      updateSelectInput(session, "xl_sheet_list", choices = sheets)
      toggleModal(session, "xl_select_modal", toggle = "open")
    }
  }

  if (grepl("^csv$|^tsv$|^txt$", ext)) {
    shinyjs::hide("xl_sheet_text_ui")
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
    actionLink(inputId = "button_change_xl_sheet",
                 label = "Change Selected Worksheet"),
    ")"
  )
  output$xl_sheet_text_ui <- renderUI({
    sheet_text_ui
  })

  xl_sheet(input$xl_sheet_list)
  load_data()

  shinyjs::show("xl_sheet_text_ui")
  toggleModal(session, "xl_select_modal", toggle = "close")
})

# User selected a worksheet, but changes it.
observeEvent(input$button_change_xl_sheet, {
  toggleModal(session, "xl_select_modal", toggle = "open")
})

# User does not select a worksheet
observeEvent(input$cancel_xl_sheet, {
  toggleModal(session, "xl_select_modal", toggle = "close")
})

# For case when user initially uploads xl file but doesn't select sheet.
observeEvent(input$button_choose_xl_sheet, {
  toggleModal(session, "xl_select_modal", toggle = "open")
})

observeEvent(dt_analyze(), {
  if (is.null(dt_analyze())) {
    shinyjs::hide("data_block")
  } else {
    shinyjs::show("data_block")
  }
  shinyjs::runjs("resetHidden(false);")
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
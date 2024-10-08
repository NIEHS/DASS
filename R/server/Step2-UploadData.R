# UPLOAD DATA =====
## Reactive Values -----
blr <- reactiveVal(FALSE)
blrSheets <- reactiveVal()
usr_dt <- reactiveVal()
dt_analyze <- reactiveVal()
demo_data <- reactiveVal()

## Update Tab -----
observeEvent(input$confirm_da, {
  for (i in tabNames[3:6]) shinyjs::runjs(sprintf("resetHiddenTab('%s');", i))
  shinyjs::reset(id = "select_col_ui_all")
  shinyjs::reset(id = "compare_setup_standard")
  
  updateTabsetPanel(inputId = "step_set", selected = "Upload Data")
  shinyjs::show("upload_data_ui")
  
  if (input$selected_da == "da_2o3" & !is.null(input$do_da_2o3_bl)) {
    if (!blr()) { 
      dt_analyze(NULL)
      shinyjs::reset(id = "upload_data_ui")
    }
    blr(TRUE)
    showHide(
      show = c("upload_blr_data_text", "blr_data_worksheet_select_block", "upload_block"),
      hide = c("upload_data_text", "use_demo_data_cb")
    )
  } else {
    if (blr()) { 
      dt_analyze(NULL)
      shinyjs::reset(id = "upload_data_ui")
      shinyjs::reset(id = "do_da_2o3_bl")
    }
    blr(FALSE)
    showHide(
      show = c("upload_data_text", "use_demo_data_cb"),
      hide = c("upload_blr_data_text", "blr_data_worksheet_select_block")
    )
  }
})

## Upload User Data -----
observeEvent(input$fpath, {
  usr_dt(NULL)
  dt_analyze(NULL)
  
  # Check file extension
  ext <- unlist(strsplit(input$fpath$name, "[.]"))
  ext <- ext[length(ext)]

  if (input$selected_da == "da_2o3" & blr()) {
    ext_ok <- c("xls", "xlsx")
  } else {
    ext_ok <- c("csv", "tsv", "txt", "xls", "xlsx")
  }
  ext_valid <- grepl(paste(sprintf("^%s$", ext_ok), collapse = "|"), ext)

  if (!ext_valid) {
    showNotification(
      type = "error",
      sprintf("Invalid file type. Accepted file extensions: %s", paste(ext_ok, collapse = ", ")),
      duration = Inf
    )
    req(ext_valid)
  }
  
  if (blr()) {
    sheets <- readxl::excel_sheets(input$fpath$datapath)
    if (length(sheets) <= 2) {
      shinyjs::show(id = "blr_ws_warn")
    } else {
      shinyjs::hide(id = "blr_ws_warn")
    }
    updateSelectInput(inputId = "blr_data_worksheet_select", choices = sheets)

    blr_sheets <- lapply(sheets, function(sheet) {
      read_excel_dass(input$fpath$datapath, sheet = sheet)
    })
    names(blr_sheets) <- sheets
    blrSheets(blr_sheets)
    
    shinyjs::show("blr_data_worksheet_select")
    
  } else {
    if (grepl("^xls|^xlsx", ext)) {
      sheets <- readxl::excel_sheets(input$fpath$datapath)
      if (length(sheets) == 1) {
        updateSelectInput(inputId = "xl_sheet_list", selected = "1")
        usr_dt(read_excel_dass(fpath = input$fpath$datapath, sheet = 1))
        dt_analyze(usr_dt())
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

        # Show excel worksheet selector
        updateSelectInput(session, "xl_sheet_list", choices = sheets)
        toggleModal(session, "xl_select_modal", toggle = "open")
        
        shinyjs::show("xl_sheet_text_ui")
      }
    } 
      
    if (grepl("^csv$|^tsv$|^txt$", ext)) {
      usr_dt(data.frame(fread(input$fpath$datapath, colClasses = "character", na.strings = c("", "na", "NA"))))
      dt_analyze(usr_dt())
      
      shinyjs::hide("xl_sheet_text_ui")
    }
    
    shinyjs::hide("blr_data_worksheet_select")
  }
}, ignoreInit = TRUE)

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

  usr_dt(read_excel_dass(fpath = input$fpath$datapath, sheet = input$xl_sheet_list))
  dt_analyze(usr_dt())

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

# Borderline table selector
observeEvent(input$blr_data_worksheet_select, {
  req(blr())
  dt_analyze(blrSheets()[[input$blr_data_worksheet_select]])
})

## Tables -----
output$dt_analyze <- DT::renderDataTable({
  datatable(dt_analyze(),
            class = "table-bordered stripe",
            options = list(
              scrollY = TRUE,
              scrollX = TRUE,
              rowCallback = JS(sprintf("function(row, data) {%s}", showNA_js))
            ),
            callback = JS("$('#dt_analyze .dataTables_scrollBody').each((i, e) => e.setAttribute('tabIndex', 0))")
  )
})

observeEvent(dt_analyze(), {
  if (is.null(dt_analyze())) {
    shinyjs::hide("data_block")
  } else {
    shinyjs::show("data_block")
  }
  # shinyjs::runjs("resetHidden(false);")
}, ignoreNULL = F)

## Load Demo Data -----
observeEvent(input$use_demo_data, {
  if (input$use_demo_data) {
    if (is.null(demo_data())) {
      demo_data(data.frame(fread("www/DASS_demo_data.csv", na.strings = c("", "na", "NA"))))
    }
    
    dt_analyze(demo_data())
    shinyjs::hide("upload_block")
  }

  if (!input$use_demo_data) {
    shinyjs::show("upload_block")
    if (is.null(usr_dt())) {
      dt_analyze(NULL)
    } else {
      dt_analyze(usr_dt())
    }
  }
})
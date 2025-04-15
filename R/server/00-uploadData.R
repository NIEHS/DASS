# UPLOAD DATA =====
# Setup -----
usr_dt <- reactiveVal()
wf <- reactiveVal("std") # id workflow: standard (std) or borderline (bl)
da <- reactiveVal() # id defined approach
xl_sheets_usr_dt <- reactiveVal() # Stores sheet names from user data

# user confirms da selection
observeEvent(input$confirm_da, {
  # Check if da() or wf() have changed and reset ui if needed
  reset_min <- 0
  reset_fpath <- 0
  reset_usr_dt <- 0

  if (input$selected_da == "da_2o3" & input$do_da_2o3_bl) {
    # std --> bl
    if (!is.null(da()) & wf() == "std") {
      reset_min <- 2
      reset_fpath <- 1
      reset_usr_dt <- 1
    }
    wf <- "bl"
  } else {
    # bl --> std
    if (wf() == "bl") {
      reset_min <- 2
      reset_fpath <- 1
      reset_usr_dt <- 1
    } else if (!is.null(da())) {
      if (input$selected_da != da()) {
        reset_min <- 3
      }
    }
    wf <- "std"
  }

  if (reset_usr_dt) {usr_dt(NULL); xl_sheets_usr_dt(NULL)}
  if (reset_fpath) runjs("resetFileInputText('fpath')")
  if (reset_min > 0) {
    for (i in reset_min:6) {
      shinyjs::runjs(sprintf("resetHiddenTab('%s');", tab_names[[i]]))
      shinyjs::reset(id = names(tab_names[i]))
    }
  }
  
  da(input$selected_da)
  wf(wf)
  
  # Update visible UI
  show_hide(show = c("upload_data", switch(wf(), std = "ui_upload_data_std", bl = "ui_upload_data_bl")))
  tab_change("tabs", tab_names[[2]])
})

# Data -----
## User Upload -----
observeEvent(input$fpath, {
  # Check file extension
  ext <- unlist(strsplit(input$fpath$name, "[.]"))
  ext <- ext[length(ext)]
  ext_ok <- switch(
    wf(),
    std = c("csv", "tsv", "txt", "xls", "xlsx"),
    bl = c("xls", "xlsx")
  )
  ext_valid <- grepl_ci(concatOrString(ext_ok), ext)
  
  if (!ext_valid) {
    showNotification(
      type = "error",
      sprintf("Invalid file type. Accepted file extensions: %s", paste(ext_ok, collapse = ", ")),
      duration = Inf
    )
  }
  req(ext_valid)
  
  # Reset items
  usr_dt(NULL)
  runjs("resetHidden('#ui_data');")
  if (ext %in% c("xls", "xlsx")) {
    show_hide(show = "ui_xl_select")
    xl_sheets_usr_dt(readxl::excel_sheets(input$fpath$datapath))
  } else {
    xl_sheets_usr_dt(NULL)
    usr_dt(read.delim(input$fpath$datapath, sep = switch(ext, txt = "\t", tsv = "\t", csv = ",")))
    show_hide(show = "ui_view_data_upload")
  }
}, ignoreInit = T)

## Demo -----
demo_data <- reactive({
  switch(
    wf(),
    "bl" = readRDS("www/DASS_demo_data_bl.rds"),
    "std" = readRDS("www/DASS_demo_data.rds") 
  )
})

### Demo Toggle -----
observeEvent(input$use_demo_data, {
  runjs("resetHidden('#ui_data');")
  if (input$use_demo_data) {
    show_hide(show = "ui_view_data_upload", hide = "ui_upload_data_input")
    if (wf() == "bl") show_hide(show = "ui_xl_select")
  } else {
    show_hide(show = "ui_upload_data_input")
    if (!is.null(usr_dt())) {
      show_hide(show = "ui_view_data_upload")
      if (!is.null(xl_sheets_usr_dt())) {
        show_hide(show = "ui_xl_select")
      }
    }
  }
})

## Excel Choices -----
xl_sheet_choices <- reactiveVal()
observeEvent({input$fpath; input$use_demo_data}, {

  choices <- ""
  if (input$use_demo_data & wf() == "bl") {
    choices <- names(demo_data())
  } else if (!input$use_demo_data & !is.null(xl_sheets_usr_dt())) {
    choices <- xl_sheets_usr_dt()
  } 
  xl_sheet_choices(choices)
  
  updatePickerInput(
    inputId = "xl_sheet",
    choices = choices,
    selected = choices[1],
    label = switch(
      wf(),
      std = "Select worksheet to upload",
      bl = "Select worksheet to view"
    )
  )
  runjs(sprintf("Shiny.setInputValue('xl_sheet', '%s', {priority: 'event'});", choices[1]))

  if (wf() == "bl" & length(xl_sheet_choices()) < 3) {
    show_hide(show = "bl_xl_warn")
  } else {
    show_hide(hide = "bl_xl_warn")
  }
}, ignoreInit = T)

# Update user data with selected worksheet
observeEvent(input$xl_sheet, {
  req(input$xl_sheet != "")
  req(!input$use_demo_data)
  req(input$fpath)
  usr_dt(data.frame(readxl::read_excel(input$fpath$datapath, sheet = input$xl_sheet, na = c("", "na", "NA"))))
  show_hide(show = "ui_view_data_upload")
}, ignoreInit = T)

# Table -----
data_shown <- reactive({
  if (input$use_demo_data) {
    switch(
      wf(),
      std = demo_data(),
      bl = demo_data()[[input$xl_sheet]]
    )
  } else {
    usr_dt()
  }
})

output$dt_analyze <- DT::renderDataTable({
    req(data_shown())
    datatable(
      data_shown(),
      class = "table-data stripe",
      rownames = F,
      filter = "top",
      selection = "none",
      options = list(
        dom = "lrtip",
        initComplete = JS("() => updateDT('dt_analyze')")
      ),
      callback = JS("tabBody(table);")
    )
})

output$user_summary_tab2 <- renderUI({
  tags$ul(
    class = "summary-list",
    tags$li(tags$b("Selected DA:"), da_dict[[da()]][["full_name"]]),
    tags$li(tags$b("Workflow:"), switch(wf(), std = "Standard", bl = "Borderline"))
  )
})
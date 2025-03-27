# SELECT COLUMNS =====
select_track <- reactiveVal()
bl_ws_idx <- reactiveVal()
data_tables <- reactiveVal()
input_file <- reactiveVal()

# Setup -----
observeEvent(input$confirm_data, {
  for (i in 4:6) {
    shinyjs::runjs(sprintf("resetHiddenTab('%s');", tab_names[[i]]))
    shinyjs::reset(id = names(tab_names[i]))
  }

  # shinyjs::reset(id = "compare_setup_standard")
  input_file(ifelse(input$use_demo_data, "Demo Data", input$fpath$name))
  output$user_summary_tab3 <- renderUI({
    tags$ul(
      class = "summary-list",
      tags$li(tags$b("Selected DA:"), da_dict[[da()]][["full_name"]]),
      tags$li(tags$b("Workflow:"), switch(wf(), std = "Standard", bl = "Borderline")),
      tags$li(tags$b("Input File:"), input_file()),
      tags$li(tags$b("Selected Worksheet:"), 
              ifelse(wf() == "std" & !is.null(xl_sheets_usr_dt()),
                     input$xl_sheet,
                     "Not applicable"))
    )
  })

  select_track(select_show_hide[[da()]][[wf()]])
  
  # Use dictionary to show/hide elements in the tab
  show_list <- unlist(lapply(select_track(), function(x) x$init$items), use.names = F)
  diff_list <- unlist(lapply(select_show_hide, function(x) {
    unlist(lapply(x[[wf()]], function(y) y$all_items$items))
  }), use.names = F)
  hide_list <- setdiff(diff_list, show_list)

  # Update picker inputs
  if (wf() == "std") {
    data_tables(data_shown())
    data_select_autocol <- readRDS("R/lists/auto_col_std.rds")
    all_selects <- unlist(lapply(select_track(), function(x) x$all_items$selects))
    
    if (!all(all_selects %in% names(data_select_autocol))) {
      warning("unmatched columns")
    }
    for (i in all_selects) {
      selected <- grep_ci(concatOrString(data_select_autocol[[i]]), names(data_shown()), value = T)[1]
      if (length(selected) == 0) selected <- ""
      updatePickerInput(
        inputId = i,
        choices = names(data_shown()),
        selected = selected
      )
    }
  } else if (wf() == "bl") {
    if (input$use_demo_data) {
      bl_ws_idx <- lapply(demo_data(), names)
    } else {
      bl_ws_idx <- lapply(xl_sheet_choices(), function(x) {
        tmp <- openxlsx::read.xlsx(input$fpath$datapath, sheet = x, rows = 1)
        names(tmp)
      })
      names(bl_ws_idx) <- xl_sheet_choices()
    }
    
    bl_ws_idx(bl_ws_idx)

    # KE1
    ke1_tmp <- grep_ci("^adra$", xl_sheet_choices())
    ke1_select <- "adra"
    if (length(ke1_tmp) == 0) {
      ke1_tmp <- grep_ci("^dpra$", xl_sheet_choices())
      if (length(ke1_tmp) > 0) {
        ke1_select <- "dpra"
      } 
    } 
    runjs(sprintf("Shiny.setInputValue('ke1_bl_assay', '%s', {priority: 'event'})", ke1_select)) # trigger observeEvent even when no change
    updateRadioButtons(
      inputId = "ke1_bl_assay",
      selected = ke1_select
    )
    updatePickerInput(
      inputId = "ke1_bl_ws",
      choices = xl_sheet_choices()
    )
    
    # KE2
    ke2_tmp <- grep_ci(concatOrString(c("ks", "keratinosens")), xl_sheet_choices())
    ke2_select <- "ks"
    if (length(ke2_tmp) == 0) {
      ke2_tmp <- grep_ci(concatOrString(c("lusens")), xl_sheet_choices())
      if (length(ke2_tmp) > 0) {
        ke2_select <- "lusens"
      } 
    }
    runjs(sprintf("Shiny.setInputValue('ke2_bl_assay', '%s', {priority: 'event'})", ke2_select)) # trigger observeEvent even when no change
    updateRadioButtons(
      inputId = "ke2_bl_assay",
      selected = ke2_select
    )
    updatePickerInput(
      inputId = "ke2_bl_ws",
      choices = xl_sheet_choices()
    )
    
    # KE3
    ke3_strings <- c(
      gard = "^gardskin$",
      hclat = concatOrString(c("hclat", "h-clat")),
      il8 = concatOrString(c("il8", "il-8", "il8 luc", "il-8 luc")),
      usens = concatOrString(c("usens", "u-sens"))
    )
    ke3_tmp <- unlist(sapply(ke3_strings, function(x) {
      grep_ci(x, xl_sheet_choices())
    })) 
    
    ke3_select <- "gard"
    if (length(ke3_tmp) > 0) {
      ke3_select <- names(ke3_tmp)[1]
    }
    runjs(sprintf("Shiny.setInputValue('ke3_bl_assay', '%s', {priority: 'event'})", ke3_select)) # trigger observeEvent even when no change
    updateRadioButtons(
      inputId = "ke3_bl_assay",
      selected = ke3_select
    )

    updatePickerInput(
      inputId = "ke3_bl_ws",
      choices = xl_sheet_choices()
    )
  }
  
  show_hide(show = c(names(tab_names[3]), show_list), hide = hide_list)
  tab_change("tabs", tab_names[[3]])
})

# Observers -----
## Standard -----
observeEvent(input$get_ke1_call, {
  test1 <- da() %in% c("da_2o3", "da_ke31")
  test2 <- wf() == "std"
  req(test1 & test2)
  all_items <- select_track()$ke1$all_items$items
  if (input$get_ke1_call) {
    if (input$get_ke1_mean_dep) {
      new <- select_track()$ke1$get_dep$items
    } else {
      new <- select_track()$ke1$get_call$items
    }
  } else {
    new <- select_track()$ke1$init$items
  }
  show_hide(
    show = intersect(all_items, new),
    hide = setdiff(all_items, new)
  )
})

observeEvent(input$get_ke1_mean_dep, {
  req(wf() == "std")
  all_items <- select_track()$ke1$all_items$items
  if (input$get_ke1_mean_dep) {
    new <- select_track()$ke1$get_dep$items
  } else {
    tmp <- ifelse(da() == "da_its", "init", "get_call")
    new <- select_track()$ke1[[tmp]]$items
  }
  show_hide(
    show = intersect(all_items, new),
    hide = setdiff(all_items, new)
  )
})

## Borderline -----
observeEvent(input$ke1_bl_assay, {
  req(bl_ws_idx())
  # Select worksheet
  ke1_tmp <- grep_ci(concatOrString(input$ke1_bl_assay), xl_sheet_choices())
  if (length(ke1_tmp) > 0) {
    ke1_ws <- xl_sheet_choices()[ke1_tmp[1]]
  } else {
    ke1_ws <- xl_sheet_choices()[1]
  }
  
  updatePickerInput(
    inputId = "ke1_bl_ws",
    selected = ke1_ws
  )
  
}, ignoreNULL =  T, priority = )

observeEvent(input$ke1_bl_ws, {
  # Select data columns
  selects <- select_track()$ke1[[input$ke1_bl_assay]]$selects
  cnames <- bl_ws_idx()[[input$ke1_bl_ws]]
  for (i in 1:length(selects)) {
    if (i <= length(cnames)) {
      selected <- cnames[i]
    } else {
      selected <- ""
    }
    
    updatePickerInput(
      inputId = selects[i],
      selected = selected,
      choices = cnames
    )
  }
})

observeEvent(input$ke2_bl_assay, {
  req(wf() == "bl")
  
  ke2_string <- switch(input$ke2_bl_assay, lusens = concatOrString("lusens"), ks = concatOrString(c("ks", "keratinosens")))
  ke2_tmp <- grep_ci(ke2_string, xl_sheet_choices())
  if (length(ke2_tmp) > 0) {
    ke2_ws <- xl_sheet_choices()[ke2_tmp[1]]
  } else {
    ke2_ws <- xl_sheet_choices()[min(2, length(xl_sheet_choices()))]
  }
  
  updatePickerInput(
    inputId = "ke2_bl_ws",
    selected = ke2_ws
  )

  all_items <- select_track()$ke2$all_items$items
  new <- select_track()$ke2[[input$ke2_bl_assay]]$items
  show_hide(
    show = intersect(all_items, new),
    hide = setdiff(all_items, new)
  )

}, ignoreNULL =  T)

observeEvent(input$ke2_bl_ws, {
  # Select data columns
  selects <- select_track()$ke2[[input$ke2_bl_assay]]$selects
  cnames <- bl_ws_idx()[[input$ke2_bl_ws]]
  for (i in 1:length(selects)) {
    if (i <= length(cnames)) {
      selected <- cnames[i]
    } else {
      selected <- ""
    }
    
    updatePickerInput(
      inputId = selects[i],
      selected = selected,
      choices = cnames
    )
  }
})

observeEvent(input$ke3_bl_assay, {
  req(wf() == "bl")
  
  ke3_string <- switch(input$ke3_bl_assay, 
                       gard = concatOrString("gardskin"), 
                       hclat = concatOrString(c("hclat", "h-clat")),
                       il8 = concatOrString(c("il8", "il-8", "il8 luc", "il-8 luc")),
                       usens = concatOrString(c("usens", "u-sens")))
  ke3_tmp <- grep_ci(ke3_string, xl_sheet_choices())
  if (length(ke3_tmp) > 0) {
    ke3_ws <- xl_sheet_choices()[ke3_tmp[1]]
  } else {
    ke3_ws <- xl_sheet_choices()[min(3, length(xl_sheet_choices()))]
  }

  updatePickerInput(
    inputId = "ke3_bl_ws",
    selected = ke3_ws
  )

  all_items <- select_track()$ke3$all_items$items
  new <- select_track()$ke3[[input$ke3_bl_assay]]$items
  show_hide(
    show = intersect(all_items, new),
    hide = setdiff(all_items, new)
  )
})

observeEvent(input$ke3_bl_ws, {
  # Select data columns
  selects <- select_track()$ke3[[input$ke3_bl_assay]]$selects
  cnames <- bl_ws_idx()[[input$ke3_bl_ws]]
  for (i in 1:length(selects)) {
    if (i <= length(cnames)) {
      selected <- cnames[i]
    } else {
      selected <- ""
    }
    
    updatePickerInput(
      inputId = selects[i],
      selected = selected,
      choices = cnames
    )
  }
})

### Modal Slideshow ------
observe({
  s <- blr_img_ids[["ke1"]][blr_label_ids[["ke1"]] == input$ke1_blr_caro_img]
  show_hide(
    show = s,
    hide = setdiff(blr_img_ids[["ke1"]], s)
  )
})

observeEvent(input$ke1_blr_prev, {
  idx <- which(blr_label_ids[["ke1"]] == input$ke1_blr_caro_img) - 1
  idx[idx == 0] <- length(blr_label_ids[["ke1"]])
  
  updatePickerInput(inputId = "ke1_blr_caro_img", selected = blr_label_ids[["ke1"]][idx])
})

observeEvent(input$ke1_blr_next, {
  idx <- which(blr_label_ids[["ke1"]] == input$ke1_blr_caro_img) + 1
  idx[idx > length(blr_label_ids[["ke1"]])] <- 1
  
  updatePickerInput(inputId = "ke1_blr_caro_img", selected = blr_label_ids[["ke1"]][idx])
})

observe({
  s <- blr_img_ids[["ke2"]][blr_label_ids[["ke2"]] == input$ke2_blr_caro_img]
  show_hide(
    show = s,
    hide = setdiff(blr_img_ids[["ke2"]], s)
  )
})

observeEvent(input$ke2_blr_prev, {
  idx <- which(blr_label_ids[["ke2"]] == input$ke2_blr_caro_img) - 1
  idx[idx == 0] <- length(blr_label_ids[["ke2"]])
  
  updatePickerInput(inputId = "ke2_blr_caro_img", selected = blr_label_ids[["ke2"]][idx])
})

observeEvent(input$ke2_blr_next, {
  idx <- which(blr_label_ids[["ke2"]] == input$ke2_blr_caro_img) + 1
  idx[idx > length(blr_label_ids[["ke2"]])] <- 1
  
  updatePickerInput(inputId = "ke2_blr_caro_img", selected = blr_label_ids[["ke2"]][idx])
})

observe({
  s <- blr_img_ids[["ke3"]][blr_label_ids[["ke3"]] == input$ke3_blr_caro_img]
  show_hide(
    show = s,
    hide = setdiff(blr_img_ids[["ke3"]], s)
  )
})

observeEvent(input$ke3_blr_prev, {
  idx <- which(blr_label_ids[["ke3"]] == input$ke3_blr_caro_img) - 1
  idx[idx == 0] <- length(blr_label_ids[["ke3"]])
  
  updatePickerInput(inputId = "ke3_blr_caro_img", selected = blr_label_ids[["ke3"]][idx])
})

observeEvent(input$ke3_blr_next, {
  idx <- which(blr_label_ids[["ke3"]] == input$ke3_blr_caro_img) + 1
  idx[idx > length(blr_label_ids[["ke3"]])] <- 1
  
  updatePickerInput(inputId = "ke3_blr_caro_img", selected = blr_label_ids[["ke3"]][idx])
})
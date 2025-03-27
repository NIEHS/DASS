# PROCESS USER INPUTS =====
# Strings for pattern matching
call0_str <- c("0", "i", "inactive", "n", "neg", "negative", "non-sensitizer", "non-sensitiser", "nonsensitizer", "nonsensitiser")
call1_str <- c("1", "a", "active", "p", "pos", "positive", "sensitizer", "sensitiser")
ke3_0_str <- c("NI", "Inf", "i", "inactive", "n", "neg", "negative", "non-sensitizer", "non-sensitiser", "nonsensitizer", "nonsensitiser")

# Reactives -----
col_req <- reactiveVal()
data_input <- reactiveVal()
dt_review_std <- reactiveVal()
dt_review_bl <- reactiveValues(
  ke1 = NULL,
  ke2 = NULL,
  ke3 = NULL,
  chem = NULL
)

# Setup -----
observeEvent(input$review_entries, {
  for (i in 4:6) {
    shinyjs::runjs(sprintf("resetHiddenTab('%s');", tab_names[[i]]))
    shinyjs::reset(id = names(tab_names[i]))
  }
  
  # Get required endpoints based on user selections
  if (wf() == "std") {
    
    if (da() == "da_its") {
      selector <- c("init", "get_dep")[input$get_ke1_mean_dep + 1]
    } else {
      ke1_type <- 1 + input$get_ke1_call * (input$get_ke1_mean_dep + 1)
      selector <- c("init", "get_call", "get_dep")[ke1_type]
      if (da() == "da_ke31") {
      }
    }
    col_req <- lapply(select_track(), function(x) x[[selector]]$selects)
  } else if (wf() == "bl") {
    col_req <- list(
      ke1 = select_track()$ke1[[input$ke1_bl_assay]]$selects,
      ke2 = select_track()$ke2[[input$ke2_bl_assay]]$selects,
      ke3 = select_track()$ke3[[input$ke3_bl_assay]]$selects
    )
  }
  
  # Confirm all required inputs have values
  no_blank <- all(sapply(unlist(col_req, use.names = F), function(x) !is.null(input[[x]])))
  if (!no_blank) {
    showNotification(
      type = "error",
      ui = "Missing required column selections.",
      duration = 10
    )
  }
  req(no_blank)

  # Pull data based on user selection and insert into template
  if (wf() == "std") {
    data_input <- select_data_template[[wf()]][[da()]]
    
    for (isrc in names(col_req)) {
      for (id in col_req[[isrc]]) {
        tmp <- data_input[[isrc]][[id]]
        tmp$col_name <- input[[id]]
        tmp$values <- data_tables()[[input[[id]]]]
        data_input[[isrc]][[id]] <- tmp
      }
    }
    show_hide(show = c(names(tab_names[4]), "ui_rev_std"))
  } else if (wf() == "bl") {
    data_input <- Map(
      function(ke, assay) {
        ke[[assay]]
      }, 
      select_data_template[[wf()]][[da()]], 
      c(input$ke1_bl_assay, input$ke2_bl_assay, input$ke3_bl_assay)
    )

    if (input$use_demo_data) {
      data_tables <- demo_data()[c(input$ke1_bl_ws, input$ke2_bl_ws, input$ke3_bl_ws)]
      names(data_tables) <- c("ke1", "ke2", "ke3")
    } else {
      data_tables <- lapply(
        c(ke1 = input$ke1_bl_ws, ke2 = input$ke2_bl_ws, ke3 = input$ke3_bl_ws), 
        function(x) {
          tmp <- openxlsx::read.xlsx(input$fpath$datapath, sheet = x)
        }
      )
    }
    data_tables(data_tables)
    
    for (isrc in names(col_req)) {
      for (id in col_req[[isrc]]) {
        tmp <- data_input[[isrc]][[id]]
        tmp$col_name <- input[[id]]
        tmp$values <- data_tables[[isrc]][[input[[id]]]]
        data_input[[isrc]][[id]] <- tmp
      }
    }
    show_hide(show = c(names(tab_names[4]), "ui_rev_bl"))
  }
  
  # check data formatting based on column type and add flags. 
  # convert values to expected format and add to object
  for (isrc in names(col_req)) {
    for (id in col_req[[isrc]]) {
      tmp <- data_input[[isrc]][[id]]
      if (tmp$col_type == "call_bin") {
        
        converted_values <- rep(NA, length(tmp$values))
        converted_values[grepl_ci(concatOrString(call1_str), tmp$values)] <- 1
        converted_values[grepl_ci(concatOrString(call0_str), tmp$values)] <- 0
        
        tmp$converted_values <- converted_values
        tmp$flagged <- !all(is.na(tmp$values[is.na(converted_values)]))
        
      } else if (tmp$col_type == "numeric") {
        
        converted_values <- suppressWarnings(as.numeric(tmp$values))
        
        tmp$converted_values <- converted_values
        tmp$flagged <- !all(is.na(tmp$values[is.na(converted_values)]))
        
      } else if (tmp$col_type == "ke3_val") {
        
        converted_values <- rep(NA, length(tmp$values))
        isNeg <- grepl_ci(concatOrString(ke3_0_str), tmp$values)
        converted_values[isNeg] <- Inf
        converted_values[!isNeg] <- suppressWarnings(as.numeric(tmp$values[!isNeg]))
        
        tmp$converted_values <- converted_values
        tmp$flagged <- !all(is.na(tmp$values[is.na(converted_values)]))
        
      } else if (tmp$col_type == "ad") {
        
        converted_values <- rep(NA, length(tmp$values))
        converted_values[grepl_ci(concatOrString(c("1", "in")), tmp$values)] <- 1
        converted_values[grepl_ci(concatOrString(c("0", "out")), tmp$values)] <- 0
        tmp$converted_values <- converted_values
        
        tmp$flagged <- !all(is.na(tmp$values[is.na(converted_values)]))
      } else if (tmp$col_type %in% c("yn_miss", "yn_nomiss")) {
        
        converted_values <- rep(NA, length(tmp$values))
        converted_values[grepl_ci(concatOrString(c("y", "yes", "1")), tmp$values)] <- "y"
        converted_values[grepl_ci(concatOrString(c("n", "no", "0")), tmp$values)] <- "n"
        tmp$converted_values <- converted_values
        
        tmp$flagged <- !is.na(tmp$values) & is.na(converted_values)
        
      } else if (tmp$col_type == "ks_bl_outcome") {
        
        tmp$converted_values <- ifelse(toupper(tmp$values) %in% c("POSITIVE", "NEGATIVE", "BORDERLINE"), tmp$values, NA)
        tmp$flagged <- !is.na(tmp$values) & is.na(tmp$converted_values)
        
      } 
      data_input[[isrc]][[id]] <- tmp
    }
  }
  
  # Create tables for user to view
  if (wf() == "std") {
    dt_review <- Map(function(isrc, cname) {
      out <- lapply(cname, function(x) {
        data.frame(data_input[[isrc]][[x]][c("display_name","col_name", "flagged")])
      })
      do.call("rbind", out)
    }, names(col_req), col_req)
    
    dt_review <- do.call("rbind", dt_review)
    names(dt_review) <- c("Endpoint", "Selected Column", "Flagged")
    
    dt_review_std(dt_review)
  } else if (wf() == "bl") {
    # Chem ID flag
    cid <- Map(function(ke_data, ke) {
      cid_col <- grep("_cid$", names(ke_data), value = T)
      out <- data.frame(cid = unique(ke_data[[cid_col]]$values))
      out[[ke]] <- "x"
      return(out)
    }, data_input, names(data_input))
    
    cid <- Reduce(function(x, y) merge(x, y, by="cid", all = T), cid)
    cid_ke_sum <- rowSums(cid[,-1] == "x", na.rm = T)
    cid$flagged <- cid_ke_sum < 2
    
    names(cid) <- c("Chemical Identifier", "KE1 Worksheet", "KE2 Worksheet", "KE3 Worksheet", "Flagged")
    dt_review_bl$chem <- cid
    
    data_input <- lapply(data_input, function(ke) {
      cid_col <- grep("_cid$", names(ke), value = T)
      ke[[cid_col]]$flagged <- unique(ke[[cid_col]]$values) %in% cid$`Chemical Identifier`[cid$Flagged]
      return(ke)
    })
    
    flags <- lapply(data_input, function(ke) {
      out <- lapply(ke, function(col_data) {
        data.frame(
          `Endpoint` = col_data$display_name,
          `Selected Column` = col_data$col_name,
          `Flagged` = any(col_data$flagged),
          check.names = F
        )
      })
      out <- do.call("rbind", out)
      rownames(out) <- NULL
      return(out)
    })
    
    dt_review_bl$ke1 <- flags$ke1
    dt_review_bl$ke2 <- flags$ke2
    dt_review_bl$ke3 <- flags$ke3
  }

  col_req(col_req)
  data_input(data_input)
  tab_change("tabs", tab_names[[4]])
})


# Output -----
## Standard -----
output$dt_review_std <- DT::renderDataTable({
  req(dt_review_std())
  datatable(
    dt_review_std(),
    class = "table-review",
    rownames = F,
    selection = "none",
    options = list(
      dom = "t",
      rowCallback = JS("styleWarnRow"),
      ordering = F
    )
  )
})

output$ui_review_text_std <- renderUI({
  std_review_summary()
})

std_review_summary <- reactive({
  ui <- list(
    select_list = NULL,
    dupe_warn = NULL,
    flag_warn = NULL
  )
  
  ext_xl <- grepl("\\.xls[x]{0,1}$", input_file()) & !input$use_demo_data
  
  select_summary <- list(
    tags$li(tags$b("Selected DA: "), da_dict[[da()]][["abbrev"]]),
    tags$li(tags$b("Input File: "), ifelse(input$use_demo_data, "Demo Data", input_file())),
    tags$li(tags$b("Selected Worksheet: "), ifelse(ext_xl, input$xl_sheet, "Not applicable")),
    ke1_tmp = NULL,
    ke3_tmp = NULL
  )
  
  
  if ((da() == "da_2o3" & input$get_ke1_call) | da() == "da_its") {
    select_summary$ke1_tmp <- tags$li(tags$b("KE1 Assay: "), assay_dict[input$ke1_std_assay])
  }
  
  if (da() == "da_its") {
    select_summary$ke3_tmp <- tags$li(tags$b("KE3 Assay: "), assay_dict[input$ke3_std_assay])
  }
  
  ui$select_list <- tags$ul(select_summary)
  
  if (any(duplicated(dt_review_std()$`Selected Column`))) {
    ui$dupe_warn <- p(strong(class = "warningText", "Warning: Identical column assigned to more than one endpoint."))
  }
  
  if (any(dt_review_std()$Flagged)) {
    ui$flag_warn <- p(strong(class = "warningText", "Warning: Selected data columns have been flagged for invalid values. Invalid values will not be evaluated in the DASS."))
  }
  tagList(ui)
  
})

## Borderline -----
output$dt_review_bl_ke1 <- DT::renderDataTable({
  req(dt_review_bl$ke1)
  datatable(
    dt_review_bl$ke1,
    class = "table-review",
    rownames = F,
    selection = "none",
    options = list(
      dom = "t",
      rowCallback = JS("styleWarnRow"),
      ordering = F
    )
  )
})

output$dt_review_bl_ke2 <- DT::renderDataTable({
  req(dt_review_bl$ke2)
  datatable(
    dt_review_bl$ke1,
    class = "table-review",
    rownames = F,
    selection = "none",
    options = list(
      dom = "t",
      rowCallback = JS("styleWarnRow"),
      ordering = F
    )
  )
})

output$dt_review_bl_ke3 <- DT::renderDataTable({
  req(dt_review_bl$ke3)
  datatable(
    dt_review_bl$ke3,
    class = "table-review",
    rownames = F,
    selection = "none",
    options = list(
      dom = "t",
      rowCallback = JS("styleWarnRow"),
      ordering = F
    )
  )
})

output$dt_review_bl_cid <- DT::renderDataTable({
  req(dt_review_bl$chem)
  datatable(
    dt_review_bl$chem,
    class = "table-review",
    rownames = F,
    selection = "none",
    options = list(
      dom = "t",
      rowCallback = JS("styleWarnRow"),
      ordering = F
    )
  )
})

output$ui_review_text_bl_gen <- renderUI({
  bl_review_gen()
})

bl_review_gen <- reactive({
  ui <- list(
    select_list =   tags$ul(
      tags$li(tags$b("Selected DA: "), "2o3"),
      tags$li(tags$b("Analysis Type: "), "Borderline"),
      tags$li(tags$b("Input File: "), input_file())
    ),
    dupe_ws_warn = NULL
  )
  
  if (any(duplicated(c(input$ke1_bl_ws, input$ke2_bl_ws, input$ke3_bl_ws)))) {
    ui$dupe_ws_warn <- p(strong(class = "warningText", "Warning: Identical worksheet assigned to more than one assay."))
  }

  tagList(ui)
})

output$ui_review_text_bl_ke1 <- renderUI({
  bl_review_ke1()
})

bl_review_ke1 <- reactive({
  ui <- list(
    select_list =   tags$ul(
      tags$li(tags$b("Selected Assay: "), assay_dict[input$ke1_bl_assay]),
      tags$li(tags$b("Selected KE1 Worksheet: "), input$ke1_bl_ws)
    ),
    dupe_warn = NULL,
    flag_warn = NULL
  )

  if(any(duplicated(dt_review_bl$ke1$`Selected Column`))) {
    ui$dupe_warn <- p(strong(class = "warningText", "Warning: Identical column assigned to more than one variable"))
  }
  
  if(any(dt_review_bl$ke1$Flagged[dt_review_bl$ke1$`Endpoint` != "Chemical Identifier"])) {
    ui$flag_warn <- p(strong(class = "warningText", "Warning: Selected data columns have been flagged for invalid values. Invalid values will not be evaluated in the DASS."))
  }

  tagList(ui)
})

output$ui_review_text_bl_ke2 <- renderUI({
  bl_review_ke2()
})

bl_review_ke2 <- reactive({
  ui <- list(
    select_list =   tags$ul(
      tags$li(tags$b("Selected Assay: "), assay_dict[input$ke2_bl_assay]),
      tags$li(tags$b("Selected KE2 Worksheet: "), input$ke2_bl_ws)
    ),
    dupe_warn = NULL,
    flag_warn = NULL
  )
  
  if(any(duplicated(dt_review_bl$ke2$`Selected Column`))) {
    ui$dupe_warn <- p(strong(class = "warningText", "Warning: Identical column assigned to more than one variable"))
  }
  
  if(any(dt_review_bl$ke2$Flagged[dt_review_bl$ke2$`Endpoint` != "Chemical Identifier"])) {
    ui$flag_warn <- p(strong(class = "warningText", "Warning: Selected data columns have been flagged for invalid values. Invalid values will not be evaluated in the DASS."))
  }
  
  tagList(ui)
})


output$ui_review_text_bl_ke3 <- renderUI({
  bl_review_ke3()
})

bl_review_ke3 <- reactive({
  ui <- list(
    select_list =   tags$ul(
      tags$li(tags$b("Selected Assay: "), assay_dict[input$ke3_bl_assay]),
      tags$li(tags$b("Selected KE3 Worksheet: "), input$ke3_bl_ws)
    ),
    dupe_warn = NULL,
    flag_warn = NULL
  )
  
  if(any(duplicated(dt_review_bl$ke3$`Selected Column`))) {
    ui$dupe_warn <- p(strong(class = "warningText", "Warning: Identical column assigned to more than one variable"))
  }
  
  if(any(dt_review_bl$ke3$Flagged[dt_review_bl$ke3$`Endpoint` != "Chemical Identifier"])) {
    ui$flag_warn <- p(strong(class = "warningText", "Warning: Selected data columns have been flagged for invalid values. Invalid values will not be evaluated in the DASS."))
  }
  
  tagList(ui)
})

output$ui_review_text_bl_cid <- renderUI({
  bl_review_cid()
})

bl_review_cid <- reactive({
  ui <- list(
    chem_summary = tags$ul(
      tags$li(tags$b("Number of Unique Identifiers: "), nrow(dt_review_bl$chem)),
      tags$li(tags$b("Number of Flagged Identifiers: "),sum(dt_review_bl$chem$Flagged))
    ),
    chem_warn = NULL
  )

  if(any(dt_review_bl$chem$Flagged)) {
    ui$chem_warn <- p(strong(class = "warningText", "Warning: At least one chemical identifier has insufficient data for the 2o3."))
  }

  tagList(ui)
})


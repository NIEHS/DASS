# Performance and Comparison =====
observeEvent(input$goToCompare, updateTabsetPanel(inputId = "step_set", selected = "Compare"))
observeEvent(input$goToCompare_blr, updateTabsetPanel(inputId = "step_set", selected = "Compare"))

hppt_ref <- fread("www/ice_references/2024June13_OECD Defined Approach to Skin Sensitization Human (R).txt", data.table = F)
llna_ref <- fread("www/ice_references/2024June13_OECD Defined Approach to Skin Sensitization LLNA (R).txt", data.table = F)

# Reactives -----
perf_ref_data <- reactiveVal()
perf_tables <- reactiveVal()
perf_plots <- reactiveVal()

# Comparison -----
observeEvent(input$do_compare, {
  ref_user_data <- NULL
  n_comp <- 0
  if (input$perf_use_my_data) {
    if (is.null(input$perf_ref_col)) {
      showNotification(
        ui = "No reference columns selected.",
        type = "warning",
        duration = 10
      )
    } else {
      ref_user_data <- as.list(all_out$result_df[,input$perf_ref_col,drop=F])
      n_comp <- n_comp + 1
    }
  }
  
  ref_hppt_data <- NULL
  ref_llna_data <- NULL
  if (input$perf_use_ice) {
    id <- switch(
      input$perf_ice_identifier_type,
      dtxsid = "DTXSID",
      casrn = "CASRN",
      smiles = "QSAR Ready SMILES"
    )
    
    if (input$perf_ice_user_identifier == "") {
      showNotification(
        ui = "No identifier column selected for ICE comparison.",
        type = "warning",
        duration = 10
      )
    } else {
      if ("hppt" %in% input$perf_ice_ref_cql) {
        rcol <- switch(
          input$perf_pred_col,
          Hazard = grep_ci("binary hazard", names(hppt_ref), value = T),
          Potency = grep_ci("potency", names(hppt_ref), value = T)
        )
        ref_hppt_data <- as.list(hppt_ref[match(all_out$result_df[[input$perf_ice_user_identifier]], hppt_ref[[id]]), rcol, drop = F])
      }
      
      if ("llna" %in% input$perf_ice_ref_cql) {
        rcol <- switch(
          input$perf_pred_col,
          Hazard = grep_ci("binary hazard", names(llna_ref), value = T),
          Potency = grep_ci("potency", names(llna_ref), value = T)
        )
        ref_llna_data <- as.list(llna_ref[match(all_out$result_df[[input$perf_ice_user_identifier]], llna_ref[[id]]), rcol, drop = F])
      }
      n_comp <- n_comp + 1
    }
  }
  
  if (n_comp == 0) {
    showNotification(
      "Missing required information. Review your selections and try again.",
      type = "error",
      duration = 10
    )
  } else {
    perf_ref_data(c(ref_user_data, ref_hppt_data, ref_llna_data))
    
    switch(
      input$perf_pred_col,
      Hazard = binary_compare(),
      Potency = cat_compare()
    )
  }
})

# Binary -----
binary_compare <- reactive({
  pred <- all_out$result_df[all_out$dass_results]
  pred <- pred[,grep_ci("hazard", names(pred))]
  pred <- factor(pred, levels = c("Positive", "Negative", "Inconclusive"), c("1", "0", "Inconclusive"))
  pred_noMiss <- (!is.na(pred)) & (pred != "Inconclusive")
  
  all_comps <- lapply(perf_ref_data(), function(x) NULL)
  ref_cols <- names(all_comps)
  
  for (i in ref_cols) {
    ref <- perf_ref_data()[[i]]

  # Check for invalid values
  ref_warn <- any(!grepl_ci(concatOrString(c(call1_str, call0_str)), na.omit(ref)))

  # Convert to 1/0
  ref[grepl_ci(concatOrString(call1_str), ref)] <- 1
  ref[grepl_ci(concatOrString(call0_str), ref)] <- 0
  ref <- factor(ref, levels = c("1","0"))

  # Count missing after 0/1
  ref_noMiss <- !is.na(ref)
  ref_error <- sum(ref_noMiss) < 5

  if (ref_error) {
    out <- list(ref_error = ref_error)
  } else if (!ref_error) {
    pred_error <- sum(ref_noMiss & pred_noMiss) < 5

  out <- list(
    ref_col = i,
    ref_dat = ref,
    ref_plot = factor(ref, levels = c("1", "0"), labels = c("Positive", "Negative")),
    pred_col = input$perf_pred_col,
    pred_dat = pred,
    pred_plot = factor(pred, levels = c("1", "0", "Inconclusive"), labels = c("Positive", "Negative", "Inconclusive")),
    id = gsub("\\s+|[.]+", "", tolower(paste(i, input$perf_pred_col, sep = "_"))),
    label = paste(i, input$perf_pred_col, sep = " vs. "),
    ref_warn = ref_warn,
    pred_error = pred_error
  )

  if (!pred_error) {
    perf <- compareBinary(
      pred = pred,
      ref = ref,
      predCol = input$perf_pred_col,
      refCol = i
    )
    out <- append(out, perf)

    base_plot_data <- data.frame(table(perf$indiv))
    base_plot_data$Var1 <- factor(base_plot_data$Var1 , levels = c("True Positive", "True Negative", "False Positive", "False Negative", "NA"))
    base_plot_data$tt_text <- sprintf("Prediction Type = %s\nCount = %i", base_plot_data$Var1, base_plot_data$Freq)

    da_lab <- switch(input$selected_da,
           da_2o3 = "Two-out-of-Three DA",
           da_its = "Integrated Testing Strategy DA",
           da_ke31 = "Key Event 3/1 STS DA")


    bp_title <- sprintf("Hazard Comparison\n%s vs. %s", da_lab, i)

    base_plot <- ggplot(base_plot_data, aes(x = Var1, y = Freq, text = tt_text)) +
      geom_bar(stat = "identity") +
      labs(x = "Prediction Type", y = "Count", title = bp_title) +
      theme_bw()
    out$base_plot <- base_plot

  }
  }
   all_comps[[i]] <- out
  }

   updateSelectInput(session = session, inputId = "reference_perf_1", choices = ref_cols, selected = ref_cols[1])

  if (length(ref_cols) > 1) {
    shinyjs::show(id = "perf_table_block_2")
    updateSelectInput(session = session, inputId = "reference_perf_2", choices = ref_cols, selected = ref_cols[2])
    shinyjs::addClass(id = "perf_table_block_1",class = "col-sm-6")
    shinyjs::removeClass(id = "perf_table_block_1",class = "col-sm-12")
  } else {
    shinyjs::hide(id = "perf_table_block_2")
    updateSelectInput(session = session, inputId = "reference_perf_2", choices = NULL)
    shinyjs::addClass(id = "perf_table_block_1",class = "col-sm-12")
    shinyjs::removeClass(id = "perf_table_block_1",class = "col-sm-6")
  }

  updateCheckboxGroupInput(
    session = session,
    inputId = "perf_table_choices",
    choiceValues = unname(sapply(all_comps, function(x) x[["ref_col"]])),
    choiceNames = unname(sapply(all_comps, function(x) x[["label"]])))

  shinyjs::show("compare_explore_standard")
  shinyjs::show("binary_defs")
  shinyjs::hide("potency_defs")
  perf_tables(all_comps)
})

# Categorical -----

cat_compare <- reactive({
  perf_pred_col <- isolate(input$perf_pred_col)
  
  pred <- all_out$result_df[all_out$dass_results]
  pred <- pred[,grep_ci("potency", names(pred))]
  pred <- factor(pred, levels = c("1A", "1B", "NC", "Inconclusive"))
  pred_noMiss <- (!is.na(pred)) & (pred != "Inconclusive")

  all_comps <- lapply(perf_ref_data(), function(x) NULL)
  ref_cols <- names(all_comps)

  for (i in ref_cols) {
    ref <- perf_ref_data()[[i]]

    # Check for invalid values
    ref_warn <- any(!grepl_ci(concatOrString(c("1A", "1B", "NC")), na.omit(ref)))

    # Convert to all caps
    ref[grepl_ci("^1A$", ref)] <- "1A"
    ref[grepl_ci("^1B$", ref)] <- "1B"
    ref[grepl_ci("^NC$", ref)] <- "NC"
    ref <- factor(ref, levels = c("1A","1B", "NC"))

    # Count missing
    ref_noMiss <- !is.na(ref)
    ref_error <- sum(ref_noMiss) < 5

    if (ref_error) {
      out <- list(ref_error = ref_error)
    } else if (!ref_error) {
      pred_error <- sum(ref_noMiss & pred_noMiss) < 5

      out <- list(
        ref_col = i,
        ref_dat = ref,
        pred_col = perf_pred_col,
        pred_dat = pred,
        id = gsub("\\s+|[.]+", "", tolower(paste(i, perf_pred_col, sep = "_"))),
        label = paste(i, perf_pred_col, sep = " vs. "),
        ref_warn = ref_warn,
        pred_error = pred_error
      )

      if (!pred_error) {
        perf <- compareCat(
          pred = pred,
          ref = ref,
          predCol = perf_pred_col,
          refCol = i
        )
        out <- append(out, perf)

        base_plot_data_ref <- expand.grid(ref = unique(ref), pred = unique(pred))
        base_plot_data_ref$val <- apply(base_plot_data_ref, 1, function(x) {
          ref_na <- is.na(x[1])
          pred_na <- is.na(x[2])
          if (ref_na & pred_na) {
            sum(is.na(ref) & is.na(pred))
          } else if (ref_na & !pred_na) {
            sum(is.na(ref) & pred == x[2], na.rm = T)
          } else if (!ref_na & pred_na) {
            sum(ref == x[1] & is.na(pred), na.rm = T)
          } else if (!ref_na & !pred_na) {
            sum(ref == x[1] & pred == x[2], na.rm = T)
          }
        })

        base_plot_data <- unique(data.frame(
          ref = ref,
          pred = pred,
          pred_type = perf$indiv
        ))
        base_plot_data <- merge(base_plot_data, base_plot_data_ref, all = T)
        base_plot_data$tt_text <- sprintf("Prediction Type = %s\nPrediction = %s\nReference = %s\nCount = %i",
                                          base_plot_data$pred_type, base_plot_data$pred, base_plot_data$ref, base_plot_data$val)
        
        da_lab <- switch(input$selected_da,
                         da_2o3 = "Two-out-of-Three DA",
                         da_its = "Integrated Testing Strategy DA",
                         da_ke31 = "Key Event 3/1 STS DA")

        bp_title <- sprintf("Potency Comparison\n%s vs. %s", da_lab, i)

        base_plot <- ggplot(base_plot_data, aes(x = ref, y = val, fill = pred, text = tt_text)) + 
          geom_bar(stat = "identity", position = position_dodge(preserve = "single"), color = "black") + 
          labs(x = "Reference Potency", y = "Count", title = bp_title) + 
          scale_x_discrete(labels = c("1A", "1B", "NC", "NA"), breaks = c("1A", "1B", "NC", NA)) +
          scale_fill_manual(name = sprintf("%s Prediction", abbrev[input$selected_da]), values = c("#382A54FF", "#357BA2FF", "#60CEACFF", "#7d7d7d"), breaks = c("1A", "1B", "NC", "Inconclusive", NA), na.value = "#F0F0F0") +
          theme_bw() 
        
        out$base_plot <- base_plot
      }
    }
    all_comps[[i]] <- out
  }

  updateSelectInput(session = session, inputId = "reference_perf_1", choices = ref_cols, selected = ref_cols[1])

  if (length(ref_cols) > 1) {
    shinyjs::show(id = "perf_table_block_2")
    updateSelectInput(session = session, inputId = "reference_perf_2", choices = ref_cols, selected = ref_cols[2])
    shinyjs::addClass(id = "perf_table_block_1",class = "col-sm-6")
    shinyjs::removeClass(id = "perf_table_block_1",class = "col-sm-12")
  } else {
    shinyjs::hide(id = "perf_table_block_2")
    updateSelectInput(session = session, inputId = "reference_perf_2", choices = NULL)
    shinyjs::addClass(id = "perf_table_block_1",class = "col-sm-12")
    shinyjs::removeClass(id = "perf_table_block_1",class = "col-sm-6")
  }

  updateCheckboxGroupInput(
    session = session,
    inputId = "perf_table_choices",
    choiceValues = unname(sapply(all_comps, function(x) x[["ref_col"]])),
    choiceNames = unname(sapply(all_comps, function(x) x[["label"]])))

  shinyjs::show("compare_explore_standard")
  shinyjs::hide("binary_defs")
  shinyjs::show("potency_defs")
  perf_tables(all_comps)
})

# Tables -----
ref_error_grob <- textGrob("Error: Fewer than 5 valid reference values.", gp = gpar(col = "#D55E00"))
pred_error_grob <- textGrob("Error: Fewer than 5 conclusive prediction values.", gp = gpar(col = "#D55E00"))

table_shown_1 <- reactiveVal()
table_shown_2 <- reactiveVal()

observe({
  req(input$reference_perf_1)
  res <- isolate(perf_tables()[[input$reference_perf_1]])

  if (!is.null(res[["ref_error"]])) {
    table_shown_1(ref_error_grob)
  } else {
    if (res$pred_error) {
      table_shown_1(pred_error_grob)
    } else {
      table_shown_1(res$fig)
    }
  }
})

output$perf_table_1 <- renderPlot(grid.draw(table_shown_1()), width = "auto")

observe({
  req(input$reference_perf_2)
  res <- isolate(perf_tables()[[input$reference_perf_2]])

  if (!is.null(res[["ref_error"]])) {
    table_shown_2(ref_error_grob)
  } else {
    if (res$pred_error) {
      table_shown_2(pred_error_grob)
    } else {
      table_shown_2(res$fig)
    }
  }
})

output$perf_table_2 <- renderPlot(grid.draw(table_shown_2()), width = "auto")

# Download -----
observeEvent(input$perf_table_all, {
  updateCheckboxGroupInput(inputId = "perf_table_choices", selected = names(perf_tables()))
})

observeEvent(input$perf_table_none, {
  updateCheckboxGroupInput(inputId = "perf_table_choices", selected = NA)
})

output$dl_perf_tables <- downloadHandler(
  filename = function() {
    if (input$use_demo_data) {
      fname <- "DemoData"
    } else {
      fname <- unlist(strsplit(input$fpath$name, "[.]"))
      fname <- paste(fname[-length(fname)], collapse = ".")
    }
    paste0(fname, "_DASSResults_Performance_", format.Date(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf")
  },
  content = function(con) {
    table_choice <- input$perf_table_choices
    pdf(con)
    for (i in table_choice) {
      grid.draw(perf_tables()[[i]][["fig"]])
      if (i != table_choice[length(table_choice)]) grid.newpage()
      }
    dev.off()
  }
)

outputOptions(output, "dl_perf_tables", suspendWhenHidden = FALSE)

output$perf_table_flat_xl <- downloadHandler(
  filename = function() {
    if (input$use_demo_data) {
      fname <- "DemoData"
    } else {
      fname <- unlist(strsplit(input$fpath$name, "[.]"))
      fname <- paste(fname[-length(fname)], collapse = ".")
    }
    paste0(fname, "_DASSResults_Performance_", format.Date(Sys.time(), "%Y%m%d-%H%M%S"), ".xlsx")
  },
  content = function(con) {
    ws_data <- lapply(perf_tables(), function(res) {
      data.frame(
        Reference = res$ref_col,
        DA = res$pred_col,
        res$perf_list,
        check.names = F
      )
    })
    ws_data <- do.call("rbind.data.frame", ws_data)

    wb <- createWorkbook()
    addWorksheet(wb, sheetName = "DASSPerformance")
    writeData(wb, sheet = "DASSPerformance", ws_data)
    saveWorkbook(wb = wb, file = con)
  }
)

outputOptions(output, "perf_table_flat_xl", suspendWhenHidden = FALSE)

output$perf_table_flat_txt <- downloadHandler(
  filename = function() {
    if (input$use_demo_data) {
      fname <- "DemoData"
    } else {
      fname <- unlist(strsplit(input$fpath$name, "[.]"))
      fname <- paste(fname[-length(fname)], collapse = ".")
    }
    paste0(fname, "_DASSResults_Performance_", format.Date(Sys.time(), "%Y%m%d-%H%M%S"), ".txt")
  },
  content = function(con) {
    ws_data <- lapply(perf_tables(), function(res) {
      data.frame(
        Reference = res$ref_col,
        DA = res$pred_col,
        res$perf_list,
        check.names = F
      )
    })
    ws_data <- do.call("rbind.data.frame", ws_data)

    write.table(x =  ws_data, file = con, quote = F, row.names = F, sep = "\t")
  }
)

outputOptions(output, "perf_table_flat_txt", suspendWhenHidden = FALSE)

# Figures -----
observe({
  req(perf_tables())
  updateSelectInput(
    inputId = "perf_fig_comparison",
    choices = names(perf_tables()))
  
  updateSelectInput(
    inputId = "perf_fig_quant_col",
    choices = c("None", names(dt_analyze()))
  )
  
})

perf_plot_error_pred <- plot_ly(
  x = 0, 
  y = 0, 
  type = "scatter", 
  mode = "text", 
  text = "Error: No comparison to plot.",
  textfont = list(color = "#C25400", size = 14))
perf_plot_error_pred$x$layout$xaxis <- list(visible = F)
perf_plot_error_pred$x$layout$yaxis <- list(visible = F)
perf_plot_error_pred$x$config$staticPlot <- T

perf_plot_error_quant <- plot_ly(
  x = 0, 
  y = 0, 
  type = "scatter", 
  mode = "text", 
  text = "Error: Plots can only be generated for quantitative data.",
  textfont = list(color = "#C25400", size = 14))

perf_plot_error_quant$x$layout$xaxis <- list(visible = F)
perf_plot_error_quant$x$layout$yaxis <- list(visible = F)
perf_plot_error_quant$x$config$staticPlot <- T

perf_shown <- reactive({
  perf_pred_col <- isolate(input$perf_pred_col)
  perf_table <- perf_tables()[[isolate(input$perf_fig_comparison)]]
  if (!is.null(perf_table[["ref_error"]])) {
    perf_plot_error_pred
  } else if (perf_table$pred_error) {
    perf_plot_error_pred
  } else {
    if (input$perf_fig_quant_col == "None") {
      plot_out <- ggplotly(perf_table[["base_plot"]], tooltip = "text")
      plot_out$x$layout$margin$t <- plot_out$x$layout$title$font$size*3
    } else {
      quant_val <- suppressWarnings(as.numeric(dt_analyze()[[input$perf_fig_quant_col]]))
      if (all(is.na(quant_val))) {
        perf_plot_error_quant
      } else {
        
        etypes <- switch(
          perf_pred_col,
          Hazard  = c("True Positive", "True Negative", "False Positive", "False Negative", NA),
          Potency = c("True 1A", "True 1B", "True NC", "Overpredicted", "Underpredicted", NA)
        )
        
        cols <- switch(
          perf_pred_col,
          Hazard  = c("#882255", "#117733", "#88CCEE", "#332288", "#999999"),
          Potency = c("#882255", "#CC6677", "#117733", "#88CCEE", "#332288", "#999999")
        )

        pt_text <- sprintf(
          "%s\nReference=%s\nDA Prediction=%s\n%s=%.2f",
          perf_table$indiv,
          perf_table$ref_dat,
          perf_table$pred_dat,
          input$perf_fig_quant_col,
          quant_val
        )
        
        pt_plt <- ggplot() + 
          geom_point(aes(x = quant_val, y = perf_table$indiv, color = perf_table$indiv, text = pt_text), position = position_jitter(width = 0)) +
          scale_color_manual(name = "Comparison", breaks = etypes, values = cols) + 
          labs(
            title = sprintf("DA %s %s Prediction vs. %s", abbrev[input$selected_da], perf_pred_col, isolate(input$perf_fig_comparison)),
            y = NULL,
            x = input$perf_fig_quant_col) +
          theme_bw()
        pt_plt <- ggplotly(pt_plt, tooltip = "text")
        
        dens_plt <- ggplot() + 
          geom_density(aes(x = quant_val, fill = perf_table$indiv, 
                           text = after_stat(sprintf("%s\n%s = %.2f\nDensity = %.2f", fill, input$perf_fig_quant_col, x, density))), alpha = 0.6, color = "#303030") +
          scale_fill_manual(name = "Comparison", breaks = etypes, values = cols) + 
          labs(y = "Density", x = input$perf_fig_quant_col) + 
          theme_bw()
        dens_plt <- ggplotly(dens_plt, tooltip = "text")
        dens_plt$x$data <- lapply(dens_plt$x$data, function(val) {
          val$hoverlabel <- list(bgcolor = val$fillcolor)
          val
        })
        
        plot_out <- subplot(pt_plt, dens_plt, nrows = 2, heights = c(0.35,0.65), shareX = T, titleX = T, titleY = T)
        plot_out$x$data <- lapply(plot_out$x$data, function(val) {
          if (val$mode == "lines") val$showlegend <- F
          return(val)
        })
        plot_out$x$layout$margin$t <- plot_out$x$layout$title$font$size*3
      }
    }
    plot_out
  }
})

output$perf_fig <- renderPlotly({
  req(input$perf_fig_comparison)
  perf_shown()
})
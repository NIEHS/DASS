# Performance and Comparison =====
observeEvent(input$goToCompare, tab_change("tabs", tab_names[[6]]))

hppt_ref <- read.table("www/ice_references/2024June13_OECD Defined Approach to Skin Sensitization Human (R).txt", sep = "\t", comment.char = "", header = T, check.names = F)
llna_ref <- read.table("www/ice_references/2024June13_OECD Defined Approach to Skin Sensitization LLNA (R).txt", sep = "\t", comment.char = "", header = T, check.names = F)

compare_pred <- reactiveVal()
compare_ref <- reactiveVal()
compare_tables <- reactiveVal()

observeEvent(input$compare_bl_ws, {
  req(wf() == "bl")
  cnames <- bl_ws_idx()[[input$compare_bl_ws]]
  updatePickerInput(
    inputId = "compare_bl_cid",
    selected = cnames[1],
    choices = cnames
  )
  updatePickerInput(
    inputId = "compare_usr_dt_col",
    selected = ifelse(length(cnames) == 1, cnames[1], cnames[2]),
    choices = cnames
  )
}, ignoreInit = T)

observeEvent(input$do_compare, {

  # Check that there are no missing selections. 
  cond_1 <- input$compare_usr_dt & is.null(input$compare_usr_dt_col)
  cond_2 <- input$compare_usr_dt & wf() == "bl" & (is.null(input$compare_bl_ws) | is.null(input$compare_bl_cid))
  cond_3 <- input$compare_ice & is.null(input$compare_ice_cql)
  cond_4 <- input$compare_ice & wf() == "std" & is.null(input$compare_ice_cid)
  input_miss <- (cond_1 | cond_2 | cond_3 | cond_4)
  
  if (input_miss) {
    showNotification(
      ui = "Missing required inputs. Please check selections and try again.",
      type = "error",
      duration = 10
    )
  }
  req(!input_miss)

  # Save predictions and set user selected reference data
  pred <- NULL
  ref_user_data <- NULL
  if (wf() == "std") {
    pred <- dass_result()$results_df[,dass_result()$dass_results,drop=F]
    pred <- pred[,grep_ci(input$compare_type, names(pred))]
    
    if ( input$compare_usr_dt ) {
      ref_user_data <- as.list(data_tables()[,input$compare_usr_dt_col,drop=F])
    }

  } else if (wf() == "bl") {
    dass_res <- dass_result_bl()$dass_results
    pred <- dass_res[["DA 2o3 Hazard"]]
    
    if ( input$compare_usr_dt ) {
      if (input$use_demo_data) {
        ref_user_data <- demo_data()[[input$compare_bl_ws]]
      } else {
        ref_user_data <- openxlsx::read.xlsx(input$fpath$datapath, sheet = input$compare_bl_ws)
      }
      
      # Match chemical identifiers to reference data. Create data frame with
      # nrow = nrow(dass_res). Rows correspond to each other.
      ref_cid <- ref_user_data[[input$compare_bl_cid]]
      if (any(duplicated(ref_cid))) {
        showNotification(
          ui = "Multiple reference values found for a single identifier. Using first value.",
          type = "warning",
          duration = 10
        )
      }
      ref_user_data <- as.list(ref_user_data[match(dass_res[["Compound ID"]], ref_cid),input$compare_usr_dt_col,drop=F])
    }
  }
  
  # Get ICE data
  ref_hppt_data <- NULL
  ref_llna_data <- NULL
  if (input$compare_ice) {
    chems <- switch(
      wf(),
      std = data_tables()[,input$compare_ice_cid],
      bl = dass_result_bl()$dass_results[["Compound ID"]]
    )
    
    id <- switch(
      input$compare_ice_id_type,
      dtxsid = "DTXSID",
      casrn = "CASRN",
      smiles = "QSAR Ready SMILES"
    )
    
    if ("hppt" %in% input$compare_ice_cql) {
      rcol <- switch(
        input$compare_type,
        Hazard = grep_ci("binary hazard", names(hppt_ref), value = T),
        Potency = grep_ci("potency", names(hppt_ref), value = T)
      )
      hppt_tmp <- hppt_ref[!is.na(hppt_ref[[id]]),]
      ref_hppt_data <- as.list(hppt_tmp[match(chems, hppt_tmp[[id]]), rcol, drop = F])
    }
    
    if ("llna" %in% input$compare_ice_cql) {
      rcol <- switch(
        input$compare_type,
        Hazard = grep_ci("binary hazard", names(llna_ref), value = T),
        Potency = grep_ci("potency", names(llna_ref), value = T)
      )
      llna_tmp <- llna_ref[!is.na(llna_ref[[id]]),]
      ref_llna_data <- as.list(llna_tmp[match(chems, llna_tmp[[id]]), rcol, drop = F])
    }
  }
  
  compare_pred(pred)
  compare_ref(c(ref_user_data, ref_hppt_data, ref_llna_data))
})

### Hazard -----
observeEvent({compare_pred(); compare_ref()}, {
  req(input$compare_type == "Hazard")
  pred <- factor(compare_pred(), levels = c("Positive", "Negative", "Inconclusive")) # Add BL?
  pred_noMiss <- (!is.na(pred)) & (pred != "Inconclusive") 
  pred_label <- paste(da_dict[[da()]]$abbrev, input$compare_type)
  
  all_comps <- lapply(compare_ref(), function(x) NULL)
  ref_cols <- names(all_comps)
  
  for (i in ref_cols) {
    ref <- compare_ref()[[i]]
    
    # Check for invalid values
    ref_warn <- any(!grepl_ci(concatOrString(c(call1_str, call0_str)), na.omit(ref)))
    
    # Convert to characters
    ref[grepl_ci(concatOrString(call1_str), ref)] <- "Positive"
    ref[grepl_ci(concatOrString(call0_str), ref)] <- "Negative"
    ref <- factor(ref, levels = c("Positive","Negative"))
    
    # Count missing after convert
    ref_noMiss <- !is.na(ref)
    ref_error <- sum(ref_noMiss) < 5
    
    if (ref_error) {
      out <- list(
        ref_error = ref_error,
        label = paste(i, pred_label, sep = " vs. ")
        )
    } else if (!ref_error) {
      pred_error <- sum(ref_noMiss & pred_noMiss) < 5
      out <- list(
        ref_col = i,
        ref_dat = ref,
        pred_col = pred_label,
        pred_dat = pred,
        id = gsub("\\s+|[.]+", "", tolower(paste(i, pred_label, sep = "_"))),
        label = paste(i, pred_label, sep = " vs. "),
        ref_warn = ref_warn,
        pred_error = pred_error
      )
      
      if (!pred_error) {
        perf <- compareBinary(
          pred = pred,
          ref = ref,
          predCol = pred_label,
          refCol = i
        )
        out <- append(out, perf)
        
        base_plot_data <- data.frame(table(perf$indiv))
        base_plot_data$Var1 <- factor(base_plot_data$Var1 , levels = c("True Positive", "True Negative", "False Positive", "False Negative", "NA"))
        base_plot_data$tt_text <- sprintf("Prediction Type = %s\nCount = %i", base_plot_data$Var1, base_plot_data$Freq)
        
        da_lab <- switch(da(),
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
  
  updatePickerInput(session = session, inputId = "compare_table_id_1", choices = ref_cols, selected = ref_cols[1])
  updatePickerInput(session = session, inputId = "comp_fig_comparison", choices = ref_cols, selected = ref_cols[1])
  updatePickerInput(session = session, inputId = "comp_fig_quant_col", choices = c("None", names(data_tables())), selected = "None")
  
  if (length(ref_cols) > 1) {
    show_hide(show = c("cm_block_vert_2", "div_compare_results"))
    updatePickerInput(session = session, inputId = "compare_table_id_2", choices = ref_cols, selected = ref_cols[2])
  } else {
    show_hide(show = "div_compare_results", hide = "cm_block_vert_2")
    updatePickerInput(session = session, inputId = "compare_table_id_2", choices = NULL)
  }
  
  updateCheckboxGroupInput(
    session = session,
    inputId = "comp_table_choices",
    choiceValues = unname(sapply(all_comps, function(x) x[["ref_col"]])),
    choiceNames = unname(sapply(all_comps, function(x) x[["label"]])),
    selected =  unname(sapply(all_comps, function(x) x[["ref_col"]])),)
  
  compare_tables(all_comps)
  
})

### Potency -----
observeEvent({compare_pred(); compare_ref()}, {
  req(input$compare_type == "Potency")
  pred <- factor(compare_pred(), levels = c("1A", "1B", "NC", "Inconclusive"))
  pred_noMiss <- (!is.na(pred)) & (pred != "Inconclusive")
  pred_label <- paste(da_dict[[da()]]$abbrev, input$compare_type)
  
  all_comps <- lapply(compare_ref(), function(x) NULL)
  ref_cols <- names(all_comps)
  
  for (i in ref_cols) {
    ref <- compare_ref()[[i]]
    
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
      out <- list(
        ref_error = ref_error,
        label = paste(i, pred_label, sep = " vs. ")
      )
    } else if (!ref_error) {
      pred_error <- sum(ref_noMiss & pred_noMiss) < 5
      out <- list(
        ref_col = i,
        ref_dat = ref,
        pred_col = pred_label,
        pred_dat = pred,
        id = gsub("\\s+|[.]+", "", tolower(paste(i, pred_label, sep = "_"))),
        label = paste(i, pred_label, sep = " vs. "),
        ref_warn = ref_warn,
        pred_error = pred_error
      )
      
      if (!pred_error) {
        perf <- compareCat(
          pred = pred,
          ref = ref,
          predCol = pred_label,
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
        
        da_lab <- switch(da(),
                         da_2o3 = "Two-out-of-Three DA",
                         da_its = "Integrated Testing Strategy DA",
                         da_ke31 = "Key Event 3/1 STS DA")
        
        bp_title <- sprintf("Potency Comparison\n%s vs. %s", da_lab, i)
        
        base_plot <- ggplot(base_plot_data, aes(x = ref, y = val, fill = pred, text = tt_text)) + 
          geom_bar(stat = "identity", position = position_dodge(preserve = "single"), color = "black") + 
          labs(x = "Reference Potency", y = "Count", title = bp_title) + 
          scale_x_discrete(labels = c("1A", "1B", "NC", "NA"), breaks = c("1A", "1B", "NC", NA)) +
          scale_fill_manual(name = sprintf("%s Prediction", da_dict[[da()]]$abbrev), values = c("#382A54FF", "#357BA2FF", "#60CEACFF", "#7d7d7d"), breaks = c("1A", "1B", "NC", "Inconclusive", NA), na.value = "#F0F0F0") +
          theme_bw() 
        
        out$base_plot <- base_plot
      }
    }
    all_comps[[i]] <- out
  }
  updatePickerInput(session = session, inputId = "compare_table_id_1", choices = ref_cols, selected = ref_cols[1])
  updatePickerInput(session = session, inputId = "comp_fig_comparison", choices = ref_cols, selected = ref_cols[1])
  updatePickerInput(session = session, inputId = "comp_fig_quant_col", choices = c("None", names(data_tables())), selected = "None")
  
  if (length(ref_cols) > 1) {
    show_hide(show = c("cm_block_vert_2", "div_compare_results"))
    updatePickerInput(session = session, inputId = "compare_table_id_2", choices = ref_cols, selected = ref_cols[2])
  } else {
    show_hide(show = "div_compare_results", hide = "cm_block_vert_2")
    updatePickerInput(session = session, inputId = "compare_table_id_2", choices = NULL)
  }
  
  updateCheckboxGroupInput(
    session = session,
    inputId = "comp_table_choices",
    choiceValues = unname(sapply(all_comps, function(x) x[["ref_col"]])),
    choiceNames = unname(sapply(all_comps, function(x) x[["label"]])),
    selected = unname(sapply(all_comps, function(x) x[["ref_col"]])))
  
  compare_tables(all_comps)
})

## Tables -----
ref_error_grob <- textGrob("Error: Fewer than 5 valid reference values.", gp = gpar(col = "#D55E00", fontsize = 16))
pred_error_grob <- textGrob("Error: Fewer than 5 conclusive prediction values.", gp = gpar(col = "#D55E00", fontsize = 16))

table_shown_1 <- reactiveVal()
table_alt_1 <- reactiveVal()
observeEvent(input$compare_table_id_1, {
  res <- isolate(compare_tables()[[input$compare_table_id_1]])

  if (!is.null(res[["ref_error"]])) {
    table_shown_1(ref_error_grob)
    table_alt_1 <- ref_error_grob$label
  } else {
    if (res$pred_error) {
      table_shown_1(pred_error_grob)
      table_alt_1 <- pred_error_grob$label
    } else {
      table_shown_1(res$fig)
      table_alt_1 <- res$fig_alt
    }
  }
  table_alt_1 <- paste(res$label, 
                       paste(switch(input$compare_type, Hazard = "2", Potency = "3"), "Tables"),
                       table_alt_1, sep = ". ")
  table_alt_1(table_alt_1)
}, ignoreNULL = T)

output$compare_table_1 <- renderPlot({req(table_shown_1()); grid.draw(table_shown_1())}, width = "auto", height = "auto", alt = reactive({table_alt_1()}))

table_shown_2 <- reactiveVal()
table_alt_2 <- reactiveVal()
observeEvent(input$compare_table_id_2, {
  res <- isolate(compare_tables()[[input$compare_table_id_2]])
  
  if (!is.null(res[["ref_error"]])) {
    table_shown_2(ref_error_grob)
    table_alt_2 <- ref_error_grob$label
  } else {
    if (res$pred_error) {
      table_shown_2(pred_error_grob)
      table_alt_2 <- pred_error_grob$label
    } else {
      table_shown_2(res$fig)
      table_alt_2 <- res$fig_alt
    }
  }
  table_alt_2 <- paste(res$label, table_alt_2, sep = ". ")
  table_alt_2(table_alt_2)
}, ignoreNULL = T)
output$compare_table_2 <- renderPlot({req(table_shown_2()); grid.draw(table_shown_2())}, width = "auto", height = "auto", alt = reactive({table_alt_2()}))

### Download -----
observeEvent(input$comp_table_all, {
  updateCheckboxGroupInput(inputId = "comp_table_choices", selected = names(compare_tables()))
})

observeEvent(input$comp_table_none, {
  updateCheckboxGroupInput(inputId = "comp_table_choices", selected = NA)
})

output$dl_comp_tables <- downloadHandler(
  filename = function() {
    if (input$use_demo_data) {
      fname <- "DemoData"
    } else {
      fname <- unlist(strsplit(input$fpath$name, "[.]"))
      fname <- paste(fname[-length(fname)], collapse = ".")
    }
    paste0(fname, "_DASSResults_Compare_", format.Date(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf")
  },
  content = function(con) {
    table_choice <- input$comp_table_choices
    pdf(con)
    for (i in table_choice) {
      if (i != 1) grid.newpage()
      grid.draw(compare_tables()[[i]][["fig"]])
      # if (i != table_choice[length(table_choice)]) grid.newpage()
    }
    dev.off()
  }
)
outputOptions(output, "dl_comp_tables", suspendWhenHidden = FALSE)

output$comp_table_flat_xl <- downloadHandler(
  filename = function() {
    if (input$use_demo_data) {
      fname <- "DemoData"
    } else {
      fname <- unlist(strsplit(input$fpath$name, "[.]"))
      fname <- paste(fname[-length(fname)], collapse = ".")
    }
    paste0(fname, "_DASSResults_Compare_", format.Date(Sys.time(), "%Y%m%d-%H%M%S"), ".xlsx")
  },
  content = function(con) {
    ws_data <- lapply(compare_tables(), function(res) {
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

outputOptions(output, "comp_table_flat_xl", suspendWhenHidden = FALSE)

output$comp_table_flat_txt <- downloadHandler(
  filename = function() {
    if (input$use_demo_data) {
      fname <- "DemoData"
    } else {
      fname <- unlist(strsplit(input$fpath$name, "[.]"))
      fname <- paste(fname[-length(fname)], collapse = ".")
    }
    paste0(fname, "_DASSResults_Compare_", format.Date(Sys.time(), "%Y%m%d-%H%M%S"), ".txt")
  },
  content = function(con) {
    ws_data <- lapply(compare_tables(), function(res) {
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

outputOptions(output, "comp_table_flat_txt", suspendWhenHidden = FALSE)

## Figures -----
# observeEvent(input$comp_fig_comparison, {
#   updatePickerInput(
#     inputId = "comp_fig_quant_col",
#     
#   )
# })

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
  compare_type <- isolate(input$compare_type)
  perf_table <- compare_tables()[[input$comp_fig_comparison]]
  
  check_pt <- input$comp_fig_comparison %in% names(compare_tables())
  req(check_pt)
  if (!is.null(perf_table[["ref_error"]])) {
    perf_plot_error_pred
  } else if (perf_table$pred_error) {
    perf_plot_error_pred
  } else {
    if (input$comp_fig_quant_col == "None") {
      plot_out <- ggplotly(perf_table[["base_plot"]], tooltip = "text")
      plot_out$x$layout$margin$t <- plot_out$x$layout$title$font$size*3
      config(plot_out, modeBarButtonsToRemove = c("autoScale", "select2d", "lasso2d"))
    } else {
      quant_val <- suppressWarnings(as.numeric(data_tables()[[input$comp_fig_quant_col]]))
      if (all(is.na(quant_val))) {
        perf_plot_error_quant
      } else {
        etypes <- switch(
          compare_type,
          Hazard  = c("True Positive", "True Negative", "False Positive", "False Negative", NA),
          Potency = c("True 1A", "True 1B", "True NC", "Overpredicted", "Underpredicted", NA)
        )
        
        cols <- switch(
          compare_type,
          Hazard  = c("#882255", "#117733", "#88CCEE", "#332288", "#999999"),
          Potency = c("#882255", "#CC6677", "#117733", "#88CCEE", "#332288", "#999999")
        )
        
        pt_text <- sprintf(
          "%s\nReference=%s\nDA Prediction=%s\n%s=%.2f",
          perf_table$indiv,
          perf_table$ref_dat,
          perf_table$pred_dat,
          input$comp_fig_quant_col,
          quant_val
        )
        
        pt_plt <- ggplot() + 
          geom_point(aes(x = quant_val, y = perf_table$indiv, color = perf_table$indiv, text = pt_text), position = position_jitter(width = 0)) +
          scale_color_manual(name = "Comparison", breaks = etypes, values = cols) + 
          labs(
            title = sprintf("DA %s %s Prediction vs. %s", da_dict[[da()]]$abbrev, compare_type, isolate(input$comp_fig_comparison)),
            y = NULL,
            x = input$comp_fig_quant_col) +
          theme_bw()
        pt_plt <- ggplotly(pt_plt, tooltip = "text")
        
        dens_plt <- ggplot() + 
          geom_density(aes(x = quant_val, fill = perf_table$indiv, 
                           text = after_stat(sprintf("%s\n%s = %.2f\nDensity = %.2f", fill, input$comp_fig_quant_col, x, density))), alpha = 0.6, color = "#303030") +
          scale_fill_manual(name = "Comparison", breaks = etypes, values = cols) + 
          labs(y = "Density", x = input$comp_fig_quant_col) + 
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
        config(plot_out, modeBarButtonsToRemove = c("autoScale", "select2d", "lasso2d"))
      }
    }
  }
})

output$comp_fig <- renderPlotly({
  req(input$comp_fig_comparison)
  perf_shown()
})
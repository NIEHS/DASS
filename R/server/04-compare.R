# Performance and Comparison =====
observeEvent(input$goToCompare, tab_change("tabs", tab_names[[6]]))

hppt_ref <- read.table("www/ice_references/2024June13_OECD Defined Approach to Skin Sensitization Human (R).txt", sep = "\t", comment.char = "", header = T, check.names = F)
llna_ref <- read.table("www/ice_references/2024June13_OECD Defined Approach to Skin Sensitization LLNA (R).txt", sep = "\t", comment.char = "", header = T, check.names = F)

compare_pred <- reactiveVal()
compare_ref <- reactiveVal()
compare_tables <- reactiveVal()
point_ids <- reactiveVal()
compare_flat <- reactiveVal()
compare_all <- reactiveVal()
quant_summary <- reactiveVal()

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

    if (input$compare_ice) {
      point_ids(data_tables()[,input$compare_ice_cid])
      show_hide(hide = "comp_fig_chem_col")
    } else {
      point_ids(NULL)
      show_hide(show = "comp_fig_chem_col")
      updatePickerInput(inputId = "comp_fig_chem_col", choices = c("None", names(data_tables())), selected = "None")
    }
  } else if (wf() == "bl") {
    dass_res <- dass_result_bl()$dass_results
    pred <- dass_res[["DA 2o3 Hazard"]]

    if ( input$compare_usr_dt ) {
      if (input$use_demo_data) {
        ref_user_data <- demo_data()[[input$compare_bl_ws]]
      } else {
        ref_user_data <- data.frame(readxl::read_excel(input$fpath$datapath, sheet = input$compare_bl_ws, na = c("", "na", "NA")))
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
      idx <- match(dass_res[["Compound ID"]], ref_cid)
      ref_list <- list()
      ref_list[[input$compare_bl_ws]] <- ref_user_data[idx,]

      data_tables(c(isolate(data_tables()), ref_list))
      ref_user_data <- as.list(ref_user_data[idx,input$compare_usr_dt_col,drop=F])
    } 
    
    point_ids(dass_res[["Compound ID"]])
    show_hide(hide = "comp_fig_chem_col")
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
  pred <- factor(compare_pred(), levels = c("Positive", "Negative", "Borderline", "Inconclusive"))
  pred_noMiss <- (!is.na(pred)) & (pred != "Inconclusive") 
  pred_label <- paste(da_dict[[da()]]$abbrev, input$compare_type)
  
  all_comps <- lapply(compare_ref(), function(x) NULL)
  ref_cols <- names(all_comps)
  
  perf_rows <- c("N", "True Positive", "False Positive","False Negative", "True Negative", "Borderline", "Inconclusive", "Sensitivity", "Specificity", "Balanced Accuracy", "Accuracy", "F1 Score")
  perf_flat <- lapply(compare_ref(), function(x) {
    tmp <- rep(NA, length(perf_rows))
    names(tmp) <- perf_rows
    return(tmp)
  })

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
        ref_col = i,
        ref_error = ref_error,
        pred_col = pred_label,
        pred_error = F,
        label = paste(i, pred_label, sep = " vs. "),
        fig = ref_error_ui
        )
    } else if (!ref_error) {
      pred_error <- sum(ref_noMiss & pred_noMiss) < 5
      out <- list(
        ref_col = i,
        ref_dat = ref,
        ref_error = F,
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
        base_plot_data$Var1 <- factor(base_plot_data$Var1 , levels = c("True Positive", "True Negative", "False Positive", "False Negative", "Borderline", "Inconclusive", "NA"))
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
        
       perf_add <- sapply(perf$perf_list, function(x) {
          if(typeof(x) == "double") x <- roundPercent(x)
          return(x)
        })
       names(perf_add) <- names(perf$perf_list)
       
       perf_idx <- match(names(perf_add), names(perf_flat[[i]]))
       perf_flat[[i]][perf_idx] <- perf_add
      } else {
        out$fig <- pred_error_ui
      }
    }
    all_comps[[i]] <- out
  }

  perf_flat <- lapply(perf_flat, function(x) data.frame(x))
  perf_flat <- do.call("cbind", perf_flat)
  colnames(perf_flat) <- ref_cols
  compare_flat(perf_flat)
  
  updatePickerInput(session = session, inputId = "compare_table_id_1", choices = ref_cols, selected = ref_cols[1])
  updatePickerInput(session = session, inputId = "comp_fig_comparison", choices = c("All", ref_cols), selected = "All")
  updatePickerInput(session = session, inputId = "comp_fig_quant_col", 
                    choices = c("None", 
                                switch(wf(), std = names(data_tables()), bl = bl_ws_idx()[[input$compare_bl_ws]])), 
                    selected = "None")
  
  show_hide(show = c("div_compare_results"))
  shinyjs::runjs("$('#tabDef_hazard').attr('open', true); $('#tabDef_potency').removeAttr('open');")
  
  updateCheckboxInput(inputId = "compare_show_flat", value = F)
  if (length(ref_cols) > 1) {
    updatePickerInput(session = session, inputId = "compare_table_id_2", choices = ref_cols, selected = ref_cols[2])
  } else {
    updatePickerInput(session = session, inputId = "compare_table_id_2", choices = NULL)
  }
  
  updateCheckboxGroupInput(
    session = session,
    inputId = "comp_table_choices",
    choiceValues = unname(sapply(all_comps, function(x) x[["ref_col"]])),
    choiceNames = unname(sapply(all_comps, function(x) x[["label"]])),
    selected =  unname(sapply(all_comps, function(x) x[["ref_col"]])))
  
  compare_tables(all_comps)

  comp_merged <- lapply(seq_along(all_comps), function(i) {
    tab <- all_comps[[i]]
    if (tab$ref_error | tab$pred_error) {
      out <- data.frame(Reference = names(all_comps)[i], Comparison = "NA", Count = 0)
    } else {
      out <- data.frame(names(all_comps)[i], table(all_comps[[i]]$indiv))
      names(out) <- c("Reference", "Comparison", "Count")
    }
    return(out)
  })
  comp_merged <- do.call("rbind", comp_merged)
  compare_all(comp_merged)
})

### Potency -----
observeEvent({compare_pred(); compare_ref()}, {
  req(input$compare_type == "Potency")
  pred <- factor(compare_pred(), levels = c("1A", "1B", "NC", "Inconclusive"))
  pred_noMiss <- (!is.na(pred)) & (pred != "Inconclusive")
  pred_label <- paste(da_dict[[da()]]$abbrev, input$compare_type)
  
  all_comps <- lapply(compare_ref(), function(x) NULL)
  ref_cols <- names(all_comps)
  
  perf_rows <- c("N", "True NC", "True 1B","True 1A", "Overpredicted", "Underpredicted", "Accuracy", "Inconclusive", "NA")
  perf_flat <- lapply(compare_ref(), function(x) {
    tmp <- rep(NA, length(perf_rows))
    names(tmp) <- perf_rows
    return(tmp)
  })
  
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
        ref_col = i,
        ref_error = ref_error,
        pred_col = pred_label,
        pred_error = F,
        label = paste(i, pred_label, sep = " vs. "),
        fig = ref_error_ui
      )
    } else if (!ref_error) {
      pred_error <- sum(ref_noMiss & pred_noMiss) < 5
      out <- list(
        ref_col = i,
        ref_dat = ref,
        ref_error = F,
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
        
        perf_add <- c(N = unname(perf$perf_list$N),
                      table(perf$indiv),
                      Accuracy = roundPercent(perf$perf_list$Accuracy))
        
        perf_idx <- match(names(perf_add), names(perf_flat[[i]]))
        perf_flat[[i]][perf_idx] <- perf_add
        
      } else {
        out$fig <- pred_error_ui
      }
    }
    all_comps[[i]] <- out
  }
  
  perf_flat <- lapply(perf_flat, function(x) data.frame(x))
  perf_flat <- do.call("cbind", perf_flat)
  colnames(perf_flat) <- ref_cols
  compare_flat(perf_flat)
  
  updatePickerInput(session = session, inputId = "compare_table_id_1", choices = ref_cols, selected = ref_cols[1])
  updatePickerInput(session = session, inputId = "comp_fig_comparison", choices = c("All", ref_cols), selected = "All")
  updatePickerInput(session = session, inputId = "comp_fig_quant_col", choices = c("None", names(data_tables())), selected = "None")
  
  show_hide(show = "div_compare_results")
  shinyjs::runjs("$('#tabDef_potency').attr('open', true); $('#tabDef_hazard').removeAttr('open');")

  updateCheckboxInput(inputId = "compare_show_flat", value = F)
  if (length(ref_cols) > 1) {
    updatePickerInput(session = session, inputId = "compare_table_id_2", choices = ref_cols, selected = ref_cols[2])
  } else {
    updatePickerInput(session = session, inputId = "compare_table_id_2", choices = NULL)
  }

  updateCheckboxGroupInput(
    session = session,
    inputId = "comp_table_choices",
    choiceValues = unname(sapply(all_comps, function(x) x[["ref_col"]])),
    choiceNames = unname(sapply(all_comps, function(x) x[["label"]])),
    selected = unname(sapply(all_comps, function(x) x[["ref_col"]])))
  
  compare_tables(all_comps)
  
  comp_merged <- lapply(seq_along(all_comps), function(i) {
    tab <- all_comps[[i]]
    if (tab$ref_error | tab$pred_error) {
      out <- data.frame(Reference = names(all_comps)[i], Comparison = "NA", Count = 0)
    } else {
      out <- data.frame(names(all_comps)[i], table(all_comps[[i]]$indiv))
      names(out) <- c("Reference", "Comparison", "Count")
    }
    return(out)
  })
  
  comp_merged <- do.call("rbind", comp_merged)
  compare_all(comp_merged)
  
})

## Tables -----
ref_error_ui <- div(class = "warningText", "Error: Fewer than 5 valid reference values.")
pred_error_ui <- div(class = "warningText", "Error: Fewer than 5 conclusive prediction values.")

output$compare_table_1 <- renderUI({
  req(input$compare_table_id_1)
  compare_tables()[[input$compare_table_id_1]]$fig
})

output$compare_table_2 <- renderUI({
  req(input$compare_table_id_2)
  compare_tables()[[input$compare_table_id_2]]$fig
})

observe({
  if (input$compare_show_flat) {
    show_hide(show = "cm_block_full", hide = c("cm_block_vert_1", "cm_block_vert_2"))
  } else {
    if(length(compare_tables()) > 1) {
      show_hide(show = c("cm_block_vert_1", "cm_block_vert_2"), hide = "cm_block_full")
    } else {
      show_hide(show = c("cm_block_vert_1"), hide = c("cm_block_full", "cm_block_vert_2"))
    }
  }
})

output$compare_flat <- renderUI({
  req(compare_flat())
  compare_flat <- compare_flat()
  tags$table(
    class = "cm-table",
    tags$caption("Comparison Table"),
    tags$thead(
      tags$tr(
        tags$td(class = "blank-cell"),
        lapply(names(compare_flat), function (x) tags$th(scope = "col", x))
      )
    ),
    tags$tbody(
      lapply(1:nrow(compare_flat), function(i) {
        tags$tr(
          tags$th(scope = "row", rownames(compare_flat)[i]),
          lapply(1:ncol(compare_flat), function(j) {
            tags$td(compare_flat[i,j])
          })
        )
      })
    )
  )
})

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
    pdf(con, height = 11, width = 8.5)
    for (i in table_choice) {
      if (!compare_tables()[[i]]$ref_error & !compare_tables()[[i]]$pred_error) {
        if (i != 1) grid.newpage()
        grid.draw(compare_tables()[[i]][["fig_gg"]])
      }
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
    compare_flat <- compare_flat()
    pred_label <- paste(da_dict[[da()]]$abbrev, input$compare_type)
    new_row <- as.data.frame(lapply(compare_flat, function(x) pred_label), check.names = F, row.names = "DA Prediction")
    ws_data <- rbind(new_row, compare_flat)
    
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = "DASSPerformance")
    writeData(wb, sheet = "DASSPerformance", ws_data, rowNames = T)
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
    compare_flat <- compare_flat()
    pred_label <- paste(da_dict[[da()]]$abbrev, input$compare_type)
    new_row <- as.data.frame(lapply(compare_flat, function(x) pred_label), check.names = F, row.names = "DA Prediction")
    ws_data <- rbind(new_row, compare_flat)
    
    write.table(x =  ws_data, file = con, quote = F, row.names = T, sep = "\t")
  }
)

outputOptions(output, "comp_table_flat_txt", suspendWhenHidden = FALSE)

## Figures -----
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

perf_plot <- reactiveVal()
perf_tab <- reactiveVal()

observeEvent({compare_tables(); input$comp_fig_comparison; input$comp_fig_quant_col; input$comp_fig_chem_col}, {
  req(input$comp_fig_comparison)
  req(compare_tables())
  quant_summary(NULL)

  do_all <- input$comp_fig_comparison == "All"
  do_quant <- input$comp_fig_quant_col != "None"
  do_cid <- !is.null(point_ids()) || input$comp_fig_chem_col != "None"
  
  if (!do_all) {
    req(input$comp_fig_comparison %in% names(compare_tables()))
    perf_table <- compare_tables()[[input$comp_fig_comparison]]
    if (perf_table[["ref_error"]]) {
      perf_plot(perf_plot_error_pred)
      req(F)
    } else if (perf_table$pred_error) {
      perf_plot(perf_plot_error_pred)
      req(F)
    }
  }
  
  if (do_quant) {
    quant_val <- suppressWarnings(as.numeric(
      switch(
        wf(),
        std = data_tables()[[input$comp_fig_quant_col]],
        bl = data_tables()[[input$compare_bl_ws]][[input$comp_fig_quant_col]]
      )
    ))
    
    if (all(is.na(quant_val))) {
      perf_plot(perf_plot_error_quant)
      req(F)
    }
  }

  compare_type <- isolate(input$compare_type)
  
  etypes <- switch(
    compare_type,
    Hazard  = c("True Positive", "True Negative", "False Positive", "False Negative", "Borderline", "Inconclusive", "NA"),
    Potency = c("True 1A", "True 1B", "True NC", "Overpredicted", "Underpredicted", "Inconclusive", "NA")
  )
  
  cols <- switch(
    compare_type,
    Hazard  = c("#882255", "#117733", "#88CCEE", "#332288", "#A6995A", "#808080", "#D3D3D3"),
    Potency = c("#882255", "#CC6677", "#117733", "#88CCEE", "#332288", "#808080", "#D3D3D3")
  )
  
  names(cols) <- etypes
  if (do_all) {
    if (!do_quant) {
      compare_all <- compare_all()
      tt_text <- sprintf(
        "Reference = %s\nPrediction Type = %s\nCount = %i",
        compare_all$Reference,
        compare_all$Comparison,
        compare_all$Count
      )
      plot_out <- ggplot(compare_all, aes(x = Comparison, y = Count, fill = Comparison, text = tt_text)) + 
        geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) + 
        facet_grid(.~Reference) +
        scale_fill_manual(breaks = etypes, values = cols) +
        scale_y_continuous(expand = expansion(mult = c(0,0.05), add = c(0,0))) +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          strip.background = element_blank(),
          strip.text = element_text(size = 11, face = "bold")
        )
      plot_out <- ggplotly(plot_out, tooltip = "text")
    } else if (do_quant) {
      compare_tables <- compare_tables()
      plot_out <- lapply(seq_along(compare_tables), function(i) {
        tab <- compare_tables[[i]]
        refCol <- names(compare_tables)[i]
        if (tab$ref_error | tab$pred_error) {
          comp <- "NA"
          dat <- data.frame(Reference = i, Comparison = "NA", Value = NA, tt_text = NA)
        } else {
          comp <- ifelse(is.na(tab$indiv), "NA", as.character(tab$indiv))
          dat <- data.frame(Reference = i, Comparison = comp, Value = quant_val)
          
          pt_text <- sprintf(
            "Reference = %s\nPrediction Type = %s\n%s=%.2f",
            refCol,
            dat$Comparison,
            input$comp_fig_quant_col,
            quant_val
          )
          
          if (!is.null(point_ids())) {
            pt_text <- paste(pt_text, "\nID =", point_ids())
          } else if (do_cid) {
            pt_text <- paste(pt_text, "\nID =", data_tables()[,input$comp_fig_chem_col])
          }
          
          dat$tt_text <- pt_text
        }

        dat$Comparison <- factor(dat$Comparison, levels = etypes)
        dat <- dat[order(dat$Comparison),]
        dat$Comparison <- droplevels(dat$Comparison)
        dat <- split(dat, dat$Comparison)
        new_plot <- plot_ly(colors = cols, type = "violin", points = "all")
        for (j in seq_along(dat)) {
          plot_data <- dat[[j]]
          new_plot <- new_plot %>%
            add_trace(
              x = plot_data$Comparison,
              y = plot_data$Value,
              color = plot_data$Comparison,
              text = plot_data$tt_text,
              side = "positive",
              legendgroup = plot_data$Comparison,
              showlegend = ifelse(i == 1, T, F),
              hoverinfo = "text"
            )
        }
        new_plot <- new_plot %>%
          layout(
            xaxis = list(title = refCol, categoryarray = levels(dat), categoryorder = "array"),
            yaxis = list(title = input$comp_fig_quant_col)
          )
        return(new_plot)
      })
      plot_out <- subplot(plot_out, shareY = T, titleX = T) %>%
        layout(title = sprintf("DA %s Predictions vs. References", da_dict[[da()]]$abbrev))
    } 
  } else if (!do_all) {
    if (!do_quant) {
      plot_out <- ggplotly(perf_table[["base_plot"]], tooltip = "text")
      plot_out$x$layout$margin$t <- plot_out$x$layout$title$font$size*3
    } else if (do_quant) {
      pt_text <- sprintf(
        "%s\nReference=%s\nDA Prediction=%s\n%s=%.2f",
        perf_table$indiv,
        perf_table$ref_dat,
        perf_table$pred_dat,
        input$comp_fig_quant_col,
        quant_val
      )
      
      if (!is.null(point_ids())) {
        pt_text <- paste(pt_text, "\nID =", point_ids())
      } else if (do_cid) {
        pt_text <- paste(pt_text, "\nID =", data_tables()[,input$comp_fig_chem_col])
      }
      
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

      quant_summary <- lapply(etypes, function(e) {
        idx <- perf_table$indiv == e
        if (sum(idx, na.rm = T) > 0) {
          x <- quant_val[idx]
          out <- data.frame(
            N = sum(!is.na(x)), 
            Min = min(x, na.rm = T), 
            Mean = mean(x, na.rm = T), 
            Median = median(x, na.rm = T), 
            Max = max(x, na.rm = T), 
            check.names = F)
          rownames(out) <- e
          return(out)
        }
      })
      quant_summary <- do.call("rbind", quant_summary)
      quant_summary(quant_summary)
    }
  }
  plot_out <- plot_out %>% config(plot_out, modeBarButtonsToRemove = c("autoScale", "select2d", "lasso2d"))
  perf_plot(plot_out)
}, ignoreNULL = F)

output$comp_fig <- renderPlotly({
  req(input$comp_fig_comparison)
  perf_plot()
})

output$compare_table_quant <- renderUI({
  quant_summary <- quant_summary()
  if (is.null(quant_summary)) {
    ""
  } else {
    tags$table(
      class = "cm-table",
      tags$caption("Quantitative Data Summary"),
      tags$thead(
        tags$tr(
          tags$td(class = "blank-cell"),
          lapply(colnames(quant_summary), function (x) tags$th(scope = "col", x))
        )
      ),
      tags$tbody(
        lapply(1:nrow(quant_summary), function(i) {
          tags$tr(
            tags$th(scope = "row", rownames(quant_summary)[i]),
            lapply(1:ncol(quant_summary), function(j) {
              if (typeof(quant_summary[i,j]) == "double") {
                tags$td(round(quant_summary[i,j], digits = 2))
              } else {
                tags$td(quant_summary[i,j])
              }
              
            })
          )
        })
      )
    )
  }
})
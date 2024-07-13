# =============================================================================#
# File Name: Step6-Performance.R
# Original Creator: ktto
# Contact Information: ICE-support@niehs.nih.gov
# Date Created: 2023-08-03
# License: MIT
# Description: server file for module with performance options
# Required Packages:
# - data.table, DT
# - openxlsx
# - shiny shinyBS shinyjs
# =============================================================================#

# Step 6: Performance -----
observeEvent(input$goToCompare, updateTabsetPanel(inputId = "step_set", selected = "Compare"))

# Update prediction column menu
observe({
  req(all_out$result_df)
  da_choices <- names(all_out$result_df)[all_out$dass_results]
  da_choices <- grep("hazard|potency", da_choices, value = T)
  updateRadioButtons(inputId = "perf_pred_col", choices = da_choices)
  updateSelectInput(inputId = "perf_ref_col", choices = names(dt_analyze()))
  updateSelectInput(inputId = "perf_ice_user_identifier", choices = names(dt_analyze()))
})

hppt_ref <- fread("www/ice_references/2024June13_OECD Defined Approach to Skin Sensitization Human (R).txt", data.table = F)
llna_ref <- fread("www/ice_references/2024June13_OECD Defined Approach to Skin Sensitization LLNA (R).txt", data.table = F)

# Reactives -----
perf_ref_data <- reactiveVal()
perf_tables <- reactiveVal()

# Do Comparison -----
observeEvent(input$do_compare, {
  pred_type <- gsub(".*\\.", "", input$perf_pred_col)
  ref_user_data <- NULL
  ref_hppt_data <- NULL
  ref_llna_data <- NULL
  
  if ("user_data" %in% input$perf_ref_col_source) {
    ref_user_data <- as.list(all_out$result_df[,input$perf_ref_col,drop=F])
  }
  
  if ("ice" %in% input$perf_ref_col_source) {
    id <- switch(
      input$perf_ice_identifier_type,
      dtxsid = "DTXSID",
      casrn = "CASRN",
      smiles = "QSAR Ready SMILES"
    )
    
    if ("hppt" %in% input$perf_ice_ref_cql) {
      rcol <- switch(
        pred_type, 
        hazard = grep_ci("binary hazard", names(hppt_ref), value = T),
        potency = grep_ci("potency", names(hppt_ref), value = T)
      )
      ref_hppt_data <- as.list(hppt_ref[match(all_out$result_df[[input$perf_ice_user_identifier]], hppt_ref[[id]]), rcol, drop = F])
    }
    if ("llna" %in% input$perf_ice_ref_cql) {
      rcol <- switch(
        pred_type, 
        hazard = grep_ci("binary hazard", names(llna_ref), value = T),
        potency = grep_ci("potency", names(llna_ref), value = T)
      )
      ref_llna_data <- as.list(llna_ref[match(all_out$result_df[[input$perf_ice_user_identifier]], llna_ref[[id]]), rcol, drop = F])
    }
  }
  
  perf_ref_data(c(ref_user_data, ref_hppt_data, ref_llna_data))
  switch(
    pred_type,
    hazard = binary_compare(),
    potency = cat_compare()
  )
  
  updateSelectInput(
    inputId = "perf_fig_quant_col",
    choices = names(quant_data_key()[quant_data_key()])
  )
  
  id_select <- NULL
  if (input$perf_use_ice_data | "ice" %in% input$perf_ref_col_source) {
    id_select <- input$perf_ice_user_identifier
  } 
  
  updateSelectInput(inputId = "perf_fig_id_col",
                    choices = names(dt_analyze()),
                    selected = id_select)
  
})

## Binary -----
binary_compare <- reactive({
  pred <- all_out$result_df[[input$perf_pred_col]]
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

  shinyjs::show("compare_table_body")
  shinyjs::show("binary_defs")
  shinyjs::hide("potency_defs")
  perf_tables(all_comps)
})

## Categorical -----
cat_compare <- reactive({
  pred <- all_out$result_df[[input$perf_pred_col]]
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
        pred_col = input$perf_pred_col,
        pred_dat = pred,
        id = gsub("\\s+|[.]+", "", tolower(paste(i, input$perf_pred_col, sep = "_"))),
        label = paste(i, input$perf_pred_col, sep = " vs. "),
        ref_warn = ref_warn,
        pred_error = pred_error
      )
      
      if (!pred_error) {
        perf <- compareCat(
          pred = pred,
          ref = ref,
          predCol = input$perf_pred_col,
          refCol = i
        )
        out <- append(out, perf)
        
        base_plot_data <- data.frame(
          ref = ref,
          pred = pred,
          pred_type = perf$indiv,
          val = 1
        )
        
        base_plot_data <- aggregate(val ~ ref + pred + pred_type, data = base_plot_data, sum)
        base_plot_data$tt_text <- sprintf("Prediction Type = %s\nPrediction = %s\nReference = %s\nCount = %i",
                                          base_plot_data$pred_type, base_plot_data$pred, base_plot_data$ref, base_plot_data$val)
        base_plot_data$ref <- sprintf("Reference = %s", base_plot_data$ref) 
        
        da_lab <- switch(input$selected_da,
                         da_2o3 = "Two-out-of-Three DA",
                         da_its = "Integrated Testing Strategy DA",
                         da_ke31 = "Key Event 3/1 STS DA")
        
        bp_title <- sprintf("Potency Comparison\n%s vs. %s", da_lab, i)
        
        base_plot <- ggplot(base_plot_data, aes(x = pred, y = val, text = tt_text)) + 
          geom_bar(stat = "identity") +
          facet_grid(.~ref) + 
          labs(x = "Potency Prediction", y = "Count", title = bp_title) + 
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
  } else {
    shinyjs::hide(id = "perf_table_block_2")
    updateSelectInput(session = session, inputId = "reference_perf_2", choices = NULL)
  }
  
  updateCheckboxGroupInput(
    session = session, 
    inputId = "perf_table_choices",
    choiceValues = unname(sapply(all_comps, function(x) x[["ref_col"]])),
    choiceNames = unname(sapply(all_comps, function(x) x[["label"]])))
  
  shinyjs::show("compare_table_body")
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

## Download -----
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

#   defineScale()

# Figures -----
observe({
  req(perf_tables())
  updateSelectInput(
    inputId = "perf_fig_comparison",
    choices = names(perf_tables()))
})

perf_plots <- reactiveVal()
observeEvent(input$create_perf_figs, {

  # Check that at least one comparison was selected
  ##
  ##
  
  num_dat <- lapply(all_out$result_df[,input$perf_fig_quant_col,drop=F], as.numeric)
  id_lab <- lapply(input$perf_fig_id_col, function(x) sprintf("%s = %s\n", x, all_out$result_df[[x]]))
  id_lab <- do.call(paste0, id_lab)
  
  # Create plotting data
  perf_plot_all <- lapply(input$perf_fig_comparison, function(comp_id) {
    dat <- data.frame(perf_tables()[[comp_id]][c("ref_plot", "pred_plot", "indiv")])
    dat$res_lab <- sprintf("%sPrediction Type = %s\nPrediction = %s\nReference = %s\n",
                           id_lab, dat$indiv, dat$pred_plot, dat$ref_plot)
    
    # Loop through each selected quantitative column and plot
    tmp <- lapply(input$perf_fig_quant_col, function(num_col) {
      dat[[num_col]] <- suppressWarnings(as.numeric(all_out$result_df[,num_col]))
      dat$res_lab <- sprintf("%s%s = %.2f", dat$res_lab, num_col, dat[[num_col]])
      
      # Remove data with no DA prediction
      dat <- dat[dat$indiv != "NA",]
      
      # Prepare histograms
      ## COUNTS ARE WRONG??
      dat$bins <- cut(dat[[num_col]], breaks = 30)
      bin_count <- aggregate(res_lab ~ indiv + bins, data = dat, length) # res_lab is used as an arbitrary placeholder to get the lengths
      dat$bin_count <- bin_count[["res_lab"]][match(dat$indiv, bin_count$indiv)]
      dat$hist_lab <- sprintf("%s Bin Range = %s\nCount = %i", num_col, dat$bins, dat$bin_count)
      hist_breaks <- as.numeric(unique(gsub("\\(|\\]", "", unlist(strsplit(levels(dat$bins), ",")))))

      # Get the y-axis location for points      
      y_max <- aggregate(bins ~ indiv, data = dat, function(x) max(table(x)) + 2)
      dat$y_max <- y_max[["bins"]][match(dat$indiv, y_max$indiv)]
      
      # Create median line
      med_dat <- aggregate(as.formula(sprintf("%s ~ indiv", num_col)), data = dat, function(x) median(x))
      med_dat$med_lab <- sprintf("%s Median = %.2f", num_col, med_dat[[num_col]])

      ggplot(dat, aes_string(x = num_col)) + 
        geom_histogram(aes(text = hist_lab), color = "black", fill = "#919bb5", breaks = hist_breaks) +
        geom_vline(data = med_dat, aes_string(xintercept = num_col, text = "med_lab"), lwd = 2) +
        geom_point(aes(y = y_max, text = res_lab),
                   position = position_jitter(width = 0, height = 0.85),
                   size = 2,
                   pch = 21, 
                   fill = "#919bb5") +
        labs(x = num_col, y = "Count") + 
        facet_wrap(indiv~., ncol = 1, scales = "free_y") + 
        theme_bw()
    })
    names(tmp) <- input$perf_fig_quant_col
    return(tmp)
  })
  names(perf_plot_all) <- input$perf_fig_comparison
  
  updateSelectInput(inputId = "perf_fig_comp_select", choices = input$perf_fig_comparison)
  updateSelectInput(inputId = "perf_fig_quant_select", choices = input$perf_fig_quant_col)
  
  perf_plots(perf_plot_all)
})


output$perf_fig <- renderPlotly({
  req(perf_plots())
  req(input$perf_fig_comp_select)
  req(input$perf_fig_quant_col)
  ggplotly(perf_plots()[[input$perf_fig_comp_select]][[input$perf_fig_quant_select]], tooltip = "text")
})

# comp_fig <- reactiveVal()
# observe({
#   req(perf_tables())
#   if (input$perf_fig_quant_col == "None") {
#     comp_fig(perf_tables()[[input$perf_fig_comparison]]$base_plot)
#   } else {
#     
#     data.frame(
#       perf_tables()[[input$perf_fig_comparison]][c("ref_dat", "pred_dat", "indiv")],
#       all_out$result_df[,input$perf_fig_quant_col,drop=F],
#       all_out$result_df[,input$perf_fig_id_col,drop = F]
#       )
#   }
# })

# output$perf_fig <- renderPlotly({
#   req(comp_fig())
#   layout(
#     ggplotly(comp_fig(), tooltip = "text"),
#     title = list(x = 0.5, automargin = T),
#     margin = list(t = 120)
#   )
# })

# observe({
#   if (is.null(input$tableChoices)) {
#     shinyjs::disable("dlPerf")
#   } else {
#     shinyjs::enable("dlPerf")
#   }
# })
 

# # Violins -----
# # violinPlot <- reactiveVal()
# # violinData <- reactiveVal()
# # violin_yMax <- reactiveVal()
# # violin_density_tt <- reactiveVal()
# # resType <- reactiveVal()
# # violin_point_tt <- reactiveVal()
# # 
# # # Pull numeric data
# # observe({
# #   violinData(numeric_data()[[input$violinDensitySelect]])
# # })
# # 
# # # Update result info
# # observe({
# #   req(violinData())
# #   refCol <- perfTabs()[[input$violinCompareSelect]][["refCol"]]
# #   predCol <- perfTabs()[[input$violinCompareSelect]][["predCol"]]
# #   
# #   resType <- perfVals()[[refCol]][[predCol]][["indiv"]]
# #   resType(resType)
# #   violin_yMax(max(by(violinData(), resType, function(x) max(density(na.omit(x))$y))))
# # 
# #   min_vals <- by(violinData(), resType, min, na.rm = T)
# #   max_vals <- by(violinData(), resType, max, na.rm = T)
# #   med_vals <- by(violinData(), resType, median, na.rm = T)
# #   
# #   violin_density_tt <- sprintf("Reference = %s<br>Prediction = %s<br>Type = %s<br>%s Range = [%.2f, %.2f]<br>%s Median = %.2f", 
# #                       refCol, predCol, 
# #                       resType,
# #                       input$violinDensitySelect,
# #                       min_vals[resType],
# #                       max_vals[resType],
# #                       input$violinDensitySelect,
# #                       med_vals[resType])
# #   violin_density_tt(violin_density_tt)
# #   
# #   
# #   violin_point_tt <- sprintf("Reference = %s<br>Prediction = %s<br>Type = %s<br>%s = %.2f<br>", 
# #                      refCol, predCol, 
# #                      resType,
# #                      input$violinDensitySelect,
# #                      violinData())
# #   violin_point_tt(violin_point_tt)
# # })
# # 
# # observe({
# #   req(violinData())
# # 
# #   myPlot <- ggplot(mapping = aes(x = violinData(), color = resType(), fill = resType())) + 
# #     geom_density(mapping = aes(text = violin_density_tt()), size = 1) + 
# #     geom_point(mapping = aes(
# #       y = violin_yMax() + violin_yMax()/4,
# #       x = violinData(),
# #       text = violin_point_tt()),
# #       position = position_jitter(width = 0, height = violin_yMax()/8)
# #     ) + 
# #     scale_y_continuous(name = "Density", expand = c(0,0,0, violin_yMax()/8)) + 
# #     violin_scale() +
# #     theme_classic() +
# #     theme(
# #       axis.line = element_line(size = 4),
# #       axis.title = element_text(size = 16),
# #       axis.text = element_text(size = 12)) + 
# #     labs(x = input$violinDensitySelect)
# #   
# #   violinPlot(myPlot)
# # })
# # 
# # output$violin <- renderPlotly({
# #   req(violinPlot())
# #   ggplotly(violinPlot(), tooltip = "text")})




# blue <- rgb(red = 0, green = 90, blue = 181, maxColorValue = 255)
# red <- rgb(red = 220, green = 50, blue = 32, maxColorValue = 255)
# gold <- rgb(red = 187, green = 142, blue = 81, maxColorValue = 255)
# 
# blue_alpha <- rgb(red = 0, green = 90, blue = 181, alpha = 150, maxColorValue = 255)
# red_alpha <- rgb(red = 220, green = 50, blue = 32, alpha = 150, maxColorValue = 255)
# gold_alpha <- rgb(red = 187, green = 142, blue = 81, alpha = 150, maxColorValue = 255)
# 
# violin_scale <- reactiveVal()
# defineScale <- reactive({
#   req(input$compareType)
#   if (input$compareType == "Hazard") {
#     violin_scale(
#       list(
#         scale_fill_manual(
#           values = c(
#             "NA" = "#808080",
#             "True Positive" = gold_alpha,
#             "True Negative" = gold_alpha,
#             "False Positive" = blue_alpha,
#             "False Negative" = red_alpha
#           )
#         ),
#         scale_color_manual(
#           values = c(
#             "NA" = "#808080",
#             "True Positive" = gold,
#             "True Negative" = gold,
#             "False Positive"  = blue,
#             "False Negative" = red
#           )
#           )
#         )
#     )
#   } else if (input$compareType == "Potency") {
#     violin_scale(
#       list(
#         scale_fill_manual(
#           values = c(
#             "NA" = "#808080",
#             `True 1A` = gold_alpha,
#             `True 1B` = gold_alpha,
#             `True NC` = gold_alpha,
#             Overpredicted = blue_alpha,
#             Underpredicted = red_alpha
#           )
#         ),
#         scale_color_manual(
#           values = c(
#             "NA" = "#808080",
#             `True 1A` = gold,
#             `True 1B` = gold,
#             `True NC` = gold,
#             Overpredicted = blue,
#             Underpredicted = red
#           )
#         )
#       )
#     )
#     }
# })
# 
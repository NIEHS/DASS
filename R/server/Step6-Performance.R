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
observeEvent(input$goToCompare, updateTabsetPanel(inputId = "stepSet", selected = "Compare"))

# Update prediction column menu
observe({
  req(dass_res$results)
  switch(input$compareType,
         Hazard = {
           daBins = grep("^DA ITS Call$|^DA 2o3 Call$|^DA KE 3/1 STS Call$", names(dass_res$results), value = T)
           updateSelectInput(inputId = "perfPredCol", choices = daBins)
           updateSelectInput(inputId = "perfRefRes", choices = c(names(dt_analyze())), selected = "")
         },
         Potency = {
           daPots = grep("^DA ITS Potency$|^DA KE 3/1 STS Potency$", names(dass_res$results), value = T)
           updateSelectInput(inputId = "perfPredCol", choices = daPots)
           updateSelectInput(inputId = "perfRefRes", choices = c(names(dt_analyze())), selected = "")
         })
  updateSelectInput(inputId = "overlayColumnSelect", choices = c(names(dt_analyze())), selected = "")
})

## Reactives -----
perfTabs <- reactiveVal()
perfVals <- reactiveVal()
plotShown <- reactiveVal()

## Compare -----
observeEvent(input$doCompare, {
  req(dass_res$results)
  req(input$perfRefRes)
  req(input$perfPredCol)
  switch(
    input$compareType,
    Hazard = binaryCompare(),
    Potency = catCompare()
  )
})


blue <- rgb(red = 0, green = 90, blue = 181, maxColorValue = 255)
red <- rgb(red = 220, green = 50, blue = 32, maxColorValue = 255)
gold <- rgb(red = 187, green = 142, blue = 81, maxColorValue = 255)

blue_alpha <- rgb(red = 0, green = 90, blue = 181, alpha = 150, maxColorValue = 255)
red_alpha <- rgb(red = 220, green = 50, blue = 32, alpha = 150, maxColorValue = 255)
gold_alpha <- rgb(red = 187, green = 142, blue = 81, alpha = 150, maxColorValue = 255)

violin_scale <- reactiveVal()
defineScale <- reactive({
  req(input$compareType)
  if (input$compareType == "Hazard") {
    violin_scale(
      list(
        scale_fill_manual(
          values = c(
            "NA" = "#808080",
            "True Positive" = gold_alpha,
            "True Negative" = gold_alpha,
            "False Positive" = blue_alpha,
            "False Negative" = red_alpha
          )
        ),
        scale_color_manual(
          values = c(
            "NA" = "#808080",
            "True Positive" = gold,
            "True Negative" = gold,
            "False Positive"  = blue,
            "False Negative" = red
          )
          )
        )
    )
  } else if (input$compareType == "Potency") {
    violin_scale(
      list(
        scale_fill_manual(
          values = c(
            "NA" = "#808080",
            `True 1A` = gold_alpha,
            `True 1B` = gold_alpha,
            `True NC` = gold_alpha,
            Overpredicted = blue_alpha,
            Underpredicted = red_alpha
          )
        ),
        scale_color_manual(
          values = c(
            "NA" = "#808080",
            `True 1A` = gold,
            `True 1B` = gold,
            `True NC` = gold,
            Overpredicted = blue,
            Underpredicted = red
          )
        )
      )
    )
    }
})

### Binary -----
binaryCompare <- reactive({
  refCols <- structure(input$perfRefRes, names = input$perfRefRes)
  predCols <- structure(input$perfPredCol, names = input$perfPredCol)

  # Set up data for analysis
  data_compare <- data.table(
    dt_analyze()[,.SD, .SDcols = refCols],
    dass_res$results[,.SD,.SDcols = predCols]
  )

  allComps <- lapply(refCols, function(refCol) {
    # Get selected columns
    ref_tmp <- data_compare[,.SD,.SDcols = c(refCol, predCols)]

    # Check for invalid values
    refWarn <- ref_tmp[
      !is.na(get(refCol)),
      any(!grepl_ci(concatOrString(c(call1_str, call0_str)), get(refCol)))]

    # Convert to 1/0
    ref_tmp[,(refCol) := fcase(
      grepl_ci(concatOrString(call1_str), get(refCol)), 1,
      grepl_ci(concatOrString(call0_str), get(refCol)), 0
    )]
    ref_tmp[,(refCol) := factor(get(refCol), levels = c("1", "0"))]
    
    # Count missing after 0/1
    ref_noMiss <- ref_tmp[,!is.na(get(refCol))]
    refError <- ref_tmp[ref_noMiss,.N < 5]
    
    if (refError) {
      out <- list(
        refError = refError
      )
    } else if (!refError) {
      # Convert prediction columns to 1/0/Inconclusive
      ref_tmp[,(predCols) := lapply(.SD, function(x) factor(x, levels = c("1", "0", "Inconclusive"))), .SDcols = input$perfPredCol]
      
      lapply(predCols, function(predCol) {
        pred_noMiss <- ref_tmp[,!is.na(get(predCol))]
        predError <- ref_tmp[ref_noMiss & pred_noMiss & (get(predCol) != "Inconclusive"), .N < 5]
        predOut <- list(
          refCol = refCol,
          predCol = predCol,
          id = gsub("\\s+|[.]+", "", tolower(paste(refCol, predCol, sep = "_"))),
          label = paste(refCol, predCol, sep = " vs. "),
          refWarn = refWarn,
          predError = predError)
        
        if (!predError) {
          perf <- compareBinary(
            pred = ref_tmp[[predCol]],
            ref = ref_tmp[[refCol]],
            predCol = predCol,
            refCol = refCol)
          predOut <- append(predOut, perf)
        }
        return(predOut)
      })
    }
  })

  updateSelectInput(session = session, inputId = "referencePerf_1", choices = refCols)
  updateSelectInput(session = session, inputId = "predictionPerf_1", choices = predCols)
  
  if (length(predCols) > 1 | length(refCols) > 1) {
    shinyjs::show(id = "perfTab_block2")

    updateSelectInput(session = session, inputId = "referencePerf_2", choices = refCols, selected = refCols[1])
    updateSelectInput(session = session, inputId = "predictionPerf_2", choices = predCols, selected = predCols[1])
    
    if (length(refCols) > 1) {
      updateSelectInput(session = session, inputId = "referencePerf_2", selected = refCols[2])
    } else if (length(predCols) > 1) {
      updateSelectInput(session = session, inputId = "predictionPerf_2", selected = predCols[2])
    }
  } else {
    shinyjs::hide(id = "perfTab_block2")
    updateSelectInput(session = session, inputId = "referencePerf_2", choices = NULL)
    updateSelectInput(session = session, inputId = "predictionPerf_2", choices = NULL)
  }
  
  for_dL <- lapply(allComps, function(refComps) {
    if (is.null(refComps[["refError"]])) {
      tmp <- lapply(refComps, function(comp) {
        if (!comp[["predError"]]) {
          comp[c("id", "label", "refCol", "predCol")]
        }
      })
    }
  })

  for_dL <- unlist(for_dL, recursive = F, use.names = F)
  names(for_dL) <- sapply(for_dL, function(x) x[["id"]])

  perfTabs(for_dL)
  updateCheckboxGroupInput(session = session, inputId = "tableChoices", 
                           choiceValues = unname(sapply(for_dL, function(x) x[["id"]])),
                           choiceNames = unname(sapply(for_dL, function(x) x[["label"]])))
  
  perfVals(allComps)

  updateSelectInput(inputId = "violinCompareSelect", choices = names(for_dL))
  updateSelectInput(inputId = "violinDensitySelect", choices = names(dt_analyze()))
  updateSelectInput(inputId = "violinIdentifiers", choices = names(dt_analyze()))
  
  shinyjs::show("compareTableBody")
  shinyjs::show("binaryDefs")
  shinyjs::hide("potencyDefs")
  
  defineScale()

})

### Categorical -----
catCompare <- reactive({
  refCols <- structure(input$perfRefRes, names = input$perfRefRes)
  predCols <- structure(input$perfPredCol, names = input$perfPredCol)
  
  # Set up data for analysis
  data_compare <- data.table(
    dt_analyze()[,.SD, .SDcols = refCols],
    dass_res$results[,.SD,.SDcols = predCols]
  )

  allComps <- lapply(refCols, function(refCol) {
    # Get selected columns
    ref_tmp <- data_compare[,.SD,.SDcols = c(refCol, predCols)]

    # Check for invalid values
    refWarn <- ref_tmp[
      !is.na(get(refCol)),
      any(!grepl_ci(concatOrString(c("1A", "1B", "NC")), get(refCol)))]

    # Convert to caps
    ref_tmp[,(refCol) := fcase(
      grepl_ci("^1A$", get(refCol)), "1A",
      grepl_ci("^1B$", get(refCol)), "1B",
      grepl_ci("^NC$", get(refCol)), "NC"
    )]

    # Count missing
    ref_noMiss <- ref_tmp[,!is.na(get(refCol))]
    refError <- ref_tmp[ref_noMiss,.N < 5]
    
    if (refError) {
      out <- list(
        refError = refError
      )
    } else if (!refError) {
      # Convert prediction columns to 1A/1B/NC/Inconclusive
      ref_tmp[,(predCols) := lapply(.SD, function(x) factor(x, levels = c("1A", "1B", "NC", "Inconclusive"))), .SDcols = predCols]
      
      lapply(predCols, function(predCol) {
        pred_noMiss <- ref_tmp[,!is.na(get(predCol))]
        predError <- ref_tmp[ref_noMiss & pred_noMiss & (get(predCol) != "Inconclusive"), .N < 5]
        
        predOut <- list(
          id = gsub("\\s+|[.]+", "", tolower(paste(refCol, predCol, sep = "_"))),
          label = paste(refCol, predCol, sep = " vs. "),
          refCol = refCol,
          predCol = predCol,
          refWarn = refWarn,
          predError = predError)
        
        if (!predError) {
          perf <- compareCat(
            pred = ref_tmp[[predCol]],
            ref = ref_tmp[[refCol]],
            predCol = predCol,
            refCol = refCol)
          predOut <- append(predOut, perf)
        }
        return(predOut)
      })
    }
  })

  updateSelectInput(session = session, inputId = "referencePerf_1", choices = refCols)
  updateSelectInput(session = session, inputId = "predictionPerf_1", choices = predCols)
  
  if (length(predCols) > 1 | length(refCols) > 1) {
    shinyjs::show(id = "perfTab_block2")
    
    updateSelectInput(session = session, inputId = "referencePerf_2", choices = refCols, selected = refCols[1])
    updateSelectInput(session = session, inputId = "predictionPerf_2", choices = predCols, selected = predCols[1])
    
    if (length(refCols) > 1) {
      updateSelectInput(session = session, inputId = "referencePerf_2", selected = refCols[2])
    } else if (length(predCols) > 1) {
      updateSelectInput(session = session, inputId = "predictionPerf_2", selected = predCols[2])
    }
  } else {
    shinyjs::hide(id = "perfTab_block2")
    updateSelectInput(session = session, inputId = "referencePerf_2", choices = NULL)
    updateSelectInput(session = session, inputId = "predictionPerf_2", choices = NULL)
  }
  
  for_dL <- lapply(allComps, function(refComps) {
    if (is.null(refComps[["refError"]])) {
      tmp <- lapply(refComps, function(comp) {
        if (!comp[["predError"]]) {
          comp[c("id", "label", "refCol", "predCol")]
        }
      })
    }
  })
  
  for_dL <- unlist(for_dL, recursive = F, use.names = F)
  names(for_dL) <- sapply(for_dL, function(x) x[["id"]])
  
  perfTabs(for_dL)
  updateCheckboxGroupInput(session = session, inputId = "tableChoices", 
                           choiceValues = unname(sapply(for_dL, function(x) x[["id"]])),
                           choiceNames = unname(sapply(for_dL, function(x) x[["label"]])))
  
  perfVals(allComps)

  updateSelectInput(inputId = "violinCompareSelect", choices = names(for_dL))
  updateSelectInput(inputId = "violinDensitySelect", choices = names(dt_analyze()))
  updateSelectInput(inputId = "violinIdentifiers", choices = names(dt_analyze()))
  
  shinyjs::show("compareTableBody")
  shinyjs::hide("binaryDefs")
  shinyjs::show("potencyDefs")
  
  defineScale()

})

refErrorGrob <- textGrob("Error: Fewer than 5 valid reference values.", gp = gpar(col = "#D55E00"))
predErrorGrob <- textGrob("Error: Fewer than 5 conclusive prediction values.", gp = gpar(col = "#D55E00"))

table_shown_1 <- reactiveVal()
table_shown_2 <- reactiveVal()

observe({
  req(input$referencePerf_1)
  ref <- isolate(perfVals()[[input$referencePerf_1]])
  
  refErr <- ref[["refError"]]
  if (!is.null(refErr)) {
    table_shown_1(refErrorGrob)
  } else {
    req(input$predictionPerf_1)
    res <- ref[[input$predictionPerf_1]]
    if (res$predError) {
      table_shown_1(predErrorGrob)
    } else {
      table_shown_1(res$fig)
    }
  }
})

output$perfFigure_1 <- renderPlot(grid.draw(table_shown_1()), width = "auto")

observe({
  req(input$referencePerf_2)
  ref <- isolate(perfVals()[[input$referencePerf_2]])
  
  refErr <- ref[["refError"]]
  if (!is.null(refErr)) {
    table_shown_2(refErrorGrob)
  } else {
    req(input$predictionPerf_2)
    res <- ref[[input$predictionPerf_2]]
    if (res$predError) {
      table_shown_2(predErrorGrob)
    } else {
      table_shown_2(res$fig)
    }
  }
})

output$perfFigure_2 <- renderPlot(grid.draw(table_shown_2()))

## Download -----
### Figures -----
observeEvent(input$perfAll, {
  updateCheckboxGroupInput(inputId = "tableChoices", selected = names(perfTabs()))
})

observeEvent(input$perfNone, {
  updateCheckboxGroupInput(inputId = "tableChoices", selected = NA)
})

observe({
  if (is.null(input$tableChoices)) {
    shinyjs::disable("dlPerf")
  } else {
    shinyjs::enable("dlPerf")
  }
})

output$dlPerf <- downloadHandler(
  filename = function() {
    if (input$useDemoData) {
      fname <- "DemoData"
    } else {
      fname <- unlist(strsplit(input$fpath$name, "[.]"))
      fname <- paste(fname[-length(fname)], collapse = ".")
    }
    paste0(fname, "_DASSResults_Performance_", format.Date(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf")
  },
  content = function(con) {
    tabChoice <- input$tableChoices
    pdf(con)
    for (i in tabChoice) {
      refCol <- perfTabs()[[i]][["refCol"]]
      predCol <- perfTabs()[[i]][["predCol"]]
      
      grid.draw(perfVals()[[refCol]][[predCol]][["fig"]])
      if (i != tabChoice[length(tabChoice)]) grid.newpage()
      }
    dev.off()
  }
)

outputOptions(output, "dlPerf", suspendWhenHidden = FALSE)

### Tables -----
output$perfFlat_xl <- downloadHandler(
  filename = function() {
    if (input$useDemoData) {
      fname <- "DemoData"
    } else {
      fname <- unlist(strsplit(input$fpath$name, "[.]"))
      fname <- paste(fname[-length(fname)], collapse = ".")
    }
    paste0(fname, "_DASSResults_Performance_", format.Date(Sys.time(), "%Y%m%d-%H%M%S"), ".xlsx")
  },
  content = function(con) {
    wsData <- lapply(perfVals(), function(refList) {
      out <- lapply(refList, function(predList) {
        data.frame(
          Reference = predList$refCol,
          DA = predList$predCol,
          predList$perf_list, 
          check.names = F)
      })
      do.call("rbind.data.frame", out)
    })
    
    wsData <- do.call("rbind.data.frame", wsData)
    
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = "DASSPerformance")
    writeData(wb, sheet = "DASSPerformance", wsData)
    saveWorkbook(wb = wb, file = con)
  }
)

outputOptions(output, "perfFlat_xl", suspendWhenHidden = FALSE)
# 
output$perfFlat_txt <- downloadHandler(
  filename = function() {
    if (input$useDemoData) {
      fname <- "DemoData"
    } else {
      fname <- unlist(strsplit(input$fpath$name, "[.]"))
      fname <- paste(fname[-length(fname)], collapse = ".")
    }
    paste0(fname, "_DASSResults_Performance_", format.Date(Sys.time(), "%Y%m%d-%H%M%S"), ".txt")
  },
  content = function(con) {
    wsData <- lapply(perfVals(), function(refList) {
      out <- lapply(refList, function(predList) {
        data.frame(
          Reference = predList$refCol,
          DA = predList$predCol,
          predList$perf_list, 
          check.names = F)
      })
      do.call("rbind.data.frame", out)
    })
    
    wsData <- do.call("rbind.data.frame", wsData)
    
    write.table(x =  wsData, file = con, quote = F, row.names = F, sep = "\t")
  }
)

outputOptions(output, "perfFlat_txt", suspendWhenHidden = FALSE)

# Violins -----
# violinPlot <- reactiveVal()
# violinData <- reactiveVal()
# violin_yMax <- reactiveVal()
# violin_density_tt <- reactiveVal()
# resType <- reactiveVal()
# violin_point_tt <- reactiveVal()
# 
# # Pull numeric data
# observe({
#   violinData(numeric_data()[[input$violinDensitySelect]])
# })
# 
# # Update result info
# observe({
#   req(violinData())
#   refCol <- perfTabs()[[input$violinCompareSelect]][["refCol"]]
#   predCol <- perfTabs()[[input$violinCompareSelect]][["predCol"]]
#   
#   resType <- perfVals()[[refCol]][[predCol]][["indiv"]]
#   resType(resType)
#   violin_yMax(max(by(violinData(), resType, function(x) max(density(na.omit(x))$y))))
# 
#   min_vals <- by(violinData(), resType, min, na.rm = T)
#   max_vals <- by(violinData(), resType, max, na.rm = T)
#   med_vals <- by(violinData(), resType, median, na.rm = T)
#   
#   violin_density_tt <- sprintf("Reference = %s<br>Prediction = %s<br>Type = %s<br>%s Range = [%.2f, %.2f]<br>%s Median = %.2f", 
#                       refCol, predCol, 
#                       resType,
#                       input$violinDensitySelect,
#                       min_vals[resType],
#                       max_vals[resType],
#                       input$violinDensitySelect,
#                       med_vals[resType])
#   violin_density_tt(violin_density_tt)
#   
#   
#   violin_point_tt <- sprintf("Reference = %s<br>Prediction = %s<br>Type = %s<br>%s = %.2f<br>", 
#                      refCol, predCol, 
#                      resType,
#                      input$violinDensitySelect,
#                      violinData())
#   violin_point_tt(violin_point_tt)
# })
# 
# observe({
#   req(violinData())
# 
#   myPlot <- ggplot(mapping = aes(x = violinData(), color = resType(), fill = resType())) + 
#     geom_density(mapping = aes(text = violin_density_tt()), size = 1) + 
#     geom_point(mapping = aes(
#       y = violin_yMax() + violin_yMax()/4,
#       x = violinData(),
#       text = violin_point_tt()),
#       position = position_jitter(width = 0, height = violin_yMax()/8)
#     ) + 
#     scale_y_continuous(name = "Density", expand = c(0,0,0, violin_yMax()/8)) + 
#     violin_scale() +
#     theme_classic() +
#     theme(
#       axis.line = element_line(size = 4),
#       axis.title = element_text(size = 16),
#       axis.text = element_text(size = 12)) + 
#     labs(x = input$violinDensitySelect)
#   
#   violinPlot(myPlot)
# })
# 
# output$violin <- renderPlotly({
#   req(violinPlot())
#   ggplotly(violinPlot(), tooltip = "text")})
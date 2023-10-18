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
## Reactives -----
perfTabs <- reactiveVal()
perfVals <- reactiveVal()
plotShown <- reactiveVal()

## Compare -----
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
})

observeEvent(input$compareToRef, {
  req(dass_res$results)
  switch(
    input$compareType,
    Hazard = binaryCompare(),
    Potency = catCompare()
  )
  shinyjs::show("perfBlock")
})

### Binary -----
binaryCompare <- reactive({
  # Set up data for analysis
  data_compare <- data.table(
    dt_analyze()[,.SD, .SDcols = input$perfRefRes],
    dass_res$results[,.SD,.SDcols = input$perfPredCol]
  )
  
  allComps <- lapply(input$perfRefRes, function(refCol) {
    # Get selected columns
    ref_tmp <- data_compare[,.SD,.SDcols = c(refCol, input$perfPredCol)]
    
    # Check for invalid values
    refWarn <- ref_tmp[
      !is.na(get(refCol)),
      any(!grepl_ci(concatOrString(c(call1_str, call0_str)), get(refCol)))]
    
    # Convert to 1/0
    ref_tmp[,(refCol) := fcase(
      grepl_ci(concatOrString(call1_str), get(refCol)), 1,
      grepl_ci(concatOrString(call0_str), get(refCol)), 0
    )]
    
    # Count missing
    ref_noMiss <- ref_tmp[,!is.na(get(refCol))]
    refError <- ref_tmp[ref_noMiss,.N < 5]

    if (refError) {
      id <- sprintf("%s_error", refCol)
      perf <- data.frame(id = id, label = id, refCol = refCol, refWarn = refWarn, refError = refError)
      perfFig <- arrangeGrob(
        grobs = list(
          textGrob(label = "Confusion Matrix and Performance Metrics", gp = gpar(font = 2, cex = 1.25)),
          textGrob(label = paste("Reference Column: ", refCol)),
          textGrob(label = "Error: Fewer than 5 valid reference values provided.", gp = gpar(col = "#D55E00"))
        ))
      perfFig$id <- id
      
      out <- list(
        vals = perf,
        figs = perfFig
      )
      return(list(out))
    } else {
      # ref_tmp <- ref_tmp[,lapply(.SD, function(x) factor(x, levels = c("1", "0")))]
      ref_tmp[,(input$perfPredCol) := lapply(.SD, function(x) factor(x, levels = c("1", "0", "Inconclusive"))), .SDcols = input$perfPredCol]
      
      oneOut <- lapply(input$perfPredCol, function(predCol) {
        pred_noMiss <- ref_tmp[,!is.na(get(predCol))]
        predError <- ref_tmp[ref_noMiss & pred_noMiss & (get(predCol) != "Inconclusive"), .N < 5]
        predOut <- list(
          id = gsub("\\s+|[.]+", "", tolower(paste(refCol, predCol, sep = "_"))),
          label = paste(refCol, predCol, sep = " vs. "),
          refCol = refCol, 
                   predCol = predCol,
                   refWarn = refWarn,
                   refError = refError,
                   predError = predError)
        
        if (predError) {
          return(predOut)
        } else {
          perf <- compareBinary(
            pred = ref_tmp[ref_noMiss & pred_noMiss,get(predCol)],
            ref = ref_tmp[ref_noMiss & pred_noMiss,get(refCol)])
          
          # nGrob <- 5 + refWarn
          
          perfFig <- arrangeGrob(
            grobs = list(
              textGrob(label = "Confusion Matrix and Performance Metrics", gp = gpar(font = 2, cex = 1.25)),
              textGrob(label = paste("Reference Column: ", predOut$refCol)),
              textGrob(label = paste("Prediction Column:", predOut$predCol)),
              perf$figs[[1]],
              perf$figs[[2]]),  layout_matrix  = matrix(c(1,2,3,rep(4, 6), rep(5, 10)))
          )
          perfFig$id <- predOut$id
          
          out <- list(
            vals = as.data.frame(list(predOut, perf$vals)),
            figs = perfFig)
          
          return(out)
        }
      })
      return(oneOut)
    }
  })
  
  allCompVals <- lapply(allComps, function(oneOut) {
    lapply(oneOut, function(x) {
      x$vals
    })
  })
  
  allCompVals <- do.call("c", allCompVals) |> rbindlist(fill = T)
  perfVals(allCompVals)
  
  allCompTabs <- lapply(allComps, function(oneOut) {
    lapply(oneOut, function(x) {
      x$figs
    })
  })
  allCompTabs <- do.call("c", allCompTabs)
  names(allCompTabs) <- sapply(allCompTabs, function(x) x$id)

  perfTabs(allCompTabs)
  updateSelectInput(inputId = "perfList", choices = names(allCompTabs))
  updateCheckboxGroupInput(inputId = "tableChoices", choices = names(allCompTabs))
})

### Categorical -----
catCompare <- reactive({
  # Set up data for analysis
  data_compare <- data.table(
    dt_analyze()[,.SD, .SDcols = input$perfRefRes],
    dass_res$results[,.SD,.SDcols = input$perfPredCol]
  )
  
  allComps <- lapply(input$perfRefRes, function(refCol) {
    # Get selected columns
    ref_tmp <- data_compare[,.SD,.SDcols = c(refCol, input$perfPredCol)]
    
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
      id <- sprintf("%s_error", refCol)
      perf <- data.frame(id = id, label = id, refCol = refCol, refWarn = refWarn, refError = refError)
      perfFig <- arrangeGrob(
        grobs = list(
          textGrob(label = "Confusion Matrix and Performance Metrics", gp = gpar(font = 2, cex = 1.25)),
          textGrob(label = paste("Reference Column: ", refCol)),
          textGrob(label = "Error: Fewer than 5 valid reference values provided.", gp = gpar(col = "#D55E00"))
        ))
      perfFig$id <- id
      
      out <- list(
        vals = perf,
        figs = perfFig
      )
      return(list(out))
    } else {
      ref_tmp[,(input$perfPredCol) := lapply(.SD, function(x) factor(x, levels = c("1A", "1B", "NC", "Inconclusive"))), .SDcols = input$perfPredCol]
      oneOut <- lapply(input$perfPredCol, function(predCol) {
        pred_noMiss <- ref_tmp[,!is.na(get(predCol))]
        predError <- ref_tmp[ref_noMiss & pred_noMiss & (get(predCol) != "Inconclusive"), .N < 5]
        predOut <- list(
          id = gsub("\\s+|[.]+", "", tolower(paste(refCol, predCol, sep = "_"))),
          label = paste(refCol, predCol, sep = " vs. "),
          refCol = refCol, 
          predCol = predCol,
          refWarn = refWarn,
          refError = refError,
          predError = predError)
        
        if (predError) {
          return(predOut)
        } else {
          perf <- compareCat(
            pred = ref_tmp[ref_noMiss & pred_noMiss,get(predCol)],
            ref = ref_tmp[ref_noMiss & pred_noMiss,get(refCol)])
          
          perfFig <- arrangeGrob(
            grobs = list(
              textGrob(label = "Confusion Matrix and Performance Metrics", gp = gpar(font = 2, cex = 1.25)),
              textGrob(label = paste("Reference Column: ", predOut$refCol)),
              textGrob(label = paste("Prediction Column:", predOut$predCol)),
              perf$figs[[1]],
              perf$figs[[2]]),  layout_matrix  = matrix(c(1,2,3,rep(4, 5), rep(5, 6)))
          )
          perfFig$id <- predOut$id
          
          out <- list(
            vals = as.data.frame(list(predOut, perf$vals[-1])),
            figs = perfFig)
          
          return(out)
        }
      })
    }
  })
  
  allCompVals <- lapply(allComps, function(oneOut) {
    lapply(oneOut, function(x) {
      x$vals
    })
  })

  allCompVals <- do.call("c", allCompVals) |> rbindlist(fill = T)
  perfVals(allCompVals)
  
  allCompTabs <- lapply(allComps, function(oneOut) {
    lapply(oneOut, function(x) {
      x$figs
    })
  })
  
  allCompTabs <- do.call("c", allCompTabs)
  names(allCompTabs) <- sapply(allCompTabs, function(x) x$id)
  
  perfTabs(allCompTabs)
  updateSelectInput(inputId = "perfList", choices = names(allCompTabs))
  updateCheckboxGroupInput(inputId = "tableChoices", choices = names(allCompTabs))
})

observeEvent(input$perfList, {
  plotShown(perfTabs()[[input$perfList]])
})

output$perfFigure <- renderPlot({grid.draw(plotShown())})

## Download -----

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
      grid.draw(perfTabs()[[i]])
      if (i != tabChoice[length(tabChoice)]) grid.newpage()
      }
    dev.off()
  }
)

outputOptions(output, "dlPerf", suspendWhenHidden = FALSE)

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
    outCols <- setdiff(names(perfVals()), c("id", "label"))
    
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = "DASSPerformance")
    writeData(wb, sheet = "DASSPerformance", perfVals()[,.SD,.SDcols = outCols])
    saveWorkbook(wb = wb, file = con)
  }
)

outputOptions(output, "perfFlat_xl", suspendWhenHidden = FALSE)

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
    outCols <- setdiff(names(perfVals()), c("id", "label"))
    write.table(x =  perfVals()[,.SD,.SDcols = outCols], file = con, quote = F, row.names = F, sep = "\t")
  }
)

outputOptions(output, "perfFlat_txt", suspendWhenHidden = FALSE)
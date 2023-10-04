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
do_binary <- reactiveVal(F)
can_compare <- reactiveVal(F)
comparisonUI <- reactiveVal()
indivCompareTable <- reactiveVal()
showUser <- reactiveVal(F)
test <- reactiveVal(0)
perfFigs <- reactiveValues(
  binaryTableSingle = NULL
)

# Update prediction column menu
observe({
  req(dass_res$results)
  switch(input$compareType,
         Hazard = {
           daBins = grep("^DA ITS Call$|^DA 2o3 Call$|^DA KE 3/1 STS Call$", names(dass_res$results), value = T)
           updateSelectInput(inputId = "perfPredCol", choices = daBins)
           updateSelectInput(inputId = "perfRefRes", choices = c(names(usr_dt()), daBins), selected = "")
         },
         Potency = {
           daPots = grep("^DA ITS Potency$|^DA KE 3/1 STS Potency$", names(dass_res$results), value = T)
           updateSelectInput(inputId = "perfPredCol", choices = daPots)
           updateSelectInput(inputId = "perfRefRes", choices = c(names(usr_dt()), daPots), selected = "")
         })
})

observeEvent(input$compareToRef, {
  req(dass_res$results)
  switch(
    input$compareType,
    Hazard = binaryCompare(),
    Potency = catCompare()
  )
})

binaryCompare <- reactive({
  refOk <- F
  
  # Set up suppCompUI
  predCol <- input$perfPredCol
  comp_ui <- vector(mode = "list", length = length(predCol) + 2)
  names(comp_ui) <- c("refWarn", "refErr", predCol)
  
  # Set up reference data
  # If reference is from user data: 
  if (input$perfRefRes %in% names(usr_dt())) {
    data_compare <- data.table(
      # usr_dt()[,input$idColumnsRes, with = F],
      usr_dt()[,.SD, .SDcols = input$perfRefRes],
      # pred = dass_res$results[,get(input$perfPredCol)]
      dass_res$results[,.SD,.SDcols = input$perfPredCol]
    )
    
        refFormat <- data_compare[
    !is.na(get(input$perfRefRes)),
    any(!grepl_ci(concatOrString(c(call1_str, call0_str)), get(input$perfRefRes)))]
    if (refFormat) {
      comp_ui$refWarn <- p(
        class = "warningText",
        "Warning: Invalid binary values in reference."
      )
    }
        
    data_compare[,(input$perfRefRes) := fcase(
      grepl_ci(concatOrString(call1_str), get(input$perfRefRes)), 1,
      grepl_ci(concatOrString(call0_str), get(input$perfRefRes)), 0
    )]
    

    # If reference is another DA output
  } else if (input$perfRefRes %in% names(dass_res$results)) {
    data_compare <- dass_res$results[,.SD,.SDcols = c(input$perfRefRes, input$perfPredCol)]

  }

  nRef <- data_compare[!is.na(get(input$perfRefRes)), .N]
  if (nRef < 5) {
    comp_ui$refErr <- p(
      class = "warningText", 
      "Error: Fewer than 5 valid reference values supplied."
    )
    comparisonUI(comp_ui)
  } else {
    refOk <- T
    binCols <- c(input$perfRefRes, input$perfPredCol)
    data_compare[,(binCols) := lapply(.SD, function(x) factor(x, levels = c("1", "0"))), .SDcols = binCols]
  }
  
  req(refOk)
  # Add context to tables (columns being compared)
  comp_ui[predCol] <- lapply(predCol, function(x) {
    data_tmp <- data_compare[!is.na(get(input$perfRefRes)) & !is.na(get(x)), .SD, .SDcols = c(input$perfRefRes, x)]
    nPred <- data_tmp[,.N]
    if (nPred < 5) {
      tmp <- p(
        class = "warningText",
        "Error: Fewer than 5 conclusive DA predictions."
      )
    } else {
      tmp <- vector(mode = "list", length = 3)
      names(tmp) <- c("compHeader", "cmTable", "perfTable")
      
      compRes <- compareBinary(pred = data_tmp[,get(x)],
                               ref = data_tmp[,get(input$perfRefRes)])
      
      names(compRes) <- gsub("([[:upper:]])", "_\\1", names(compRes)) |>
        strsplit(split = "_") |>
        sapply(X = _, paste, collapse = " ") |>
        gsub("^(.){1}", "\\U\\1", x = _, perl = T)
      
      tmp$compHeader <- tags$h2(style = "text-align: center;", "Confusion Matrix and Performance Metrics")
      tmp$cmTable <- tags$div(tags$table(
        class = "compTable",
        tags$thead(
          tags$tr(
            tags$th(
              class = "cmBlank", colspan = "2", rowspan = "2"
            ),
            tags$th(
              class = "cmHead", colspan = "2", "Reference"
            )
          ),
          tags$tr(
            tags$th(class = "cmHead", "Positive"),
            tags$th(class = "cmHead", "Negative")
          )
        ),
        tags$body(
          tags$tr(
            tags$td(
              class = "cmHead", rowspan = "2",
              tags$span(
                class = "cmVert",
                "Predicted"
              )
            ),
            tags$td(class = "cmHead", "Positive"),
            tags$td(class = "cmCell", compRes$`True Positive`),
            tags$td(class = "cmCell", compRes$`False Positive`)
          ),
          tags$tr(
            tags$td(class = "cmHead", "Negative"),
            tags$td(class = "cmCell", compRes$`False Negative`),
            tags$td(class = "cmCell", compRes$`True Negative`)
          )
        )
      ))
      
      tmp$perfTable <- tags$table(
        class = "compTable",
        tags$thead(
          tags$tr(
            tags$th(class = "cmHead", "Metric"),
            tags$th(class = "cmHead", "Value")
          )
        ),
        tags$tbody(
          lapply(names(compRes)[-1], function(x) {
            tags$tr(
              tags$td(class = "cmHead", x),
              tags$td(class = "cmCell", compRes[[x]])
            )
          })
        )
      )
    }
    return(tmp)
  })
  comparisonUI(comp_ui)
})

output$binaryTableFigure <- renderPlot({
  perfFigs$binaryTableSingle
})

catCompare <- reactive({
  refOk <- F
  
  # Set up suppCompUI
  predCol <- input$perfPredCol
  comp_ui <- vector(mode = "list", length = length(predCol) + 2)
  names(comp_ui) <- c("refWarn", "refErr", predCol)
  
  # Set up reference data
  # If reference is from user data: 
  if (input$perfRefRes %in% names(usr_dt())) {
    data_compare <- data.table(
      usr_dt()[,.SD, .SDcols = input$perfRefRes],
      dass_res$results[,.SD,.SDcols = input$perfPredCol]
    )
    
    refFormat <- data_compare[
      !is.na(get(input$perfRefRes)),
      any(!grepl_ci(concatOrString(c("1A", "1B", "NC")), get(input$perfRefRes)))]
    if (refFormat) {
      comp_ui$refWarn <- p(
        class = "warningText",
        "Warning: Invalid potency values in reference."
      )
    }
    
    data_compare[,(input$perfRefRes) := fcase(
      grepl_ci("1A", get(input$perfRefRes)), "1A",
      grepl_ci("1B", get(input$perfRefRes)), "1B",
      grepl_ci("NC", get(input$perfRefRes)), "NC"
    )]

    # If reference is another DA output
  } else if (input$perfRefRes %in% names(dass_res$results)) {
    data_compare <- dass_res$results[,.SD,.SDcols = c(input$perfRefRes, input$perfPredCol)]
  }
  
  nRef <- data_compare[!is.na(get(input$perfRefRes)), .N]
  if (nRef < 5) {
    comp_ui$refErr <- p(
      class = "warningText", 
      "Error: Fewer than 5 valid reference values supplied."
    )
    comparisonUI(comp_ui)
  } else {
    refOk <- T
    potCols <- c(input$perfRefRes, input$perfPredCol)
    data_compare[,(potCols) := lapply(.SD, function(x) factor(x, levels = c("1A", "1B", "NC"))), .SDcols = potCols]
  }
  
  req(refOk)
  # Add context to tables (columns being compared)
  comp_ui[predCol] <- lapply(predCol, function(x) {
    data_tmp <- data_compare[!is.na(get(input$perfRefRes)) & !is.na(get(x)), .SD, .SDcols = c(input$perfRefRes, x)]
    nPred <- data_tmp[,.N]
    if (nPred < 5) {
      tmp <- p(
        class = "warningText",
        "Error: Fewer than 5 conclusive DA predictions."
      )
    } else {
      tmp <- vector(mode = "list", length = 3)
      names(tmp) <- c("compHeader", "cmTable", "perfTable")

      compRes <- compareCat(pred = data_tmp[,get(x)],
                               ref = data_tmp[,get(input$perfRefRes)])

      tmp$compHeader <- tags$h2(style = "text-align: center;", "Confusion Matrix and Performance Metrics")
      tmp$cmTable <- tags$div(tags$table(
        class = "compTable",
        tags$thead(
          tags$tr(
            tags$th(
              class = "cmBlank", colspan = "2", rowspan = "2"
            ),
            tags$th(
              class = "cmHead", colspan = "3", "Reference"
            )
          ),
          tags$tr(
            tags$th(class = "cmHead", "1A"),
            tags$th(class = "cmHead", "1B"),
            tags$th(class = "cmHead", "NC")
          )
        ),
        tags$body(
          tags$tr(
            tags$td(
              class = "cmHead", rowspan = "3",
              tags$span(
                class = "cmVert",
                "Predicted"
              )
            ),
            tags$td(class = "cmHead", "1A"),
            lapply(compRes$confusionMatrix["1A",], function(x) tags$td(class = "cmCell", x))
          ),
          tags$tr(
            tags$td(class = "cmHead", "1B"),
            lapply(compRes$confusionMatrix["1B",], function(x) tags$td(class = "cmCell", x))
          ),
          tags$tr(
            tags$td(class = "cmHead", "NC"),
            lapply(compRes$confusionMatrix["NC",], function(x) tags$td(class = "cmCell", x))
          )
        )
      ))

      names(compRes) <- gsub("([[:upper:]])", "_\\1", names(compRes)) |>
        strsplit(split = "_") |>
        sapply(X = _, paste, collapse = " ") |>
        gsub("^(.){1}", "\\U\\1", x = _, perl = T)
      
      tmp$perfTable <- tags$table(
        class = "compTable",
        tags$thead(
          tags$tr(
            tags$th(class = "cmHead", "Metric"),
            tags$th(class = "cmHead", "Value")
          )
        ),
        tags$tbody(
          lapply(names(compRes)[-1], function(x) {
            tags$tr(
              tags$td(class = "cmHead", x),
              tags$td(class = "cmCell", compRes[[x]])
            )
          })
        )
      )
    }
    return(tmp)
  })
  comparisonUI(comp_ui)
})









observeEvent(input$downloadSupp, {
  fname <- unlist(strsplit(input$fpath$name, "[.]"))
  fname <- paste(fname[-length(fname)], collapse = ".")
  fname <- paste0(fname, "_DASSAppCompare_", format.Date(Sys.time(), "%Y%m%d-%H%M%S"))
  screenshot(selector = "#suppCompare_ui", 
             filename =  fname,
             scale = 2)
})

output$indivCompareTable <- renderDataTable({
  datatable(indivCompareTable())
})
output$suppCompare_ui <- renderUI({comparisonUI()})
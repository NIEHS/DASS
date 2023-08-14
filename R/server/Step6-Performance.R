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
can_compare <- reactiveVal(F)
comparisonUI <- reactiveVal()
indivCompareTable <- reactiveVal()
showUser <- reactiveVal(F)
test <- reactiveVal(0)

compare_call <- reactive({
  comp_ui <- list()

  # Perform checks
  # If select column from user data
  if (input$perfRefRes %in% names(usr_dt())) {
    data_compare <- data.table(
      usr_dt()[,.SD,.SDcols = input$idColumnsRes],
      ref = usr_dt()[,get(input$perfRefRes)],
      pred = dass_res$results[,get(input$perfPredCol)]
    )

    checkRef <- data_compare[
      !is.na(ref),
      any(!grepl_ci(concatOrString(c(call1_str, call0_str)), ref))]

    if (checkRef) {
      comp_ui$binWarnText <- p(
        class = "warningText",
        "Warning: Invalid binary values in reference."
      )
    }

    data_compare[,ref := fcase(
      grepl_ci(concatOrString(call1_str), ref), 1,
      grepl_ci(concatOrString(call0_str), ref), 0
    )]
  } else if (input$perfRefRes %in% names(dass_res$results)) {
    data_compare <- dass_res$results[,.(ref = get(input$perfRefRes),
                                  pred = get(input$perfPredCol))]
    data_compare <- data.table(
      usr_dt()[,.SD,.SDcols = input$idColumnsRes],
      data_compare
    )
  }

  cols <- c("ref", "pred")
  data_compare[,(cols) := lapply(.SD, factor, levels = c("1", "0")), .SDcols = cols]

  data_compare_noNA <- data_compare[!is.na(pred) & !is.na(ref)]

  if (nrow(data_compare_noNA) < 5) {
    comp_ui$binErrText <- p(
      class = "warningText",
      "Error: Fewer than 5 available values for comparison."
    )
    indivCompareTable(data_compare)
    comp_ui$indivDataTable <- dataTableOutput("indivCompareTable")
    comparisonUI(comp_ui)
  } else {
    can_compare(T)
  }

  req(can_compare())
  compRes <- compareBinary(pred = data_compare[,pred],
                           ref = data_compare[,ref])

  names(compRes) <- gsub("([[:upper:]])", "_\\1", names(compRes)) |>
    strsplit(split = "_") |>
    sapply(X = _, paste, collapse = " ") |>
    gsub("^(.){1}", "\\U\\1", x = _, perl = T)

  comp_ui$compHead <- tags$h2("Confusion Matrix and Performance Metrics")
  comp_ui$cmTable <- tags$div(tags$table(
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



  comp_ui$perfTable <- tags$table(
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
          tags$td(class = "cmCell", round(compRes[[x]], digits = 4))
        )
      })
    )
  )

  comparisonUI(comp_ui)
  # test(compRes)
  # shinyjs::runjs("$('#browser').click()")
})

output$indivCompareTable <- renderDataTable({
  datatable(indivCompareTable())
})
output$suppCompare_ui <- renderUI({comparisonUI()})

observeEvent(input$compareToTable, {
  compare_call()
})
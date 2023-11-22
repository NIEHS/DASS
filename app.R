#=============================================================================#
# File Name: app.R
# Original Creator: Kim To
# Contact Information: ICE-support@niehs.nih.gov
# Date Created: 2021-12-03
# License: MIT
# Version: 1.0
# Description: Loads required packages. Reads in functions. Loads UI and
# server files to build app.
# Required Packages:
# - data.table, DT
# - ggplot2, grid, gridExtra
# - openxlsx
# - readxl
# - shiny shinyBS shinyjqui shinyjs
#=============================================================================#

# Load packages
require(data.table)
require(DT)
require(ggplot2)
require(grid)
require(gridExtra)
require(openxlsx)
require(readxl)
require(shiny)
require(shinyBS)
require(shinyjqui)
require(shinyjs)

# Load functions
source("R/dass_predict.R")

# Load ui from file
source("R/ui_obj.R")
ui <- fluidPage(
  useShinyjs(),
  # Set CSS styles
  tags$head(
    HTML("<title>NICEATM DASS App</title>"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),

  htmltools::findDependencies(selectizeInput("foo", "bar", choices = "a")),
  htmltools::findDependencies(icon("box", lib = "font-awesome")),
  ui_dass,
  # for debugging
  # div(style = "position: fixed; top: 0;", actionButton("browser", "browser")),
  tags$footer(
    tags$script(src = "js.js")
  )
)

attr(ui, "lang") <- "en"

# Read in server files
server <- function(input, output, session) {
  source("R/server/Step1-Select.R", local = TRUE)
  source("R/server/Step2-UploadData.R", local = TRUE)
  source("R/server/Step3-SelectColumns.R", local = TRUE)
  source("R/server/Step4-ReviewColumns.R", local = TRUE)
  source("R/server/Step5-Results.R", local = TRUE)
  source("R/server/Step6-Performance.R", local = TRUE)
  # for debugging
  # observeEvent(input$browser,{
  #   browser()
  # })
}

# Create App -----
shinyApp(ui = ui, server = server)
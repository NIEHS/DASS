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
# - htmltools
# - openxlsx
# - readxl
# - shiny shinyBS shinyjs
#=============================================================================#

# Load packages
require(data.table)
require(DT)
require(openxlsx)
require(readxl)
require(shiny)
require(shinyBS)
require(shinyjqui)
require(shinyjs)

# Load functions
source("dass_predict.R")

# Load ui from file
source("ui_obj.R")
ui <- fluidPage(
  useShinyjs(),
  # Set CSS styles
  tags$head(
    HTML("<title>NICEATM DASS App</title>"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    # tags$script(src = "sendHeight.js")
  ),
  ## for debugging
  # actionButton("browser", "browser"),
  ##
  
  ui_dass
)

attr(ui, "lang") <- "en"

# Read in server files
server <- function(input, output, session) {
  source("server/Step1-Select.R", local = TRUE)
  source("server/Step2-UploadData.R", local = TRUE)
  source("server/Step3-SelectColumns.R", local = TRUE)
  source("server/Step4-ReviewColumns.R", local = TRUE)
  source("server/Step5-Results.R", local = TRUE)

  # jqui_resizable(".modal-content")
  # jqui_draggable(".modal-content")
  # for debugging
  # observeEvent(input$browser,{
  #   browser()
  # })
  #

}

# Create App -----
shinyApp(ui = ui, server = server)
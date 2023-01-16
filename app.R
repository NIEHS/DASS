#=============================================================================#
# File Name: app.R                                                            #
# Original Creator: Kim To                                                    #
# Contact Information: comptox@ils-inc.com                                    #
# Date Created: 2021-12-03                                                    #s
# License: MIT                                                                #
# Version: 0.9.1                                                              #
# Description: Loads required packages. Reads in functions. Loads UI and      #
# server files to build app.                                                  #
# Required Packages:                                                          #
# - data.table, DT                                                            #
# - openxlsx                                                                  #
# - readxl                                                                    #
# - shiny shinyBS shinyjs                                                     #
# Note: The app was first created using ITSv2, which uses OECD QSAR TB.       #
# In silico results can come from either Derek Nexus or OECD QSAR TB,         #
# however variable names throughout the code for this app will be named after #
# OECD QSAR TB.                                                               #
#=============================================================================#

# Load packages
require(data.table)
require(DT)
require(shiny)
require(shinyBS)
require(shinyjs)
require(openxlsx)
require(shinyjqui)

# Load functions
source("dass_predict.R")

# Load ui from file
source("ui_obj.R")
ui <- fluidPage(
  useShinyjs(),
  # Set CSS styles
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  #titlePanel(""),
  ui_dass
)

# Read in server files
server <- function(input, output, session) {
  # source("server/Setup.R", local = TRUE)
  source("server/Step1-Select.R", local = TRUE)
  source("server/Step2-UploadData.R", local = TRUE)
  source("server/Step3-SelectColumns.R", local = TRUE)
  source("server/Step4-ReviewColumns.R", local = TRUE)
  source("server/Step4.R", local = TRUE)
}

# Create App -----
shinyApp(ui = ui, server = server)
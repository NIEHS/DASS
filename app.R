#=============================================================================#
# File Name: app.R                                                            #
# Original Creator: Kim To                                                    #
# Contact Information: comptox@ils-inc.com                                    #
# Date Created: 2021-12-03                                                    #
# License: MIT                                                                #
# Version: 0.9                                                                #
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

# Load functions
source("dass_predict.R")

# Load ui from file
source("ui_obj.R")
ui <- fluidPage(
  useShinyjs(),
  # Set CSS styles
  tags$head(
    tags$style(
      "
      .btn-qs {
      color:#0072B2;
      padding:0px;
      border-color:transparent;
      }
      .btn-qs:focus, .btn-qs:hover {
      color:#0C1669;
      background-color:transparent;
      border-color:transparent;
      }
      .control-label {
      white-space:nowrap;
      }
      .modal-dialog {
      position:relative; top:calc(20%); bottom:calc(20%);
      }
      #fpath_progress {
      visibility:hidden !important;
      }
      #shiny-notification-panel {
      top:0;
      }
      #usr_dt .dataTables_scrollBody,
      #dt_results .dataTables_scrollBody{
      height: 50vh;
      }
      "
    )
  ),
  #titlePanel(""),
  ui_dass
)

# Read in server files
server <- function(input, output, session) {
  source("server/Setup.R", local = TRUE)
  source("server/Step1.R", local = TRUE)
  source("server/Step2.R", local = TRUE)
  source("server/Step3.R", local = TRUE)
  source("server/Step4.R", local = TRUE)
}

# Create App -----
shinyApp(ui = ui, server = server)
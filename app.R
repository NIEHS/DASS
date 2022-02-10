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

# Load ui objects
source("ui_obj.R")
ui <- fluidPage(
  tags$head(
    tags$style(
      "
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
  titlePanel(""),
  ui_dass
)

# Read in server file
server <- function(input, output, session) {
  source("server_obj.R", local = TRUE)
}

# Create App -----
shinyApp(ui = ui, server = server)
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
# - shiny shinyBS shinyjs                                                     #
#=============================================================================#

# Load packages
require(data.table)
require(DT)
require(shiny)
require(shinyBS)
require(shinyjs)

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
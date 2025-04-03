#=============================================================================#
# File Name: app.R
# Original Creator: Kim To
# Contact Information: ICE-support@niehs.nih.gov
# Date Created: 2021-12-03
# Last Modified: 2025-03-27
# License: MIT
# Version: 2.0.2
# Description: Loads in files and builds DASS App
#=============================================================================#

# Load packages
require(openxlsx)
require(shiny)
require(shinyjs)
require(shinyWidgets)
require(bslib)
require(DT)
require(ggplot2)
require(grid)
require(gridExtra)
require(plotly)

options(DT.TOJSON_ARGS = list(na = "string")) # show NA and Inf in DT

# Load functions
source("R/dass_predict.R")
source("R/interpret_assay.R")
source("R/utils.R")

# Read in templates/dicts
select_show_hide <- readRDS("R/lists/select_show_hide.rds")
select_data_template <- readRDS("R/lists/select_data_template.rds")
source("R/lists/dictionaries.R")

# Load UI elements
source("R/ui/tabs.R")
source("R/ui/modals.R")

ui <- page_fluid(
  title = "NICEATM DASS App",
  lang = "en",
  theme = bs_theme(
    "body-color" = "black",
    "font-size-root" = "16px"
  ),
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  htmltools::findDependencies(icon("box", lib = "font-awesome")),
  ui_header,
  tabsetPanel(
    id = "tabs",
    tab_select_da,
    tab_upload_data,
    tab_select_columns,
    tab_review_selection,
    tab_results,
    tab_compare
  ),
  modal_list,
  div(style = "position: fixed; top: 0;", actionButton("browser", "browser")),
  tags$footer(
    tags$script(src = "js.js")
  )
)

server <- function(input, output, session) {
  source("R/server/00-uploadData.R", local = T)
  source("R/server/01-selectDataColumns.R", local = T)
  source("R/server/02-prepareSelection.R", local = T)
  source("R/server/03-analysis.R", local = T)
  source("R/server/04-compare.R", local = T)

  observeEvent(input$browser,{
    browser()
  })
}

# Create App -----
shinyApp(ui = ui, server = server)
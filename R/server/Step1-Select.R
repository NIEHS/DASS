# =============================================================================#
# File Name: Step1-Select.R
# Original Creator: ktto
# Contact Information: ICE-support@niehs.nih.gov
# Date Created: 2021-02-10
# License: MIT
# Description: server object for module with da selection
# Required Packages:
# - shiny
# =============================================================================#

# Step 1: Select Approaches -----
# link to select all strategies
observeEvent(input$dass_all, {
  updateCheckboxInput(
    inputId = "do_da_2o3",
    value = TRUE
  )
  updateCheckboxInput(
    inputId = "do_da_its",
    value = TRUE
  )
  updateCheckboxInput(
    inputId = "do_da_ke31",
    value = TRUE
  )
})

# link to select no strategies
observeEvent(input$dass_none, {
  updateCheckboxInput(
    inputId = "do_da_2o3",
    value = FALSE
  )
  updateCheckboxInput(
    inputId = "do_da_its",
    value = FALSE
  )
  updateCheckboxInput(
    inputId = "do_da_ke31",
    value = FALSE
  )
})
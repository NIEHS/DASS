# =============================================================================#
# File Name: Step3-SelectColumns.R
# Original Creator: ktto
# Contact Information: ICE-support@niehs.nih.gov
# Date Created: 2021-02-10
# License: MIT
# Description: server file for module with column selection
# Required Packages:
# - data.table, DT
# - shiny, shinyBS, shinyjs
# =============================================================================#

## Dictionaries -----
# HTML IDs to unhide based on DA selections
ke_block_id <- list(
  da_2o3 = c("ke1_select_ui", "ke2_select_ui", "ke3_select_ui", "ke1_call_select", "ke3_call_select"),
  da_its = c("ke1_select_ui", "ke3_select_ui", "insil_select_ui", "ke1_dep_select", "ke3_val_select"),
  da_ke31 = c("ke1_select_ui", "ke3_select_ui", "ke1_call_select", "ke3_val_select")
)

# List to store selection details
tmp_list <- function(x) list(display_name = x, col_name = NULL, values = NULL, converted_values = NULL, flagged = NULL)
data_select_template <- list(
  ke1_call_col         = tmp_list("KE1 Assay Call"),
  ke1_mean_c_l_dep_col = tmp_list("KE1 Mean Depletion (%)"),
  ke1_c_dep_col        = tmp_list("KE1 Cys/NAC Depletion (%)"),
  ke1_l_dep_col        = tmp_list("KE1 Lys/NAL Depletion (%)"),
  ke2_call_col         = tmp_list("KE2 Assay Call"),
  ke2_val_col          = tmp_list("KE2 Quantiative Endpoint"),
  ke3_call_col         = tmp_list("KE3 Assay Call"),
  ke3_val_col          = tmp_list("KE3 Quantiative Endpoint"),
  insil_call_col       = tmp_list("In Silico Call Prediction"),
  insil_ad_col         = tmp_list("In Silico Applicability Domain")
)

quant_data_key <- reactiveVal()

## Observers -----
observe({
  if (input$ke1_call_interpret) {
    shinyjs::disable("ke1_call_col")
    shinyjs::show("ke1_dep_select")
  } else if (!input$ke1_call_interpret) {
    shinyjs::enable("ke1_call_col")
    if (!(input$selected_da == "da_2o3" & input$do_da_2o3_bl)){
      shinyjs::hide("ke1_dep_select")
    }
  }
})

# Enable or disable dropdown menus for depletion values
observe({
  if (input$ke1_choose_dep) {
    shinyjs::disable("ke1_mean_c_l_dep_col")
    shinyjs::enable("ke1_c_dep_col")
    shinyjs::enable("ke1_l_dep_col")
  } else if (!input$ke1_choose_dep) {
    shinyjs::enable("ke1_mean_c_l_dep_col", )
    shinyjs::disable("ke1_c_dep_col")
    shinyjs::disable("ke1_l_dep_col")
  }
})

# Step 3: Select Columns -----
## Set up Panel 3 -----
observeEvent(input$confirm_data, {
  req(dt_analyze())
  shinyjs::runjs("resetHidden(false);")
  
  # Unhide Step 3 UI
  shinyjs::show("select_col_ui")
  shinyjs::enable("tab_select_columns")

  # Create vector of UI IDs to show based on selected DAs
  to_show <- ke_block_id[[input$selected_da]]
  
  for (i in names(data_select_template)) {
    selected <- gsub("_col", "", i)
    if (!selected %in% names(dt_analyze())) {
      selected <- ""
    }
    updateSelectInput(inputId = i, choices = c("", names(dt_analyze())), selected = selected)
  }
  
  if (input$selected_da == "da_2o3") {
    updateRadioButtons(
      inputId = "ke1_assay_name",
      choiceNames = c("ADRA", "DPRA"),
      choiceValues = c("adra", "dpra"),
      inline = T
    )
    updateRadioButtons(
      inputId = "ke3_assay_name",
      choiceNames = c("GARDskin", "h-CLAT", "IL-8 Luc", "U-SENS"),
      choiceValues = c("gardskin", "hclat", "il8luc", "usens"),
      inline = T
    )
    if (input$do_da_2o3_bl) {
      to_show <- c(to_show, "ke1_dep_select", "ke2_value_select", "ke3_value_select")
    }
  } else if (input$selected_da == "da_its") {
    updateRadioButtons(
      inputId = "ke1_assay_name",
      choiceNames = c("ADRA", "DPRA"),
      choiceValues = c("adra", "dpra"),
      inline = T
    )
    updateRadioButtons(
      inputId = "ke3_assay_name",
      choiceNames = c("GARDskin", "h-CLAT", "U-SENS"),
      choiceValues = c("gardskin", "hclat", "usens"),
      inline = T
    )
  } else if (input$selected_da == "da_ke31") {
    updateRadioButtons(
      inputId = "ke1_assay_name",
      choiceNames = c("DPRA"),
      choiceValues = c("dpra"),
      inline = T
    )
    updateRadioButtons(
      inputId = "ke3_assay_name",
      choiceNames = c("h-CLAT"),
      choiceValues = c("hclat"),
      inline = T
    )
  }

  for (i in to_show) {
    shinyjs::runjs(sprintf("$('#%s').show()", i))
  }
  
  updateTabsetPanel(inputId = "step_set", selected = "Select Data Columns")
  shinyjs::runjs("$('#step_set')[0].scrollIntoView();")
  
  # Label numeric data for performance section
  quant_data_key(suppressWarnings(apply(dt_analyze(), 2, function(x) {
    tmp <- as.numeric(x)
    length(na.omit(tmp)) >= 5
  })))
})



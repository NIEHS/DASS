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

# Step 3: Select Columns -----
## Observers -----
# Enable or disable KE1 call selection menu
ke1_dep_watch <- reactiveVal(F)
observe({
  if (input$ke1_call_interpret) {
    shinyjs::disable("ke1_call_col")
    shinyjs::show("ke1_dep_select")
  } else if (!input$ke1_call_interpret) {
    shinyjs::enable("ke1_call_col")
    if (!ke1_dep_watch()){
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

## Dictionaries -----
# HTML IDs to unhide based on DA selections
ke_block_id <- list(
  da_2o3 = c("ke1_select_ui", "ke2_select_ui", "ke3_select_ui", "ke1_call_select", "ke3_call_select"),
  da_its = c("ke1_select_ui", "ke3_select_ui", "insil_select_ui", "ke1_dep_select", "ke3_val_select"),
  da_ke31 = c("ke1_select_ui", "ke3_select_ui", "ke1_call_select", "ke3_val_select")
)

# List to store selection details
tmp_list <- function(x) list(display_name = x, da_2o3 = "", da_its = "", da_ke31 = "", col_name = NULL, values = NULL, converted_values = NULL, flagged = NULL)
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

## Set up Panel 3 -----
observeEvent(input$confirm_data, {
  req(dt_analyze())
  check_select <- c(da_2o3  = input$do_da_2o3,
                    da_its  = input$do_da_its,
                    da_ke31 = input$do_da_ke31)


  if (!any(check_select)) {
    showNotification(
      type     = "error",
      ui       = "No defined approaches selected.",
      duration = 10
    )
  }

  req(any(check_select))
  # Unhide Step 3 UI
  shinyjs::show("select_col_ui")
  shinyjs::enable("tab_select_columns")

  # Create vector of UI IDs to show based on selected DAs
  to_show <- unlist(ke_block_id[names(check_select)[check_select]])
  
  if (input$do_da_2o3 & input$do_da_2o3_BL) {
    to_show <- c(to_show, "ke1_dep_select", "ke2_value_select", "ke3_val_select")
  }
  to_show <- unique(to_show)

  if ("ke1_dep_select" %in% to_show) {
    ke1_dep_watch(T)
  } else {
    ke1_dep_watch(F)
  }
  
  for (i in to_show) {
    shinyjs::runjs(sprintf("$('#%s').show()", i))
  }

  for (i in names(data_select_template)) {
    selected <- gsub("_col", "", i)
    if (!selected %in% names(dt_analyze())) {
      selected <- NULL
    }
    updateSelectInput(inputId = i, choices = c("", names(dt_analyze())), selected = selected)
  }

  if (input$do_da_ke31) {
    runjs("toggleAssaySelect('ke1_assay_name', 'adra', true);")
    updateRadioButtons(inputId = "ke1_assay_name", selected = "dpra")
    
    runjs("toggleAssaySelect('ke3_assay_name', 'gardskin', true);")
    runjs("toggleAssaySelect('ke3_assay_name', 'il8luc', true);")
    runjs("toggleAssaySelect('ke3_assay_name', 'usens', true);")
    updateRadioButtons(inputId = "ke3_assay_name", selected = "hclat")
    
  } else {
    runjs("toggleAssaySelect('ke1_assay_name', 'adra', false);")
    updateRadioButtons(inputId = "ke1_assay_name", selected = "adra")
    
    runjs("toggleAssaySelect('ke3_assay_name', 'gardskin', false);")
    runjs("toggleAssaySelect('ke3_assay_name', 'usens', false);")
    updateRadioButtons(inputId = "ke3_assay_name", selected = "gardskin")
    if (input$do_da_its) {
      runjs("toggleAssaySelect('ke3_assay_name', 'il8luc', true);")
    } else if (input$do_da_2o3) {
      runjs("toggleAssaySelect('ke3_assay_name', 'il8luc', false);")
    }
  }
  
  updateTabsetPanel(inputId = "step_set", selected = "Select Data Columns")
  
})
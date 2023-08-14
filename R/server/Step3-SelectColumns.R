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
# output tables will show "NA" instead of blanks
## Reactive Values -----
# selected tests ordered as: 2o3, its, ke 3/1 sts
dass_choice <- reactiveVal()

resetApp_newDASS <- reactive({
  dat_for_anlz$col_data <- dat_for_anlz$col_dict <- NULL
  dt_review(NULL)
  flagged(NULL)
  dass_res$results <- dass_res$user_select <- dass_res$da_input <- dass_res$da_output <- NULL
  
  shinyjs::runjs("resetHidden(false);")
  shinyjs::runjs("rmDPRAListener();")
})

## Set Up Panel 3 -----
observeEvent(input$confirm_data, {
  req(usr_dt())
  check_select <- all(!input$do_da_2o3 &
                        !input$do_da_its & !input$do_da_ke31)
  if (check_select) {
    showNotification(
      type = "error",
      ui = "No defined approaches selected.",
      duration = 10
    )
  } 
  req(!check_select)
  
  if (!is.null(dass_choice())) {
    resetApp_newDASS()
  }
  
  # set up values for dropdown lists
  col_select_input <- colnames(usr_dt())
  template_cols <- c("dpra_call","dpra_pC","dpra_pK","hclat_call","hclat_mit",
                     "ks_call","ks_imax","insilico_call","insilico_ad")
  template_col_select <- list(
    dpra_call = "",
    dpra_pC = "",
    dpra_pK = "",
    hclat_call = "",
    hclat_mit = "",
    ks_call = "",
    ks_imax = "",
    insilico_call = "",
    insilico_ad = ""
  )
  for (i in template_cols) {
    out <- col_select_input[grep_ci(paste0("^", i, "$"), trimws(col_select_input))]
    if (length(out) > 0) {
      template_col_select[[i]] <- out
    }
  }

  # Names of DASS options
  dass_opts <- c("da_2o3", "da_its", "da_ke31")
  # Variables needed for each DASS
  dass_vars <- list(
    da_2o3 = c("ks_call", "dpra_call", "hclat_call"),
    da_its = c("hclat_mit", "dpra_pC", "dpra_pK", "insilico"),
    da_ke31 = c("hclat_mit", "dpra_call")
  )
  # Get user DASS selection as logical vector
  dass_selected <- c(
    input$do_da_2o3,
    input$do_da_its,
    input$do_da_ke31
  )
  # Filter DASS options and DASS variable vectors based on selection
  dass_opts <- dass_opts[dass_selected]
  dass_choice(dass_opts)
  dass_vars <- dass_vars[dass_selected]
  dass_vars <- sort(unique(unlist(dass_vars)))
  
  # Set up UI for Panel 3
  # DPRA %C- and %K-depletion
  # For DPRA call, if %C- and %K depletion are provided, user may choose
  # how to determine call - as the DPRA call column, or evaluated from
  # the assay values
  id_list <- list(
    dpra_call = list(
      toShow = c("dpraCallSelect", "dpraDepSelect"),
      toUpdate = c("dpra_call_col", "dpra_pC_col", "dpra_pK_col")
    ),
    hclat_call = list(
      toShow = "hclatCallSelect",
      toUpdate = "hclat_call_col"
    ),
    hclat_mit = list(
      toShow = "hclatMitSelect",
      toUpdate = "hclat_mit_col"
    ),
    ks_call = list(
      toShow = "ksCallSelect",
      toUpdate = "ks_call_col"
    ),
    insilico = list(
      toShow = "inSilicoSelect",
      toUpdate = c("insilico_call_col", "insilico_ad_col")
    )
  )
  
  colLoop <- id_list[intersect(names(id_list), dass_vars)]
  
  if (all(c("dpra_pC", "dpra_pK") %in% dass_vars)) {
    if (!"dpra_call" %in% dass_vars) {
      colLoop$dpraDep <- list(
        toShow = "dpraDepSelect",
        toUpdate = c("dpra_pC_col", "dpra_pK_col"))
    }
  }

  toShow <- sapply(colLoop, function(x) x[["toShow"]]) |> unlist(use.names = F)
  toUpdate <- sapply(colLoop, function(x) x[["toUpdate"]]) |> unlist(use.names = F)
  
  if (!all(c("dpra_pC", "dpra_pK") %in% dass_vars)) {
    if ("dpra_call" %in% dass_vars) {
      toShow <- toShow[toShow != "dpraDepSelect"]
      shinyjs::runjs("addDPRAListener()")
    }
  }

  for (i in toUpdate) {
    tmp <- gsub("_col$", "", i)
    updateSelectInput(inputId = i, choices = col_select_input, selected = template_col_select[[tmp]])
  }
  for (i in toShow) {
    shinyjs::runjs(sprintf("$('#%s').show()", i))
  }
  shinyjs::runjs(sprintf("showScroll('%s', '%s', '%s', '%s')", "selectcol_ui", "div", "value", "panel_col_options"))

  updateCollapse(session, id = "panelGroup", open = "panel_col_options", close = c("panel_dass_options", "panel_data_upload"))
}, ignoreInit = T)
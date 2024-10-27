# SELECT COLUMNS =====
# STANDARD -----
## Dictionaries -----
# HTML IDs to unhide based on DA selections
ke_block_id <- list(
  da_2o3  = c("ke1_select_ui", "ke2_select_ui", "ke3_select_ui", "ke1_call_select", "ke3_call_select"),
  da_its  = c("ke1_select_ui", "ke3_select_ui", "insil_select_ui", "ke1_assay_select", "ke1_dep_select", "ke3_val_select"),
  da_ke31 = c("ke1_select_ui", "ke3_select_ui", "ke1_call_select", "ke3_val_select")
)

# List to store selection details
tmp_list <- function(x) list(display_name = x, col_name = NULL, values = NULL, converted_values = NULL, flagged = NULL)
data_select_template <- list(
  ke1_call_col         = tmp_list(column_text_labels["ke1_call_col"]),
  ke1_mean_c_l_dep_col = tmp_list(column_text_labels["ke1_mean_c_l_dep_col"]),
  ke1_c_dep_col        = tmp_list(column_text_labels["ke1_c_dep_col"]),
  ke1_l_dep_col        = tmp_list(column_text_labels["ke1_l_dep_col"]),
  ke2_call_col         = tmp_list(column_text_labels["ke2_call_col"]),
  ke3_call_col         = tmp_list(column_text_labels["ke3_call_col"]),
  ke3_val_col          = tmp_list(column_text_labels["ke3_val_col"]),
  insil_call_col       = tmp_list(column_text_labels["insil_call_col"]),
  insil_ad_col         = tmp_list(column_text_labels["insil_ad_col"])
)

data_select_auto_col <- list(
  ke1_call_col         = c(adra = "ADRA_call", dpra = "DPRA_call"),
  ke1_mean_c_l_dep_col = c(adra = "ADRA_mean_dep", dpra = "DPRA_mean_dep"),
  ke1_c_dep_col        = c(adra = "ADRA_NAC_dep", dpra = "DPRA_Cys_dep"),
  ke1_l_dep_col        = c(adra = "ADRA_NAL_dep", dpra = "DPRA_Lys_dep"),
  ke2_call_col         = c(keratinosens = "KeratinoSens_call", lusens = "LuSens_call"),
  ke3_call_col         = c(gardskin = "GARDskin_call", hclat = "hCLAT_call", usens = "USENS_call"),
  ke3_val_col          = c(gardskin = "GARDskin_input_conc", hclat = "hCLAT_MIT", usens = "USENS_EC150"),
  insil_call_col       = c("insil_call", "Derek_prediction", "iSafeRat_prediction", "Leadscope_prediction", "OECDQSARTB_prediction", "StopTox_prediction"),
  insil_ad_col         = c("insil_ad", "Derek_ad", "iSafeRat_ad", "Leadscope_ad", "OECDQSARTB_ad", "StopTox_ad")
)

## Set up UI -----
observeEvent(input$confirm_data, {
  for (i in tabNames[4:6]) shinyjs::runjs(sprintf("resetHiddenTab('%s');", i))
  shinyjs::reset(id = "compare_setup_standard")
  
  updateTabsetPanel(inputId = "step_set", selected = "Select Data Columns")
  showHide(show = "select_col_ui_all")

  req(!blr())
  req(dt_analyze())
  showHide(show = "select_col_ui", hide = "select_blr_col_ui")

  output$data_da_header <- renderUI({
    HTML(sprintf("<p><b>Selected DA</b>: %s<br><b>Input File</b>: %s</p>",
                 switch(isolate(input$selected_da), da_2o3 = "2o3", da_its = "ITS", da_ke31 = "KE 3/1 STS"), 
                 fn = ifelse(input$use_demo_data, "Demo Data", input$fpath$name)))
  })
  
  if (input$selected_da == "da_ke31") {
    updateRadioButtons(inputId = "ke1_assay_name", selected = "dpra")
    updateRadioButtons(inputId = "ke3_assay_name", selected = "hclat")

  }

  for (i in names(data_select_template)) {
    selected <- ""
    cmatch <- names(dt_analyze()) %in% data_select_auto_col[[i]]
    if (any(cmatch)) {
      selected <- names(dt_analyze())[cmatch][1]
    }

    updateSelectInput(inputId = i, choices = c("", names(dt_analyze())), selected = selected)
  }

  # Create vector of UI IDs to show based on selected DAs
  to_show <- ke_block_id[[input$selected_da]]
  to_hide <- setdiff(unlist(ke_block_id), to_show)
  
  if (input$selected_da == "da_ke31") {
    to_hide <- c(to_hide, "ke3_assay_select")
    runjs("updateSelectUI(true);")
  } else {
    to_show <- c(to_show, "ke3_assay_select")
    runjs("updateSelectUI(false);")
  }

  showHide(show = to_show, hide = to_hide)
})

## Observers -----
observeEvent(input$ke1_call_interpret, {
  req(!blr())
  if (input$ke1_call_interpret) {
    if (input$selected_da == "da_2o3") {
      showHide(show = c("ke1_dep_select", "ke1_assay_select"), hide = "ke1_call_col")
    } else {
      showHide(show = c("ke1_dep_select"), hide = c("ke1_call_col", "ke1_assay_select")) 
    }
  } else {
    showHide(show = "ke1_call_col", hide = c("ke1_dep_select", "ke1_assay_select"))
  }
})

observe({
  req(!blr())
  
  if (input$ke1_call_interpret) {
    switch(
      isolate(input$selected_da),
      da_2o3 = showHide(show = c("ke1_dep_select", "ke1_assay_select"), hide = "ke1_call_col"),
      da_ke31 = showHide(show = c("ke1_dep_select"), hide = c("ke1_call_col", "ke1_assay_select")) 
    )
    
  } else if (!input$ke1_call_interpret) {
    showHide(show = "ke1_call_col", hide = c("ke1_dep_select", "ke1_assay_select"))
  }
})

observeEvent(input$ke1_choose_dep, {
  if (input$ke1_choose_dep) {
    showHide(show = c("ke1_c_dep_col", "ke1_l_dep_col"), hide = "ke1_mean_c_l_dep_col")
  } else if (!input$ke1_choose_dep) {
    showHide(show = "ke1_mean_c_l_dep_col", hide = c("ke1_c_dep_col", "ke1_l_dep_col"))
  }
})

observe({
  req(dt_analyze())
  for (i in c("ke1_call_col", "ke1_mean_c_l_dep_col", "ke1_c_dep_col", "ke1_l_dep_col")) {
    val <- data_select_auto_col[[i]][[input$ke1_assay_name]]
    cmatch <- grepl_ci(sprintf("^%s$", val), names(dt_analyze()))
    if (any(cmatch)) {
      updateSelectInput(inputId = i, selected = names(dt_analyze())[cmatch][1])
    }
  }
})

observe({
  req(dt_analyze())
  for (i in c("ke3_call_col", "ke3_val_col")) {
    val <- data_select_auto_col[[i]][[input$ke3_assay_name]]
    cmatch <- grepl_ci(sprintf("^%s$", val), names(dt_analyze()))
    if (any(cmatch)) {
      updateSelectInput(inputId = i, selected = names(dt_analyze())[cmatch][1])
    }
  }
})

observe({
  switch(
    input$ke1_assay_name,
    dpra = {updateSelectInput(inputId = "ke1_c_dep_col", label = "Cys Depletion Column"); updateSelectInput(inputId = "ke1_l_dep_col", label = "Lys Depletion Column")},
    adra = {updateSelectInput(inputId = "ke1_c_dep_col", label = "NAC Depletion Column"); updateSelectInput(inputId = "ke1_l_dep_col", label = "NAL Depletion Column")}
  )
})

observe({
  switch(
    input$ke3_assay_name,
    gardskin = updateSelectInput(inputId = "ke3_val_col", label = "Input Concentration Column"),
    hclat = updateSelectInput(inputId = "ke3_val_col", label = "Minimum Induction Threshold Column"),
    usens = updateSelectInput(inputId = "ke3_val_col", label = "EC150 Column")
  )
})

# BORDERLINE -----
col_req <- reactiveValues(ke1 = NULL, ke2 = NULL, ke3 = NULL)

## Dictionaries -----
tmp_blr_col_list <- function(cid_column, run_column = NULL, conc_column = NULL, 
                         numeric_columns = NULL, text_columns = NULL, 
                         yn_nomiss_columns = NULL, yn_miss_columns = NULL,
                         ks_pn_column = NULL) {
  list(
    cid_column        = cid_column,
    run_column        = run_column,
    conc_column       = conc_column,
    numeric_columns   = numeric_columns,
    text_columns      = text_columns,
    yn_nomiss_columns = yn_nomiss_columns,
    yn_miss_columns   = yn_miss_columns,
    ks_pn_column      = ks_pn_column
  )
}

ke1_blr_id <- list(
  adra = tmp_blr_col_list(cid_column = "ke1_blr_cid_col", numeric_columns = c("ke1_blr_c_dep_col", "ke1_blr_l_dep_col"), yn_miss_columns = "ke1_blr_c_only_col"),
  dpra = tmp_blr_col_list(cid_column = "ke1_blr_cid_col", numeric_columns = c("ke1_blr_c_dep_col", "ke1_blr_l_dep_col"), yn_miss_columns = "ke1_blr_c_only_col")
)

ke2_blr_id <- list(
  ks = tmp_blr_col_list(cid_column = "ke2_blr_cid_col", ks_pn_column = "ke2_blr_ks_call_col"),
  lusens = tmp_blr_col_list(cid_column = "ke2_blr_cid_col",run_column = "ke2_blr_run_col", conc_column = "ke2_blr_conc_col", numeric_columns = c("ke2_blr_fi_col", "ke2_blr_cv_col", "ke2_blr_p_col"))
)

ke3_blr_id <- list(
  gard  = tmp_blr_col_list(cid_column = "ke3_blr_cid_col", numeric_columns = "ke3_blr_gard_meanDV_col"),
  hclat = tmp_blr_col_list(
    cid_column      = "ke3_blr_cid_col",
    run_column      = "ke3_blr_run_col",
    numeric_columns = c(
      "ke3_blr_hclat_cd54_col",
      "ke3_blr_hclat_cd86_col",
      "ke3_blr_hclat_cv_col"
    )
  ),
  il8   = tmp_blr_col_list(
    cid_column        = "ke3_blr_cid_col",
    run_column        = "ke3_blr_run_col",
    numeric_columns   = c(
      "ke3_blr_il8_ind_col",
      "ke3_blr_il8_indLCL_col",
      "ke3_blr_il8_inh_col"
    ),
    yn_nomiss_columns = "ke3_blr_il8_ws_col"
  ),
  usens = tmp_blr_col_list(
    cid_column      = "ke3_blr_cid_col",
    run_column      = "ke3_blr_run_col",
    conc_column     = "ke3_blr_conc_col",
    numeric_columns = c("ke3_blr_usens_cd86_col", "ke3_blr_usens_cv_col")
  )
)

## Set up UI -----
observeEvent(input$confirm_data, {
  req(blr())
  showHide(show = "select_blr_col_ui", hide = "select_col_ui")
  
  output$data_da_header <- renderUI({
    HTML(sprintf("<p><b>Selected DA</b>: DA 2o3 + Borderline<br><b>Input File</b>: %s</p>",
                 input$fpath$name))
  })
  
  blr_ws <- c("ke1_blr_ws", "ke2_blr_ws", "ke3_blr_ws")
  for (i in 1:3) {
    updateSelectInput(inputId = blr_ws[i], choices = names(blrSheets()), selected = names(blrSheets())[i])
  }
})

## Observers -----
observe({
  switch(
    input$ke2_blr_assay_name,
    ks = showHide(show = "ke2_blr_ks_call_col", hide = "ke2_blr_lusens_col_select"),
    lusens = showHide(show = "ke2_blr_lusens_col_select", hide = "ke2_blr_ks_call_col")
  )
})

observe({
  switch(
    input$ke3_blr_assay_name,
    gard  = showHide(
      show = c("ke3_blr_gard_meanDV_col"),
      hide = c(
        "ke3_blr_run_col",
        "ke3_blr_conc_col",
        "ke3_blr_hclat_col_select",
        "ke3_blr_usens_col_select",
        "ke3_blr_il8_col_select"
      )
    ),
    hclat = showHide(
      show = c("ke3_blr_run_col", "ke3_blr_hclat_col_select"),
      hide = c(
        "ke3_blr_conc_col",
        "ke3_blr_gard_meanDV_col",
        "ke3_blr_usens_col_select",
        "ke3_blr_il8_col_select",
        "ke3_blr_conc_col"
      )
    ),
    il8   = showHide(
      show = c("ke3_blr_run_col", "ke3_blr_il8_col_select"),
      hide = c(
        "ke3_blr_conc_col",
        "ke3_blr_gard_meanDV_col",
        "ke3_blr_usens_col_select",
        "ke3_blr_hclat_col_select"
      )
    ),
    usens = showHide(
      show = c("ke3_blr_run_col", "ke3_blr_conc_col", "ke3_blr_usens_col_select"),
      hide = c(
        "ke3_blr_gard_meanDV_col",
        "ke3_blr_il8_col_select",
        "ke3_blr_hclat_col_select"
      )
    )
  )
})

observe({
  req(blr())
  ke1_cols <- unlist(ke1_blr_id[[1]], use.names = F)
  ws_cols <- names(isolate(blrSheets())[[input$ke1_blr_ws]])
  for (i in 1:length(ke1_cols)) {
    updateSelectInput(inputId = ke1_cols[i], choices = ws_cols, selected = ws_cols[i])
  }
})


observe({
  req(blr())
  ke2_cols <- unlist(ke2_blr_id[[input$ke2_blr_assay_name]], use.names = F)
  ws_cols <- names(isolate(blrSheets())[[input$ke2_blr_ws]])
  for (i in 1:length(ke2_cols)) {
    updateSelectInput(inputId = ke2_cols[i], choices = ws_cols, selected = ws_cols[i])
  }
})


observe({
  req(blr())
  ke3_cols <- unlist(ke3_blr_id[[input$ke3_blr_assay_name]], use.names = F)
  ws_cols <- names(isolate(blrSheets())[[input$ke3_blr_ws]])
  for (i in 1:length(ke3_cols)) {
    updateSelectInput(inputId = ke3_cols[i], choices = ws_cols, selected = ws_cols[i])
  }
})

# Modal slideshows -----
observe({
  s <- blr_img_ids[["ke1"]][blr_label_ids[["ke1"]] == input$ke1_blr_caro_img]
  showHide(
    show = s,
    hide = setdiff(blr_img_ids[["ke1"]], s)
  )
})

observeEvent(input$ke1_blr_prev, {
  idx <- which(blr_label_ids[["ke1"]] == input$ke1_blr_caro_img) - 1
  idx[idx == 0] <- length(blr_label_ids[["ke1"]])

  updateSelectInput(inputId = "ke1_blr_caro_img", selected = blr_label_ids[["ke1"]][idx])
})

observeEvent(input$ke1_blr_next, {
  idx <- which(blr_label_ids[["ke1"]] == input$ke1_blr_caro_img) + 1
  idx[idx > length(blr_label_ids[["ke1"]])] <- 1

  updateSelectInput(inputId = "ke1_blr_caro_img", selected = blr_label_ids[["ke1"]][idx])
})

observe({
  s <- blr_img_ids[["ke2"]][blr_label_ids[["ke2"]] == input$ke2_blr_caro_img]
  showHide(
    show = s,
    hide = setdiff(blr_img_ids[["ke2"]], s)
  )
})

observeEvent(input$ke2_blr_prev, {
  idx <- which(blr_label_ids[["ke2"]] == input$ke2_blr_caro_img) - 1
  idx[idx == 0] <- length(blr_label_ids[["ke2"]])
  
  updateSelectInput(inputId = "ke2_blr_caro_img", selected = blr_label_ids[["ke2"]][idx])
})

observeEvent(input$ke2_blr_next, {
  idx <- which(blr_label_ids[["ke2"]] == input$ke2_blr_caro_img) + 1
  idx[idx > length(blr_label_ids[["ke2"]])] <- 1
  
  updateSelectInput(inputId = "ke2_blr_caro_img", selected = blr_label_ids[["ke2"]][idx])
})

observe({
  s <- blr_img_ids[["ke3"]][blr_label_ids[["ke3"]] == input$ke3_blr_caro_img]
  showHide(
    show = s,
    hide = setdiff(blr_img_ids[["ke3"]], s)
  )
})

observeEvent(input$ke3_blr_prev, {
  idx <- which(blr_label_ids[["ke3"]] == input$ke3_blr_caro_img) - 1
  idx[idx == 0] <- length(blr_label_ids[["ke3"]])
  
  updateSelectInput(inputId = "ke3_blr_caro_img", selected = blr_label_ids[["ke3"]][idx])
})

observeEvent(input$ke3_blr_next, {
  idx <- which(blr_label_ids[["ke3"]] == input$ke3_blr_caro_img) + 1
  idx[idx > length(blr_label_ids[["ke3"]])] <- 1
  
  updateSelectInput(inputId = "ke3_blr_caro_img", selected = blr_label_ids[["ke3"]][idx])
})

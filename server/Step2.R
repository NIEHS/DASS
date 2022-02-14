# =============================================================================#
# File Name: Step2.R                                                           #
# Original Creator: ktto                                                       #
# Contact Information: comptox@ils-inc.com                                     #
# Date Created: 2021-02-10                                                     #
# License: MIT                                                                 #
# Description: server object for Step 2 of  DASS app.                          #
# Required Packages:                                                           #
# - data.table, DT                                                             #
# - openxlsx                                                                   #
# - readxl                                                                     #
# - shiny shinyBS shinyjs                                                      #
# =============================================================================#

# Step 2: Select Columns -----
# Set Up Panel 2 and populate select values
observeEvent(input$load_cols, {
  req(usr_dt())
  if (all(!input$do_da_2o3 &
          !input$do_da_itsv2 & !input$do_da_ke31)) {
    showNotification(
      type = "error",
      ui = "No defined approaches selected.",
      duration = 10
    )
  } else {
    # set up values for dropdown lists
    col_select_input$selected <- NULL
    col_select_input$deselected <- colnames(usr_dt())

    # Names of DASS options
    dass_opts <- c("da_2o3", "da_itsv2", "da_ke31")
    # Variables needed for each DASS
    dass_vars <- list(
      da_2o3 = c("ks_call", "dpra_call", "hclat_call"),
      da_itsv2 = c("hclat_mit", "dpra_pC", "dpra_pK", "oecd_tb"),
      da_ke31 = c("hclat_mit", "dpra_call")
    )
    # Get user DASS selection as logical vector
    dass_selected <- c(
      input$do_da_2o3,
      input$do_da_itsv2,
      input$do_da_ke31
    )
    # Filter DASS options and DASS variable vectors based on selection
    dass_opts <- dass_opts[dass_selected]
    dass_choice(dass_opts)
    dass_vars <- dass_vars[dass_selected]
    dass_vars <- sort(unique(unlist(dass_vars)))
    
    # Set up UI for Panel 2
    # List of ui objects
    dt_col_ui <- list()
    
    dt_col_ui$intro <-  HTML(
      "<p>The endpoint values required for the selected DAs are shown below.",
      "Use the drop down-menus to select the columns from your data",
      "that correspond to the given endpoints. A column must be selected for each",
      "endpoint shown. When you are finished, click 'Done'.<br><br>",
      "Click on the question circles for information about the column",
      "requirements. Values that are incorrectly formatted or invalid",
      "will not be evaluated and may affect the results. More details are given",
      "in the User Guide.</p>"
    )
    
    # DPRA %C- and %K-depletion
    # For DPRA call, if %C- and %K depletion are provided, user may choose
    # how to determine call - as the DPRA call column, or evaluated from
    # the assay values
    if (all(c("dpra_pC", "dpra_pK") %in% dass_vars)) {
      dt_col_ui$dpra_depl <- fluidRow(div(
        style = "margin-left:25px",
        HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>DPRA % Depletion</span>"),
        actionLink(inputId = "info_dpradep", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
        HTML("</p>"),
        div(
          style = "margin-left:25px",
          selectInput(
            inputId = "dpra_pC_col",
            label = "DPRA %-Cysteine Depletion Column",
            choices = c("", col_select_input$deselected)
          ),
          selectInput(
            inputId = "dpra_pK_col",
            label = "DPRA %-Lysine Depletion Column",
            choices = c("", col_select_input$deselected)
          )
        )
      ))
      if ("dpra_call" %in% dass_vars) {
        dt_col_ui$dpra_call <- fluidRow(div(
          style = "margin-left:25px",
          HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>DPRA Hazard Identification</span>"),
          actionLink(inputId = "info_dpracall_1", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
          HTML("</p>"),
          div(
            style = "margin-left:25px",
            radioButtons(
              inputId = "dpra_call_choice",
              label = NULL,
              choiceNames = c(
                "Use Hazard Identification",
                "Use %-Depletion Values"
              ),
              choiceValues = c(
                "call",
                "pdepl"
              )
            ),
            div(
              style = "margin-left:25px",
              conditionalPanel(
                condition = "input.dpra_call_choice=='call'",
                selectInput(
                  inputId = "dpra_call_col",
                  label = "DPRA Hazard Identification Column",
                  choices = c("", col_select_input$deselected)
                )
              )
            )
          )
        ))
      }
    } else if ("dpra_call" %in% dass_vars) {
      dt_col_ui$dpra_call <- fluidRow(div(
        style = "margin-left:25px",
        HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>DPRA Hazard Identification</span>"),
        actionLink(inputId = "info_dpracall_2", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
        HTML("</p>"),
        div(
          style = "margin-left:25px",
          radioButtons(
            inputId = "dpra_call_choice",
            label = NULL,
            choiceNames = c(
              "Use DPRA Hazard Identification",
              "Use %-Depletion Values"
            ),
            choiceValues = c(
              "call",
              "pdepl"
            )
          ),
          div(
            style = "margin-left:25px",
            conditionalPanel(
              condition = "input.dpra_call_choice=='call'",
              selectInput(
                inputId = "dpra_call_col",
                label = "DPRA Hazard Identification Column",
                choices = c("", col_select_input$deselected)
              )
            ),
            conditionalPanel(
              condition = "input.dpra_call_choice=='pdepl'",
              selectInput(
                inputId = "dpra_pC_col",
                label = "DPRA %-Cysteine Depletion Column",
                choices = c("", col_select_input$deselected)
              ),
              selectInput(
                inputId = "dpra_pK_col",
                label = "DPRA %-Lysine Depletion Column",
                choices = c("", col_select_input$deselected)
              )
            )
          )
        )
      ))
    }
    
    # h-CLAT sens/nonsens
    if ("hclat_call" %in% dass_vars) {
      dt_col_ui$hclat_call <- fluidRow(div(
        style = "margin-left:25px",
        HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>h-CLAT Hazard Identification</span>"),
        actionLink(inputId = "info_hclatcall", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
        HTML("</p>"),
        div(
          style = "margin-left:25px",
          selectInput(
            inputId = "hclat_call_col",
            label = "h-CLAT Hazard Identification Column",
            choices = c("", col_select_input$deselected)
          )
        )
      ))
    }
    
    # h-CLAT MIT
    if ("hclat_mit" %in% dass_vars) {
      dt_col_ui$hclat_mit <- fluidRow(div(
        style = "margin-left:25px",
        HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>h-CLAT MIT</span>"),
        actionLink(inputId = "info_hclatmit", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
        HTML("</p>"),
        div(
          style = "margin-left:25px",
          selectInput(
            inputId = "hclat_mit_col",
            label = "h-CLAT Minimum Induction Threshold (MIT) Column",
            choices = c("", col_select_input$deselected)
          )
        )
      ))
    }
    
    # KS Call - User selects how it should be loaded, as the call column,
    # or as KS iMax to be evaluated as Positive/Negative
    if ("ks_call" %in% dass_vars) {
      dt_col_ui$ks <- fluidRow(div(
        style = "margin-left:25px",
        HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>KeratinoSens&trade; Hazard Identification</span>"),
        actionLink(inputId = "info_kscall", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
        HTML("</p>"),
        div(
          style = "margin-left:25px",
          radioButtons(
            inputId = "ks_choice",
            label = NULL,
            choiceNames = c(
              "Use KS Hazard Identification",
              "Use KS iMax"
            ),
            choiceValues = c(
              "call",
              "imax"
            )
          ),
          div(
            style = "margin-left:25px",
            conditionalPanel(
              condition = "input.ks_choice=='call'",
              selectInput(
                inputId = "ks_call_col",
                label = "KS Hazard Identification Column",
                choices = c("", col_select_input$deselected),
                selected = FALSE
              )
            ),
            conditionalPanel(
              condition = "input.ks_choice=='imax'",
              selectInput(
                inputId = "ks_imax_col",
                label = "KS iMax Column",
                choices = c("", col_select_input$deselected),
                selected = FALSE
              )
            )
          )
        )
      ))
    }
    
    # in silico
    if ("oecd_tb" %in% dass_vars) {
      dt_col_ui$oecd <- fluidRow(div(
        style = "margin-left:25px",
        HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>In Silico Hazard Identification</span>"),
        actionLink(inputId = "info_oecdtbcall", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
        HTML("</p>"),
        div(
          style = "margin-left:25px",
          selectInput(
            inputId = "oecd_call_col",
            label = "In Silico Hazard Identification Column",
            choices = c("", col_select_input$deselected)
          ),
          selectInput(
            inputId = "oecd_ad_col",
            label = "In Silico Applicability Domain",
            choices = c("", col_select_input$deselected)
          )
        )
      ))
    }
    dt_col_ui$done_button <-   actionButton(inputId = "review_entries",
                                            label = "Done")
    
    output$step2ui <- renderUI({
      dt_col_ui
    })
    show("step2ui")
    updateCollapse(session,
                   id = "panels",
                   close = "panel_dass_options"
    )
    updateCollapse(session,
                   id = "panels",
                   open = "panel_col_options"
    )
  }
})

## Calculated Options -----
observeEvent(input$dpra_call_choice, {
  if (input$dpra_call_choice == "call") {
    if (!"da_itsv2" %in% dass_choice()) {
      updateSelectInput(inputId = "dpra_pC_col", selected = "")
      updateSelectInput(inputId = "dpra_pK_col", selected = "")
    }
  } else if (input$dpra_call_choice == "pdepl") {
    updateSelectInput(inputId = "dpra_call_col", selected = "")
  }
})

observeEvent(input$ks_choice, {
  if (input$ks_choice == "call") {
    updateSelectInput(inputId = "ks_imax_col", selected = "")
  } else if (input$ks_choice == "imax") {
    updateSelectInput(inputId = "ks_call_col", selected = "")
  }
})

## Questions -----
### DPRA -----
observeEvent(input$info_dpradep, {
  showModal(modalDialog(
    title = "DPRA % Depletion",
    HTML(
      "%-Cysteine and %-Lysine depletion values from the direct peptide reactivity",
      "assay (DPRA) are used in ITS.<br><br>The columns corresponding to %-Cysteine",
      "and %-Lysine depletion should only contain numeric values. Missing values",
      "should be blank or labeled as 'NA'.",
      "<br><br>For more details, see",
      "<em>OECD Test No. 442C: In Chemico Skin Sensitisation</em>[<a",
      "href='https://doi.org/10.1787/9789264229709-en'",
      "target = '_blank'>4</a>].</p>"
    ),
    easyClose = T
  ))
})

observeEvent(input$info_dpracall_1, {
  showModal(modalDialog(
    title = "DPRA Hazard Identification",
    HTML(
      "Chemical hazard identifications from the direct peptide reactivity assay (DPRA)",
      "are used in the 2o3 and KE3/1 STS defined approaches.<br><br>",
      "The column corresponding to DPRA hazard identifications should only contain the values:<ul style='margin-bottom:0px;'>",
      "<li>'p', 'pos', 'positive', or 1 to indicate positive outcomes (sensitizers)*</li>",
      "<li>'n', 'neg', 'negative', or 0 to indicate negative outcomes (non-sensitizers)*</li>",
      "<li>Missing values should be blank or labeled as 'NA'</li></ul>",
      "<span style='font-size: 90%;'><em>* Case insensitive</em></span><br>",
      "<br>Alternatively, the %-Cysteine and %-Lysine depletion values can be evaluated",
      "for hazard identifications. Hazard identifications are made using Tables 1 and 2 from <em>OECD Test No. 442C:",
      "In Chemico Skin Sensitisation</em>[<a",
      "href='https://doi.org/10.1787/9789264229709-en'",
      "target = '_blank'>4</a>]</p>"
    ),
    easyClose = T
  ))
})

observeEvent(input$info_dpracall_2, {
  showModal(modalDialog(
    title = "DPRA Hazard Identification",
    HTML(
      "Chemical hazard identifications from the direct peptide reactivity assay (DPRA)",
      "are used in the 2o3 and KE3/1 STS defined approaches.<br><br>",
      "The column corresponding to DPRA hazard identifications should only contain the values:<ul style='margin-bottom:0px;'>",
      "<li>'p', 'pos', 'positive', or 1 to indicate positive outcomes (sensitizers)*</li>",
      "<li>'n', 'neg', 'negative', or 0 to indicate negative outcomes (non-sensitizers)*</li>",
      "<li>Missing values should be blank or labeled as 'NA'</li></ul>",
      "<span style='font-size: 90%;'><em>* Case insensitive</em></span><br>",
      "<br>Alternatively, the %-Cysteine and %-Lysine depletion values can be evaluated",
      "for hazard identifications. The columns corresponding to %-Cysteine",
      "and %-Lysine depletion should only contain numeric values. Missing values",
      "should be blank or labeled as 'NA'. Hazard identifications are made using Tables 1 and 2 from",
      "<em>OECD Test No. 442C: In Chemico Skin Sensitisation</em>[<a",
      "href='https://doi.org/10.1787/9789264229709-en'",
      "target = '_blank'>4</a>]</p>"
    ),
    easyClose = T
  ))
})

### h-CLAT -----
observeEvent(input$info_hclatcall, {
  showModal(modalDialog(
    title = "h-CLAT Hazard Identification",
    HTML(
      "Chemical hazard identifications from the human cell line activation test (h-CLAT)",
      "are used in the 2o3 defined approach.<br><br>",
      "The column corresponding to h-CLAT hzard identification should only contain the values:<ul style='margin-bottom:0px;'>",
      "<li>'p', 'pos', 'positive', or 1 to indicate positive outcomes (sensitizers)*</li>",
      "<li>'n', 'neg', 'negative', or 0 to indicate negative outcomes (non-sensitizers)*</li>",
      "<li>Missing values should be blank or labeled as 'NA'</li></ul>",
      "<span style='font-size: 90%;'><em>* Case insensitive</em></span><br><br>",
      "For more details, see <em>OECD Test No. 442E: In Vitro Skin",
      "Sensitisation</em>[<a href='https://doi.org/10.1787/9789264264359-en'",
      "target = '_blank'>5</a>]</p>"
    ),
    easyClose = T
  ))
})

observeEvent(input$info_hclatmit, {
  showModal(modalDialog(
    title = "h-CLAT MIT",
    HTML(
      "Minimum induction threshold (MIT) from the human cell line activiation test (h-CLAT)",
      "is used in the ITS and KE3/1 STS defined approaches.<br><br>",
      "The column corresponding to h-CLAT MIT should only contain:<ul style='margin-bottom:0px;'>",
      "<li>Numeric values</li>",
      "<li>'n', 'neg', 'negative', or 'Inf' to indicate negative outcomes (non-sensitizers)*</li>",
      "<li>Missing values should be blank or labeled as 'NA'</li></ul>",
      "<span style='font-size: 90%;'><em>* Case insensitive</em></span><br><br>",
      "For more details, see <em>OECD Test No. 442E: In Vitro Skin",
      "Sensitisation</em>[<a href='https://doi.org/10.1787/9789264264359-en'",
      "target = '_blank'>5</a>]</p>"
    ),
    easyClose = T
  ))
})

### KeratinoSens -----
observeEvent(input$info_kscall, {
  showModal(modalDialog(
    title = HTML("KeratinoSens&trade; Hazard Identification"),
    HTML(
      "Chemical hazard identifications from the KeratinoSens&trade; (KS) assay",
      "are used in the 2o3 defined approach.<br><br>",
      "The column corresponding to KS hazard identification should only contain the values:<ul style='margin-bottom:0px;>",
      "<li>'p', 'pos', 'positive', or 1 to indicate positive outcomes (sensitizers)*</li>",
      "<li>'n', 'neg', 'negative', or 0 to indicate negative outcomes (non-sensitizers)*</li>",
      "<li>Missing values should be blank or labeled as 'NA'</li></ul>",
      "<span style='font-size: 90%;'><em>* Case insensitive</em></span><br><br>",
      "Alternatively, iMax values can be provided and evaluated for hazard identification. The",
      "column corresponding to KS iMax should contain only numeric values. Missing",
      "values should be labeled 'NA'. Chemicals with KS iMax values &geq;1.5",
      "are labeled as positive and chemicals with KS iMax values &lt;1.5 are",
      "labeled as negative.<br><br>",
      "For more details, see <em>OECD Test No. 442D: In Vitro",
      "Skin Sensitisation</em>[<a",
      "href='https://doi.org/10.1787/9789264229822-en' target = '_blank'>6</a>]</p>"
    ),
    easyClose = T
  ))
})

### In Silico -----
observeEvent(input$info_oecdtbcall, {
  showModal(modalDialog(
    title = "In Silico Hazard Identification",
    HTML(
      "The ITS defined approach uses <em>in silico</em> predictions of hazard identification from either",
      "<a href='https://www.lhasalimited.org/products/skin-sensitisation-assessment-using-derek-nexus.htm'",
      "target = '_blank'>Derek Nexus</a> or the OECD QSAR Toolbox[<a",
      "href='https://doi.org/10.1016/j.comtox.2019.01.006' target = '_blank'>7</a>].",
      "<br><br>",
      "The column corresponding to <em>in silico</em> hazard identification predictions should only contain the values:<ul style='margin-bottom:0px;'>",
      "<li>'p', 'pos', 'positive', or 1 to indicate positive outcomes (sensitizers)*</li>",
      "<li>'n', 'neg', 'negative', or 0 to indicate negative outcomes (non-sensitizers)*</li>",
      "<li>Missing values should be blank or labeled as 'NA'</li></ul>",
      "<span style='font-size: 90%;'><em>* Case insensitive</em></span><br><br>",
      "Additionally, a column corresponding to applicability domain (AD) should",
      "be provided. This column should only contain the values:<ul style='margin-bottom:0px;>",
      "<li>'in' or 1 to indicate the chemical is within the AD*</li>",
      "<li>'out' or 0 to indicate the chemical is outside the AD*. Values for",
      "chemicals outside the AD will not be evaluated</li>",
      "<li>Missing values should be blank or labeled as 'NA'</li></ul>",
      "<span style='font-size: 90%;'><em>* Case insensitive</em></span>",
      "</p>"
    ),
    easyClose = T
  ))
})

## Dropdown Updates -----
# remove selected columns from dropdowns to prevent duplicate column selection
# column selections
observeEvent(input$dpra_call_col, {
  # Get previously selected
  old_select <- dt_col_select$dpra_call
  # Get new selected column
  new_select <- input$dpra_call_col
  
  # If a change was made...
  if (old_select != new_select) {
    # If nothing was selected previously
    if (old_select == "" & new_select != "") {
      col_select_input$selected <-
        c(col_select_input$selected, new_select)
      col_select_input$deselected <-
        col_select_input$deselected[col_select_input$deselected != new_select]
      # If removing selected column
    } else if (old_select != "" & new_select == "") {
      col_select_input$selected <-
        col_select_input$selected[col_select_input$selected != old_select]
      col_select_input$deselected <-
        c(col_select_input$deselected, old_select)
      # If changing previously selected column to different column
    } else if (old_select != "" & new_select != "") {
      col_select_input$selected <-
        c(col_select_input$selected, new_select)
      col_select_input$deselected <-
        col_select_input$deselected[col_select_input$deselected != new_select]
      col_select_input$selected <-
        col_select_input$selected[col_select_input$selected != old_select]
      col_select_input$deselected <-
        c(col_select_input$deselected, old_select)
    }
    # Ensure that previous selections are maintained
    si_temp <- si_ids[si_ids != "dpra_call_col"]
    col_des_up <- col_select_input$deselected
    col_des_up <- na.omit(col_des_up[match(colnames(usr_dt()), col_des_up)])
    for (si in si_temp) {
      sel <- input[[si]]
      updateSelectInput(
        inputId = si,
        choices = c("", sel, col_des_up),
        selected = sel
      )
    }
    dt_col_select$dpra_call <- input$dpra_call_col
  }
})

observeEvent(input$dpra_pC_col, {
  old_select <- dt_col_select$dpra_pC
  new_select <- input$dpra_pC_col
  
  if (old_select != new_select) {
    if (old_select == "" & new_select != "") {
      col_select_input$selected <-
        c(col_select_input$selected, new_select)
      col_select_input$deselected <-
        col_select_input$deselected[col_select_input$deselected != new_select]
    } else if (old_select != "" & new_select == "") {
      col_select_input$selected <-
        col_select_input$selected[col_select_input$selected != old_select]
      col_select_input$deselected <-
        c(col_select_input$deselected, old_select)
    } else if (old_select != "" & new_select != "") {
      col_select_input$selected <-
        c(col_select_input$selected, new_select)
      col_select_input$deselected <-
        col_select_input$deselected[col_select_input$deselected != new_select]
      col_select_input$selected <-
        col_select_input$selected[col_select_input$selected != old_select]
      col_select_input$deselected <-
        c(col_select_input$deselected, old_select)
    }
    si_temp <- si_ids[si_ids != "dpra_pC_col"]
    col_des_up <- col_select_input$deselected
    col_des_up <- na.omit(col_des_up[match(colnames(usr_dt()), col_des_up)])
    for (si in si_temp) {
      sel <- input[[si]]
      updateSelectInput(
        inputId = si,
        choices = c("", sel, col_des_up),
        selected = sel
      )
    }
    dt_col_select$dpra_pC <- input$dpra_pC_col
  }
})

observeEvent(input$dpra_pK_col, {
  old_select <- dt_col_select$dpra_pK
  new_select <- input$dpra_pK_col
  
  if (old_select != new_select) {
    if (old_select == "" & new_select != "") {
      col_select_input$selected <-
        c(col_select_input$selected, new_select)
      col_select_input$deselected <-
        col_select_input$deselected[col_select_input$deselected != new_select]
    } else if (old_select != "" & new_select == "") {
      col_select_input$selected <-
        col_select_input$selected[col_select_input$selected != old_select]
      col_select_input$deselected <-
        c(col_select_input$deselected, old_select)
    } else if (old_select != "" & new_select != "") {
      col_select_input$selected <-
        c(col_select_input$selected, new_select)
      col_select_input$deselected <-
        col_select_input$deselected[col_select_input$deselected != new_select]
      col_select_input$selected <-
        col_select_input$selected[col_select_input$selected != old_select]
      col_select_input$deselected <-
        c(col_select_input$deselected, old_select)
    }
    si_temp <- si_ids[si_ids != "dpra_pK_col"]
    col_des_up <- col_select_input$deselected
    col_des_up <- na.omit(col_des_up[match(colnames(usr_dt()), col_des_up)])
    for (si in si_temp) {
      sel <- input[[si]]
      updateSelectInput(
        inputId = si,
        choices = c("", sel, col_des_up),
        selected = sel
      )
    }
    dt_col_select$dpra_pK <- input$dpra_pK_col
  }
})

observeEvent(input$hclat_call_col, {
  old_select <- dt_col_select$hclat_call
  new_select <- input$hclat_call_col
  
  if (old_select != new_select) {
    if (old_select == "" & new_select != "") {
      col_select_input$selected <-
        c(col_select_input$selected, new_select)
      col_select_input$deselected <-
        col_select_input$deselected[col_select_input$deselected != new_select]
    } else if (old_select != "" & new_select == "") {
      col_select_input$selected <-
        col_select_input$selected[col_select_input$selected != old_select]
      col_select_input$deselected <-
        c(col_select_input$deselected, old_select)
    } else if (old_select != "" & new_select != "") {
      col_select_input$selected <-
        c(col_select_input$selected, new_select)
      col_select_input$deselected <-
        col_select_input$deselected[col_select_input$deselected != new_select]
      col_select_input$selected <-
        col_select_input$selected[col_select_input$selected != old_select]
      col_select_input$deselected <-
        c(col_select_input$deselected, old_select)
    }
    si_temp <- si_ids[si_ids != "hclat_call_col"]
    col_des_up <- col_select_input$deselected
    col_des_up <- na.omit(col_des_up[match(colnames(usr_dt()), col_des_up)])
    for (si in si_temp) {
      sel <- input[[si]]
      updateSelectInput(
        inputId = si,
        choices = c("", sel, col_des_up),
        selected = sel
      )
    }
    dt_col_select$hclat_call <- input$hclat_call_col
  }
})

observeEvent(input$hclat_mit_col, {
  old_select <- dt_col_select$hclat_mit
  new_select <- input$hclat_mit_col
  
  if (old_select != new_select) {
    if (old_select == "" & new_select != "") {
      col_select_input$selected <-
        c(col_select_input$selected, new_select)
      col_select_input$deselected <-
        col_select_input$deselected[col_select_input$deselected != new_select]
    } else if (old_select != "" & new_select == "") {
      col_select_input$selected <-
        col_select_input$selected[col_select_input$selected != old_select]
      col_select_input$deselected <-
        c(col_select_input$deselected, old_select)
    } else if (old_select != "" & new_select != "") {
      col_select_input$selected <-
        c(col_select_input$selected, new_select)
      col_select_input$deselected <-
        col_select_input$deselected[col_select_input$deselected != new_select]
      col_select_input$selected <-
        col_select_input$selected[col_select_input$selected != old_select]
      col_select_input$deselected <-
        c(col_select_input$deselected, old_select)
    }
    si_temp <- si_ids[si_ids != "hclat_mit_col"]
    col_des_up <- col_select_input$deselected
    col_des_up <- na.omit(col_des_up[match(colnames(usr_dt()), col_des_up)])
    for (si in si_temp) {
      sel <- input[[si]]
      updateSelectInput(
        inputId = si,
        choices = c("", sel, col_des_up),
        selected = sel
      )
    }
    dt_col_select$hclat_mit <- input$hclat_mit_col
  }
})

observeEvent(input$ks_call_col, {
  old_select <- dt_col_select$ks_call
  new_select <- input$ks_call_col
  
  if (old_select != new_select) {
    if (old_select == "" & new_select != "") {
      col_select_input$selected <-
        c(col_select_input$selected, new_select)
      col_select_input$deselected <-
        col_select_input$deselected[col_select_input$deselected != new_select]
    } else if (old_select != "" & new_select == "") {
      col_select_input$selected <-
        col_select_input$selected[col_select_input$selected != old_select]
      col_select_input$deselected <-
        c(col_select_input$deselected, old_select)
    } else if (old_select != "" & new_select != "") {
      col_select_input$selected <-
        c(col_select_input$selected, new_select)
      col_select_input$deselected <-
        col_select_input$deselected[col_select_input$deselected != new_select]
      col_select_input$selected <-
        col_select_input$selected[col_select_input$selected != old_select]
      col_select_input$deselected <-
        c(col_select_input$deselected, old_select)
    }
    si_temp <- si_ids[si_ids != "ks_call_col"]
    col_des_up <- col_select_input$deselected
    col_des_up <- na.omit(col_des_up[match(colnames(usr_dt()), col_des_up)])
    for (si in si_temp) {
      sel <- input[[si]]
      updateSelectInput(
        inputId = si,
        choices = c("", sel, col_des_up),
        selected = sel
      )
    }
    dt_col_select$ks_call <- input$ks_call_col
  }
})

observeEvent(input$ks_imax_col, {
  old_select <- dt_col_select$ks_imax
  new_select <- input$ks_imax_col
  
  if (old_select != new_select) {
    if (old_select == "" & new_select != "") {
      col_select_input$selected <-
        c(col_select_input$selected, new_select)
      col_select_input$deselected <-
        col_select_input$deselected[col_select_input$deselected != new_select]
    } else if (old_select != "" & new_select == "") {
      col_select_input$selected <-
        col_select_input$selected[col_select_input$selected != old_select]
      col_select_input$deselected <-
        c(col_select_input$deselected, old_select)
    } else if (old_select != "" & new_select != "") {
      col_select_input$selected <-
        c(col_select_input$selected, new_select)
      col_select_input$deselected <-
        col_select_input$deselected[col_select_input$deselected != new_select]
      col_select_input$selected <-
        col_select_input$selected[col_select_input$selected != old_select]
      col_select_input$deselected <-
        c(col_select_input$deselected, old_select)
    }
    si_temp <- si_ids[si_ids != "ks_imax_col"]
    col_des_up <- col_select_input$deselected
    col_des_up <- na.omit(col_des_up[match(colnames(usr_dt()), col_des_up)])
    for (si in si_temp) {
      sel <- input[[si]]
      updateSelectInput(
        inputId = si,
        choices = c("", sel, col_des_up),
        selected = sel
      )
    }
    dt_col_select$ks_imax <- input$ks_imax_col
  }
})

observeEvent(input$oecd_call_col, {
  old_select <- dt_col_select$oecd_tb_call
  new_select <- input$oecd_call_col
  
  if (old_select != new_select) {
    if (old_select == "" & new_select != "") {
      col_select_input$selected <-
        c(col_select_input$selected, new_select)
      col_select_input$deselected <-
        col_select_input$deselected[col_select_input$deselected != new_select]
    } else if (old_select != "" & new_select == "") {
      col_select_input$selected <-
        col_select_input$selected[col_select_input$selected != old_select]
      col_select_input$deselected <-
        c(col_select_input$deselected, old_select)
    } else if (old_select != "" & new_select != "") {
      col_select_input$selected <-
        c(col_select_input$selected, new_select)
      col_select_input$deselected <-
        col_select_input$deselected[col_select_input$deselected != new_select]
      col_select_input$selected <-
        col_select_input$selected[col_select_input$selected != old_select]
      col_select_input$deselected <-
        c(col_select_input$deselected, old_select)
    }
    si_temp <- si_ids[si_ids != "oecd_call_col"]
    col_des_up <- col_select_input$deselected
    col_des_up <- na.omit(col_des_up[match(colnames(usr_dt()), col_des_up)])
    for (si in si_temp) {
      sel <- input[[si]]
      updateSelectInput(
        inputId = si,
        choices = c("", sel, col_des_up),
        selected = sel
      )
    }
    dt_col_select$oecd_tb_call <- input$oecd_call_col
  }
})

observeEvent(input$oecd_ad_col, {
  old_select <- dt_col_select$oecd_tb_ad
  new_select <- input$oecd_ad_col
  
  if (old_select != new_select) {
    if (old_select == "" & new_select != "") {
      col_select_input$selected <-
        c(col_select_input$selected, new_select)
      col_select_input$deselected <-
        col_select_input$deselected[col_select_input$deselected != new_select]
    } else if (old_select != "" & new_select == "") {
      col_select_input$selected <-
        col_select_input$selected[col_select_input$selected != old_select]
      col_select_input$deselected <-
        c(col_select_input$deselected, old_select)
    } else if (old_select != "" & new_select != "") {
      col_select_input$selected <-
        c(col_select_input$selected, new_select)
      col_select_input$deselected <-
        col_select_input$deselected[col_select_input$deselected != new_select]
      col_select_input$selected <-
        col_select_input$selected[col_select_input$selected != old_select]
      col_select_input$deselected <-
        c(col_select_input$deselected, old_select)
    }
    si_temp <- si_ids[si_ids != "oecd_ad_col"]
    col_des_up <- col_select_input$deselected
    col_des_up <- na.omit(col_des_up[match(colnames(usr_dt()), col_des_up)])
    for (si in si_temp) {
      sel <- input[[si]]
      updateSelectInput(
        inputId = si,
        choices = c("", sel, col_des_up),
        selected = sel
      )
    }
    dt_col_select$oecd_tb_ad <- input$oecd_ad_col
  }
})

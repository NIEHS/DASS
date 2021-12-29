#=============================================================================#
# File Name: server_obj.r                                                     #
# Original Creator: ktto                                                      #
# Contact Information: comptox@ils-inc.com                                    #
# Date Created: 2021-12-03                                                    #
# License: MIT                                                                #
# Description: server objects for dass app                                    #
# Required Packages:                                                          #
# - data.table, DT                                                            #
# - shiny shinyBS shinyjs                                                     #
#=============================================================================#

# Reactive Values -----
# tracks selected column names to prevent duplicate column selection
col_select_input <- reactiveValues()
# selected tests ordered as: 2o3, itsv2, ke 3/1 sts
dass_choice <- reactiveVal() 
# DASS results
dass_res <- reactiveVal()
# formatted data
dat_for_anlz <- reactiveValues()
# list with user data column names
dt_col_select <- reactiveValues(
  dpra_call = "",
  dpra_pC = "",
  dpra_pK = "",
  hclat_call = "",
  hclat_mit = "",
  ks_call = "",
  ks_imax = "",
  oecd_tb_call = "",
  oecd_tb_ad = ""
) 
# table with selected columns for user to review
dt_review <- reactiveVal()
# indicator to trigger popup warning if running with flagged columns
flagged <- reactiveVal()
# text shown during review of selected columns
review_label <- reactiveVal()
# indicator to trigger run
run_dass <- reactiveVal()
# user's uploaded data
usr_dt <- reactiveVal()

# Selection Names -----
# IDs for column selection drop down menus
si_ids <- c(
  "dpra_call_col",
  "dpra_pC_col",
  "dpra_pK_col",
  "hclat_call_col",
  "hclat_mit_col",
  "ks_call_col",
  "ks_imax_col",
  "oecd_call_col",
  "oecd_ad_col"
)

# Formatting -----
# output tables will show "na" instead of blanks
rowCallback <- c(
  "function(row, data) {",
  "for (var i=0; i<data.length; i++) {",
  "if(data[i]===null){",
  "$('td:eq('+i+')', row).html('NA')",
  ".css({'color': 'rgb(89,89,89)'});",
  "}",
  "}",
  "}"
)

# Data Loading -----
# Once the user selects a file, load the data onto the page and
# show the defined approaches menu
observeEvent(input$fpath, {
  # Check file extension
  ext <- unlist(strsplit(input$fpath$name, "[.]"))
  ext <- ext[length(ext)]
  if (!grepl("^csv$|^tsv$|^txt$", ext)) {
    showNotification(
      type = "error",
      "Incorrect file type. Data must be comma-delimited (csv) or tab-delimited (tsv, txt)",
      duration = 10)
  } else {
    # Read in data
    dt <- read_data(input$fpath$datapath)
    usr_dt(dt)
    col_select_input$selected <- NULL
    col_select_input$deselected <- colnames(dt)
    updateCollapse(session,
                   id = "panels",
                   open = "panel_dass_options")
    # Resets reactive values for new dataset
    for (i in 1:length(si_ids)){
      updateSelectInput(inputId =si_ids[i], selected = "")
    }
    review_label(NULL)
    flagged(NULL)
    dt_review(NULL)
    # dass_res(NULL)
    # Close all other panels
    updateCollapse(session,
                   id = "panels",
                   close = c("panel_col_options", "panel_review", "panel_results"))
  }
})

output$usr_dt <- renderDataTable({
  req(usr_dt())
  datatable(usr_dt(),
            filter = "top",
            class = "cell-border stripe hover",
            options = list(scrollY = TRUE,
                           scrollX = TRUE,
                           rowCallback = JS(rowCallback)))
})

# Step 1: Select Approaches -----
# Select all strategies
observeEvent(input$dass_all, {
  updateCheckboxInput(inputId = "do_da_2o3",
                      value = TRUE)
  updateCheckboxInput(inputId = "do_da_itsv2",
                      value = TRUE)
  updateCheckboxInput(inputId = "do_da_ke31",
                      value = TRUE)
})

# Select no strategies
observeEvent(input$dass_none, {
  updateCheckboxInput(inputId = "do_da_2o3",
                      value = FALSE)
  updateCheckboxInput(inputId = "do_da_itsv2",
                      value = FALSE)
  updateCheckboxInput(inputId = "do_da_ke31",
                      value = FALSE)
})

## Questions -----
observeEvent(input$info_2o3, {
  showModal(modalDialog(
    title = "2 out of 3",
    HTML(
      "<p>The 2 out of 3 (2o3) Defined Approach is a sequential testing strategy",
      "that classifies chemicals as sensitizers or non-sensitizers based on at",
      "least 2 concordant results among the direct peptide reactivity assay (DPRA),",
      "KeratinoSens&#8482;, and human cell-line activiation test (h-CLAT).",
      "<br><br>",
      "For more details, see <i>OECD Guideline No. 497: Defined Approaches on Skin",
      "Sensitisation</i>[<a href='https://doi.org/https://doi.org/10.1787/b92879a4-en'",
      "target = '_blank'>1</a>]</p>"
      ),
    easyClose = T
  ))
})

observeEvent(input$info_itsv2, {
  showModal(modalDialog(
    title = "Integrated Testing Strategy v.2",
    HTML(
      "<p>This app implements version 2 of the Integrated Testing Strategy (ITSv2)",
      "Defined Approach. ITSv2 classifies chemicals as sensitizers or non-sensitizers",
      "and predicts GHS potency category by scoring results from the the direct",
      "peptide reactivity assay (DPRA), human cell-line activiation test (h-CLAT),",
      "and <i>in silico</i> predictions from the OECD QSAR Toolbox.",
      "<br><br>",
      "For more details, see <i>OECD Guideline No. 497: Defined Approaches on Skin",
      "Sensitisation</i>[<a href='https://doi.org/https://doi.org/10.1787/b92879a4-en'",
      "target = '_blank'>1</a>]</p>"
    ),
    easyClose = T
  ))
})

observeEvent(input$info_ke31, {
  showModal(modalDialog(
    title = "Key Event 3/1 (KE 3/1) Sequential Testing Strategy (STS)",
    HTML(
      "<p>The Key Event 3/1 Sequential Testing Strategy is a sequential testing strategy",
      "that classifies chemicals as sensitizers or non-sensitizers",
      "and predicts GHS potency category based on results from the the direct",
      "peptide reactivity assay (DPRA) and human cell-line activiation test (h-CLAT).",
      "<br><br>",
      "For more details, see EPA's <i>Interim Science Policy: Use of Alternative",
      "Approaches for Skin Sensitization as a Replacement for Laboratory Animal",
      "Testing Draft for Public Comment</i>[<a",
      "href='https://www.regulations.gov/document/EPA-HQ-OPP-2016-0093-0090'",
      "target = '_blank'>3</a>]</p>"
    ),
    easyClose = T
  ))
})

# Step 2: Select Columns -----
# Set Up Panel 2 and populate select values
observeEvent(input$load_cols, {
  req(usr_dt())
  if (all(!input$do_da_2o3 &
          !input$do_da_itsv2 & !input$do_da_ke31)) {
    showNotification(type = "error",
                     ui = "No defined approaches selected.",
                     duration = 10)
  } else {
    # Names of DASS options
    dass_opts <- c("da_2o3", "da_itsv2", "da_ke31")
    # Variables needed for each DASS
    dass_vars <- list(
      da_2o3 = c("ks_call", "dpra_call", "hclat_call"),
      da_itsv2 = c("hclat_mit", "dpra_pC", "dpra_pK", "oecd_tb"),
      da_ke31 = c("hclat_mit", "dpra_call")
    )
    # Get user DASS selection as logical vector
    dass_selected <- c(input$do_da_2o3,
                       input$do_da_itsv2,
                       input$do_da_ke31)
    # Filter DASS options and DASS variable vectors based on selection
    dass_opts <- dass_opts[dass_selected]
    dass_choice(dass_opts)
    dass_vars <- dass_vars[dass_selected]
    dass_vars <- sort(unique(unlist(dass_vars)))
    
    # Set up UI for Panel 2
    # List of ui objects
    dt_col_ui <- list()
    
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
          HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>DPRA Call</span>"),
          actionLink(inputId = "info_dpracall_1", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
          HTML("</p>"),
          div(
            style = "margin-left:25px",
            radioButtons(
              inputId = "dpra_call_choice",
              label = NULL,
              choiceNames = c("Use Positive/Negative Call",
                              "Use %-Depletion Values"),
              choiceValues = c("call",
                               "pdepl")
            ),
            div(
              style = "margin-left:25px",
              conditionalPanel(
                condition = "input.dpra_call_choice=='call'",
                selectInput(
                  inputId = "dpra_call_col",
                  label = "DPRA Call Column",
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
        HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>DPRA Call</span>"),
        actionLink(inputId = "info_dpracall_2", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
        HTML("</p>"),
        div(
          style = "margin-left:25px",
          radioButtons(
            inputId = "dpra_call_choice",
            label = NULL,
            choiceNames = c("Use Positive/Negative Call",
                            "Use %-Depletion Values"),
            choiceValues = c("call",
                             "pdepl")
          ),
          div(
            style = "margin-left:25px",
            conditionalPanel(
              condition = "input.dpra_call_choice=='call'",
              selectInput(
                inputId = "dpra_call_col",
                label = "DPRA Call Column",
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
    
    # h-CLAT Call
    if ("hclat_call" %in% dass_vars) {
      dt_col_ui$hclat_call <- fluidRow(div(
        style = "margin-left:25px",
        HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>h-CLAT Call</span>"),
        actionLink(inputId = "info_hclatcall", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
        HTML("</p>"),
        div(
          style = "margin-left:25px",
          selectInput(
            inputId = "hclat_call_col",
            label = "h-CLAT Call Column",
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
        HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>KeratinoSens&trade; Call</span>"),
        actionLink(inputId = "info_kscall", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
        HTML("</p>"),
        div(
          style = "margin-left:25px",
          radioButtons(
            inputId = "ks_choice",
            label = NULL,
            choiceNames = c("Use Positive/Negative Call",
                            "Use iMax"),
            choiceValues = c("call",
                             "imax")
          ),
          div(
            style = "margin-left:25px",
            conditionalPanel(
              condition = "input.ks_choice=='call'",
              selectInput(
                inputId = "ks_call_col",
                label = "KS Call Column",
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

    # OECD QSAR Toolbox
    if ("oecd_tb" %in% dass_vars) {
      dt_col_ui$oecd <- fluidRow(div(
        style = "margin-left:25px",
        HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>OECD QSAR Toolbox Call</span>"),
        actionLink(inputId = "info_oecdtbcall", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
        HTML("</p>"),
        div(
          style = "margin-left:25px",
          selectInput(
            inputId = "oecd_call_col",
            label = "OECD QSAR TB Call Column",
            choices = c("", col_select_input$deselected)
          ),
          selectInput(
            inputId = "oecd_ad_col",
            label = "OECD QSAR TB Applicability Domain",
            choices = c("", col_select_input$deselected)
          )
        )
      ))
    }
    output$step2ui <- renderUI({
      dt_col_ui
    })
    updateCollapse(session,
                   id = "panels",
                   close = "panel_dass_options")
    updateCollapse(session,
                   id = "panels",
                   open = "panel_col_options")
  }
})

## Questions -----
observeEvent(input$info_dpradep, {
  showModal(modalDialog(
    title = "Direct Peptide Reactivity Assay",
    HTML(
      "%-Cysteine and %-Lysine depletion values from the direct peptide reactivity",
      "assay (DPRA) are used in ITSv2.<br><br>The columns corresponding to %-Cysteine",
      "and %-Lysine depletion should only contain numeric values. Missing values",
      "should be labeled as 'NA'. If negative values are reported, then co-elution",
      "is assumed and the value will not be used for scoring.<br><br>For more details, see",
      "<i>OECD Test No. 442C: In Chemico Skin Sensitisation</i>[<a",
      "href='https://doi.org/10.1787/9789264229709-en'",
      "target = '_blank'>4</a>].</p>"
    ),
    easyClose = T
  ))
})

observeEvent(input$info_dpracall_1, {
  showModal(modalDialog(
    title = "Direct Peptide Reactivity Assay",
    HTML(
      "Chemical calls from the direct peptide reactivity assay (DPRA)",
      "are used in the 2o3 and KE3/1 STS defined approaches.<br><br>",
      "The column corresponding to DPRA call should only contain the values:<ul>",
      "<li>'p', 'pos', 'positive', or 1 to indicate positive calls (sensitizers)</li>",
      "<li>'n', 'neg', 'negative', or 0 to indicate negative calls (non-sensitizers)</li>",
      "<li>Missing values should be labeled as 'NA'.</li></ul>",
      "<br>Alternatively, the %-Cysteine and %-Lysine values can be evaluated",
      "for call. Calls are made using Tables 1 and 2 from <i>OECD Test No. 442C:",
      "In Chemico Skin Sensitisation</i>[<a",
      "href='https://doi.org/10.1787/9789264229709-en'",
      "target = '_blank'>4</a>]</p>"
    ),
    easyClose = T
  ))
})

observeEvent(input$info_dpracall_2, {
  showModal(modalDialog(
    title = "Direct Peptide Reactivity Assay",
    HTML(
      "Chemical calls from the direct peptide reactivity assay (DPRA)",
      "are used in the 2o3 and KE3/1 STS defined approaches.<br><br>",
      "The column corresponding to DPRA call should only contain the values:<ul>",
      "<li>'p', 'pos', 'positive', or 1 to indicate positive calls (sensitizers)</li>",
      "<li>'n', 'neg', 'negative', or 0 to indicate negative calls (non-sensitizers)</li>",
      "<li>Missing values should be labeled as 'NA'.</li></ul>",
      "<br>Alternatively, the %-Cysteine and %-Lysine values can be evaluated",
      "for call. The columns corresponding to %-Cysteine",
      "and %-Lysine depletion should only contain numeric values. Missing values",
      "should be labeled as 'NA'. If negative values are reported, then co-elution",
      "is assumed and the chemical will not be evaluated. Calls are made using Tables 1 and 2 from",
      "<i>OECD Test No. 442C: In Chemico Skin Sensitisation</i>[<a",
      "href='https://doi.org/10.1787/9789264229709-en'",
      "target = '_blank'>4</a>]</p>"
    ),
    easyClose = T
  ))
})

observeEvent(input$info_hclatcall, {
  showModal(modalDialog(
    title = "Human Cell Line Activation Test",
    HTML(
      "Chemical calls from the human cell line activation test (h-CLAT)",
      "are used in the 2o3 defined approach.<br><br>",
      "The column corresponding to h-CLAT call should only contain the values:<ul>",
      "<li>'p', 'pos', 'positive', or 1 to indicate positive calls (sensitizers)</li>",
      "<li>'n', 'neg', 'negative', or 0 to indicate negative calls (non-sensitizers)</li>",
      "<li>Missing values should be labeled as 'NA'.</li></ul>",
      "For more details, see <i>OECD Test No. 442E: In Vitro Skin",
      "Sensitisation</i>[<a href='https://doi.org/10.1787/9789264264359-en'",
      "target = '_blank'>5</a>]</p>"
    ),
    easyClose = T
  ))
})

observeEvent(input$info_hclatmit, {
  showModal(modalDialog(
    title = "Human Cell Line Activation Test",
    HTML(
      "Minimum induction threshold (MIT) from the human cell line activiation test (h-CLAT)",
      "is used in the ITSv2 and KE3/1 STS defined approaches.<br><br>",
      "The column corresponding to h-CLAT MIT should only contain:<ul>",
      "<li>Numeric Values</li>",
      "<li>'n', 'neg', 'negative', or 'Inf' to indicate negative calls.</li>",
      "<li>Missing values should be labeled as 'NA'.</li></ul>",
      "For more details, see <i>OECD Test No. 442E: In Vitro Skin",
      "Sensitisation</i>[<a href='https://doi.org/10.1787/9789264264359-en'",
      "target = '_blank'>5</a>]</p>"
    ),
    easyClose = T
  ))
})

observeEvent(input$info_kscall, {
  showModal(modalDialog(
    title = HTML("KeratinoSens&trade;"),
    HTML(
      "Chemical calls from the KeratinoSens&trade; (KS) assay",
      "are used in the 2o3 defined approach.<br><br>",
      "The column corresponding to KS call should only contain the values:<ul>",
      "<li>'p', 'pos', 'positive', or 1 to indicate positive calls (sensitizers)</li>",
      "<li>'n', 'neg', 'negative', or 0 to indicate negative calls (non-sensitizers)</li>",
      "<li>Missing values should be labeled as 'NA'.</li></ul>",
      "Alternatively, iMax values can be provided and evaluated for call. The",
      "column corresponding to KS iMax should contain numeric values. Missing",
      "values should be labeled 'NA'. Chemicals with KS iMax values &gt;1.5",
      "are labeled as positive and chemicals with KS iMax values &leq;1.5 are",
      "labeled as negative.<br><br>",
      "For more details, see <i>OECD Test No. 442D: In Vitro",
      "Skin Sensitisation</i>[<a",
      "href='https://doi.org/10.1787/9789264229822-en' target = '_blank'>6</a>]</p>"
    ),
    easyClose = T
  ))
})

observeEvent(input$info_oecdtbcall, {
  showModal(modalDialog(
    title = "OECD QSAR Toolbox",
    HTML(
      "Chemical call predictions from the OECD QSAR Toolbox (TB) are used in the",
      "ITSv2 defined approach.<br><br>",
      "The column corresponding to OECD QSAR TB call should only contain the values:<ul>",
      "<li>'p', 'pos', 'positive', or 1 to indicate positive calls (sensitizers)</li>",
      "<li>'n', 'neg', 'negative', or 0 to indicate negative calls (non-sensitizers)</li>",
      "<li>Missing values should be labeled as 'NA'.</li></ul>",
      "Additionally, a column corresponding to applicability domain (AD) should",
      "be provided. This column should only contain the values:<ul>",
      "<li>'in' or 1 to indicate the chemical is within the AD.</li>",
      "<li>'out' or 0 to indicate the chemical is outside the AD. Values for",
      "chemicals outside the AD will not be evaluated.</li>",
      "<li>Missing values should be labeled 'NA'.</li>",
      "</ul>",
      "For more details, see <i>Automated and standardized workflows in the",
      "OECD QSAR Toolbox</i>[<a href='https://doi.org/10.1016/j.comtox.2019.01.006'",
      "target = '_blank'>7</a>].</p>"
    ),
    easyClose = T
  ))
})

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
      updateSelectInput(inputId = si,
                        choices = c("", sel, col_des_up),
                        selected = sel)
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
      updateSelectInput(inputId = si,
                        choices = c("", sel, col_des_up),
                        selected = sel)
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
      updateSelectInput(inputId = si,
                        choices = c("", sel, col_des_up),
                        selected = sel)
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
      updateSelectInput(inputId = si,
                        choices = c("", sel, col_des_up),
                        selected = sel)
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
      updateSelectInput(inputId = si,
                        choices = c("", sel, col_des_up),
                        selected = sel)
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
      updateSelectInput(inputId = si,
                        choices = c("", sel, col_des_up),
                        selected = sel)
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
      updateSelectInput(inputId = si,
                        choices = c("", sel, col_des_up),
                        selected = sel)
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
      updateSelectInput(inputId = si,
                        choices = c("", sel, col_des_up),
                        selected = sel)
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
      updateSelectInput(inputId = si,
                        choices = c("", sel, col_des_up),
                        selected = sel)
    }
    dt_col_select$oecd_tb_ad <- input$oecd_ad_col
  }
})

# Step 3: Review Selections -----
# Labels for displayed table
end_labels <- list(
  dpra_call = "DPRA Call",
  dpra_pC = "DPRA %C-Depletion",
  dpra_pK = "DPRA %K-Depletion",
  hclat_call = "h-CLAT Call",
  hclat_mit = "h-CLAT MIT",
  ks_call = "KeratinoSens Call",
  ks_imax = "KeratinoSens iMax",
  oecd_tb_call = "OECD QSAR TB Call",
  oecd_tb_ad = "OECD QSAR TB Applicability Domain"
)

# Flags for review
end_flags <- list(
  dpra_call = "Must be '0', 'N', 'Neg', or 'Negative' to indicate negative outcomes and '1', 'P', 'Pos', or 'Positive', to indicate positive outcomes.",
  dpra_pC = "Must be numeric",
  dpra_pK = "Must be numeric",
  hclat_call = "Must be '0', 'N', 'Neg', or 'Negative' to indicate negative outcomes and '1', 'P', 'Pos', or 'Positive', to indicate positive outcomes.",
  hclat_mit = "Must be 'N', 'Neg', 'Negative', or missing value to indicate negative outcomes and numeric for positive outcomes.",
  ks_call = "Must be '0', 'N', 'Neg', or 'Negative' to indicate negative outcomes and '1', 'P', 'Pos', or 'Positive', to indicate positive outcomes.",
  ks_imax = "Must be numeric",
  oecd_tb_call = "Must be '0', 'N', 'Neg', or 'Negative' to indicate negative outcomes and '1', 'P', 'Pos', or 'Positive', to indicate positive outcomes.",
  oecd_tb_ad = "Must be '0' or 'Out' for chemicals outside the applicability domain and '1' or 'In' for chemicals in the applicability domain"
)

observeEvent(input$review_entries, {
  # Get selected columns
  col_summary <- reactiveValuesToList(dt_col_select)
  
  # Check that all variables have a column assigned
  cols_to_check <- check_cols(dass = dass_choice(),
                              ks_call_method = input$ks_choice,
                              dpra_call_method = input$dpra_call_choice)
  cols_to_check <- unlist(col_summary[cols_to_check])
  col_blank <- any(cols_to_check == "")
  if (col_blank) showNotification(type = "error", ui = "Missing required columns.", duration = 10)
  req(!col_blank)

  # List of formatted data
  dt_list <- list()
  
  # Get data from selected columns
  col_dict <- dat_for_anlz$col_dict <- col_summary[names(cols_to_check)]
  col_vec <- unlist(col_dict, use.names = F)
  col_data <- usr_dt()[,.SD,.SDcols = col_vec]
  setnames(col_data, old = col_vec, new = names(col_dict))
  
  col_flags <- vector(mode = "list", length = ncol(col_data))
  names(col_flags) <- names(col_data)

  # Check call columns
  call_cols <- c("ks_call", "dpra_call", "hclat_call", "oecd_tb_call")
  if (any(names(col_data) %in% call_cols)) {
    call_col_names <- names(col_data)[names(col_data) %in% call_cols]
    # Data can be entered as: 
    # Positive: 1, Pos, Positive, P
    # Negative: 0, Neg, Negative, N
    #  Count number of entries per column that have invalid values
    call_check <- col_data[,lapply(.SD, function(x) {
      !(grepl_ci("^1$|^0$|^p$|^n$|^pos$|^neg$|^positive$|^negative$", x)|is.na(x))
    }), .SDcols = call_col_names
    ][,lapply(.SD, sum), .SDcols=call_col_names]
    call_check_id <- which(call_check >0)
    if (length(call_check_id) > 0) {
      col_flags[names(call_check)[call_check_id]] <- 1
    }
    # Replace positive with 1 and negative with 0
    dt_list$call_cols <- col_data[,lapply(.SD, function(x) {
      fcase(
        grepl_ci("^1$|^p$|^pos$|^positive$", x), 1,
        grepl_ci("^0$|^n$|^neg$|^negative$", x), 0
      )
    }), .SDcols = call_col_names]
  }
  
  # Check numeric columns
  num_cols <- c("dpra_pC", "dpra_pK", "ks_imax")
  if (any(names(col_data) %in% num_cols)) {
    num_col_names <- names(col_data)[names(col_data) %in% num_cols]
    # Values must be numeric
    num_check <- col_data[,lapply(.SD, function(x) {
      # Value provided, but it is not numeric
      any(!is.na(x) & is.na(suppressWarnings(as.numeric(x))))
    }), .SDcols = num_col_names]
    num_check <- unlist(num_check)

    num_check_id <- which(num_check)
    if (length(num_check_id) > 0) {
      col_flags[num_col_names[num_check_id]] <- 1
    }

    dt_list$num_cols <- col_data[,lapply(.SD, function(x) suppressWarnings(as.numeric(x))),
                                 .SDcols = num_col_names]
  }

  # Check h-CLAT MIT
  if (any(names(col_data) == "hclat_mit")) {
    mit_check <- col_data[,.(hclat_mit, hclat_mit_num = suppressWarnings(as.numeric(hclat_mit)), flag = T)]
    
    # Can have numeric or 'negative'
    # Change flag to F for numeric or 'negative'
    mit_check[!is.na(hclat_mit_num), flag := F]
    mit_check[is.na(hclat_mit), flag := F]
    mit_check[grepl_ci("^n$|^neg$|^negative$", hclat_mit), flag := F]

    if (any(mit_check[,flag])) {
      col_flags$hclat_mit <- 1
    } 

    mit_check[is.na(hclat_mit_num) & flag, hclat_mit := NA]
    mit_check[,flag := NULL]
    dt_list$mit_col <- mit_check
  }

  # Check applicability domain
  if (any(names(col_data) == "oecd_tb_ad")) {
    ad_check <- col_data[,oecd_tb_ad]
    # Can be 0, 1, in, or out
    ad_check <- !(grepl_ci("^1$|^0$|^in$|^out$", ad_check)|is.na(ad_check))

    if (any(ad_check)) {
      col_flags$oecd_tb_ad <- 1
    }
    
    dt_list$ad_col <- col_data[,.SD,.SDcols = "oecd_tb_ad"
             ][,.(oecd_tb_ad = fcase(grepl_ci("^1$|^in$", oecd_tb_ad), 1,
                      grepl_ci("^0$|^out$", oecd_tb_ad), 0))
             ]
  }
  
  col_flags <- lapply(col_flags, function(x) fifelse(is.null(x), 0, 1))
  names(dt_list) <- NULL
  updateCollapse(session,
                 id = "panels",
                 close = "panel_col_options")
  updateCollapse(session,
                 id = "panels",
                 open = "panel_review")
  dt_anlz <- do.call("cbind", dt_list)
  dat_for_anlz$col_data <- dt_anlz
  dt_review <- data.table(
    Variable = names(col_dict),
    `Selected Column` = unlist(col_dict, use.names = F),
    Flag = unlist(col_flags, use.names = F)
  )

  dt_review[,Flag := fcase(
    Flag == 0, "",
    Flag == 1, unlist(end_flags[Variable])
  )]
  dt_review[,Variable := unlist(end_labels[Variable], use.names = F)]
  flag_row <- which(dt_review$Flag != "")

  dt_review <- datatable(dt_review,
                           class = "table-bordered",
                           rownames = FALSE,
                           escape = FALSE,
                         selection = "none",
                           options = list(dom = "t",
                                          ordering = F))
  
  if(length(flag_row) > 0) {
    dt_review <- formatStyle(dt_review, 0, target = "row", color = styleRow(flag_row, rep("#D55E00", length = length(flag_row))))
    review_label(paste("<p style='color:#D55E00; font-weight:bold;'>Warning:",
                       "Selected columns have been flagged for invalid values.</p>",
                       "<p>Review the selected columns and flags in the table below.",
                       "Upload an updated dataset or select new columns.<br><br>",
                       "Click 'Run' to run DASS anyway. Invalid values will be",
                       "considered missing (NA) and will not be used to evaluate",
                       "skin sensitization hazard or potency.</p><br>"))
    flagged(1)
  } else {
    review_label("<p>Review the selected columns and click 'Run' to run DASS.</p><br>")
    flagged(0)
    }

  dt_review(dt_review)
})

output$dt_review <- renderDataTable({dt_review()})
output$review_label <- renderText({review_label()})

observeEvent(input$run_dass, {
  req(flagged())
  if (flagged() == 1) {
    showModal(modalDialog(
      title = NULL,
      footer = NULL,
      p("The selected columns have been flagged for invalid values. Invalid values",
        "will be considered as 'no data'. Continue?"),
      actionButton(inputId = "run_with_flags", label = "Run"),
      actionButton(inputId = "cancel_run", label = "Cancel")
    ))
  } else if (flagged() == 0) {
    run_dass()
  }
})

observeEvent(input$run_with_flags, {
  run_dass()
  removeModal()
})

observeEvent(input$cancel_run, {
  removeModal()
})

# Step 4: Results -----
run_dass <- reactive({
  da_out <- dass_predict(
    dt = dat_for_anlz$col_data,
    dass = dass_choice(),
    ks_call_method = input$ks_choice,
    dpra_call_method = input$dpra_call_choice
  )
  
  dass_res(cbind(usr_dt(), da_out))
  updateCollapse(session,
                 id = "panels",
                 close = "panel_review")
  updateCollapse(session,
                 id = "panels",
                 open = "panel_results")
  
  output$step4ui <- renderUI({
    downloadButton("downloadres", "Download Results")
  })
})

da_colnames <- c("DA_2o3_Call", "DA_ITSv2_Call", "DA_ITSv2_Potency", "DA_KE31STS_Call", "DA_KE31STS_Potency")

output$dt_results <- renderDataTable({
  col_sty <- da_colnames[da_colnames %in% colnames(dass_res())]
  datatable(
    dass_res(),
    class = "table-bordered",
    rownames = FALSE,
    selection = "none",
    options = list(
      scrollX = TRUE,
      rowCallback = JS(rowCallback)
    )
  ) %>% 
    formatStyle(columns = col_sty,
                backgroundColor = "#DBE8FF",
                fontWeight = "bold")
})

output$downloadres <- downloadHandler(
  filename = function() {
    fname <- unlist(strsplit(input$fpath$name, "[.]"))
    fname <- paste(fname[-length(fname)], collapse = ".")
    paste0(fname, "_DASSResults_", Sys.Date(), ".csv")
  },
  content = function(con) {
    write.csv(x = dass_res(), file = con)
  }
)
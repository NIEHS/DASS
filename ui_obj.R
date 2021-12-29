#=============================================================================#
# File Name: ui.R                                                             #
# Original Creator: ktto                                                      #
# Contact Information: comptox@ils-inc.com                                    #
# Date Created: 2021-12-03                                                    #
# License: MIT                                                                #
# Description: UI script for app                                              #
# - data.table, DT                                                            #
# - shiny shinyBS shinyjs                                                     #
#=============================================================================#

# Data Tab -----
tab_data <- bsCollapsePanel(title = "View Data",
                            dataTableOutput("usr_dt")
                            )

# Step 1: Select Approaches -----
tab_dass_select <- bsCollapsePanel(
  title = "Step 1: Select the Defined Approaches to Apply",
  value = "panel_dass_options",
  p(
    "Select the defined approaches (DA) below. Click on the question",
    "circles for more information about the DA. More details are given",
    "in the user guide. When you are finished, click 'Done'."
  ),
  div(
    style = "margin-left:25px",
    actionLink(inputId = "dass_all",
               label = "Select All"),
    "|",
    actionLink(inputId = "dass_none",
               label = "Deselect All"),
    HTML(
      "<div class='form-group shiny-input-container' style='width:100%;'>",
      "<div class='checkbox'>",
      "<label>",
      "<input id='do_da_2o3' type='checkbox'/>",
      "<span>2 out of 3 (2o3)</span>",
      "</label>",
      "<a id='info_2o3' href='#' class='action-button'>",
      "<i class='glyphicon glyphicon-question-sign' role='presentation' aria-label='question-circle icon'> </i>",
      "</a>",
      
      "</div>",
      "</div>"
    ),
    HTML(
      "<div class='form-group shiny-input-container' style='width:100%;'>",
      "<div class='checkbox'>",
      "<label>",
      "<input id='do_da_itsv2' type='checkbox'/>",
      "<span>Integrated Testing Strategy v.2 (ITSv2)</span>",
      "</label>",
      "<a id='info_itsv2' href='#' class='action-button'>",
      "<i class='glyphicon glyphicon-question-sign' role='presentation' aria-label='question-circle icon'> </i>",
      "</a>",
      "</div>",
      "</div>"
    ),
    HTML(
      "<div class='form-group shiny-input-container' style='width:100%;'>",
      "<div class='checkbox'>",
      "<label>",
      "<input id='do_da_ke31' type='checkbox'/>",
      "<span>Key Event 3/1 (KE 3/1) Sequential Testing Strategy (STS)</span>",
      "</label>",
      "<a id='info_ke31' href='#' class='action-button'>",
      "<i class='glyphicon glyphicon-question-sign' role='presentation' aria-label='question-circle icon'> </i>",
      "</a>",
      "</div>",
      "</div>"
    )
  ),
  actionButton(inputId = "load_cols",
               label = "Done")
)

# Step 2: Select Columns -----
tab_dass_cols <- bsCollapsePanel(
  title = "Step 2: Select Data Columns for Predictions",
  value = "panel_col_options",
  HTML(
    "<p>The endpoint values required for the selected DAs are shown below.",
    "Use the drop down-menus to select the columns from your data",
    "that correspond to the given endpoints. A column must be selected for each",
    "endpoint shown. When you are finished, click 'Done'.<br><br>",
    "Click on the question circles for information about the column",
    "requirements. Values that are incorrectly formatted or invalid",
    "will not be evaluated and may affect the results. More details are given",
    "in the User Guide.</p>"
    ),
  uiOutput("step2ui"),
  actionButton(inputId = "review_entries",
               label = "Done")
)

# Step 3: Review Selection -----
tab_dass_pred <- bsCollapsePanel(
  title = "Step 3: Review Selection",
  value = "panel_review",
  htmlOutput("review_label"),
  dataTableOutput("dt_review"),
  br(),
  actionButton(inputId = "run_dass",
               label = "Run")
)

# Step 4: Run -----
tab_dass_results <- bsCollapsePanel(
  title = "Step 4: Results",
  value = "panel_results",
  p(
    "Results are shown in the table below in highlighted columns",
    "appended to the end of your data. In addition, the values",
    "used for evaluation are appended to your data."
  ),
  uiOutput("step4ui"),
  br(),
  dataTableOutput("dt_results")
)

# References -----
tab_ref <- bsCollapsePanel(
  title = "References",
  value = "panel_ref",
  HTML(
    "<ol>",
    "<li>OECD (2021), Guideline No. 497: Defined Approaches on Skin",
    "Sensitisation, OECD Guidelines for the Testing of Chemicals, Section 4,",
    "OECD Publishing, Paris, https://doi.org/10.1787/b92879a4-en.</li>",
    "<li>OECD. The adverse outcome pathway for skin sensitisation initiated by",
    "covalent binding to proteins. 2014. p. 105.",
    "doi:https://doi.org/https://doi.org/10.1787/9789264221444-en",
    "<li>Environmental Protection Agency (2018), Interim Science Policy:",
    "Use of Alternative Approaches for Skin Sensitization as a Replacement for",
    "Laboratory Animal Testing. Draft for Public Comment, Office of Chemical",
    "Safety and Pollution Prevention, ",
    "https://www.regulations.gov/document/EPA-HQ-OPP-2016-0093-0090</li>",
    "<li>OECD. Test no. 442C: In chemico skin sensitisation. 2021. p. 40.",
    "doi:https://doi.org/https://doi.org/10.1787/9789264229709-en</li>",
    "<li>OECD. Test no. 442E: In vitro skin sensitisation. 2018. p. 65.",
    "doi:https://doi.org/https://doi.org/10.1787/9789264264359-en</li>",
    "<li>OECD. Test no. 442D: In vitro skin sensitisation. 2018. p. 51.",
    "doi:https://doi.org/https://doi.org/10.1787/9789264229822-en</li>",
    "<li>Yordanova D, Schultz TW, Kuseva C, Tankova K, Ivanova H, Dermen I, et al.",
    "Automated and standardized workflows in the OECD QSAR toolbox.",
    "Computational Toxicology. 2019;10: 89–104.",
    "doi:https://doi.org/10.1016/j.comtox.2019.01.006</li>",
    "</ol>"
  )
)

# Whole Page -----
ui_dass <- fluidPage(fluidRow(column(
  width = 10,
  offset = 1,
  wellPanel(style = "padding-left: 50px; padding-right:50px;",
            fluidRow(
              column(
                12,
                HTML(
                  "<h2>Defined Approaches on Skin Sensitization</h2>",
                  "<p>This app applies Defined Approaches on Skin Sensitization (DASS)",
                  "outlined in <i>OECD Guideline No. 497</i>[<a href='https://doi.org/https://doi.org/10.1787/b92879a4-en'", 
                  "target = '_blank'>1</a>]. The defined approaches (DA) are", 
                  "based on key events in the <i>Adverse Outcome Pathway (AOP)", 
                  "for Skin Sensitization Initiated by Covalaent Binding to", 
                  "Proteins</i>[<a href='https://doi.org/https://doi.org/10.1787/9789264221444-en'", 
                  "target = '_blank'>2</a>]. To begin, click 'Browse' below", 
                  "and select your file. Data must be comma-delimited (.csv)", 
                  "or tab-delimited (.txt, .tsv). Your data will be viewable by", 
                  "clicking the Data tab. Follow the steps in each tab.", 
                  "For more details, see the <a target = '_blank' href='user_guide.pdf'>User Guide</a>.",
                  "</p><br><br>"
                )
              ),
              column(6,
                     offset = 3,
                     fileInput(inputId = "fpath",
                               label = NULL,))
            )),
  bsCollapse(
    id = "panels",
    tab_data,
    tab_dass_select,
    tab_dass_cols,
    tab_dass_pred,
    tab_dass_results,
    tab_ref,
    multiple = T
  )
)))
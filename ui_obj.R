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
tab_data <- bsCollapsePanel(
  title = "View Data",
  dataTableOutput("usr_dt")
)

# Step 1: Select Approaches -----
tab_dass_select <- bsCollapsePanel(
  title = "Step 1: Select the Defined Approaches to Apply",
  value = "panel_dass_options",
  p("Select the defined approaches (DA) below. Click on the question",
    "circles for more information about the DA. More details are given",
    "in the user guide. When you are finished, click 'Done'."),
  div(style = "margin-left:25px",
  actionLink(
    inputId = "dass_all",
    label = "Select All"
  ),
  "|",
  actionLink(
    inputId = "dass_none",
    label = "Deselect All"
  ),
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
  )),
  actionButton(
    inputId = "load_cols",
    label = "Done"
  )
)

# Step 2: Select Columns -----
tab_dass_cols <- bsCollapsePanel(
  title = "Step 2: Select Data Columns for Predictions",
  value = "panel_col_options",
  p("The endpoint values required for the selected DAs are shown below.",
    "Use the drop down-menus to select the columns from your data",
    "that correspond to the given endpoints. Click on the question circles",
    "for information about the column requirements. More details are",
    "given in the user guide. When you are finished, click 'Done'."),
  uiOutput("step2ui"),
  actionButton(
    inputId = "review_entries",
    label = "Done"
  )
)

# Step 3: Review Selection -----
tab_dass_pred <- bsCollapsePanel(
  title = "Step 3: Review Selection",
  value = "panel_review",
  htmlOutput("review_label"),
  dataTableOutput("dt_review"),
  br(),
  actionButton(
    inputId = "run_dass",
    label = "Run"
  )
)

# Step 4: Run -----
tab_dass_results <- bsCollapsePanel(
  title = "Step 4: Results",
  value = "panel_results",
  p("Results are shown in the table below in highlighted columns",
    "appended to the end of your data. In addition, the values",
    "used for evaluation are appended to your data."),
  uiOutput("step4ui"),
  br(),
  dataTableOutput("dt_results")
)

# Whole Page -----
ui_dass <- fluidPage(
  fluidRow(
    column(
      width = 10,
      offset = 1,
      wellPanel(
        style = "padding-left: 50px; padding-right:50px;",
        fluidRow(
        column(
        12,
        HTML("<h2>Defined Approaches on Skin Sensitization</h2><p>This app applies Defined Approaches for Skin Sensitization outlined in OECD Guideline No.497. ",
             "To begin, click 'Browse' below and select your file. Data must be comma-delimited (.csv) ",
             "or tab-delimited (.txt, .tsv). Your data will",
             "be viewable by clicking the Data tab. Follow the steps in the",
             "tabs. For more details, see the <a target = '_blank' href='user_guide.pdf'",
             ">User Guide</a>.<br><br></p>"
        )),
        column(
          6,
          offset = 3,
        fileInput(
          inputId = "fpath",
          label = NULL,
        )),
        column(
          12,
          HTML("<h4>Relevant Links</h4>",
               "<ul>",
               "<li><a href='https://doi.org/https://doi.org/10.1787/b92879a4-en'>",
               "OECD Guideline No. 497: Defined Approaches on Skin Sensitisation</a></li>",
               "<li><a href='https://doi.org/10.1787/9789264221444-en'>",
               "The Adverse Outcome Pathway for Skin Sensitisation Initiated by Covalent Binding to Proteins</a></li>",
               "</ul>")
        )
      )),
      bsCollapse(
        id = "panels",
        tab_data,
        tab_dass_select,
        tab_dass_cols,
        tab_dass_pred,
        tab_dass_results, multiple = T
      )
    )
  )
)
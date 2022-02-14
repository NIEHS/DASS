# =============================================================================#
# File Name: ui.R                                                             #
# Original Creator: Kim To                                                    #
# Contact Information: comptox@ils-inc.com                                    #
# Date Created: 2021-12-03                                                    #
# License: MIT                                                                #
# Description: Builds UI                                                      #
# - data.table, DT                                                            #
# - openxlsx                                                                  #
# - readxl                                                                    #
# - shiny shinyBS shinyjs                                                     #
# =============================================================================#

# Edit the bsCollapsePanel function to use h2 tag in title
bsCollapsePanel_h2 <- function(title, ..., value = title, style = NULL) {
  content <- list(...)
  id <- paste0("cpanel", sprintf("%07i", as.integer(stats::runif(
    1,
    1, 1e+06
  ))))
  if (is.null(value)) {
    value <- title
  }
  if (is.null(style)) {
    style <- "default"
  }
  bsTag <- shiny::tags$div(class = paste0(
    "panel panel-",
    style
  ), value = value, shiny::tags$div(
    class = "panel-heading",
    role = "tab", id = paste0("heading_", id),
    #shiny::tags$h4(class = "panel-title", shiny::tags$a(
    shiny::tags$h2(class = "panel-title", shiny::tags$a(
      `data-toggle` = "collapse",
      href = paste0("#", id), title
    ))
  ), shiny::tags$div(
    id = id,
    class = "panel-collapse collapse", role = "tabpanel",
    shiny::tags$div(class = "panel-body", content)
  ))
  htmltools::attachDependencies(bsTag, shinyBSDep)
}

environment(bsCollapsePanel_h2) <- asNamespace("shinyBS")

# Create page
ui_dass <- fluidPage(fluidRow(column(
  width = 10,
  offset = 1,
  # Welcome Panel -----
  wellPanel(
    style = "padding-left: 50px; padding-right:50px; margin-top:20px",
    fluidRow(
      column(
        12,
        ### for debugging
        ### use $('#browser').show(); to launch
        actionButton("browser", "browser"),
        # tags$script("$('#browser').hide();"),
        ###
        HTML(
          "<h1>Defined Approaches on Skin Sensitization</h1>",
          "<p>",
          "This app applies Defined Approaches on Skin Sensitization (DASS)",
          "outlined in <em>OECD Guideline No. 497</em>[<a href='https://doi.org/https://doi.org/10.1787/b92879a4-en'",
          "target = '_blank'>1</a>]. The defined approaches (DA) are",
          "based on key events in the <em>Adverse Outcome Pathway (AOP)",
          "for Skin Sensitization Initiated by Covalaent Binding to",
          "Proteins</em>[<a href='https://doi.org/https://doi.org/10.1787/9789264221444-en'",
          "target = '_blank'>2</a>]. To begin, click 'Browse' below",
          "and select your file. Data must be comma-delimited (.csv),",
          "tab-delimited (.txt, .tsv) or in the first worksheet of an",
          "excel file (.xls, .xlsx). Your data will be viewable by",
          "clicking the View Data tab. Follow the steps in each tab.",
          "For more details, see the <a target = '_blank' href='user_guide.pdf'>User Guide</a>.",
          "</p>",
          "<br><br>"
        )
      ),
      column(6,
             offset = 3,
             fileInput(
               inputId = "fpath",
               label = NULL,
             )
      )
    )
  ),
  # Set up collapsible panels
  bsCollapse(
    id = "panels",
    # Data Tab -----
    bsCollapsePanel_h2(
      title = "View Data",
      dataTableOutput("usr_dt")
    ),
    # Step 1: Select Approaches -----
    bsCollapsePanel_h2(
      title = "Step 1: Select the Defined Approaches to Apply",
      value = "panel_dass_options",
      p(
        "Select the defined approaches (DA) below and click 'Done'. Click on the question",
        "circles for more information about the DA."
      ),
      div(
        style = "margin-left:25px",
        # Buttons for select/deselect all
        actionLink(
          inputId = "dass_all",
          label = "Select All"
        ),
        "|",
        actionLink(
          inputId = "dass_none",
          label = "Deselect All"
        ),
        # Creates checkbox widgets (checkboxInput()), written in HTML 
        # to allow action button in label
        HTML(
          "<div class='form-group shiny-input-container' style='width:100%;'>",
          "<div class='checkbox'>",
          "<label>",
          "<input id='do_da_2o3' type='checkbox'/>",
          "<span>2 out of 3 (2o3)</span>",
          "</label>",
          "<button id='info_2o3' type='button' class='btn btn-default action-button btn-qs'>",
          "<i class='glyphicon glyphicon-question-sign' role='presentation' aria-label='question-circle icon'> </i>",
          "</button>",
          "</div>",
          "</div>",
          "<div class='form-group shiny-input-container' style='width:100%;'>",
          "<div class='checkbox'>",
          "<label>",
          "<input id='do_da_itsv2' type='checkbox'/>",
          "<span>Integrated Testing Strategy (ITS)</span>",
          "</label>",
          "<button id='info_itsv2' type='button' class='btn btn-default action-button btn-qs'>",
          "<i class='glyphicon glyphicon-question-sign' role='presentation' aria-label='question-circle icon'> </i>",
          "</button>",
          "</div>",
          "</div>",
          "<div class='form-group shiny-input-container' style='width:100%;'>",
          "<div class='checkbox'>",
          "<label>",
          "<input id='do_da_ke31' type='checkbox'/>",
          "<span>Key Event 3/1 (KE 3/1) Sequential Testing Strategy (STS)</span>",
          "</label>",
          "<button id='info_ke31' type='button' class='btn btn-default action-button btn-qs'>",
          "<i class='glyphicon glyphicon-question-sign' role='presentation' aria-label='question-circle icon'> </i>",
          "</a>",
          "</div>",
          "</div>"
        )
      ),
      actionButton(
        inputId = "load_cols",
        label = "Done"
      )
    ),
    # Step 2: Select Columns -----
    bsCollapsePanel_h2(
      title = "Step 2: Select Data Columns for Predictions",
      value = "panel_col_options",
      uiOutput("step2ui")
    ),
    # Step 3: Review Selection -----
    bsCollapsePanel_h2(
      title = "Step 3: Review Selection",
      value = "panel_review",
      div(
        id = "review_contents",
        style = "display:none;",
        htmlOutput("review_label"),
        dataTableOutput("dt_review"),
        br(),
        actionButton(inputId = "run_dass",
                     label = "Run") 
      )
    ),
    bsCollapsePanel_h2(
      title = "Step 4: Results",
      value = "panel_results",
      div(id = "result_contents",
          style = "display:none;",
          HTML(
            "<p>",
            "The DASS hazard predictions are shown in the table below in",
            "<span style='background-color: #56B4E9;'>blue</span>",
            "highlighted columns appended to the end of your data. The columns in your data",
            "that were selected in Step 2 are highlighted in",
            "<span style='background-color: #F0E442;'>yellow.</span>",
            "The selected data columns are reformatted for use in the DAs. The reformatted",
            "colulmns are appended to your data and highlighted in",
            "<span style='background-color: #CC79A7;'>pink.</span>",
            "These are the actual values used for evaluation. It may be useful to",
            "review the selected columns and their transformations to ensure",
            "your data were properly interpreted, especially if the DAs were",
            "run with flagged data.<br>",
            "For more details about the appended columns, see the User Guide.",
            "</p>"
          ),
          downloadButton("downloadres", "Download Results"),
          br(),
          dataTableOutput("dt_results"))
    ),
    # References -----
    bsCollapsePanel_h2(
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
        "Computational Toxicology. 2019;10: 89-104.",
        "doi:https://doi.org/10.1016/j.comtox.2019.01.006</li>",
        "</ol>"
      )
    ),
    multiple = T
  )
)))

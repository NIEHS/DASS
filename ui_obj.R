# =============================================================================#
# File Name: ui.R
# Original Creator: Kim To
# Contact Information: comptox@ils-inc.com
# Date Created: 2021-12-03
# License: MIT
# Description: Builds UI
# - data.table, DT
# - htmltools
# - shiny shinyBS shinyjs
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
# Welcome -----

welcome_panel <- div(
  class = "panel panel-default",
  div(class = "panel-heading", "Welcome to the DASS App!"),
  div(class = "panel-body",
      p(
        "The DASS App predicts skin sensitization hazard and potency by applying",
        "Defined Approaches on Skin Sensitisation (DASS) that are outlined in",
        a(
          href="https://doi.org/https://doi.org/10.1787/b92879a4-en",
          target = "_blank",
          tags$em("OECD Guideline No. 497")
        ), ". and the U.S. EPA's",
        a(
          href="https://www.regulations.gov/document/EPA-HQ-OPP-2016-0093-0090",
          target="_blank",
          tags$em(
            "Interim Science Policy: Use of Alternative Approaches for Skin",
            "Sensitization as a Replacement for Laboratory Animal Testing."
          )
        ), ". The defined approaches (DA) integrate data from in vitro assays and",
        "in silico predictions that represent key events in the",
        a(
          href="https://doi.org/https://doi.org/10.1787/9789264221444-en",
          target="_blank",
          tags$em(
            "Adverse Outcome Pathway (AOP) for Skin Sensitization Initiated",
            "by Covalaent Binding to Proteins")
        )
      ),
      p(
        "More details are available in the",
        a(
          href="user_guide.pdf",
          target = "_blank",
          "User Guide")
      ))
  )

# Step 1: Select DAs -----
selectda_panel <-  bsCollapsePanel_h2(
  title = "Step 1: Select the Defined Approaches to Apply",
  value = "panel_dass_options",
  p(
    "To begin, select the DAs to be implemented. Click on the question",
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
      "<input id='do_da_2o3' type='checkbox' checked='checked'/>",
      "<span>2 out of 3 (2o3)</span>",
      "</label>",
      "<button id='info_2o3' type='button' class='btn action-button btn-qs' title='hello'>",
      "<i class='glyphicon glyphicon-question-sign' role='presentation' aria-label='question-circle icon'> </i>",
      "</button>",
      "</div>",
      "</div>",
      "<div class='form-group shiny-input-container' style='width:100%;'>",
      "<div class='checkbox'>",
      "<label>",
      "<input id='do_da_its' type='checkbox' checked='checked'/>",
      "<span>Integrated Testing Strategy (ITS)</span>",
      "</label>",
      "<button id='info_its' type='button' class='btn action-button btn-qs'>",
      "<i class='glyphicon glyphicon-question-sign' role='presentation' aria-label='question-circle icon'> </i>",
      "</button>",
      "</div>",
      "</div>",
      "<div class='form-group shiny-input-container' style='width:100%;'>",
      "<div class='checkbox'>",
      "<label>",
      "<input id='do_da_ke31' type='checkbox' checked='checked'/>",
      "<span>Key Event 3/1 (KE 3/1) Sequential Testing Strategy (STS)</span>",
      "</label>",
      "<button id='info_ke31' type='button' class='btn action-button btn-qs'>",
      "<i class='glyphicon glyphicon-question-sign' role='presentation' aria-label='question-circle icon'> </i>",
      "</a>",
      "</div>",
      "</div>"
    )
  )
)

# Step 2: Upload Data -----
uploaddata_panel <- bsCollapsePanel_h2(
  title = "Step 2: Upload Data",
  value = "panel_data_upload",
  HTML(
    "<div class='warn-block'>",
    "<div>",
    "<i class='fa fa-exclamation-circle' role='presentation' aria-label='exclamation-circle-icon'></i>",
    "</div>",
    "<div>",
    "<p style='margin-bottom:0;'>Before uploading your file, ensure that the data meet the",
       "<a id='show_upload_req' href = '#' class = 'action-button'>",
       "<b>data and formatting requirements</b></a>.</p>",
    "</div>",
    "</div>"),
  br(),
  HTML("<p>A table template is provided in tab-delimited or Excel format.",
       "The template contains columns for every possible assay endpoint.",
       "If an assay endpoint will not be used, the corresponding column can be",
       "deleted but that is not required. Using the template is not required.",
       "</p>"),
  a(href="DASSApp-dataTemplate.xlsx", "Download Data Template (.xlsx)",
    download = NA, target = "_blank"),
  br(),
  a(href="DASSApp-dataTemplate.txt", "Download Data Template (.txt)",
    download = NA, target = "_blank"),
  hr(style = "width:50%"),
  p("Click 'Browse' below and select your file. If uploading an Excel file, use the dropdown menu to select the worksheet to use. Click 'Upload' to load your data."),
  div(
    class="form-group shiny-input-container",
    style = "width:100%",
    tags$label(
      class="control-label shiny-label-null",
      `for`="fpath",
      id="fpath-label"
    ),
    div(
      class = "input-group",
      tags$label(
        class="input-group-btn input-group-prepend",
        span(
          class="btn btn-browse btn-file",
          "Browse...",
          tags$input(
            id = "fpath",
            name = "fpath",
            type = "file",
            style="position: absolute !important; top: -99999px !important; left: -99999px !important;",
            `data-shinyjs-resettable-id`="fpath",
            `data-shinyjs-resettable-type`="File",
            `data-shinyjs-resettable-value`="",
            `class`="shinyjs-resettable shiny-bound-input"
          )
        )
      ),
      tags$input(
        type = "text",
        class = "form-control",
        style = "border-color:#232b5f; width:100%",
        placeholder = "No file selected.",
        readonly="readonly"
      ),
      tags$label(
        class="input-group-btn input-group-append",
        style = "display: none; width:25vw;",
        id = "xlsheet-label",
        tags$select(
          id = "xlsheet",
          class = "form-control",
          style = "border-color:#232b5f;"
        )
      ),
      tags$label(
        class="input-group-btn input-group-append",
        actionButton(
          inputId = "button_upload", 
          label = "Upload")
      )
    )
  ),
  div(
    id = "user_data_block",
    style = "display:none;", 
    hr(style = "width:50%"),
    dataTableOutput("usr_dt"),
    hr(style = "width:50%"),
    div(
      id = "user_data_block_confirm",
      p("Once you have finished selecting the DAs and uploading your data, click 'Continue' to proceed to the next step."),
      actionButton(inputId = "confirm_data",
                   label = "Continue",
                   width = "100%")),
    div(
      id = "user_data_block_reload",
      style = "display:none",
      p("To change the selected DAs or uploaded data file, click 'Reload App.'"),
      actionButton(
        inputId = "reload_button",
        label = "Reload App",
        width = "100%"
      )
    )
  )
  )

# Step 3: Select Columns -----
selectcolumns_panel <- bsCollapsePanel_h2(
  title = "Step 3: Select Data Columns for Predictions",
  value = "panel_col_options",
  uiOutput("selectcol_ui")
)

# Step 4: Review Selection -----
reviewselection_panel <- bsCollapsePanel_h2(
  title = "Step 4: Review Selection",
  value = "panel_review",
  div(
    id = "review_contents",
    style = "display:none;",
    htmlOutput("dupe_label"),
    htmlOutput("review_label"),
    dataTableOutput("dt_review"),
    br(),
    actionButton(
      inputId = "run_dass",
      width = "100%",
      label = "Run"
    )
  )
)

# Step 5: Results -----
results_panel <- bsCollapsePanel_h2(
  title = "Step 5: Results",
  value = "panel_results",
  div(
    id = "result_contents",
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
    div(
      class = "dropdown",
      tags$button(
        class = "dropbtn",
        style = "padding:1vh;",
        "Download Results",
        icon("caret-down")
      ),
      div(
        class = "dropdown-content",
        downloadButton(outputId = "downloadres_xl", "Excel (.xlsx)", icon = icon("file-excel"), class = "btn-dl"),
        downloadButton("downloadres_txt", "Tab-Delimited (.txt)", icon = icon("file-alt"), class = "btn-dl"),
      )
    ),
    br(),
    fluidRow(column(12, align = "center",
    dataTableOutput("dt_results")))
  )
)

# Modals -----
datareq_modal <- bsModal(
  id = "data_req_modal",
  title = "DASS App Data Requirements", 
  size = "large",
  trigger = "show_upload_req",
  h4("General"),
  tags$ol(
    tags$li(
      "Data can be comma-delimited (.csv), tab-delimited (.tsv, .txt),",
      "or a Microsoft Excel workbook (.xls, .xlsx)."
    ),
    tags$li(
      "Data should be in a tabular format with each row corresponding to",
      "a single substance and a column for each required assay endpoint.",
      tags$ul(
        tags$li("The first row should contain column names.")
      ),
    ),
    tags$li("Missing values should be indicated by a blank cell or as 'NA' (without quotes).")
  ),
  h4("Assay Endpoints"),
  p("Each assay endpoint that is required for implementing the DAs should have",
    "a column that is formatted according to the formatting requirements shown in",
    "the table. Values that do not meet the assay endpoint requirements will",
    "be treated as missing data and will not be used to derive predictions."),
  dataTableOutput("ae_req")
)

# Build page -----
ui_dass <- fluidPage(
  fluidRow(
    column(
      width = 10,
      offset = 1,
      ## for debugging. uncomment observer in step2
      # actionButton("browser", "browser"),
      ##
      welcome_panel,
      ###
      bsCollapse(
        id = "panels",
        open = c("panel_dass_options", "panel_data_upload"),
        multiple = TRUE,
        selectda_panel,
        uploaddata_panel,
        selectcolumns_panel,
        reviewselection_panel,
        results_panel)
      ),
    datareq_modal
))
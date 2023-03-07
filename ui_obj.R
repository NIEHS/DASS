# =============================================================================#
# File Name: ui.R
# Original Creator: Kim To
# Contact Information: ICE-support@niehs.nih.gov
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
  div(class = "panel-heading", "Welcome to the DASS App (Beta)!"),
  div(class = "panel-body",
      p(
        "The DASS App predicts skin sensitization hazard (sensitizer or non-sensitizer) and potency by applying",
        "Defined Approaches on Skin Sensitisation (DASS) that are described in",
        a(
          href="https://doi.org/https://doi.org/10.1787/b92879a4-en",
          target = "_blank",
          class = "external-link",
          "OECD Guideline No. 497",
        ), " and the U.S. EPA's",
        a(
          href="https://www.regulations.gov/document/EPA-HQ-OPP-2016-0093-0090",
          target="_blank",
          class = "external-link",
          "Interim Science Policy: Use of Alternative Approaches for Skin",
          "Sensitization as a Replacement for Laboratory Animal Testing"
        ), 
        ". The defined approaches (DAs) integrate data from in vitro assays",
        "that represent key events in the",
        a(
          href="https://doi.org/https://doi.org/10.1787/9789264221444-en",
          target="_blank",
          class = "external-link",
          "Adverse Outcome Pathway (AOP) for Skin Sensitisation Initiated",
          "by Covalent Binding to Proteins"
        ), 
        "and in silico hazard predictions."
      ),
      p(
        "More details are available in the",
        a(
          href="user_guide.pdf",
          target = "_blank",
          class = "external-link",
          "User Guide"), "."
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
      "<button id='info_2o3' type='button' class='btn action-link btn-qs' aria-label='2o3 info'>",
      "<i class='glyphicon glyphicon-question-sign' role='presentation'> </i>",
      "</button>",
      "</div>",
      "</div>",
      "<div class='form-group shiny-input-container' style='width:100%;'>",
      "<div class='checkbox'>",
      "<label>",
      "<input id='do_da_its' type='checkbox' checked='checked'/>",
      "<span>Integrated Testing Strategy (ITS)</span>",
      "</label>",
      "<button id='info_its' type='button' class='btn action-button btn-qs' aria-label='ITS info'>",
      "<i class='glyphicon glyphicon-question-sign' role='presentation'> </i>",
      "</button>",
      "</div>",
      "</div>",
      "<div class='form-group shiny-input-container' style='width:100%;'>",
      "<div class='checkbox'>",
      "<label>",
      "<input id='do_da_ke31' type='checkbox' checked='checked'/>",
      "<span>Key Event 3/1 (KE 3/1) Sequential Testing Strategy (STS)</span>",
      "</label>",
      "<button id='info_ke31' type='button' class='btn action-button btn-qs' aria-label='STS info'>",
      "<i class='glyphicon glyphicon-question-sign' role='presentation'> </i>",
      "</a>",
      "</div>",
      "</div>"
    )
  )
)

## Modals -----
info2o3_modal <- bsModal(
  id = "info_2o3_modal",
  title = "2 out of 3",
  trigger = "info_2o3",
  p(
    "The 2 out of 3 (2o3) DA predicts skin sensitization hazard based on at",
    "least 2 concordant results among the direct peptide reactivity assay (DPRA),",
    "KeratinoSens\u2122, and human cell line activation test (h-CLAT)."
  ),
  img(
    class = "da-diagram",
    src="diagrams/2o3_diagram-v1.png", 
    alt = "Diagram of 2o3 data interpretation procedure"),
  HTML(
    "<p><a href = 'diagrams/2o3_diagram-v1.png' target='blank' style='font-size:90%;' class = 'external-link'>",
    "View full-size image",
    "</a></p>"
  ), 
  p(
    "For more details, see",
    a(href="https://doi.org/https://doi.org/10.1787/b92879a4-en",
      target = "_blank",
      class = "external-link",
      "OECD Guideline No. 497: Defined Approaches on Skin Sensitisation"
    ), "."
  )
)

infoits_modal <- bsModal(
  id = "info_its_modal",
  title = "Integrated Testing Strategy",
  trigger = "info_its",
  p(
    "The Integrated Testing Strategy (ITS) DA predicts skin sensitization",
    "hazard and GHS potency category by scoring the mean percent depletion",
    "for both Cysteine and Lysine from the the direct peptide reactivity assay (DPRA),",
    "the minimum induction threshold from the human cell-line activation test (h-CLAT), and",
    "in silico predictions from either",
    a(
      href = "https://www.lhasalimited.org/products/skin-sensitisation-assessment-using-derek-nexus.htm",
      target = "_blank",
      class = 'external-link',
      "Derek Nexus"
    ),
    "or the",
    a(
      href = "https://doi.org/10.1016/j.comtox.2019.01.006",
      target = "_blank",
      class = 'external-link',
      "OECD QSAR Toolbox"
    ), "."
  ),
  img(
    class = "da-diagram",
    src = "diagrams/ITS_diagram-v1.png",
    alt = "Diagram of ITS data interpretation procedure"
  ),
  HTML(
    "<p><a href = 'diagrams/ITS_diagram-v1.png' target='blank' style='font-size:90%;' class = 'external-link'>",
    "View full-size image",
    "</a></p>"
  ), 
  p(
    "For more details, see",
    a(
      href = "https://doi.org/https://doi.org/10.1787/b92879a4-en",
      target = "_blank",
      class = 'external-link',
      "OECD Guideline No. 497: Defined Approaches on Skin Sensitisation"
    ), "."
  )
)

infoke31_modal <- bsModal(
  id = "info_ke31_modal",
  title = "Key Event 3/1 (KE 3/1) Sequential Testing Strategy (STS)",
  trigger = "info_ke31",
  p(
    "The Key Event 3/1 Sequential Testing Strategy is a sequential testing strategy",
    "that predicts skin sensitization hazard and GHS potency category.",
    "Predictions are based on the minimum induction threshold from the human cell line activation test (h-CLAT) and",
    "hazard results from the direct peptide reactivity assay (DPRA).",
  ),
  img(
    class = "da-diagram",
    src="diagrams/KE31STS_diagram-v1.png", 
    alt = "Diagram of KE 3/1 STS data interpretation procedure"),
  HTML(
    "<p><a href = 'diagrams/KE31STS_diagram-v1.png' target='blank' style='font-size:90%;' class = 'external-link'>",
    "View full-size image",
    "</a></p>"
  ), 
  p(
    "For more details, see EPA's",
    a(
      href="https://www.regulations.gov/document/EPA-HQ-OPP-2016-0093-0090",
      target = "_blank",
      class = 'external-link',
      "Interim Science Policy: Use of Alternative Approaches for Skin Sensitization",
      "as a Replacement for Laboratory Animal Testing Draft for Public Comment"
    ), "."
  )
)

# Step 2: Upload Data -----
uploaddata_panel <- bsCollapsePanel_h2(
  title = "Step 2: Upload Data",
  value = "panel_data_upload",
  HTML(
    "<div class='warn-block'>",
    "<div>",
    "<i class='glyphicon glyphicon-exclamation-sign' role='presentation'></i>",
    "</div>",
    "<div>",
    "<p style='margin-bottom:0;'>Before uploading your file, ensure that the data meet the",
       "<a id='show_upload_req' href = '#' class = 'action-link' aria-label='data and formatting requirements'>",
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
  p("Click 'Browse' below and select your file."),
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
          class="btn btn-default btn-file",
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
      )
    )
  ),
  uiOutput("xlsheet_text_ui"),
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

## Modals -----
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
      "a single substance and a column for each required assay endpoint."),
    tags$li("The first row should contain column names. Column names must be unique."),
    tags$li("Missing values should be indicated by a blank cell or as 'NA' (without quotes).")
  ),
  h4("Assay Endpoints"),
  p("Each assay endpoint that is required for implementing the DAs should have",
    "a column that is formatted according to the formatting requirements shown in",
    "the table. Values that do not meet the assay endpoint requirements will",
    "be treated as missing data and will not be used to derive predictions."),
  dataTableOutput("ae_req")
)

xl_sheet_modal <- bsModal(
  id = "xl_select_modal",
  title = "Excel sheet selection dialog box",
  trigger = "select_sheet",
  selectInput(
    inputId = "xl_sheet_list",
    label = "Select the Excel worksheet to upload", 
    choices = NULL,
    selectize = FALSE
  ),
  actionButton(inputId = "confirm_xl_sheet", label = "Upload Data"),
  actionButton(inputId = "cancel_xl_sheet", label = "Cancel")
)

# Step 3: Select Columns -----
selectcolumns_panel <- bsCollapsePanel_h2(
  title = "Step 3: Select Data Columns for Predictions",
  value = "panel_col_options",
  uiOutput("selectcol_ui")
)

## Modals -----
### DPRA -----
info_dpra_dep_modal <- bsModal(
  id = "dpra_dep_modal",
  title = "DPRA % Depletion",
  trigger = "info_dpradep",
  p("%-Cysteine and %-Lysine depletion values from the direct peptide reactivity",
    "assay (DPRA) are used in ITS."),
  p("The columns corresponding to %-Cysteine and %-Lysine depletion should only",
    "contain numeric values. Missing values should be blank or labeled as 'NA'."),
  p("For more details, see",
    a(
      href='https://doi.org/10.1787/9789264229709-en',
      target = "_blank",
      class = "external-link",
      "OECD Test No. 442C: In Chemico Skin Sensitisation"
    ), ".")
)

info_dpra_call_modal <- bsModal(
  id = "dpra_call_modal",
  title = "DPRA Binary Call",
  trigger = "info_dpra_call_1",
  p(
    "Results from the direct peptide reactivity assay (DPRA)",
    "are used in the 2o3 and KE3/1 STS defined approaches."
  ),
  p(
    "The column corresponding to DPRA Binary Call should only contain the values:",
    tags$ul(
      tags$li(
        "'sensitizer', 'sensitiser', 'a', 'active', 'p', 'pos', 'positive', or 1 to indicate positive assay outcomes.*"
      ),
      tags$li(
        "'non-sensitizer', 'non-sensitiser', 'i', 'inactive', 'n', 'neg', 'negative', or 0 to indicate negative assay outcomes.*"
      ),
      tags$li(
        "Missing values should be blank or labeled as 'NA'."
      )
    ),
    span(
      style = "font-size:90%",
      "* Case insensitive"
    )
  ),
  p(
    "Alternatively, the %-Cysteine and %-Lysine depletion values can be evaluated",
    "to derive the binary call. Binary calls are made using Tables 1 and 2 from",
    a(
      href = 'https://doi.org/10.1787/9789264229709-en',
      target = "_blank",
      class = "external-link",
      "OECD Test No. 442C: In Chemico Skin Sensitisation"
    ), "."
  )
)

### hCLAT -----
info_hclat_call_modal <- bsModal(
  id = "hclat_call_modal",
  title = "h-CLAT Binary Call",
  trigger = "info_dpradep",
  p("Results from the human cell line activation test (h-CLAT) are used in the 2o3 defined approach."),
  p("The column corresponding to h-CLAT binary call should only contain the values:",
    tags$ul(
      tags$li(
        "'sensitizer', 'sensitiser', 'a', 'active', 'p', 'pos', 'positive', or 1 to indicate positive assay outcomes.*"
      ),
      tags$li(
        "'non-sensitizer', 'non-sensitiser', 'i', 'inactive', 'n', 'neg', 'negative', or 0 to indicate negative assay outcomes.*"
      ),
      tags$li(
        "Missing values should be blank or labeled as 'NA'."
      )
    )
    ),
  span(
    style = "font-size:90%",
    "* Case insensitive"
  ),
  p("For more details, see ",
    a(
      href='https://doi.org/10.1787/9789264264359-en',
      target = '_blank',
      class = "external-link",
      "OECD Test No. 442E: In Vitro Skin Sensitisation"
    ), ".")
)

info_hclat_mit_modal <- bsModal(
  id = "hclat_mit_modal",
  title = "h-CLAT MIT",
  trigger = "info_hclatmit",
  p("Minimum induction threshold (MIT) from the human cell line activiation test (h-CLAT)",
    "is used in the ITS and KE3/1 STS defined approaches."),
  p("The column corresponding to h-CLAT MIT should only contain:",
    tags$ul(
      tags$li(
        "Numeric values."
      ),
      tags$li(
        "'non-sensitizer', 'non-sensitiser', 'i', 'inactive', 'n', 'neg', 'negative', or 'Inf' to indicate negative assay outcomes*"
      ),
      tags$li(
        "Missing values should be blank or labeled as 'NA'."
      )
    ),
    span(
      style = "font-size:90%",
      "* Case insensitive"
    )),
  p(
    "For more details, see",
    a(
      href='https://doi.org/10.1787/9789264264359-en',
      target = "_blank",
      class = "external-link",
      "OECD Test No. 442E: In Vitro Skin Sensitisation"
    )
  )
)

### KeratinoSens -----
info_ks_call_modal <- bsModal(
  id = "ks_call_modal",
  title = HTML("KeratinoSens&trade; Binary Call"),
  trigger = "info_kscall",
  p("Results from the KeratinoSens (KS) assay are used in the 2o3 defined approach."),
  p("The column corresponding to KS hazard should only contain the values:",
    tags$ul(
      tags$li(
        "'sensitizer', 'sensitiser', 'a', 'active', 'p', 'pos', 'positive', or 1 to indicate positive assay outcomes.*"
      ),
      tags$li(
        "'non-sensitizer', 'non-sensitiser', 'i', 'inactive', 'n', 'neg', 'negative', or 0 to indicate negative assay outcomes.*"
      ),
      tags$li(
        "Missing values should be blank or labeled as 'NA'."
      )
    ),
    span(
      style = "font-size:90%",
      "* Case insensitive"
    )),
  p("For more details, see",
    a(
      href='https://doi.org/10.1787/9789264229822-en',
      target = '_blank',
      class = "external-link",
      "OECD Test No. 442D: In Vitro Skin Sensitisation"
    ), ".")
)

### In Silico -----
info_insilico_modal <- bsModal(
  id = "insilico_modal",
  title = "In Silico Binary Call Prediction and Applicability Domain",
  trigger = "info_insilico_call",
  p(
    "The ITS defined approach uses in silico predictions of binary call. ITSv1 uses predictions from",
    a(
      href = "https://www.lhasalimited.org/products/skin-sensitisation-assessment-using-derek-nexus.htm",
      target = "_blank",
      class = "external-link",
      "Derek Nexus"
    ), ". ITSv2 uses predictions from",
    a(
      href='https://doi.org/10.1016/j.comtox.2019.01.006',
      target = '_blank',
      class = "external-link",
      "OECD QSAR Toolbox"
    ), "."),
  p(
    "The column corresponding to in silico binary call predictions should only contain the values",
    tags$ul(
      tags$li(
        "'sensitizer', 'sensitiser', 'a', 'active', 'p', 'pos', 'positive', or 1 to indicate positive assay outcomes.*"
      ),
      tags$li(
        "'non-sensitizer', 'non-sensitiser', 'i', 'inactive', 'n', 'neg', 'negative', or 0 to indicate negative assay outcomes.*"
      ),
      tags$li(
        "Missing values should be blank or labeled as 'NA'."
      )
    ),
    span(
      style = "font-size:90%",
      "* Case insensitive"
    )
  ),
  p(
    "Additionally, a column corresponding to applicability domain (AD) should", 
    "be provided. This column should only contain the values:",
    tags$ul(
      tags$li(
        "'in' or 1 to indicate the chemical is within the AD*"
      ),
      tags$li(
        "'out' or 0 to indicate the chemical is outside the AD*. Values for",
        "chemicals outside the AD will not be evaluated."
      ),
      tags$li(
        "Missing values should be blank or labeled as 'NA'."
      )
    ),
    span(
      style = "font-size:90%",
      "* Case insensitive"
    )
  )
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
    # style = "display:none;",
    p(
      "Results of the DASS App analysis are shown in the table below. Use the",
      "scroll bar along the bottom of the table to view all the columns.",
      "Use the 'Download Results' button to export your results to an Excel",
      "spreadsheet or text file, which may allow easier viewing.",
    ),
    div(
      class = "key-block",
      h1("Table Key", style = "font-size:1em; font-weight:bold; margin:10px"),
      tags$ul(
        tags$li(
          tags$span(
            style="background-color: #F0E442;",
            "Yellow columns"
          ),
          "- The data columns that you selected in Step 3. The column names are annotated with an asterisk."
        ),
        tags$li(
          tags$span(
            style="background-color: #CC79A7;",
            "Pink columns"
          ),
          "- Appended columns showing how your data inputs were interpreted.",
          "The corresponding column names begin with ‘Input’. For values that",
          "were calculated by the app, the column name will also end with 'Calculated'.",
          actionLink(inputId = "info_pink", class = "btn-qs", label = NULL, icon = icon("question-sign", lib = "glyphicon"))
        ),
        tags$li(
          tags$span(
            style="background-color: #56B4E9;",
            "Blue columns"
          ),
          "Appended columns with the DASS predictions. The corresponding column",
          "names begin with ‘DA’ and the name of the DA.",
          actionLink(inputId = "info_blue", class = "btn-qs", label = NULL, icon = icon("question-sign", lib = "glyphicon"))
        )
      ),
      "For more details about the appended columns, see the User Guide.",
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

## Modals -----
info_pink_modal <- bsModal(
  id = "res_pink_modal",
  title = "Input and Calculated Columns",
  trigger = "info_pink",
  p("These are the actual values used for evaluation. It may be useful to",
    "review the selected columns and their transformations to ensure your data",
    "were properly interpreted, especially if the DAs were run with flagged data."),
  p("Binary call data are transformed to ‘0’ for negative results and ‘1’ for positive results."),
  p("If the h-CLAT minimum induction threshold was used, negative results are transformed to ‘Inf’.")
)

info_blue_modal <- bsModal(
  id = "res_blue_modal",
  title = "Input and Calculated Columns",
  trigger = "info_blue",
  p("If the ITS DA was selected, then the individual ITS scores are also appended",
    "and highlighted in blue. The corresponding columns end with ‘Score’."),
  p("Hazard call predictions are ‘0’ if the result is negative and ‘1’ if the result is positive."),
  p("For assigning potency predictions, the DASS App uses categories established",
    "by the United Nations Globally Harmonized System for Classification and Labelling of Chemicals (GHS).")
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
    info2o3_modal,
    infoits_modal,
    infoke31_modal,
    datareq_modal,
    xl_sheet_modal,
    info_dpra_dep_modal,
    info_dpra_call_modal,
    info_hclat_call_modal,
    info_hclat_mit_modal,
    info_ks_call_modal,
    info_insilico_modal,
    info_pink_modal,
    info_blue_modal
))
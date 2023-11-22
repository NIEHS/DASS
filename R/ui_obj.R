# =============================================================================#
# File Name: ui.R
# Original Creator: Kim To
# Contact Information: ICE-support@niehs.nih.gov
# Date Created: 2021-12-03
# License: MIT
# Description: Builds UI
# Required Packages:
# - data.table, DT
# - htmltools
# - shiny shinyBS shinyjs
# =============================================================================#

source("R/modifyCollapses.R")

# Create page
# Welcome -----
welcome_panel <- fluidRow(
  class = "header-row",
  div(
    class = "header-panel title-panel",
    h1("The DASS App"),
    div(class = "link-list",
    tags$a(
      class = "btn btn-default",
      href = "user_guide.pdf",
      target = "_blank",
      "User Guide"
    ),
    tags$a(
      class = "btn btn-default",
      href = "mailto:ICE-support@niehs.nih.gov",
      "Contact Us"
    ),
    tags$a(
      class = "btn btn-default external-link",
      href = "https://github.com/NIEHS/DASS",
      target = "_blank",
      "Source Code"
    ),
    tags$a(
      class = "btn btn-default external-link",
      href = "https://rstudio.niehs.nih.gov/dass/",
      target = "_blank",
      "Launch App in New Window"
    ),
    ),
    span("Last updated: 2023-Oct-20")
  ),
  div(
    class = "header-panel details-panel",
    p(
      "The DASS App applies defined approaches on skin sensitization (DASS) that are described in",
      a(
        href = "https://doi.org/https://doi.org/10.1787/b92879a4-en",
        target = "_blank",
        class = "external-link",
        "OECD Guideline No. 497 ",
      ), " and the U.S. EPA's",
      a(
        href = "https://www.regulations.gov/document/EPA-HQ-OPP-2016-0093-0090",
        target = "_blank",
        class = "external-link",
        "Interim Science Policy: Use of Alternative Approaches for Skin",
        "Sensitization as a Replacement for Laboratory Animal Testing"
      ),
      ". The defined approaches (DAs) predict skin sensitization hazard",
      "(either a sensitizer or non-sensitizer) and potency (UN GHS categories) by integrating data",
      "from in vitro assays that represent key events in the",
      a(
        href = "https://doi.org/https://doi.org/10.1787/9789264221444-en",
        target = "_blank",
        class = "external-link",
        "Adverse Outcome Pathway (AOP) for Skin Sensitisation Initiated",
        "by Covalent Binding to Proteins"
      ),
      "and in silico hazard predictions."
    ),
  )
)

# Step 1: Select DAs -----
selectda_panel <- tabPanel(
  title = "Select Defined Approaches",
  id = "selectDAPanel",
  fluidRow(
    class = "bordered-panel",
    column(
      width = 12,
      p(
        "To begin, select the DAs to be implemented. Click on the green information",
        "buttons to view a description of the DA and the test mehods required to implement the DA."
      ),
      div(
        style = "margin: 1em 2em;",
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
    ),
    actionButton(
      inputId = "confirmDAs",
      label = "Continue",
      width = "100%"
    )
  )
)

daModals <- list(
  bsModal(
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
      src = "diagrams/2o3_diagram-v1.png",
      alt = "Diagram of 2o3 data interpretation procedure"
    ),
    HTML(
      "<p><a href = 'diagrams/2o3_diagram-v1.png' target='blank' style='font-size:90%;' class = 'external-link'>",
      "View full-size image",
      "</a></p>"
    ),
    p(
      "For more details, see",
      a(
        href = "https://doi.org/https://doi.org/10.1787/b92879a4-en",
        target = "_blank",
        class = "external-link",
        "OECD Guideline No. 497: Defined Approaches on Skin Sensitisation"
      ), "."
    )
  ),
  bsModal(
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
        class = "external-link",
        "Derek Nexus"
      ),
      "or the",
      a(
        href = "https://doi.org/10.1016/j.comtox.2019.01.006",
        target = "_blank",
        class = "external-link",
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
        class = "external-link",
        "OECD Guideline No. 497: Defined Approaches on Skin Sensitisation"
      ), "."
    )
  ),
  bsModal(
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
      src = "diagrams/KE31STS_diagram-v1.png",
      alt = "Diagram of KE 3/1 STS data interpretation procedure"
    ),
    HTML(
      "<p><a href = 'diagrams/KE31STS_diagram-v1.png' target='blank' style='font-size:90%;' class = 'external-link'>",
      "View full-size image",
      "</a></p>"
    ),
    p(
      "For more details, see EPA's",
      a(
        href = "https://www.regulations.gov/document/EPA-HQ-OPP-2016-0093-0090",
        target = "_blank",
        class = "external-link",
        "Interim Science Policy: Use of Alternative Approaches for Skin Sensitization",
        "as a Replacement for Laboratory Animal Testing Draft for Public Comment"
      ), "."
    )
  )
)

# Step 2: Upload Data -----
uploaddata_panel <- tabPanel(
  title = "Upload Data",
  fluidRow(
    class = "bordered-panel",
    column(
      width = 12,
      HTML(
        "<div class='warn-block'>",
        "<div>",
        "<i class='glyphicon glyphicon-exclamation-sign' role='presentation'></i>",
        "</div>",
        "<div>",
        "<p style='margin-bottom:0;'>Before uploading your file, ensure that the data meet the",
        "<a id='show_upload_req' href = 'dassApp-dataRequirements.html' target = '_blank' class = 'action-link' aria-label='data and formatting requirements'>",
        "<b>data and formatting requirements</b></a>.</p>",
        "</div>",
        "</div>"
      ),
      br(),
      p(
        "A table template is provided in tab-delimited or Excel format.",
        "The template contains columns for every possible assay endpoint.",
        "If an assay endpoint will not be used, the corresponding column can be",
        "deleted but that is not required. Using the template is not required."
      ),
      a(
        href = "DASSApp-dataTemplate.xlsx", "Download Data Template (.xlsx)",
        download = NA, target = "_blank"
      ),
      br(),
      a(
        href = "DASSApp-dataTemplate.txt", "Download Data Template (.txt)",
        download = NA, target = "_blank"
      ),
      hr(style = "width:50%"),
      div(
        id = "uploadBlock",
        div(
          class = "form-group shiny-input-container",
          style = "width:100%",
          tags$label(
            class = "control-label",
            `for` = "fpath",
            id = "fpath-label",
            value = "File input",
            "Click 'Browse' below and select your file."
          ),
          div(
            class = "input-group",
            tags$label(
              class = "input-group-btn input-group-prepend",
              span(
                class = "btn btn-default btn-file",
                "Browse...",
                tags$input(
                  id = "fpath",
                  name = "fpath",
                  type = "file",
                  style = "position: absolute !important; top: -99999px !important; left: -99999px !important;",
                  `data-shinyjs-resettable-id` = "fpath",
                  `data-shinyjs-resettable-type` = "File",
                  `data-shinyjs-resettable-value` = "",
                  `class` = "shinyjs-resettable shiny-bound-input"
                )
              )
            ),
            tags$input(
              type = "text",
              class = "form-control",
              title = "Form control for file input",
              style = "border-color:#232b5f; width:100%",
              placeholder = "No file selected.",
              readonly = "readonly"
            )
          ),
        ),
        uiOutput("xlsheet_text_ui")),
        HTML(
          "<div class='form-group shiny-input-container' style='width:100%;'>",
          "<div class='checkbox'>",
          "<label>",
          "<input id='useDemoData' type='checkbox'/>",
          "<span>Use demo data</span>",
          "</label>",
          "<button id='info_demo' type='button' class='btn action-link btn-qs' aria-label='demo data info'>",
          "<i class='glyphicon glyphicon-question-sign' role='presentation'> </i>",
          "</button>",
          "</div>",
          "</div>"
        ),
      div(
        class = "hiddenBlock",
        id = "data_block",
        hr(style = "width:50%"),
        DT::dataTableOutput("dt_analyze"),
        hr(style = "width:50%"),
        p("Once you have finished selecting the DAs and uploading your data, click 'Continue' to proceed to the next step."),
        actionButton(
          inputId = "confirm_data",
          label = "Continue",
          width = "100%"
      ))
    )
  )
)

dataModals <- list(
  bsModal(
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
    ),
    bsModal(
      id = "demo_data_modal",
      title = "Demo Data",
      trigger = "info_demo",
      p("Select this option to load a demo data set instead of uploading your own data."),
      p("The data set includes values for all possible endpoints. The column names are set up so",
      "that the selections in Step 3 are automatically filled."),
      p("If you select the ITS DA, the 'dpra_pC' column will be flagged in Step 4 because the value for OTNE (Row 60)",
      "contains a symbol. This example demonstrates how the app processes invalid values.")
    )
)

# Step 3: Select Columns -----
selectcolumns_panel <- tabPanel(
  title = "Select Data Columns",
  div(
    id = "selectcol_ui",
    class = "bordered-panel hiddenBlock",
    p("The assay endpoints that are required for the selected DAs are shown below.",
      "Use the dropdown lists to select the columns from your data",
      "that correspond to the given endpoints. Columns are automatically selected for an endpoint",
      "if the column name matches the corresponding column name in the data template.",
      "A column must be selected for each",
      "endpoint shown. When you are finished, click 'Done'."),
    p(
      "Click on the information buttons next to the assay endpoint names to view",
      "information about the endpoints and data formatting requirements.",
      "Values that are incorrectly formatted or invalid",
      "will be treated as missing data and may affect the results. More details are given",
      "in the User Guide."),
    div(
      id = "dpraCallSelect",
      class = "hiddenBlock",
      tags$h2("DPRA Binary Call",
              HTML(
              "<button id='info_dpracall' type='button' class='btn action-link btn-qs' aria-label='DPRA Call info'>",
              "<i class='glyphicon glyphicon-question-sign' role='presentation'> </i>",
              "</button>")),
      div(
        class = "col_assay_endpoint",
        radioButtons(
          inputId = "dpra_call_choice",
          label = "Data Source",
          choiceNames = c(
            "Use DPRA Binary Call",
            "Use %-Depletion Values"
          ),
          choiceValues = c(
            "call",
            "pdepl"
          )
        )),

      div(
        class = "col_assay_endpoint",
        conditionalPanel(
          condition = "input.dpra_call_choice=='call'",
          selectInput(
            inputId = "dpra_call_col",
            label = "DPRA Binary Call Column",
            choices = NULL,
            selectize = F
          )
        ))
    ),
    div(
      id = "dpraDepSelect",
      class = "hiddenBlock",
      tags$h2("DPRA % Depletion",
              HTML(
                "<button id='info_dpradep' type='button' class='btn action-link btn-qs' aria-label='DPRA Depletion info'>",
                "<i class='glyphicon glyphicon-question-sign' role='presentation'> </i>",
                "</button>")
              ),
      div(
        class = "col_assay_endpoint",
        selectInput(
          inputId = "dpra_pC_col",
          label = "DPRA %-Cysteine Depletion Column",
          choices = NULL,
          selectize = F
        ),
        selectInput(
          inputId = "dpra_pK_col",
          label = "DPRA %-Lysine Depletion Column",
          choices = NULL,
          selectize = F
        )
      )
    ),
    div(
      id = "hclatCallSelect",
      class = "hiddenBlock",
      tags$h2("h-CLAT Binary Call",
              HTML(
                "<button id='info_hclatcall' type='button' class='btn action-link btn-qs' aria-label='hCLAT call info'>",
                "<i class='glyphicon glyphicon-question-sign' role='presentation'> </i>",
                "</button>")),
      div(
        class = "col_assay_endpoint",
        selectInput(
          inputId = "hclat_call_col",
          label = "h-CLAT Binary Call Column",
          choices = NULL,
          selectize = F
        )
      )
    ),
    div(
      id = "hclatMitSelect",
      class = "hiddenBlock",
      tags$h2("h-CLAT MIT",
              HTML(
                "<button id='info_hclatmit' type='button' class='btn action-link btn-qs' aria-label='hCLAT MIT info'>",
                "<i class='glyphicon glyphicon-question-sign' role='presentation'> </i>",
                "</button>")),
      div(
        class = "col_assay_endpoint",
        selectInput(
          inputId = "hclat_mit_col",
          label = "h-CLAT Minimum Induction Threshold (MIT) Column",
          choices = NULL,
          selectize = F
        )
      )
    ),
    div(
      id = "ksCallSelect",
      class = "hiddenBlock",
      tags$h2("KeratinoSens(TM) Binary Call",
              HTML(
                "<button id='info_kscall' type='button' class='btn action-link btn-qs' aria-label='keratinosens call info'>",
                "<i class='glyphicon glyphicon-question-sign' role='presentation'> </i>",
                "</button>")),
      div(
        class = "col_assay_endpoint",
        selectInput(
          inputId = "ks_call_col",
          label = "KS Binary Call Column",
          choices = NULL,
          selectize = F
        )
      )
    ),
    div(
      id = "inSilicoSelect",
      class = "hiddenBlock",
      tags$h2("In Silico Binary Call",
              HTML(
                "<button id='info_insilico_call' type='button' class='btn action-link btn-qs' aria-label='in silico call info'>",
                "<i class='glyphicon glyphicon-question-sign' role='presentation'> </i>",
                "</button>")),
      div(
        class = "col_assay_endpoint",
        selectInput(
          inputId = "insilico_call_col",
          label = "In Silico Binary Call Column",
          choices = NULL,
          selectize = F
        ),
        selectInput(
          inputId = "insilico_ad_col",
          label = "In Silico Applicability Domain",
          choices = NULL,
          selectize = F
        )
      )
    ),
    actionButton(inputId = "review_entries",
                 label = "Done",
                 width = "100%")
    )
  )

## Modals -----
selectModals <- list(
  ### DPRA -----
  bsModal(
    id = "dpra_dep_modal",
    title = "DPRA % Depletion",
    trigger = "info_dpradep",
    p(
      "%-Cysteine and %-Lysine depletion values from the direct peptide reactivity",
      "assay (DPRA) are used in ITS."
    ),
    p(
      "The columns corresponding to %-Cysteine and %-Lysine depletion should only",
      "contain numeric values. Missing values should be blank or labeled as 'NA'."
    ),
    p(
      "For more details, see",
      a(
        href = "https://doi.org/10.1787/9789264229709-en",
        target = "_blank",
        class = "external-link",
        "OECD Test No. 442C: In Chemico Skin Sensitisation"
      ), "."
    )
  ),
  bsModal(
    id = "dpra_call_modal",
    title = "DPRA Binary Call",
    trigger = "info_dpracall",
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
        href = "https://doi.org/10.1787/9789264229709-en",
        target = "_blank",
        class = "external-link",
        "OECD Test No. 442C: In Chemico Skin Sensitisation"
      ), "."
    )
  ),
  ### hCLAT -----
  bsModal(
    id = "hclat_call_modal",
    title = "h-CLAT Binary Call",
    trigger = "info_hclatcall",
    p("Results from the human cell line activation test (h-CLAT) are used in the 2o3 defined approach."),
    p(
      "The column corresponding to h-CLAT binary call should only contain the values:",
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
    p(
      "For more details, see ",
      a(
        href = "https://doi.org/10.1787/9789264264359-en",
        target = "_blank",
        class = "external-link",
        "OECD Test No. 442E: In Vitro Skin Sensitisation"
      ), "."
    )
  ),
  bsModal(
    id = "hclat_mit_modal",
    title = "h-CLAT MIT",
    trigger = "info_hclatmit",
    p(
      "Minimum induction threshold (MIT) from the human cell line activiation test (h-CLAT)",
      "is used in the ITS and KE3/1 STS defined approaches."
    ),
    p(
      "The column corresponding to h-CLAT MIT should only contain:",
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
      )
    ),
    p(
      "For more details, see",
      a(
        href = "https://doi.org/10.1787/9789264264359-en",
        target = "_blank",
        class = "external-link",
        "OECD Test No. 442E: In Vitro Skin Sensitisation"
      )
    )
  ),

  ### KeratinoSens -----
  bsModal(
    id = "ks_call_modal",
    title = HTML("KeratinoSens&trade; Binary Call"),
    trigger = "info_kscall",
    p("Results from the KeratinoSens (KS) assay are used in the 2o3 defined approach."),
    p(
      "The column corresponding to KS hazard should only contain the values:",
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
      "For more details, see",
      a(
        href = "https://doi.org/10.1787/9789264229822-en",
        target = "_blank",
        class = "external-link",
        "OECD Test No. 442D: In Vitro Skin Sensitisation"
      ), "."
    )
  ),

  ### In Silico -----
  bsModal(
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
        href = "https://doi.org/10.1016/j.comtox.2019.01.006",
        target = "_blank",
        class = "external-link",
        "OECD QSAR Toolbox"
      ), "."
    ),
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
)

# Step 4: Review Selection -----
reviewselection_panel <- tabPanel(
  title = "Review Selection",
  div(
    id = "review_contents",
    class = "bordered-panel hiddenBlock",
    div(
      id = "dupe_col_warning",
      class = "warningText hiddenBlock",
      p(
        strong("Warning: Single column assigned to more than one variable.")
      )
    ),
    div(
      id = "panel4_warn",
      class = "hiddenBlock",
      div(
        id = "col_flag_warning",
        class = "warningText",
        p(
          strong("Warning: Selected data columns have been flagged for invalid values.")
        )
      ),
      div(
        id = "review_instructions_warn",
        p("Review the selected columns and flags in the table below. Upload an updated dataset or select new columns."),
        p("Click 'Run' to run DASS anyway. Invalid values will be",
          "considered missing (NA) and will", strong("not"), "be used to evaluate",
          "skin sensitization hazard identification or potency.")
      )
    ),
    div(
      id = "panel4_nowarn",
      class = "hiddenBlock",
      p("Review the selected columns and click 'Run' to run DASS.")
    ),
  dataTableOutput("dt_review"),
  br(),
  actionButton(
    inputId = "run_dass",
    width = "100%",
    label = "Run"
  ))
)

reviewModals <- list(
  bsModal(
    id = "confirm_run_with_flag",
    title = "Warning",
    trigger = NULL,
    p(
      "The selected columns have been flagged for invalid values. Invalid",
      "values will be considered missing (NA) and will", strong("not"), "be used",
      "to evaluate skin sensitization hazard identification or potency. Continue?"
    ),
    actionButton(inputId = "run_with_flags", label = "Run"),
    actionButton(inputId = "cancel_run", label = "Cancel")
  )
)

# Step 5: Results -----
results_panel <- tabPanel(
  title = "Results",
  div(
    id = "result_contents",
    class = "bordered-panel hiddenBlock",
    p(
      "Results of the DASS App analysis are shown in the table below. Use the",
      "scroll bar along the bottom of the table to view all the columns.",
      "The buttons above the table can be used to hide or show columns.",
      "Use the 'Download Results' button to export your results to an Excel",
      "spreadsheet or text file, which may allow easier viewing.",
    ),
    div(
      class = "key-block",
      h2("Table Key", style = "font-size:1.2em; font-weight:bold; margin-top:10px;"),
      tags$dl(
        tags$dt(
          "Yellow columns"
        ),
        tags$dd(
          "The data columns that you selected in Step 3. The column names are annotated with an asterisk."
        ),
        tags$dt(
          "Pink columns",
          HTML(
            "<button id='info_pink' type='button' class='btn action-link btn-qs' aria-label='Information button for details about pink columns'>",
            "<i class='glyphicon glyphicon-question-sign' role='presentation'> </i>",
            "</button>"
          )
        ),
        tags$dd(
          "Appended columns showing how your data inputs were interpreted.",
          "The corresponding column names begin with 'Input'. For values that",
          "were calculated by the app, the column name will also end with 'Calculated'."
        ),
        tags$dt(
          "Blue columns",
          HTML(
            "<button id='info_blue' type='button' class='btn action-link btn-qs' aria-label='Information button for details about blue columns'>",
            "<i class='glyphicon glyphicon-question-sign' role='presentation'> </i>",
            "</button>"
          )
        ),
        tags$dd(
          "Appended columns with the DASS predictions. The corresponding column",
          "names begin with 'DA' and the name of the DA."
        )
      ),
      "For more details about the appended columns, see the User Guide.",
    ),
    fluidRow(column(12,
                    div(
                      class = "dropdown",
                      id = "dlDropdown",
                      tags$button(
                        class = "btn dropbtn btn-default",
                        # style = "padding:1vh;",
                        "Download Results",
                        HTML(
                          "<i class='fas fa-caret-down' role='presentation'> </i>"
                        )
                      ),
                      div(
                        class = "dropdown-content",
                        downloadButton(outputId = "downloadres_xl", "Excel (.xlsx)", icon = icon("file-excel"), class = "btn-dl"),
                        downloadButton(outputId = "downloadres_txt", "Tab-Delimited (.txt)", icon = icon("file-alt"), class = "btn-dl"),
                      )
                    ),
                    actionButton(inputId = "goToCompare",
                                 label = "Compare Results")
    )),
    br(),
  dataTableOutput("dt_results")
  )
)

resultsModals <- list(
  bsModal(
    id = "res_pink_modal",
    title = "Input and Calculated Columns",
    trigger = "info_pink",
    p(
      "These are the actual values used for evaluation. It may be useful to",
      "review the selected columns and their transformations to ensure your data",
      "were properly interpreted, especially if the DAs were run with flagged data."
    ),
    p("Binary call data are transformed to ‘0’ for negative results and ‘1’ for positive results."),
    p("If the h-CLAT minimum induction threshold was used, negative results are transformed to ‘Inf’.")
  ),
  bsModal(
    id = "res_blue_modal",
    title = "DA Columns",
    trigger = "info_blue",
    p(
      "If the ITS DA was selected, then the individual ITS scores are also appended",
      "and highlighted in blue. The corresponding columns end with ‘Score’."
    ),
    p("Hazard call predictions are ‘0’ if the result is negative and ‘1’ if the result is positive."),
    p(
      "For assigning potency predictions, the DASS App uses categories established",
      "by the United Nations Globally Harmonized System for Classification and Labelling of Chemicals (GHS)."
    )
  )
)

resultsModals <- list(
  bsModal(
    id = "res_pink_modal",
    title = "Input and Calculated Columns",
    trigger = "info_pink",
    p(
      "These are the actual values used for evaluation. It may be useful to",
      "review the selected columns and their transformations to ensure your data",
      "were properly interpreted, especially if the DAs were run with flagged data."
    ),
    p("Binary call data are transformed to ‘0’ for negative results and ‘1’ for positive results."),
    p("If the h-CLAT minimum induction threshold was used, negative results are transformed to ‘Inf’.")
  ),
  bsModal(
    id = "res_blue_modal",
    title = "DA Columns",
    trigger = "info_blue",
    p(
      "If the ITS DA was selected, then the individual ITS scores are also appended",
      "and highlighted in blue. The corresponding columns end with ‘Score’."
    ),
    p("Hazard call predictions are ‘0’ if the result is negative and ‘1’ if the result is positive."),
    p(
      "For assigning potency predictions, the DASS App uses categories established",
      "by the United Nations Globally Harmonized System for Classification and Labelling of Chemicals (GHS)."
    )
  )
)

# Compare -----
compare_panel <- tabPanel(
  title = "Compare",
  div(
    id = "performanceUI",
    class = "bordered-panel hiddenBlock",
    p(
      "This section allows you to compare the DA outcomes to reference data.",
      "The reference data should be provided in your uploaded data file."
    ),
    p(
      "Use the dropdown lists to select the DA predictions you want to evaluate and specify ",
      "the columns containing your reference data. You may select more than one prediction or",
      "reference column. Click 'Compare' to generate a confusion matrix and accuracy metrics.",
      "Comparisons will only be performed if there are valid values",
      "for at least five prediction-reference pairs."
    ),
    radioButtons(
      inputId = "compareType",
      label = "Select type of comparison",
      choices = c("Hazard", "Potency")
    ),
    div(
      id = "referenceCompare_ui",
      selectInput(
        inputId = "perfPredCol",
        label = "Select Prediction Column(s)",
        selectize = TRUE,
        choices = NULL,
        multiple = TRUE
      ),
      selectInput(
        inputId = "perfRefRes",
        label = list(
          span("Select Reference Column(s)"),
          HTML("<button id='info_refCol' type='button' class='btn action-link btn-qs' aria-label='reference column info'>",
          "<i class='glyphicon glyphicon-question-sign' role='presentation'> </i>",
          "</button>")
        ),
        selectize = TRUE,
        choices = NULL,
        multiple = TRUE
      ),
      actionButton(
        inputId = "compareToRef",
        label = "Compare"
      )
    ),
  div(
    class = "hiddenBlock",
    id = "perfBlock",
    hr(style = "width:50%"),
    p(
      "Confusion matrices and performance metrics are shown below. Use the dropdown list",
      "to select the comparison you would like to view. Use the 'Download' button to open",
      "the download menu."
    ),
    actionButton(inputId = "downloadSupp", label = "Download"),
    selectInput(inputId = "perfList", label = "Select Output", choices = NULL),
    plotOutput("perfFigure"),
    hr(style = "width:50%"),
    div(
      class = "hiddenBlock",
      id = "binaryDefs",
      h2("Table Definitions", style = "font-size: 1em; text-align: center;"),
      HTML("<table class = 'defTab' border=1>
<tr> <th> Metric </th> <th> Definition </th>  </tr>
<tr> <td> N </td> <td> The number of valid reference values </td>  </tr>
  <tr> <td> Accuracy  </td> <td>  (True positives + True negatives) / (All positives + All negatives) </td> </tr>
  <tr> <td> Balanced Accuracy  </td> <td>  (True positive rate + True negative rate)/2 </td> </tr>
  <tr> <td> F1 Score  </td> <td>  (2&times;True positives) / (2&times;True positives + False positives + False negatives) </td> </tr>
  <tr> <td> True Positive Rate (Sensitivity)  </td> <td>  True positives / All positives </td> </tr>
  <tr> <td> False Positive Rate  </td> <td>  False positives / All positives </td> </tr>
  <tr> <td> True Negative Rate (Specificity)  </td> <td>  True negatives / All negatives </td> </tr>
  <tr> <td> False Negative Rate  </td> <td>  False negatives / All negatives </td> </tr>
   </table>")
    ),
  div(
    class = "hiddenBlock",
    id = "potencyDefs",
    h2("Table Definitions", style = "font-size: 1em; text-align: center;"),
    HTML("<table class = 'defTab' border=1>
         <tr> <th> Metric </th> <th> Definition </th>  </tr>
    <tr> <td> N </td> <td> The number of valid reference values </td>  </tr>
    <tr> <td> Accuracy  </td> <td>  The percentage of predicted values equal to reference values </td> </tr>
    <tr> <td> Overpredicted </td> <td>  The percentage of predicted values with a more potent GHS category than the corresponding reference value </td> </tr>
    <tr> <td> Underpredicted  </td> <td>  The percentage of predicted values with a less potent GHS category than the corresponding reference value </td> </tr>
     </table>")
  )
  ))
)

compareModals <- list(
  bsModal(
    id = "refCol_modal",
    title = "Reference column",
    trigger = "info_refCol",
    div(
      h2("Hazard"),
      p(
        "The columns corresponding to reference hazard calls should only contain the values:",
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
      )
    ),
    div(
      h2("Potency"),
      p(
        "The columns corresponding to reference potency classifications should only contain the values:",
        tags$ul(
          tags$li(
            "1A, 1B, NC"
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
  ),
  bsModal(
    id = "perfDLMenu",
    title = "Download Performance Output",
    trigger = "downloadSupp",
    div(
      p(
        "Confusion matrices and performance tables can be downloaded as a PDF file.",
        "Use the checkboxes to select the output you would like to download.",
        "More details about the performance output are available in the User Guide."
      )
    ),
    div(
      actionLink(inputId = "perfAll", label = "Select All"), " | ",
      actionLink(inputId = "perfNone", label = "Deselect All"),
      checkboxGroupInput(inputId = "tableChoices", label = "Select Output to Download", choices = NULL),
      downloadButton(outputId = "dlPerf", label = "Download Output")
    ),
    hr(style = "width:50%"),
    p(
      "You can download the results in table format as an Excel file or text file.",
    ),
    p(
      downloadButton(outputId = "perfFlat_xl", label = "Excel (.xlsx)", icon = icon("file-excel"), class = "btn-dl"),
      downloadButton(outputId = "perfFlat_txt", label = "Tab-Delimited (.txt) ", icon = icon("file-alt"), class = "btn-dl")
    )
    
  )
)

# Build page -----
ui_dass <- list(
  welcome_panel,
  tabsetPanel(
    id = "stepSet",
    selectda_panel,
    uploaddata_panel,
    selectcolumns_panel,
    reviewselection_panel,
    results_panel,    
    compare_panel
  ),
  daModals,
  dataModals,
  selectModals,
  reviewModals,
  resultsModals,
  compareModals
)
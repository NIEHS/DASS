# =============================================================================#
# File Name: modals.R
# Original Creator: Kim To
# Contact Information: ICE-support@niehs.nih.gov
# Date Created: 2024-05-08
# License: MIT
# Description: UI for modals
# =============================================================================#


# User Guide -----


# Step 1: Select DAs -----
da_modals <- list(
  bsModal(
    id = "user_guide_modal",
    title = "User Guide",
    trigger = "test",
    size = "large",
    tags$iframe(
      style = "width:100%;",
      src = "user_guide.pdf"
    )
  ),
  bsModal(
    id = "info_2o3_modal",
    title = "2 out of 3",
    trigger = "info_2o3",
    p(
      "The 2 out of 3 (2o3) DA predicts skin sensitization hazard based on at",
      "least 2 concordant results among the direct peptide reactivity assay (DPRA),",
      "KeratinoSens\u2122, and human cell line activation test (h-CLAT)."
    ),
    img(class = "da-diagram",
        src = "diagrams/2o3_diagram-v1.png",
        alt = "Diagram of 2o3 data interpretation procedure"),
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
      ),
      "."
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
      ),
      "."
    ),
    img(class = "da-diagram",
        src = "diagrams/ITS_diagram-v1.png",
        alt = "Diagram of ITS data interpretation procedure"),
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
      ),
      "."
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
    img(class = "da-diagram",
        src = "diagrams/KE31STS_diagram-v1.png",
        alt = "Diagram of KE 3/1 STS data interpretation procedure"),
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
      ),
      "."
    )
  )
)

data_modals <- list(
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
    p(
      "Select this option to load a demo data set instead of uploading your own data."
    ),
    p(
      "The data set includes values for all possible endpoints. The column names are set up so",
      "that the selections in Step 3 are automatically filled."
    ),
    p(
      "If you select the ITS DA, the 'dpra_pC' column will be flagged in Step 4 because the value for OTNE (Row 60)",
      "contains a symbol. This example demonstrates how the app processes invalid values."
    )
  )
)

## Modals -----
select_modals <- list(
  ### DPRA -----
  bsModal(
    id = "dpra_dep_modal",
    title = "DPRA % Depletion",
    trigger = "info_dpra_dep",
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
      ),
      "."
    )
  ),
  bsModal(
    id = "dpra_call_modal",
    title = "DPRA Binary Call",
    trigger = "info_dpra_call",
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
        tags$li("Missing values should be blank or labeled as 'NA'.")
      ),
      span(style = "font-size:90%",
           "* Case insensitive")
    ),
    p(
      "Alternatively, the %-Cysteine and %-Lysine depletion values can be evaluated",
      "to derive the binary call. Binary calls are made using Tables 1 and 2 from",
      a(
        href = "https://doi.org/10.1787/9789264229709-en",
        target = "_blank",
        class = "external-link",
        "OECD Test No. 442C: In Chemico Skin Sensitisation"
      ),
      "."
    )
  ),
  ### hCLAT -----
  bsModal(
    id = "hclat_call_modal",
    title = "h-CLAT Binary Call",
    trigger = "info_hclat_call",
    p(
      "Results from the human cell line activation test (h-CLAT) are used in the 2o3 defined approach."
    ),
    p(
      "The column corresponding to h-CLAT binary call should only contain the values:",
      tags$ul(
        tags$li(
          "'sensitizer', 'sensitiser', 'a', 'active', 'p', 'pos', 'positive', or 1 to indicate positive assay outcomes.*"
        ),
        tags$li(
          "'non-sensitizer', 'non-sensitiser', 'i', 'inactive', 'n', 'neg', 'negative', or 0 to indicate negative assay outcomes.*"
        ),
        tags$li("Missing values should be blank or labeled as 'NA'.")
      )
    ),
    span(style = "font-size:90%",
         "* Case insensitive"),
    p(
      "For more details, see ",
      a(
        href = "https://doi.org/10.1787/9789264264359-en",
        target = "_blank",
        class = "external-link",
        "OECD Test No. 442E: In Vitro Skin Sensitisation"
      ),
      "."
    )
  ),
  bsModal(
    id = "hclat_mit_modal",
    title = "h-CLAT MIT",
    trigger = "info_hclat_mit",
    p(
      "Minimum induction threshold (MIT) from the human cell line activiation test (h-CLAT)",
      "is used in the ITS and KE3/1 STS defined approaches."
    ),
    p(
      "The column corresponding to h-CLAT MIT should only contain:",
      tags$ul(
        tags$li("Numeric values."),
        tags$li(
          "'non-sensitizer', 'non-sensitiser', 'i', 'inactive', 'n', 'neg', 'negative', or 'Inf' to indicate negative assay outcomes*"
        ),
        tags$li("Missing values should be blank or labeled as 'NA'.")
      ),
      span(style = "font-size:90%",
           "* Case insensitive")
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
    trigger = "info_ks_call",
    p(
      "Results from the KeratinoSens (KS) assay are used in the 2o3 defined approach."
    ),
    p(
      "The column corresponding to KS hazard should only contain the values:",
      tags$ul(
        tags$li(
          "'sensitizer', 'sensitiser', 'a', 'active', 'p', 'pos', 'positive', or 1 to indicate positive assay outcomes.*"
        ),
        tags$li(
          "'non-sensitizer', 'non-sensitiser', 'i', 'inactive', 'n', 'neg', 'negative', or 0 to indicate negative assay outcomes.*"
        ),
        tags$li("Missing values should be blank or labeled as 'NA'.")
      ),
      span(style = "font-size:90%",
           "* Case insensitive")
    ),
    p(
      "For more details, see",
      a(
        href = "https://doi.org/10.1787/9789264229822-en",
        target = "_blank",
        class = "external-link",
        "OECD Test No. 442D: In Vitro Skin Sensitisation"
      ),
      "."
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
      ),
      ". ITSv2 uses predictions from",
      a(
        href = "https://doi.org/10.1016/j.comtox.2019.01.006",
        target = "_blank",
        class = "external-link",
        "OECD QSAR Toolbox"
      ),
      "."
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
        tags$li("Missing values should be blank or labeled as 'NA'.")
      ),
      span(style = "font-size:90%",
           "* Case insensitive")
    ),
    p(
      "Additionally, a column corresponding to applicability domain (AD) should",
      "be provided. This column should only contain the values:",
      tags$ul(
        tags$li("'in' or 1 to indicate the chemical is within the AD*"),
        tags$li(
          "'out' or 0 to indicate the chemical is outside the AD*. Values for",
          "chemicals outside the AD will not be evaluated."
        ),
        tags$li("Missing values should be blank or labeled as 'NA'.")
      ),
      span(style = "font-size:90%",
           "* Case insensitive")
    )
  )
)

review_modals <- list(
  bsModal(
    id = "confirm_run_with_flag",
    title = "Warning",
    trigger = NULL,
    p(
      "The selected columns have been flagged for invalid values. Invalid",
      "values will be considered missing (NA) and will",
      strong("not"),
      "be used",
      "to evaluate skin sensitization hazard identification or potency. Continue?"
    ),
    actionButton(inputId = "run_with_flags", label = "Run"),
    actionButton(inputId = "cancel_run", label = "Cancel")
  )
)

results_modals <- list(
  bsModal(
    id = "res_pink_modal",
    title = "Input and Calculated Columns",
    trigger = "info_pink",
    p(
      "These are the actual values used for evaluation. It may be useful to",
      "review the selected columns and their transformations to ensure your data",
      "were properly interpreted, especially if the DAs were run with flagged data."
    ),
    p(
      "Binary call data are transformed to ‘0’ for negative results and ‘1’ for positive results."
    ),
    p(
      "If the h-CLAT minimum induction threshold was used, negative results are transformed to ‘Inf’."
    )
  ),
  bsModal(
    id = "res_blue_modal",
    title = "DA Columns",
    trigger = "info_blue",
    p(
      "If the ITS DA was selected, then the individual ITS scores are also appended",
      "and highlighted in blue. The corresponding columns end with ‘Score’."
    ),
    p(
      "Hazard call predictions are ‘0’ if the result is negative and ‘1’ if the result is positive."
    ),
    p(
      "For assigning potency predictions, the DASS App uses categories established",
      "by the United Nations Globally Harmonized System for Classification and Labelling of Chemicals (GHS)."
    )
  )
)


compare_modals <- list(
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
          tags$li("Missing values should be blank or labeled as 'NA'.")
        ),
        span(style = "font-size:90%",
             "* Case insensitive")
      )
    ),
    div(
      h2("Potency"),
      p(
        "The columns corresponding to reference potency classifications should only contain the values:",
        tags$ul(
          tags$li("1A, 1B, NC"),
          tags$li("Missing values should be blank or labeled as 'NA'.")
        ),
        span(style = "font-size:90%",
             "* Case insensitive")
      )
    )
  ),
  bsModal(
    id = "perf_dl_menu",
    title = "Download Performance Output",
    trigger = "download_compare_tables",
    div(
      p(
        "Confusion matrices and performance tables can be downloaded as a PDF file.",
        "Use the checkboxes to select the output you would like to download.",
        "More details about the performance output are available in the User Guide."
      )
    ),
    div(
      actionLink(inputId = "perf_table_all", label = "Select All"),
      " | ",
      actionLink(inputId = "perf_table_none", label = "Deselect All"),
      checkboxGroupInput(
        inputId = "perf_table_choices",
        label = "Select Output to Download",
        choices = NULL,
        width = "100%"
      ),
      downloadButton(outputId = "dl_perf_tables", label = "Download Output")
    ),
    hr(style = "width:50%"),
    p(
      "You can download the results in table format as an Excel file or text file.",
    ),
    p(
      downloadButton(
        outputId = "perf_table_flat_xl",
        label = "Excel (.xlsx)",
        icon = icon("file-excel"),
        class = "btn-dl"
      ),
      downloadButton(
        outputId = "perf_table_flat_txt",
        label = "Tab-Delimited (.txt) ",
        icon = icon("file-alt"),
        class = "btn-dl"
      )
    )
  )
)

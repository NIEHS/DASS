# =============================================================================#
# File Name: modals.R
# Original Creator: Kim To
# Contact Information: ICE-support@niehs.nih.gov
# Date Created: 2024-05-08
# License: MIT
# Description: UI for modals
# =============================================================================#

# DICTIONARIES -----
## Links -----
get_link <- list(
  gl497 = list(
    url = "https://doi.org/https://doi.org/10.1787/b92879a4-en",
    label = "OECD Guideline No. 497 (GL497): Defined Approaches on Skin Sensitisation"
  ),
  tg442c = list(
    url = "https://doi.org/10.1787/9789264229709-en",
    label = "OECD Test Guideline No. 442C: In Chemico Skin Sensitisation"
  ),
  tg442d = list(
    url = "https://doi.org/10.1787/9789264229822-en",
    label = "OECD Test Guideline No. 442D: In Vitro Skin Sensitisation"
  ),
  tg442e = list(
    url = "https://doi.org/10.1787/9789264264359-en",
    label = "OECD Test Guideline No. 442E: In Vitro Skin Sensitisation"
  ),
  epaopp = list(
    url = "https://www.regulations.gov/document/EPA-HQ-OPP-2016-0093-0090",
    label = "Interim Science Policy: Use of Alternative Approaches for Skin Sensitization as a Replacement for Laboratory Animal Testing Draft for Public Comment" 
  ),
  user_guide = list(
    url = "user_guide.pdf",
    label = "User Guide"
  )
)
# //
get_link <- lapply(get_link, function(x) a(href = x[[1]], target = "_blank", class = "external-link", x[[2]]))

## DA Figures -----
get_da_fig <- list(
  da2o3 = list(
    url = "da_diagrams/2o3_diagram-v2.png",
    alt = "Diagram of 2o3 data interpretation procedure",
    caption = "\u2020 GL497 evaluates the 2o3 DA using DPRA, KeratinoSens, and h-CLAT. The additional key event assays are under evaluation for inclusion in GL497."
  ),
  
  daits = list(
    url = "da_diagrams/ITS_diagram-v2.png",
    alt = "Diagram of ITS data interpretation procedure",
    caption = "\u2020 GL497 evaluates the ITS DA using DPRA, h-CLAT, and either OECD QSAR Toolbox or Derek Nexus. The additional key event assays and in silico models are under evaluation for inclusion in GL497."
  ),
  
  dake31 = list(
    url = "da_diagrams/KE31STS_diagram-v1.png",
    alt = "Diagram of KE 3/1 STS data interpretation procedure",
    caption = ""
  )
)
# //
get_da_fig <- lapply(get_da_fig, function(x) {
  tags$figure(
    a(
      title = "Click to open in new window.",
      href = x[["url"]],
      target = "_blank",
      img(class = "da-diagram", src = x[["url"]], alt = x[["alt"]])),
    tags$figcaption(x[["caption"]])
  )
})

## BLR Figures -----
get_blr_fig <- list(
  ke1 = list(
    adra_blr = list(
      img_id = "blr_1",
      label = "ADRA",
      url = "blr_diagrams/0_ke1_adra_tree.png",
      alt = "Decision tree for a single run of the amino acid reactivity assay (ADRA)",
      caption = p("The amino acid reactivity assay (ADRA) measures depletion of two peptides containing either cysteine or lysine derivatives (NAC and NAL, respectively) due to covalent binding.The borderline range for the mean NAC and NAL % depletion is [4.06, 5.94]. If only NAC % depletion can be evaluated, the borderline range is [4.67, 6.83]. For more details, see", get_link[["tg442c"]], ".")
    ),
    dpra_blr = list(
      img_id = "blr_2",
      label = "DPRA",
      url = "blr_diagrams/1_ke1_dpra_tree.png",
      alt = "Decision tree for a single run of the direct peptide reactivity assay (DPRA)",
      caption =  p("The direct peptide reactivity assay (DPRA) measures depletion of two peptides containing either cysteine or lysine residues due to covalent binding.The borderline range for the mean cysteine and lysine % depletion is [4.95, 8.32]. If only cysteine % depletion can be evaluated, the borderline range is [10.56, 18.47]. For more details, see", get_link[["tg442c"]], ".")
    )
  ),
  ke2 = list(
    keratinosens_blr = list(
      img_id = "blr_3",
      label = "KeratinoSens",
      url = "blr_diagrams/2_ke2_keratinosens_tree.png",
      alt = "Decision tree for a single run of the KeratinoSens test method",
      caption = p("The ARE-Nrf2 Luciferase", HTML("KeratinoSens&trade;"), " test method assesses activation of the Nrf2-Keap1 pathway. The borderline range for maximal average fold induction (Imax) is (1.35, 1.67). Note, the DASS App does not apply the KeratinoSens decision tree, because borderline results can be obtained from the KeratinoSens data evaluation template. For more details, see", get_link[["tg442d"]],".")
    ),
    lusens_blr = list(
      img_id = "blr_4",
      label = "LuSens",
      url = "blr_diagrams/3_ke2_lusens_tree.png",
      alt = "Decision tree for a single run of the LuSens test method",
      caption = p("The ARE-Nrf2 Luciferase LuSens test method assesses activation of the Nrf2-Keap1 pathway. The borderline range for luciferase induction threshold is (1.28, 1.76). For more details, see", get_link[["tg442d"]],".")
    )
  ),
  ke3 = list(
    gardskin_blr = list(
      img_id = "blr_5",
      label = "GARDskin",
      url = "blr_diagrams/4_ke3_gardskin_tree.png",
      alt = "Decision tree for a single run of the GARDskin test method",
      caption = p("The Genomic Allergen Rapid Detection", HTML("(GARD&trade;)"), "for Assessment of Skin Sensitisers (GARDskin) method assesses genomic biomarker signatures  associated with dendritic cell activation. The borderline range for the decision value (DV) is (-0.450, 0.450). For more details, see", get_link[["tg442e"]],".")
    ),
    hclat_blr = list(
      img_id = "blr_6",
      label = "h-CLAT",
      url = "blr_diagrams/5_ke3_hclat_tree.png",
      alt = "Decision tree for a single run of the h-CLAT",
      caption = p("The Human Cell Line Activation Test (h-CLAT) quantifies changes in the expression of cell surface markers associated with the process of activation of monocytes and dendritic cells (CD86 and CD54). The borderline range for CD86 induction is (122, 184]. The borderline range for CD54 induction is (157, 255]. For more details, see", get_link[["tg442e"]],".")
    ),
    il8luc_blr = list(
      img_id = "blr_7",
      label = "IL-8 Luc",
      url = "blr_diagrams/6_ke3_il8luc_tree.png",
      alt = "Decision tree for a single run of the IL-8 Luc assay",
      caption = p("The Interleukin-8 Reporter Gene (IL-8 Luc) assay quantifies changes in IL-8 expression, a cytokine associated with the activation of dendritic cells. The borderline range for luciferase induction reflecting IL-8 promoter activity is [1.25, 1.67). For more details, see", get_link[["tg442e"]],".")
    ),
    usens_blr = list(
      img_id = "blr_8",
      label = "U-SENS",
      url = "blr_diagrams/7_ke3_usens_tree.png",
      alt = "Decision tree for a single run of the U-SENS test",
      caption = p("The U937 Cell Line Activation", HTML("(U-SENS&trade;)"), "test quantifies changes in the expression of cell surface markers associated with the process of activation of monocytes and dendritic cells (CD86) in the human histiocytic lymphoma cell line U937. For more details, see", get_link[["tg442e"]],".")
    )
  )
)
# //
blr_img_ids <- lapply(get_blr_fig, function(x) unname(sapply(x, function(y) y[["img_id"]])))
blr_label_ids <- lapply(get_blr_fig, function(x) unname(sapply(x, function(y) y[["label"]])))
get_blr_fig <- lapply(get_blr_fig, function(x) {
  lapply(x, function(y) {
    div(
      class = "caro-fig", 
      id = y[["img_id"]], 
      tags$figure(
        a(
          title = "Click to open in new window.",
          href = y[["url"]],
          target = "_blank",
          img(class = "da-diagram", src = y[["url"]], alt = y[["alt"]])),
        tags$figcaption(y[["caption"]])
      )
    )
  })
})

## Data Format -----
format_info <- list(
  binary_call = p(
    "The column corresponding to this endpoint should only contain the values:",
    tags$ul(
      tags$li("\"sensitizer\", \"sensitiser\", \"a\", \"active\", \"p\", \"pos\", \"positive\", or \"1\" to indicate positive assay outcomes.*"),
      tags$li("\"non-sensitizer\", \"non-sensitiser\", \"i\", \"inactive\", \"n\", \"neg\", \"negative\", or \"0\" to indicate negative assay outcomes.*"),
      tags$li("Missing values should be blank or labeled as \"NA\".")
    ),
    span(style = "font-size: 90%", "*Case insensitive")
    ),
  binary_call_ke1 = p("Alternatively, depletion values can be used to derive binary calls using the prediction models in", get_link[["tg442c"]], "."),
  numeric = p("The column corresponding to this endpoint should only contain numeric values. Missing values should be blank or labeled as \"NA\"."),
  
  ke3_value = p(
    "The column corresponding this endpoint should only contain:",
    tags$ul(
      tags$li("Numeric values corresponding to positive assay outcomes"),
      tags$li("\"Inf\", \"NI\", \"non-sensitizer\", \"non-sensitiser\", \"i\", \"inactive\", \"n\", \"neg\", \"negative\", or \"0\" to indicate negative assay outcomes.*")
    )
  ),
  
  # neg_inf = p("Negative results should be labeled as \"Inf\"."),
  insilico_ad = p(
    "The column corresponding to this endpoint should only contain the values:",
    tags$ul(
      tags$li("\"in\" or \"1\" to indicate the chemical is within the AD.*"),
      tags$li("\"out\" or \"0\" to indicate the chemical is outside the AD.* Values for chemicals outside of the AD will not be evaluated."),
      tags$li("Missing values should be blank or labeled as \"NA\".")
    ),
    span(style = "font-size: 90%", "*Case insensitive")
    ),
  blr_cid = p("A chemical identfier must be entered in every row of the data worksheet. Results from the KE1, KE2, and KE3 assays will be matched based on these identifiers."),
  cys_ind = p(
    "The column corresponding to this endpoint should only contain the values:",
    tags$ul(
      tags$li("\" FALSE\", \" F\", \" No\", \"n\", or \"0\" to use the average of both depletion values.*"),
      tags$li("\"TRUE\", \"T\", \" Yes\", \" y\", or \" 1\" to use only the cysteine depletion value.*"),
      tags$li("Missing values should be blank or labeled as \"NA\".")
    ),
    span(style = "font-size: 90%", "*Case insensitive")
    ),
  blr_run = p("A run ID must be entered for every row of the data worksheet. The run ID is used to group results from the same run for evaluation."),
  conc_numeric = p("The column corresponding to concentration should only contain numeric values and will be sorted during the evaluation. Missing values should be blank or labeled as \"NA\".")
)

# MODAL LIST -----
info_modals <- list(
  ## Step 1: Select DA -----
  ### 2o3 -----
  bsModal(
    id = "info_2o3_modal",
    trigger = "info_2o3",
    title = "2 out of 3",
    p("The 2 out of 3 (2o3) DA predicts skin sensitization hazard based on at least 2 concordant results among a set of KE1, KE2, and KE3 assays. For more details, see", get_link[["gl497"]], "."),
    get_da_fig[["da2o3"]]
  ),
  ### ITS -----
  bsModal(
    id = "info_its_modal",
    trigger = "info_its",
    size = "large",
    title = "Integrated Testing Strategy",
    p("The Integrated Testing Strategy (ITS) DA predicts skin sensitization hazard and GHS potency category by scoring results from a KE1 assay, KE3 assay, and an in silico model. For more details, see", get_link[["gl497"]], "."),
    get_da_fig[["daits"]]
  ),
  ### KE 3/1 sts
  bsModal(
    id = "info_ke31_modal",
    trigger = "info_ke31",
    title = "Key Event 3/1 Sequential Testing Strategy",
    p("The Key Event 3/1 (KE 3/1) Sequential Testing Strategy (STS) is a sequential testing strategy that predicts skin sensitization hazard and GHS potency category. Predictions are based on the minimum induction threshold from the human cell line activation test (h-CLAT) and hazard results from the direct peptide reactivity assay (DPRA). For more details, see EPA's", get_link[["epaopp"]], "."),
    get_da_fig[["dake31"]]
  ),
  ### 2o3 Borderline -----
  bsModal(
    id = "info_da2o3_blr_modal",
    size = "large",
    trigger = "info_2o3_bl",
    title = "2 out of 3 Borderline Evaluation",
    p("The 2 out of 3 (2o3) DA uses hazard outcomes from a set of KE1, KE2, and KE3 assays. Results from the assays may fall within a 'borderline range' of the assay decision threshold, which can yield inconclusive 2o3 results. Assay-specific decision trees can be applied to generate conclusive positive, conclusive negative, and borderline outcomes. For more details, see", get_link[["gl497"]], "."),
    p("You may upload data from individual assay runs. The data will be processed using assay-specific decision trees and the results will be used to generate 2o3 predictions. Data formatting information is available in the next step and in the", get_link[["user_guide"]], ".")
  ),
  ## Step 2: Data -----
  ### XL WS Selector -----
  bsModal(
    id = "xl_select_modal",
    trigger = "select_sheet",
    title = "Excel sheet selection dialog box",
    selectInput(
      inputId = "xl_sheet_list",
      label = "Select the Excel worksheet to upload",
      choices = NULL,
      selectize = FALSE
    ),
    actionButton(inputId = "confirm_xl_sheet", label = "Upload Data"),
    actionButton(inputId = "cancel_xl_sheet", label = "Cancel")
  ),
  ### Demo Data -----
  bsModal(
    id = "demo_data_modal",
    title = "Demo Data",
    trigger = "info_demo",
    p("Select this option to load a demo data set instead of uploading your own data."),
    p("The data set includes values for all possible endpoints across the available DAs. The column names are set up so that the selections in the next step are automatically assigned.")
  ),
  ## Step 3: Select Columns -----
  ### Standard -----
  #### KE1 -----
  bsModal(
    id = "info_ke1Call_modal",
    trigger = "info_ke1Call",
    title = "Key Event 1 Assay Endpoint: Call",
    p("Binary hazard results (sensitizer or non-sensitizer) from a KE1 assay are used in the 2o3 and KE3/1 STS defined approaches."),
    format_info[["binary_call"]],
    format_info[["binary_call_ke1"]]
  ),
  bsModal(
    id = "info_ke1AssayName_modal",
    trigger = "info_ke1AssayName",
    title = "Key Event 1 Assay",
    p("Hazard identification or potency classification will be based on thresholds specific to the ADRA or DPRA KE1 test methods.")
  ),
  bsModal(
    id = "info_ke1DepValue_modal",
    title = "Key Event 1 Assay Endpoint: Mean Depletion Value",
    trigger = "info_ke1DepValue",
    p("The ADRA and DPRA measure depletion of two peptides containing either cysteine or lysine residues due to covalent binding. The ITS DA uses mean depletion values to predict potency."),
    p("You may provide either the mean depletion values or the individual cysteine and lysine depletion values."),
    format_info[["numeric"]]
  ),
  
  #### KE2 -----
  bsModal(
    id = "info_ke2Call_modal",
    trigger = "info_ke2Call",
    title = "Key Event 2 Assay Endpoint: Call",
    p("Binary hazard results (sensitizer or non-sensitizer) from a KE2 assay are used in the 2o3 defined approach."),
    format_info[["binary_call"]]
  ),
  
  #### KE3 -----
  bsModal(
    id = "info_ke3Call_modal",
    trigger = "info_ke3_call",
    title = "Key Event 3 Assay Endpoint: Call",
    p("Binary hazard results (sensitizer or non-sensitizer) from a KE3 assay are used in the 2o3 defined approach."),
    format_info[["binary_call"]]
  ),
  bsModal(
    id = "info_ke3_assayName_modal",
    trigger = "info_ke3_assayName",
    title = "Key Event 3 Assay",
    p("Potency classification will be based on thresholds specific to the GARDskin, h-CLAT, or U-SENS test methods.")
  ),
  bsModal(
    id = "info_ke3_value_modal",
    trigger = "info_ke3_value",
    title = "Key Event 3 Assay Endpoint: Quantitative Value",
    p("The ITS uses a quantiative endpoint from a KE3 assay to assign a score.",
      tags$dl(
        tags$dt("GARDskin: Input Concentration"),
        tags$dd(format_info[["ke3_value"]]),
        tags$dt("h-CLAT: Minimum Induction Threshold"),
        tags$dd(format_info[["ke3_value"]]),
        tags$dt("U-SENS: CD86 Induction EC150"),
        tags$dd(
          p("The concentration needed to reach a stimulation index of 150% for CD86 induction."),
          format_info[["ke3_value"]])
      ))
  ),
  #### In Silico Model -----
  bsModal(
    id = "info_insil_pred_modal",
    trigger = "info_insil_pred",
    title = "In Silico Model",
    p("The ITS uses in silico binary hazard results (sensitizer or non-sensitizer). You must provide the prediction and the applicability domain."),
    tags$dl(
      tags$dt("In Silico Call Prediction"),
      tags$dd(format_info[["binary_call"]]),
      tags$dt("In Silico Applicability Domain"),
      tags$dd(format_info[["insilico_ad"]])
    )
  ),
  ### Borderline -----
  #### KE1 -----
  bsModal(
    id = "info_blr_ke1AssayName_modal",
    trigger = "info_blr_ke1AssayName",
    title = "Key Event 1 Assay",
    p("The 2o3 uses results from a KE1 assay. You can apply decision trees specific to the ADRA or DPRA KE1 test methods."),
    div(
      class = "slideshow-container",
      div(
        class = "blr_caro_nav",
        actionLink(class = "prev", inputId = "ke1_blr_prev", HTML("&#9664;")),
        selectInput(
          inputId = "ke1_blr_caro_img",
          label = NULL,
          width = "20ch",
          choices = blr_label_ids[["ke1"]]
        ),
        actionLink(class = "next", inputId = "ke1_blr_next", HTML("&#9654;"))
      ),
      get_blr_fig[["ke1"]]
    )
  ),
  bsModal(
    id = "info_blr_ke1Columns_modal",
    trigger = "info_blr_ke1Columns",
    size = "large",
    title = "Key Event 1 Assay Data Columns",
    tags$details(
      open = "open",
      tags$summary("ADRA"),
      div(
        class = "detailsBody",
        tags$dl(
          tags$dt("Chemical Identifier"),
          tags$dd(format_info[["blr_cid"]]),
          tags$dt("NAC Depletion"),
          tags$dd(format_info[["numeric"]]),
          tags$dt("NAL Depletion"),
          tags$dd(format_info[["numeric"]]),
          tags$dt("Cys-Only Indicator"),
          tags$dd(
            p("Use this column to indicate whether only the NAC depletion value should be evaluated for the corresponding data row."),
            format_info[["cys_ind"]]
          )
        )
      )
    ),
    tags$details(
      tags$summary("DPRA"),
      div(
        class = "detailsBody",
        tags$dl(
          tags$dt("Chemical Identifier"),
          tags$dd(format_info[["blr_cid"]]),
          tags$dt("Cys Depletion"),
          tags$dd(format_info[["numeric"]]),
          tags$dt("Lys Depletion"),
          tags$dd(format_info[["numeric"]]),
          tags$dt("Cys-Only Indicator"),
          tags$dd(
            p("Use this column to indicate whether only the Lys depletion value should be evaluated for the corresponding data row."),
            format_info[["cys_ind"]]
          )
        )
      )
    )
  ),
  #### KE2 -----
  bsModal(
    id = "info_blr_ke2AssayName_modal",
    trigger = "info_blr_ke2AssayName",
    title = "Key Event 2 Assay",
    p("The 2o3 uses results from a KE2 assay. You can apply decision trees specific to the KeratinoSens or LuSens KE2 test methods."),
    div(
      class = "slideshow-container",
      div(
        class = "blr_caro_nav",
        actionLink(class = "prev", inputId = "ke2_blr_prev", HTML("&#9664;")),
        selectInput(
          inputId = "ke2_blr_caro_img",
          label = NULL,
          width = "20ch",
          choices = blr_label_ids[["ke2"]]
        ),
        actionLink(class = "next", inputId = "ke2_blr_next", HTML("&#9654;"))
      ),
      get_blr_fig[["ke2"]]
    )
  ),
  bsModal(
    id = "info_blr_ke2Columns_modal",
    trigger = "info_blr_ke2Columns",
    title = "Key Event 2 Assay Data Columns",
    tags$details(
      open = "open",
      tags$summary("KeratinoSens"),
      div(
        class = "detailsBody",
        tags$dl(
          tags$dt("Chemical Identifier"),
          tags$dd(format_info[["blr_cid"]])
        )
      )
    ),
    tags$details(
      tags$summary("LuSens"),
      div(
        class = "detailsBody",
        tags$dl(
          tags$dt("Chemical Identifier"),
          tags$dd(format_info[["blr_cid"]]),
          tags$dt("Run Identifier"),
          tags$dd(format_info[["blr_run"]]),
          tags$dt("Concentration (\u03BCM)"),
          tags$dd(format_info[["conc_numeric"]]),
          tags$dt("Fold Induction"),
          tags$dd(format_info[["numeric"]]),
          tags$dt("Cell Viability"),
          tags$dd(format_info[["numeric"]]),
          tags$dt("T Test p-value"),
          tags$dd(
            p("The p value corresponding to a statistical test comparing luciferase activity induction between the solvent/vehicle control and replicate sample."),
            format_info[["numeric"]]
          )
        )
      )
    )
  ),
  #### KE3 -----
  bsModal(
    id = "info_blr_ke3AssayName_modal",
    trigger = "info_blr_ke3AssayName",
    title = "Key Event 3 Assay",
    p("The 2o3 uses results from a KE3 assay. You can apply decision trees specific to the GARDskin, h-CLAT, IL-8 Luc, or U-SENS KE3 test methods."),
    div(
      class = "slideshow-container",
      div(
        class = "blr_caro_nav",
        actionLink(class = "prev", inputId = "ke3_blr_prev", HTML("&#9664;")),
        selectInput(
          inputId = "ke3_blr_caro_img",
          label = NULL,
          width = "20ch",
          choices = blr_label_ids[["ke3"]]
        ),
        actionLink(class = "next", inputId = "ke3_blr_next", HTML("&#9654;"))
      ),
      get_blr_fig[["ke3"]]
    )
  ),
  bsModal(
    id = "info_blr_ke3Columns_modal",
    trigger = "info_blr_ke3Columns",
    title = "Key Event 3 Assay Data Columns",
    tags$details(
      open = "open",
      tags$summary("GARDskin"),
      div(
        class = "detailsBody",
        tags$dl(
          tags$dt("Chemical Identifier"),
          tags$dd(format_info[["blr_cid"]]),
          tags$dt("Mean Decision Value"),
          tags$dd(format_info[["numeric"]])
        )
      )
    ),
    tags$details(
      tags$summary("h-CLAT"),
      div(
        class = "detailsBody",
        tags$dl(
          tags$dt("Chemical Identifier"),
          tags$dd(format_info[["blr_cid"]]),
          tags$dt("Run Identifier"),
          tags$dd(format_info[["blr_run"]]),
          tags$dt("Concentration (\u03BCM)"),
          tags$dd(format_info[["conc_numeric"]]),
          tags$dt("CD54 Relative Fluorescence Intensity"),
          tags$dd(format_info[["numeric"]]),
          tags$dt("CD86 Relative Fluorescence Intensity"),
          tags$dd(format_info[["numeric"]]),
          tags$dt("Viability"),
          tags$dd(format_info[["numeric"]])
        )
      )
    ),
    tags$details(
      tags$summary("IL-8 Luc"),
      div(
        class = "detailsBody",
        tags$dl(
          tags$dt("Chemical Identifier"),
          tags$dd(format_info[["blr_cid"]]),
          tags$dt("Run Identifier"),
          tags$dd(format_info[["blr_run"]]),
          tags$dt("Concentration (\u03BCM)"),
          tags$dd(format_info[["conc_numeric"]]),
          tags$dt("Ind-IL8LA"),
          tags$dd(format_info[["numeric"]]),
          tags$dt("Ind-IL8LA LCL"),
          tags$dd(p(
            "The lower limit of the 95% confidence interval of Ind_IL8LA.",
            format_info[["numeric"]])),
          tags$dt("Inh-GAPLA"),
          tags$dd(format_info[["numeric"]]),
          tags$dt("Solubility"),
          tags$dd(p(
            "The solubility of the chemical in the row. For each unique chemical identifier, only one row needs to have water solubility entered. The app will use the first valid, non-missing value.",
            format_info[["numeric"]]))
        )
      )
    ),
    tags$details(
      tags$summary("U-SENS"),
      div(
        class = "detailsBody",
        tags$dl(
          tags$dt("Chemical Identifier"),
          tags$dd(format_info[["blr_cid"]]),
          tags$dt("Run Identifier"),
          tags$dd(format_info[["blr_run"]]),
          tags$dt("Concentration (\u03BCM)"),
          tags$dd(format_info[["conc_numeric"]]),
          tags$dt("CD86 SI"),
          tags$dd(format_info[["numeric"]]),
          tags$dt("Viability"),
          tags$dd(format_info[["numeric"]])
        )
      )
    )
  ),
  ## Step 4: Review -----
  bsModal(
    id = "confirm_run_with_flag",
    title = "Warning",
    trigger = NULL,
    p("The selected columns have been flagged for invalid values. Invalid values will be considered missing (NA) and will", strong("not"), "be used to evaluate skin sensitization hazard identification or potency. Continue?"),
    actionButton(inputId = "run_with_flags", label = "Run"),
    actionButton(inputId = "cancel_run", label = "Cancel")
  ),
  ## Step 5: Results -----
  bsModal(
    id = "info_pink_modal",
    title = "Input and Calculated Columns",
    trigger = "info_pink",
    p("These are the actual values used for evaluation. It may be useful to review the selected columns and their transformations to ensure your data were properly interpreted, especially if the DAs were run with flagged data."),
    p("Binary call data are transformed to \"0\" for negative outcomes and \"1\" for positive outcomes.")
  ),
  bsModal(
    id = "info_blue_modal",
    title = "DA Columns",
    trigger = "info_blue",
    p("If the ITS DA was selected, then the individual ITS scores are also appended and highlighted in blue. The corresponding columns end with ‘Score’."),
    p("Hazard call predictions are ‘0’ if the result is negative and ‘1’ if the result is positive."),
    p("For assigning potency predictions, the DASS App uses categories established by the United Nations Globally Harmonized System for Classification and Labelling of Chemicals (GHS).")
  ),
  ## Compare -----
  bsModal(
    id = "info_perf_ref_select_modal",
    title = "Reference Data for Comparisons",
    trigger = "info_perf_ref_select"
  ),
  bsModal(
    id = "info_perf_ice_select_modal",
    title = "Reference Data from ICE",
    trigger = "info_perf_ice_select"
  ),
  bsModal(
    id = "info_perf_table_modal",
    title = "Performance Metrics",
    trigger = "info_perf_table",
    div(
      class = "hiddenBlock",
      id = "binary_defs",
      h2("Table Definitions", style = "font-size: 1em; text-align: center;"),
      HTML(
        "<table class = 'defTab' border=1>
<tr> <th> Metric </th> <th> Definition </th>  </tr>
<tr> <td> N </td> <td> The number of valid reference values </td>  </tr>
  <tr> <td> Accuracy  </td> <td>  (True positives + True negatives) / (All positives + All negatives) </td> </tr>
  <tr> <td> Balanced Accuracy  </td> <td>  (True positive rate + True negative rate)/2 </td> </tr>
  <tr> <td> F1 Score  </td> <td>  (2&times;True positives) / (2&times;True positives + False positives + False negatives) </td> </tr>
  <tr> <td> True Positive Rate (Sensitivity)  </td> <td>  True positives / All positives </td> </tr>
  <tr> <td> False Positive Rate  </td> <td>  False positives / All positives </td> </tr>
  <tr> <td> True Negative Rate (Specificity)  </td> <td>  True negatives / All negatives </td> </tr>
  <tr> <td> False Negative Rate  </td> <td>  False negatives / All negatives </td> </tr>
   </table>"
      )
    ),
    div(
      class = "hiddenBlock",
      id = "potency_defs",
      h2("Table Definitions", style = "font-size: 1em; text-align: center;"),
      HTML(
        "<table class = 'defTab' border=1>
         <tr> <th> Metric </th> <th> Definition </th>  </tr>
    <tr> <td> N </td> <td> The number of valid reference values </td>  </tr>
    <tr> <td> Accuracy  </td> <td>  The percentage of predicted values equal to reference values </td> </tr>
    <tr> <td> Overpredicted </td> <td>  The percentage of predicted values with a more potent GHS category than the corresponding reference value </td> </tr>
    <tr> <td> Underpredicted  </td> <td>  The percentage of predicted values with a less potent GHS category than the corresponding reference value </td> </tr>
     </table>"
      )
    )
  ),
  bsModal(
    id = "download_compare_tables_modal",
    title = "Download Performance Tables",
    trigger = "download_compare_tables",
    div(p("Confusion matrices and performance tables can be downloaded as a PDF file. Use the checkboxes to select the output you would like to download. More details about the performance output are available in the User Guide.")),
    div(
      actionLink(inputId = "perf_table_all", label = "Select All"), " | ",
      actionLink(inputId = "perf_table_none", label = "Deselect All"),
      checkboxGroupInput(inputId = "perf_table_choices", label = "Select Output to Download", choices = NULL),
      downloadButton(outputId = "dl_perf_tables", label = "Download Output")
    ),
    br(),
    div(
      p("You can download the results in table format as an Excel file or text file."),
      downloadButton(outputId = "perf_table_flat_xl", label = "Excel (.xlsx)", icon = icon("file-excel"), class = "btn-dl"),
      downloadButton(outputId = "perf_table_flat_txt", label = "Tab-Delimited (.txt) ", icon = icon("file-alt"), class = "btn-dl")
    )
  )
)
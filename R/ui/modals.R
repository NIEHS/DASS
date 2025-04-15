# Modal Functions -----
#' Create UI for modal
#' Assign call and potency using ITS scores
#' @param id The modal id. For info boxes, this should be {info box id}_modal.
#' @param title modal title
#' @param size 'm', 'l', or 'xl'
#' @param footer modal footer UI 
#' @param ... additional ui elements for the modal body
modal_ui <- function(id, title, ..., size = "m", footer = modalButton("Close")) {
  div(
    class = "modal fade",
    id = id,
    tabindex = "-1",
    # `aria-hidden`=T,
    div(
      class = switch(
        size,
        m="modal-dialog",
        l="modal-dialog modal-lg",
        xl="modal-dialog modal-xl"),
      div(
        class = "modal-content",
        div(
          class = "modal-header",
          title,
          tags$button(
            type = "button",
            class = "btn-close",
            `data-bs-dismiss` = "modal",
            `aria-label` = "Close"
          )
        ),
        div(
          class = "modal-body",
          list(...)
        ),
        div(
          class = "modal-footer",
          footer
        )
      )
    )
  )
}

#' toggle modal with given id
modal_toggle <- function(id, e = "toggle") {
  shinyjs::runjs(sprintf("$('#%s').modal('%s');", id, e))
}

# Links -----
get_link <- list(
  gl497 = list(
    url = "https://doi.org/https://doi.org/10.1787/b92879a4-en",
    label = "OECD Guideline No. 497 (TG497): Defined Approaches on Skin Sensitisation"
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
    url = "user_guide.html",
    label = "User Guide"
  )
)
get_link <- lapply(get_link, function(x) a(href = x[[1]], target = "_blank", class = "external-link", x[[2]]))

# DA Figures -----
get_da_fig <- list(
  da2o3 = list(
    url = "da_diagrams/2o3_diagram-v2.png",
    alt = "Diagram of 2o3 data interpretation procedure",
    caption = "\u2020 TG497 evaluates the 2o3 DA using DPRA, KeratinoSens, and h-CLAT. The additional key event assays are under evaluation for inclusion in TG497."
  ),
  daits = list(
    url = "da_diagrams/ITS_diagram-v2.png",
    alt = "Diagram of ITS data interpretation procedure",
    caption = "\u2020 TG497 evaluates the ITS DA using DPRA, h-CLAT, and either OECD QSAR Toolbox or Derek Nexus. The additional key event assays and in silico models are under evaluation for inclusion in TG497."
  ),
  dake31 = list(
    url = "da_diagrams/KE31STS_diagram-v1.png",
    alt = "Diagram of KE 3/1 STS data interpretation procedure",
    caption = ""
  )
)

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

# Data Formats -----
format_info <- list(
  ### three binary_call items. should only differ in how borderline is described
  binary_call = p(
    "The column corresponding to this endpoint should only contain the values:",
    tags$ul(
      tags$li("\"sensitizer\", \"sensitiser\", \"a\", \"active\", \"p\", \"pos\", \"positive\", or \"1\" to indicate positive assay outcomes.*"),
      tags$li("\"non-sensitizer\", \"non-sensitiser\", \"i\", \"inactive\", \"n\", \"neg\", \"negative\", or \"0\" to indicate negative assay outcomes.*"),
      tags$li("Missing values should be blank or labeled as \"NA\".")
    ),
    span(style = "font-size: 90%", "*Case insensitive")
  ),
  binary_call_2o3_ke13 = p(
    "The column corresponding to this endpoint should only contain the values:",
    tags$ul(
      tags$li("\"sensitizer\", \"sensitiser\", \"a\", \"active\", \"p\", \"pos\", \"positive\", or \"1\" to indicate positive assay outcomes.*"),
      tags$li("\"non-sensitizer\", \"non-sensitiser\", \"i\", \"inactive\", \"n\", \"neg\", \"negative\", or \"0\" to indicate negative assay outcomes.*"),
      tags$li("For the 2o3 DA only, \"borderline\", \"border\", or \"bl\" to indicate borderline assay outcomes.*"),
      tags$li("Missing values should be blank or labeled as \"NA\".")
    ),
    span(style = "font-size: 90%", "*Case insensitive")
  ),
  binary_call_2o3_ke2 = p(
    "The column corresponding to this endpoint should only contain the values:",
    tags$ul(
      tags$li("\"sensitizer\", \"sensitiser\", \"a\", \"active\", \"p\", \"pos\", \"positive\", or \"1\" to indicate positive assay outcomes.*"),
      tags$li("\"non-sensitizer\", \"non-sensitiser\", \"i\", \"inactive\", \"n\", \"neg\", \"negative\", or \"0\" to indicate negative assay outcomes.*"),
      tags$li("\"borderline\", \"border\", or \"bl\" to indicate borderline assay outcomes."),
      tags$li("Missing values should be blank or labeled as \"NA\".")
    ),
    span(style = "font-size: 90%", "*Case insensitive")
  ),
  ###
  binary_call_ke1 = p("Alternatively, depletion values can be used to derive binary calls using the prediction models in", get_link[["tg442c"]], "."),
  numeric = p("The column corresponding to this endpoint should only contain numeric values. Missing values should be blank or labeled as \"NA\"."),
  ke3_value = p(
    "The column corresponding this endpoint should only contain:",
    tags$ul(
      tags$li("Numeric values corresponding to positive assay outcomes"),
      tags$li("\"Inf\", \"NI\", \"non-sensitizer\", \"non-sensitiser\", \"i\", \"inactive\", \"n\", \"neg\", \"negative\", or \"0\" to indicate negative assay outcomes.*")
    )
  ),
  insilico_ad = p(
    "The column corresponding to this endpoint should only contain the values:",
    tags$ul(
      tags$li("\"in\" or \"1\" to indicate the chemical is within the AD.*"),
      tags$li("\"out\" or \"0\" to indicate the chemical is outside the AD.* Values for chemicals outside of the AD will not be evaluated."),
      tags$li("Missing values should be blank or labeled as \"NA\".")
    ),
    span(style = "font-size: 90%", "*Case insensitive")
  ),
  bl_cid = p("A chemical identfier must be entered in every row of the data worksheet. Results from the KE1, KE2, and KE3 assays will be matched based on these identifiers."),
  cys_ind = p(
    "The column corresponding to this endpoint should only contain the values:",
    tags$ul(
      tags$li("\" FALSE\", \" F\", \" No\", \"n\", or \"0\" to use the average of both depletion values.*"),
      tags$li("\"TRUE\", \"T\", \" Yes\", \" y\", or \" 1\" to use only the cysteine depletion value.*"),
      tags$li("Missing values should be blank or labeled as \"NA\".")
    ),
    span(style = "font-size: 90%", "*Case insensitive")
  ),
  bl_run = p("A run ID must be entered for every row of the data worksheet. The run ID is used to group results from the same run for evaluation."),
  conc_numeric = p("The column corresponding to concentration should only contain numeric values and will be sorted during the evaluation. Missing values should be blank or labeled as \"NA\"."),
  potency_ref = p("The column corresponding to this endpoint should only contain GHS potency categories \"1A\", \"1B\", or \"NC\".")
)

# BL Figures -----
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
      caption =  p("The direct peptide reactivity assay (DPRA) measures depletion of two peptides containing either cysteine or lysine residues due to covalent binding.The borderline range for the mean cysteine and lysine % depletion is [4.95, 8.32]. If only cysteine % depletion can be evaluated, the borderline range is [10.56, 17]. For more details, see", get_link[["tg442c"]], ".")
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

# Modals -----
modal_list <- tagList(
  ## Select DA -----
    modal_ui(
      id = "info_2o3_modal",
      title = h4("2 out of 3"),
      p("The 2 out of 3 (2o3) DA predicts skin sensitization hazard based on at least 2 concordant results among a set of KE1, KE2, and KE3 assays. For more details, see", get_link[["gl497"]], "."),
      get_da_fig[["da2o3"]]
    ),
    modal_ui(
      id = "info_its_modal",
      title = h4("Integrated Testing Strategy"),
      p("The Integrated Testing Strategy (ITS) DA predicts skin sensitization hazard and GHS potency category by scoring results from a KE1 assay, KE3 assay, and an in silico model. For more details, see", get_link[["gl497"]], "."),
      get_da_fig[["daits"]],
      size = "xl"
    ),
    modal_ui(
      id = "info_ke31_modal",
      title = h4("Key Event 3/1 Sequential Testing Strategy"),
      p("The Key Event 3/1 (KE 3/1) Sequential Testing Strategy (STS) is a sequential testing strategy that predicts skin sensitization hazard and GHS potency category. Predictions are based on the minimum induction threshold from the human cell line activation test (h-CLAT) and hazard results from the direct peptide reactivity assay (DPRA). For more details, see EPA's", get_link[["epaopp"]], "."),
      get_da_fig[["dake31"]]
    ),
    modal_ui(
      id = "info_2o3_bl_modal",
      title = h4("2 out of 3 Borderline Evaluation"),
      p("The 2 out of 3 (2o3) DA uses hazard outcomes from a set of KE1, KE2, and KE3 assays. Results from the assays may fall within a \"borderline range\" of the assay decision threshold, which can yield inconclusive 2o3 results. Assay-specific decision trees can be applied to generate conclusive positive, conclusive negative, and borderline outcomes. For more details, see", get_link[["gl497"]], "."),
      p("You may upload data from individual assay runs. The data will be processed using assay-specific decision trees and the results will be used to generate 2o3 predictions. Data formatting information is available in the next step and in the", get_link[["user_guide"]], ".")
    ),
  ## Data -----
  modal_ui(
    id = "info_demo_data_modal",
    title = h4("Demo Data"),
    p("Select this option to load a demo data set instead of uploading your own data."),
    p("For the standard workflow, the demo data set includes values for all possible endpoints across the available DAs. The column names are set up so that the selections in the next step are automatically assigned. There are additional columns in the data set that can be used to demo the \"Compare\" section of the app."),
    p("For the 2o3 borderline workflow, the demo data includes simulated data sets for the DPRA, KeratinoSens, and GARDskin. Use the worksheet dropdown list to view data for a specific assay. The worksheet and column names are set up so that the selections in the next step are automatically assigned. The demo data set also includes a worksheet that can be used to demo the 'Compare' section of the app. Chemical identifiers and values in this data set were arbitrarily assigned for demonstration purposes only.")
  ),
  ## Select Columns -----
  ### Standard -----
  modal_ui(
    id = "info_ke1_call_modal",
    title = h4("Key Event 1 Assay Endpoint: Call"),
    p("Binary hazard results (sensitizer or non-sensitizer) from a KE1 assay are used in the 2o3 and KE3/1 STS defined approaches."),
    format_info[["binary_call_2o3_ke13"]],
    format_info[["binary_call_ke1"]]
  ),
  modal_ui(
    id = "info_ke1_std_assay_modal",
    title = h4("Key Event 1 Assay"),
    p("For the 2o3, KE1 calls can be generated using quantitative data from either the ADRA or DPRA test methods. "),
    p("For the ITS, scores can be generated using quantitative data from either the ADRA or DPRA test methods.")
  ),
  modal_ui(
    id = "info_ke1_mean_dep_modal",
    title = h4("Key Event 1 Assay Endpoint: Mean Depletion Value"),
    p("For the KE1 depletion values, you may provide either the mean depletion values or the individual cysteine and lysine depletion values."),
    p("The 2o3 and ITS DAs can be used with data from ADRA or DPRA. The KE 3/1 STS DA can be used with data from the DPRA only."),
    format_info[["numeric"]],
    p("More details about the thresholds applied to the data are available in the user guide.")
  ),
  modal_ui(
    id = "info_ke2_std_call_modal",
    title = h4("Key Event 2 Assay Endpoint: Call"),
    p("Binary hazard results (sensitizer or non-sensitizer) from a KE2 assay are used in the 2o3 defined approach."),
    format_info[["binary_call_2o3_ke2"]]
  ),
  modal_ui(
    id = "info_ke3_call_modal",
    title = h4("Key Event 3 Assay Endpoint: Call"),
    p("Binary hazard results (sensitizer or non-sensitizer) from a KE3 assay are used in the 2o3 defined approach."),
    format_info[["binary_call_2o3_ke13"]]
  ),
  modal_ui(
    id = "info_ke3_std_assay_modal",
    title = h4("Key Event 3 Assay"),
    p("ITS scores for the the KE3 information source are assigned using thresholds specific to the GARDskin, h-CLAT, or U-SENS KE3 test methods.")
  ),
  modal_ui(
    id = "info_ke3_std_val_modal",
    title = h4("Key Event 3 Assay Endpoint: Quantitative Value"),
    p("The ITS uses a quantitative endpoint from a KE3 assay to assign a score. The KE 3/1 uses the minimum induction threshold from h-CLAT.",
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
  modal_ui(
    id = "info_insil_pred_modal",
    title = h4("In Silico Model"),
    p("The ITS uses in silico binary hazard results (sensitizer or non-sensitizer). You must provide the prediction and the applicability domain."),
    tags$dl(
      tags$dt("In Silico Call Prediction"),
      tags$dd(format_info[["binary_call"]]),
      tags$dt("In Silico Applicability Domain"),
      tags$dd(format_info[["insilico_ad"]])
    )
  ),
  ### Borderline -----
  modal_ui(
    id = "info_ke1_bl_assay_modal",
    title = h4("Key Event 1 Assay"),
    p("The 2o3 uses results from a KE1 assay. You can apply decision trees specific to the ADRA or DPRA KE1 test methods."),
    div(
      class = "slideshow-container",
      div(
        class = "blr_caro_nav",
        actionLink(class = "prev", inputId = "ke1_blr_prev", HTML("&#9664;")),
        pickerInput(
          inputId = "ke1_blr_caro_img",
          label = span(class = "sr-only", "Choose assay decision tree to view"),
          width = "fit-content",
          choices = blr_label_ids[["ke1"]],
        ),
        actionLink(class = "next", inputId = "ke1_blr_next", HTML("&#9654;"))
      ),
      get_blr_fig[["ke1"]]
    )
  ),
  modal_ui(
    id = "info_ke1_bl_columns_modal",
    title = h4("Key Event 1 Assay Data Columns"),
    p("During the evaluation, if depletion values indicate that a second run was required, but data from only one run is provided, the result will not be used in the 2o3."),
    tags$details(
      open = "open",
      tags$summary(h5("ADRA")),
      div(
        class = "detailsBody",
        tags$dl(
          tags$dt("Chemical Identifier"),
          tags$dd(format_info[["bl_cid"]]),
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
      tags$summary(h5("DPRA")),
      div(
        class = "detailsBody",
        tags$dl(
          tags$dt("Chemical Identifier"),
          tags$dd(format_info[["bl_cid"]]),
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
    ),
    size = "l"
  ),
  modal_ui(
    id = "info_ke2_bl_assay_modal",
    title = h4("Key Event 2 Assay"),
    p("The 2o3 uses results from a KE2 assay. You can provide data for the KeratinoSens and LuSens KE2 test methods."),
    div(
      class = "slideshow-container",
      div(
        class = "blr_caro_nav",
        actionLink(class = "prev", inputId = "ke2_blr_prev", HTML("&#9664;")),
        pickerInput(
          inputId = "ke2_blr_caro_img",
          label = span(class = "sr-only", "Choose assay decision tree to view"),
          width = "fit-content",
          choices = blr_label_ids[["ke2"]]
        ),
        actionLink(class = "next", inputId = "ke2_blr_next", HTML("&#9654;"))
      ),
      get_blr_fig[["ke2"]]
    ),
    size = "l"
  ),
  modal_ui(
    id = "info_ke2_bl_columns_modal",
    title = h4("Key Event 2 Assay Data Columns"),
    tags$details(
      open = "open",
      tags$summary(h5("KeratinoSens")),
      div(
        class = "detailsBody",
        tags$dl(
          tags$dt("Chemical Identifier"),
          tags$dd(format_info[["bl_cid"]])
        ),
        tags$dl(
          tags$dt("Outcome"),
          tags$dd("The overall outcome from the KeratinoSens evaluation template. Only one outcome should be provided for a chemical. The DASS App does not process KeratinoSens data and instead takes the borderline outcomes provided by the test method developers. The template is available as supplementary material to the KeratinoSens protocol in the", tags$a(href = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EURL-ECVAM/datasets/DBALM/LATEST/online/dbalm.html?search=155", target = "_blank", class = "external-link", "DB-ALM dataset"))
        )
      )
    ),
    tags$details(
      tags$summary(h5("LuSens")),
      div(
        class = "detailsBody",
        tags$dl(
          tags$dt("Chemical Identifier"),
          tags$dd(format_info[["bl_cid"]]),
          tags$dt("Run Identifier"),
          tags$dd(format_info[["blr_run"]], p("Each chemical should have data from at least two runs.")),
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
    ),
    size = "l"
  ),
  modal_ui(
    id = "info_ke3_bl_assay_modal",
    title = h4("Key Event 3 Assay"),
    p("The 2o3 uses results from a KE3 assay. You can apply decision trees specific to the GARDskin, h-CLAT, IL-8 Luc, or U-SENS KE3 test methods."),
    div(
      class = "slideshow-container",
      div(
        class = "blr_caro_nav",
        actionLink(class = "prev", inputId = "ke3_blr_prev", HTML("&#9664;")),
        pickerInput(
          inputId = "ke3_blr_caro_img",
          label = span(class = "sr-only", "Choose assay decision tree to view"),
          width = "fit-content",
          choices = blr_label_ids[["ke3"]]
        ),
        actionLink(class = "next", inputId = "ke3_blr_next", HTML("&#9654;"))
      ),
      get_blr_fig[["ke3"]]
    ),
    size = "l"
  ),
  modal_ui(
    id = "info_ke3_bl_columns_modal",
    title = h4("Key Event 3 Assay Data Columns"),
    tags$details(
      open = "open",
      tags$summary(h5("GARDskin")),
      div(
        class = "detailsBody",
        tags$dl(
          tags$dt("Chemical Identifier"),
          tags$dd(format_info[["bl_cid"]]),
          tags$dt("Mean Decision Value"),
          tags$dd(format_info[["numeric"]])
        )
      )
    ),
    tags$details(
      tags$summary(h5("h-CLAT")),
      div(
        class = "detailsBody",
        tags$dl(
          tags$dt("Chemical Identifier"),
          tags$dd(format_info[["bl_cid"]]),
          tags$dt("Run Identifier"),
          tags$dd(format_info[["blr_run"]], p("Each chemical should have data from at least two runs.")),
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
      tags$summary(h5("IL-8 Luc")),
      div(
        class = "detailsBody",
        tags$dl(
          tags$dt("Chemical Identifier"),
          tags$dd(format_info[["bl_cid"]]),
          tags$dt("Run Identifier"),
          tags$dd(format_info[["blr_run"]], p("Each chemical should have data from at least two runs.")),
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
      tags$summary(h5("U-SENS")),
      div(
        class = "detailsBody",
        tags$dl(
          tags$dt("Chemical Identifier"),
          tags$dd(format_info[["bl_cid"]]),
          tags$dt("Run Identifier"),
          tags$dd(format_info[["blr_run"]], p("Each chemical should have data from at least two runs.")),
          tags$dt("Concentration (\u03BCM)"),
          tags$dd(format_info[["conc_numeric"]]),
          tags$dt("CD86 SI"),
          tags$dd(format_info[["numeric"]]),
          tags$dt("Viability"),
          tags$dd(format_info[["numeric"]])
        )
      )
    ),
    size = "l"
  ),
  ## Review -----
  modal_ui(
    id = "run_warning",
    title = h4("Warning"),
    p("The selected columns have been flagged. Invalid values will be considered missing (NA) and will", strong("not"), "be included in the analysis. Continue?"),
    footer = tagList(
      actionButton(inputId = "confirm_run_dass", label = "Run"),
      modalButton("Cancel")
    )
  ),
  ## Results -----
  modal_ui(
    id = "table_key",
    title = h4("DA Result Table Key"),
    size = "l",
    tags$dl(
      tags$dt("Selected Data Columns (Yellow)"),
      tags$dd(
        "The data columns that you selected in Step 3 are yellow and the column names are annotated with an asterisk."
      ),
      tags$dt("DA Prediction Column(s) (Blue)"),
      tags$dd(
        p("DA predictions are appended to the table as blue columns. If the ITS DA was selected, then the individual and total ITS scores are also appended and highlighted in blue."),
        p("For assigning potency predictions, the DASS App uses categories established by the United Nations Globally Harmonized System for Classification and Labelling of Chemicals (GHS).")
      ),
      tags$dt("Processed Data Columns (Pink, Hidden by Default)"),
      tags$dd(
        p("The DASS App processes your data columns for evaluation."),
        tags$ul(
          tags$li("Binary hazard calls are transformed to \"0\" for negative outcomes and \"1\" for positive outcomes."),
          tags$li("Invalid numeric values are replaced with \"NA\"."),
          tags$li("For the ITS, negative KE3 results are replaced with \"Inf\"."),
          tags$li("For the ITS, applicability domain values are transformed to \"0\" to indicate \"out of domain\" and \"1\" to indicate \"in domain\".")
        ),
        p("In the results table, the processed data columns are appended to your data, but hidden by default. Use the column visibility dropdown list to show or hide these columns. The columns are pink and column names are the original column names appended with \"_input\"."),
        p("For values that were derived by the app (i.e., assay call, mean depletion values), the column name will end with \"calculated\"."),
        p("The values in these columns are the actual values used for evaluation. It may be useful to review the selected columns and their transformations to ensure your data were properly interpreted, especially if the DAs were run with flagged data.")
      )
    )
  ),
  ## Compare -----
  modal_ui(
    id = "info_compare_ref_modal",
    title = h4("Reference Data for Comparisons"),
    p(
      "For the standard worfklow, reference data should be included as additional columns in the file upload. For the borderline workflow, reference data should be included as an additional worksheet and include chemical identifiers to map to the DA results."
    ),
    p(
      tags$dl(
        tags$dt("Hazard"),
        tags$dd(format_info[["binary_call"]]),
        tags$dt("Potency"),
        tags$dd(format_info[["potency_ref"]]),
      )
    )
  ),
  modal_ui(
    id = "info_compare_ice_modal",
    title = h4("Reference Data from ICE"),
    p("You can compare the DA results against reference data that was used in the development of ", get_link[["gl497"]], ". The reference data were sourced from the Integrated Chemical Environment. The human reference data comprise human predictive patch test results for 66 chemicals. The local lymph node assay (LLNA) data comprise results for 156 chemicals."),
    p("For the standard workflow, you must provide chemical identifiers (CASRN, DTXSID, or QSAR-Ready SMILES). The selected identifier will be used to pair DA results and reference data for comparison."),
    p(""),
    p(
      "Downloads: ", br(),
      div(
        class = "sub-ind",
        tags$a(href = "ice_references/2024June13_OECD Defined Approach to Skin Sensitization Human (R).txt", "Download human reference data (.txt)", target = "_blank", download = NA), br(),
        tags$a(href = "ice_references/2024June13_OECD Defined Approach to Skin Sensitization Human (R)_metadata.txt", "Download human reference metadata (.txt)", target = "_blank", download = NA), br(),
        tags$a(href = "ice_references/2024June13_OECD Defined Approach to Skin Sensitization LLNA (R).txt", "Download LLNA reference data (.txt)", target = "_blank", download = NA), br(),
        tags$a(href = "ice_references/2024June13_OECD Defined Approach to Skin Sensitization LLNA (R)_metadata.txt", "Download LLNA reference metadata (.txt)", target = "_blank", download = NA))
    )
  ),
  modal_ui(
    id = "info_compare_tables_modal",
    title = h4("Performance Metric Table Definitions"),
    tags$details(
      id = "tabDef_hazard",
      tags$summary(h5("Hazard")),
      div(
        class = "detailsBody",
        tags$table(
          class = "cm-table",
          tags$thead(
            tags$tr(
              tags$th(scope = "col", "Metric"),
              tags$th(scope = "col", "Definition")
            )
          ),
          tags$tbody(
            tags$tr(tags$td("N"), tags$td("The number of valid reference-prediction pairs for comparison")),
            tags$tr(tags$td("True Positive"), tags$td("The number of true positive DA predictions")),
            tags$tr(tags$td("False Positive"), tags$td("The number of false positive DA predictions")),
            tags$tr(tags$td("False Negative"), tags$td("The number of false negative DA predictions")),
            tags$tr(tags$td("True Negative"), tags$td("The number of true negative DA predictions")),
            tags$tr(tags$td("Borderline"), tags$td("The number of borderline DA predictions")),
            tags$tr(tags$td("Inconclusive"), tags$td("The number of inconclusive DA predictions")),
            tags$tr(tags$td("True Positive Rate (Sensitivity)"), tags$td("True positives / All positives")),
            tags$tr(tags$td("True Negative Rate (Specificity)"), tags$td("True negatives / All negatives")),
            tags$tr(tags$td("Balanced Accuracy"), tags$td("(True positive rate + True negative rate)/2")),
            tags$tr(tags$td("Accuracy"), tags$td("(True positives + True negatives) / (All positives + All negatives)")),
            tags$tr(tags$td("F1 Score"), tags$td("(2×True positives) / (2×True positives + False positives + False negatives)"))
          )
        )
      )
    ),
    tags$details(
      id = "tabDef_potency",
      tags$summary(h5("Potency")),
      div(
        class = "detailsBody",
        tags$table(
          class = "cm-table",
          tags$thead(
            tags$tr(
              tags$th(scope = "col", "Metric"),
              tags$th(scope = "col", "Definition")
            )
          ),
          tags$tbody(
            tags$tr(tags$td("N"), tags$td("The number of valid reference-prediction pairs for comparison")),
            tags$tr(tags$td("Accuracy"), tags$td("The percentage of predicted values equal to reference values")),
            tags$tr(tags$td("Overpredicted"), tags$td("The percentage of predicted values with a more potent GHS category than the corresponding reference value")),
            tags$tr(tags$td("Underpredicted"), tags$td("The percentage of predicted values with a less potent GHS category than the corresponding reference value")),
            tags$tr(tags$td("Sensitivity"), tags$td("Among reference values with a given GHS category, the percentage of predicted values equal to the GHS category.")),
            tags$tr(tags$td("Specificity"), tags$td("Among reference values not equal to a given GHS category, the percentage of predicted values not equal to the GHS category.")),
            tags$tr(tags$td("Balanced Accuracy"), tags$td("(Sensitivity + Specificity)/2"))
          )
        )
      )
    ),
    size = "l"
  ),
  modal_ui(
    id = "download_compare_tables_modal",
    title = h4("Download Comparison Tables"),
    div(p("Confusion matrices and performance tables can be downloaded as a PDF file. Use the checkboxes to select the output you would like to download. More details about the performance output are available in the User Guide.")),
    div(
      actionLink(inputId = "comp_table_all", label = "Select All"), " | ",
      actionLink(inputId = "comp_table_none", label = "Deselect All"),
      checkboxGroupInput(inputId = "comp_table_choices", label = "Select Output to Download", choices = NULL, width = "100%"),
      downloadButton(outputId = "dl_comp_tables", label = "Download Output")
    ),
    br(),
    div(
      p("You can download the flat table as an Excel file or text file."),
      downloadButton(outputId = "comp_table_flat_xl", label = "Excel (.xlsx)", icon = icon("file-excel"), class = "btn-dl"),
      downloadButton(outputId = "comp_table_flat_txt", label = "Tab-Delimited (.txt) ", icon = icon("file-alt"), class = "btn-dl")
    )
  ),
  modal_ui(
    id = "info_comp_fig_modal",
    title = h4("Visualizing Results"),
    p("Two types of figures can be generated."),
    p("If the quantitative endpoint column is \"None\", a bar chart will summarize the comparison results. You can view a bar chart for a specific comparison by selecting the reference from the first dropdown list. If  \"All\" is selected, all comparisons will be shown."),
    p("If a quantitative endpoint column is selected, the quantitative data values be plotted in a scatter plot next to a density curve, with data split based on the comparison result. This may provide useful context for exploring your results.")
  )
)
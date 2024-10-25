source("R/modals.R")

abbrev <- c(
  da_2o3 = "2o3",
  da_its = "ITS",
  da_ke31 = "KE 3/1 STS",
  
  adra = "ADRA",
  dpra = "DPRA",
  ks = "KeratinoSens",
  lusens = "LuSens",
  gard = "GARDskin",
  gardskin = "GARDskin",
  hclat = "h-CLAT",
  il8 = "IL-8 Luc",
  usens = "U-SENS"
)

column_text_labels <- c(
  ke1_call_col = "KE1 Call",
  ke1_mean_c_l_dep_col = "KE1 Mean Depletion",
  ke1_c_dep_col = "KE1 Cys/NAC Depletion",
  ke1_l_dep_col = "KE1 Lys/NAL Depletion",
  ke2_call_col = "KE2 Call",
  ke3_call_col = "KE3 Call",
  ke3_val_col = "KE3 Quantiative Value",
  insil_call_col = "In Silico Call",
  insil_ad_col = "In Silico Applicability Domain",
  
  ke1_blr_cid_col = "Chemical Identifier",
  ke1_blr_c_dep_col = "Cys/NAC Depletion (%)",
  ke1_blr_l_dep_col = "Lys/NAL Depletion (%)",
  ke1_blr_c_only_col = "Cys-Only Indicator",
  
  ke2_blr_cid_col = "Chemical Identifier",
  ke2_blr_ks_call_col = "KeratinoSens Outcome",
  ke2_blr_run_col = "Run Identifier",
  ke2_blr_conc_col = "Concentration",
  ke2_blr_fi_col = "Fold Induction",
  ke2_blr_cv_col = "Cell Viability (%)",
  ke2_blr_p_col = "T Test p-value",
  
  ke3_blr_cid_col = "Chemical Identifier",
  
  ke3_blr_gard_meanDV_col = "GARDSkin Mean DV",
  ke3_blr_run_col = "Run Identifier",
  ke3_blr_conc_col = "Concentration",
  ke3_blr_hclat_cd54_col = "H-CLAT CD54 RFI",
  ke3_blr_hclat_cd86_col = "H-CLAT CD86 RFI",
  ke3_blr_hclat_cv_col = "H-CLAT Viability (%)",
  
  ke3_blr_il8_ind_col = "IL-8 Luc Ind-IL8LA",
  ke3_blr_il8_indLCL_col = "IL-8 Luc Ind-IL8LA LCL",
  ke3_blr_il8_inh_col = "IL-8 Luc Inh-GAPLA",
  ke3_blr_il8_ws_col = "IL-8 Luc Solubility",
  
  ke3_blr_usens_cd86_col = "U-SENS CD86 SI",
  ke3_blr_usens_cv_col = "U-SENS Viability (%)"
)

tabNames <- c(
  "Select Defined Approach",
  "Upload Data",
  "Select Data Columns",
  "Review Selection",
  "Results",
  "Compare"
)

# Create page
# Welcome -----
welcome_panel <- fluidRow(tags$header(
  class = "header-row",
  div(
    class = "header-details-panel",
    div(class = "page-title", h1("The DASS App")),
    tags$div(class = "page-details", tags$section(
      p(
        "The DASS App applies defined approaches on skin sensitization (DASS)",
        "to predict skin sensitization hazard (sensitizer or non-sensitizer)",
        "and potency (based on UN GHS categories). The defined approaches (DA)",
        "generate predictions by integrating data from in vitro assays that",
        "represent key events in the Adverse Outcome Pathway for Skin",
        "Sensitization and in silico hazard predictions."
      ),
      p(
        "For more information about DASS and their regulatory applications, visit the",
        tags$abbr(title = "NTP Interagency Center for the Evaluation of Alternative Toxicological Methods", "NICEATM"),
        tags$a(
          href = "https://ntp.niehs.nih.gov/go/40498",
          "Defined Approaches to Identify Potential Skin Sensitizers",
          tags$span(class = "sr-only", "Opens new window.")
        ),
        "webpage."
      ),
    ))
  ),
  div(
    class = "link-panel",
    h2(class = "sr-only", "Links"),
    tags$ul(
      class = "link-list",
      tags$li(
        tags$a(
          class = "btn btn-default external-link",
          id = "user_guide_button",
          href = "user_guide.html",
          target = "_blank",
          "User Guide",
          tags$span(class = "sr-only", "Opens in new window.")
        )
      ),
      tags$li(
        tags$a(
          class = "btn btn-default",
          href = "mailto:ICE-support@niehs.nih.gov",
          target = "_blank",
          "Contact Us",
          tags$span(class = "sr-only", "Opens default mail client.")
        )
      ),
      tags$li(
        tags$a(
          class = "btn btn-default external-link",
          href = "https://doi.org/10.1186/s12859-023-05617-1",
          target = "_blank",
          "Manuscript",
          tags$span(class = "sr-only", "External link. Opens in new window.")
        )
      ),
      tags$li(
        tags$a(
          class = "btn btn-default external-link",
          href = "https://rstudio.niehs.nih.gov/dass/",
          target = "_blank",
          "Launch App in New Window",
          tags$span(class = "sr-only", "External link.")
        )
      )
    ),
    p(
      style = "font-size: 12px",
      tags$a(
        class = "external-link",
        style = "color: white",
        href = "https://github.com/NIEHS/DASS",
        target = "_blank",
        "Source Code",
        tags$span(class = "sr-only", "External link. Opens in new window.")
      ),
      br(),
      "Last updated: 2024-Oct-25"
    )
  )
))

# Step 1: Select DAs -----
select_da_panel <- tabPanel(
  title = tabNames[1],
  fluidRow(
    class = "bordered-panel",
    column(
      width = 12,
      tags$h2(class = "sr-only", "Step 1. Select Defined Approach."),
      div(
        radioButtons(
          inputId = "selected_da",
          width = "100%",
          label = "To begin, select the DA to be implemented. Click on the information buttons next to the DA names to view a description of the DA and the test methods required to implement the DA.",
          choiceValues = c("da_2o3", "da_its", "da_ke31"),
          choiceNames = list(
            HTML(
              "<span id = '2o3_radio_label'>2 out of 3 (2o3)</span>",
              info_button("info_2o3", "2o3 information")
            ),
            HTML(
              "<span id = 'its_radio_label'>Integrated Testing Strategy (ITS)</span>",
              info_button("info_its", "ITS information")
            ),
            HTML(
              "<span id = 'ke31_radio_label'>Key Event 3/1 (KE 3/1) Sequential Testing Strategy (STS)</span>",
              info_button("info_ke31", "STS information")
            )
          )
        ),
        conditionalPanel(
          hr(width = "50%"),
          condition = "input.selected_da=='da_2o3'",
          checkboxGroupInput(
            inputId = "do_da_2o3_bl",
            label = "2o3 Borderline Evaluation",
            choiceNames = tagList(
              HTML(
                "<span id = '2o3_bl_cb_label'>Flag borderline assay results prior to applying DA 2o3 (Requires data from individual runs)</span>",
                info_button("info_2o3_bl", "Information about borderline results for DA 2o3.")
              )
            ),
            choiceValues = T,
            width = "100%"
          )
        )
      )
    ),
    actionButton(
      inputId = "confirm_da",
      label = "Confirm DA Selection",
      width = "100%"
    )
  ))

# Step 2: Upload Data -----
upload_data_panel <- tabPanel(
  title = tabNames[2],
  tags$h2(class = "sr-only", "Step 2. Upload Data"),
  fluidRow(
    id = "upload_data_ui",
    class = "bordered-panel hiddenBlock",
    column(
      width = 12,
      HTML(
        "<div class='warn-block'>",
        "<div>",
        "<i class='glyphicon glyphicon-exclamation-sign' role='presentation'></i>",
        "</div>",
        "<div>",
        "<p style='margin-bottom:0;'>Before uploading your file, ensure that the data meet the data and formatting requirements. More details are available in the user guide.</p>",
        "</div>",
        "</div>"
      ),
    ),
    br(),
    ## Hidden - Data Text -----
    column(
      width = 12,
      div(
        class = "hiddenBlock",
        id = "upload_data_text",
        p(
          "A table template is provided in tab-delimited or Excel format.",
          "The template contains columns for every possible assay endpoint.",
          "If an assay endpoint will not be used, the corresponding column can be",
          "deleted but that is not required. Using the template is not required."
        ),
        a(
          href = "DASSApp-dataTemplate.xlsx",
          "Download Data Template (.xlsx)",
          download = NA,
          target = "_blank"
        ),
        br(),
        a(
          href = "DASSApp-dataTemplate.txt",
          "Download Data Template (.txt)",
          download = NA,
          target = "_blank"
        )
      ),
      div(
        class = "hiddenBlock",
        id = "upload_blr_data_text",
        p(
          "A data template is provided in Excel format.",
          "The template contains one worksheet template for every assay",
          "and each worksheet shows the information needed to perform",
          "the borderline evaluation."
        ),
        a(
          href = "DASSApp-dataTemplate-borderline.xlsx",
          "Download Data Template (.xlsx)",
          download = NA,
          target = "_blank"
        )
      ),
      hr(width = "50%")
    ),
    column(
      width = 12,
      id = "upload_block",
      # Generates read-only input with text after Browse. SR flagged b/c no label. 
      fileInput(
        inputId = "fpath",
        label = "Click 'Browse' below and select your file.",
        width = "100%"
      ),
      uiOutput("xl_sheet_text_ui")
    ),
    column(
      width = 12,
      div(
        class = "hiddenBlock",
        id = "use_demo_data_cb",
        HTML(
          "<div class='form-group shiny-input-container' style='width:100%;'>",
          "<div class='checkbox'>",
          "<label>",
          "<input id='use_demo_data' type='checkbox'/>",
          "<span>Use demo data</span>",
          "</label>",
          "<button id='info_demo' type='button' class='btn action-link btn-qs' aria-label='demo data info'>",
          "<i class='glyphicon glyphicon-question-sign' role='presentation'> </i>",
          "</button>",
          "</div>",
          "</div>"
        )
      )
    ),
    column(
      width = 12,
      id = "data_block",
      class = "hiddenBlock",
      hr(style = "width:50%"),
      div(
        class = "hiddenBlock",
        id = "blr_data_worksheet_select_block",
        span(
          id = "blr_ws_warn",
          class = "warningText",
          tags$strong("WARNING: Expected at least three worksheets in file.")
        ),
        selectInput(
          inputId = "blr_data_worksheet_select",
          label = "Select worksheet to view",
          choices = NULL,
          selectize = F
        )
      ),
      DT::dataTableOutput("dt_analyze"),
      hr(style = "width:50%"),
      p(
        "Once you have finished selecting the DAs and uploading your data, click 'Continue' to proceed to the next step."
      ),
      actionButton(
        inputId = "confirm_data",
        label = "Continue",
        width = "100%"
      )
    )
  )
)

# Step 3: Select Columns -----
select_columns_panel <- tabPanel(
  title = tabNames[3],
  fluidRow(
    id = "select_col_ui_all",
    class = "bordered-panel hiddenBlock",
    column(12, uiOutput("data_da_header")),
    ## Standard -----
    column(
      id = "select_col_ui",
      width = 12,
      p(
        "Below, there are sections corresponding to the information sources",
        "required for your selected DA. Within each section, use the",
        "dropdown lists to select the columns containing the data for",
        "the endpoint shown. Columns are automatically selected for an",
        "endpoint if the column name matches the corresponding column name",
        "in the data template. A column must be selected for each endpoint",
        "shown. When you are finished, click \"Done\"."
      ),
      p(
        "Click on the information buttons next to the dropdown list label",
        "to view information about the required data and formatting.",
        "Values that are incorrectly formatted or invalid will be treated as",
        "missing data and may affect the results.",
        "More details are given in the User Guide."
      ),
      ### KE1 -----
      div(id = "ke1_select_ui", tags$details(
        open = "open",
        tags$summary("Key Event 1"),
        div(
          class = "detailsBody",
          div(
            id = "ke1_call_select",
            tags$h2(span("KE1 Call"), info_button("info_ke1Call", "KE1 Call information")),
            div(
              class = "sub-ind",
              checkboxInput(
                inputId = "ke1_call_interpret",
                label = "Derive call from quantitative data.",
                value = F
              ),
              selectInput(
                inputId = "ke1_call_col",
                label = "Call Column",
                choices = NULL,
                selectize = F
              )
            )
          ),
          # Keep separate to hide for KE 3/1 STS
          div(
            id = "ke1_assay_select",
            tags$h2(
              "KE1 Assay",
              info_button("info_ke1AssayName", "KE1 Assay Information")
            ),
            div(
              class = "sub-ind",
              radioButtons(
                inputId = "ke1_assay_name",
                label = "Select KE1 Assay",
                choiceNames = c("ADRA", "DPRA"),
                choiceValues = c("adra", "dpra"),
                inline = T
              )
            )
          ),
          
          div(
            id = "ke1_dep_select",
            tags$h2(
              span("KE1 Mean Depletion Value"),
              info_button("info_ke1DepValue", "KE1 Mean Depletion Value Information")
            ),
            div(
              class = "sub-ind",
              checkboxInput(
                inputId = "ke1_choose_dep",
                label = "Derive mean depletion value from data.",
                value = F
              ),
              selectInput(
                inputId = "ke1_mean_c_l_dep_col",
                label = "Mean Depletion Column",
                choices = NULL,
                selectize = F
              ),
              selectInput(
                inputId = "ke1_c_dep_col",
                label = "Cys/NAC Depletion Column",
                choices = NULL,
                selectize = F
              ),
              selectInput(
                inputId = "ke1_l_dep_col",
                label = "Lys/NAL Depletion Column",
                choices = NULL,
                selectize = F
              )
            )
          )
          
        )
      )),
      ### KE2 -----
      div(id = "ke2_select_ui", tags$details(
        open = "open",
        tags$summary("Key Event 2"),
        div(class = "detailsBody", div(
          id = "ke2_call_select",
          tags$h2("KE2 Call", info_button("info_ke2Call", "KE2 Call information")),
          div(
            class = "sub-ind",
            selectInput(
              inputId = "ke2_call_col",
              label = "Call Column",
              choices = NULL
            )
          )
          
        ))
      )),
      ### KE3 -----
      div(id = "ke3_select_ui", tags$details(
        open = "open",
        tags$summary("Key Event 3"),
        div(
          class = "detailsBody",
          div(
            id = "ke3_call_select",
            tags$h2(
              "KE3 Call",
              info_button("info_ke3_call", "KE3 Call information")
            ),
            div(
              class = "sub-ind",
              selectInput(
                inputId = "ke3_call_col",
                label = "Call Column",
                choices = NULL,
                selectize = F
              )
            )
          ),
          div(
            id = "ke3_val_select",
            div(
              id = "ke3_assay_select",
              tags$h2(
                "KE3 Assay",
                info_button("info_ke3_assayName", "KE3 Assay Information")
              ),
              div(
                class = "sub-ind",
                radioButtons(
                  inputId = "ke3_assay_name",
                  label = "Select KE3 Assay",
                  choiceNames = c("GARDskin", "h-CLAT", "U-SENS"),
                  choiceValues = c("gardskin", "hclat", "usens"),
                  inline = T
                )
              )
            ),
            div(
              id = "ke3_quant_ui",
              tags$h2(
                span("KE3 Quantitative Endpoint"),
                info_button("info_ke3_value", "KE3 Value information")
              ),
              div(
                class = "sub-ind",
                selectInput(
                  inputId = "ke3_val_col",
                  label = "Quantitative Endpoint Column",
                  choices = NULL,
                  selectize = F
                )
              )
            )
          )
        )
      )),
      ### In Silico -----
      div(
        id = "insil_select_ui",
        class = "ui_is",
        tags$details(
          open = "open",
          tags$summary("In Silico Model"),
          div(
            class = "detailsBody",
            tags$h2(
              "In Silico Call Prediction",
              info_button("info_insil_pred", "In Silico Prediction information")
            ),
            div(
              class = "sub-ind",
              selectInput(
                inputId = "insil_call_col",
                label = "Call Prediction Column",
                choices = NULL,
                selectize = F
              ),
              selectInput(
                inputId = "insil_ad_col",
                label = "Applicability Domain Column",
                choices = NULL,
                selectize = F
              )
            )
          )
        )
      )
    ),
    ## Borderline -----
    column(
      id = "select_blr_col_ui",
      width = 12,
      p(
        "Below, there are three sections corresponding to the three key events that underly the 2o3 DA. Within each section, specify the assay for which you are providing data and use the dropdown list to select the worksheet that contains the data for the selected assay. Dropdown lists will be displayed for the data columns required for the specific assay. The dropdown lists will be populated with column names from the selected worksheet. Use these dropdown lists to specify the columns containing data for the endpoints shown. When you are finished, click \"Done\""
      ),
      p(
        "Click on the information buttons to view information about the required data and formatting. Values that are incorrectly formatted or invalid will be treated as missing data and may affect the results. More details are given in the User Guide."
      ),
      ### KE1 -----
      div(id = "ke1_select_blr_ui", tags$details(
        open = "open",
        tags$summary("Key Event 1"),
        div(
          class = "detailsBody",
          div(
            id = "ke1_assay_select_blr",
            tags$h2(
              "KE1 Assay",
              info_button("info_blr_ke1AssayName", "KE1 Assay Information")
            ),
            div(
              class = "sub-ind",
              radioButtons(
                inputId = "ke1_blr_assay_name",
                label = "Select KE1 Assay",
                choiceNames = c("ADRA", "DPRA"),
                choiceValues = c("adra", "dpra"),
                inline = T
              ),
              selectInput(
                inputId = "ke1_blr_ws",
                label = "KE1 Worksheet",
                choices = NULL
              )
            ),
            tags$h2(
              "KE1 Data Columns",
              info_button("info_blr_ke1Columns", "KE1 Data Column Information")
            ),
            div(
              class = "sub-ind",
              selectInput(
                inputId = "ke1_blr_cid_col",
                label = "Chemical Identifier Column",
                choices = NULL
              ),
              selectInput(
                inputId = "ke1_blr_c_dep_col",
                label = "Cys/NAC Depletion (%) Column",
                choices = NULL
              ),
              selectInput(
                inputId = "ke1_blr_l_dep_col",
                label = "Lys/NAL Depletion (%) Column",
                choices = NULL
              ),
              selectInput(
                inputId = "ke1_blr_c_only_col",
                label = "Cys-Only Indicator Column",
                choices = NULL
              )
            )
          )
        )
      )),
      ### KE2 -----
      div(id = "ke2_select_blr_ui", tags$details(
        open = "open",
        tags$summary("Key Event 2"),
        div(
          class = "detailsBody",
          div(
            id = "ke2_assay_select_blr",
            tags$h2(
              "KE2 Assay",
              info_button("info_blr_ke2AssayName", "KE2 Assay Information")
            ),
            div(
              class = "sub-ind",
              radioButtons(
                inputId = "ke2_blr_assay_name",
                label = "KE2 Assay",
                choiceNames = c("KeratinoSens", "LuSens"),
                choiceValues = c("ks", "lusens"),
                inline = T
              ),
              selectInput(
                inputId = "ke2_blr_ws",
                label = "KE2 Worksheet",
                choices = NULL
              )
            ),
            tags$h2(
              "KE2 Data Columns",
              info_button("info_blr_ke2Columns", "KE2 Data Column Information")
            ),
            div(
              class = "sub-ind",
              selectInput(
                inputId = "ke2_blr_cid_col",
                label = "Chemical Identifier Column",
                choices = NULL
              ),
              selectInput(
                inputId = "ke2_blr_ks_call_col",
                label = "KeratinoSens Outcome Column",
                choices = NULL
              ),
              div(
                id = "ke2_blr_lusens_col_select",
                selectInput(
                  inputId = "ke2_blr_run_col",
                  label = "Run Identifier Column",
                  choices = NULL
                ),
                selectInput(
                  inputId = "ke2_blr_conc_col",
                  label = "Concentration Column",
                  choices = NULL
                ),
                selectInput(
                  inputId = "ke2_blr_fi_col",
                  label = "Fold Induction Column",
                  choices = NULL
                ),
                selectInput(
                  inputId = "ke2_blr_cv_col",
                  label = "Cell Viability (%) Column",
                  choices = NULL
                ),
                selectInput(
                  inputId = "ke2_blr_p_col",
                  label = "T Test p-value Column",
                  choices = NULL
                )
              )
            )
          )
        )
      )),
      ### KE3 -----
      div(id = "ke3_select_blr_ui", tags$details(
        open = "open",
        tags$summary("Key Event 3"),
        div(
          class = "detailsBody",
          div(
            id = "ke3_assay_select_blr",
            tags$h2(
              "KE3 Assay",
              info_button("info_blr_ke3AssayName", "KE3 Assay Information")
            ),
            div(
              class = "sub-ind",
              radioButtons(
                inputId = "ke3_blr_assay_name",
                label = "KE3 Assay",
                choiceNames = c("GARDskin", "h-CLAT", "IL-8 Luc", "U-SENS"),
                choiceValues = c("gard", "hclat", "il8", "usens"),
                inline = T
              ),
              selectInput(
                inputId = "ke3_blr_ws",
                label = "KE3 Worksheet",
                choices = NULL
              )
            ),
            tags$h2(
              "KE3 Data Columns",
              info_button("info_blr_ke3Columns", "KE3 Data Column Information")
            ),
            div(
              class = "sub-ind",
              selectInput(
                inputId = "ke3_blr_cid_col",
                label = "Chemical Identifier Column",
                choices = NULL
              ),
              
              selectInput(
                inputId = "ke3_blr_run_col",
                label = "Run Identifier Column",
                choices = NULL
              ),
              selectInput(
                inputId = "ke3_blr_conc_col",
                label = "Concentration Column",
                choices = NULL
              ),
              selectInput(
                inputId = "ke3_blr_gard_meanDV_col",
                label = "GARDSkin Mean DV Column",
                choices = NULL
              ),
              div(
                id = "ke3_blr_hclat_col_select",
                selectInput(
                  inputId = "ke3_blr_hclat_cd54_col",
                  label = "H-CLAT CD54 RFI Column",
                  choices = NULL
                ),
                selectInput(
                  inputId = "ke3_blr_hclat_cd86_col",
                  label = "H-CLAT CD86 RFI Column",
                  choices = NULL
                ),
                selectInput(
                  inputId = "ke3_blr_hclat_cv_col",
                  label = "H-CLAT Viability (%) Column",
                  choices = NULL
                )
              ),
              div(
                id = "ke3_blr_usens_col_select",
                selectInput(
                  inputId = "ke3_blr_usens_cd86_col",
                  label = "U-SENS CD86 SI Column",
                  choices = NULL
                ),
                selectInput(
                  inputId = "ke3_blr_usens_cv_col",
                  label = "U-SENS Viability (%) Column",
                  choices = NULL
                )
              ),
              div(
                id = "ke3_blr_il8_col_select",
                selectInput(
                  inputId = "ke3_blr_il8_ind_col",
                  label = "IL-8 Luc Ind-IL8LA Column",
                  choices = NULL
                ),
                selectInput(
                  inputId = "ke3_blr_il8_indLCL_col",
                  label = "IL-8 Luc Ind-IL8LA LCL Column",
                  choices = NULL
                ),
                selectInput(
                  inputId = "ke3_blr_il8_inh_col",
                  label = "IL-8 Luc Inh-GAPLA Column",
                  choices = NULL
                ),
                selectInput(
                  inputId = "ke3_blr_il8_ws_col",
                  label = "IL-8 Luc Solubility Column",
                  choices = NULL
                )
              )
            )
          )
        )
      ))
    ),
    fluidRow(
      actionButton(
        inputId = "review_entries",
        label = "Done",
        width = "100%"
      )
    )
  )
)

# Step 4: Review Selection -----
review_selection_panel <- tabPanel(
  title = tabNames[4],
  fluidRow(
    id = "review_contents_ui",
    class = "bordered-panel hiddenBlock",
    fluidRow(
      id = "review_contents",
      class = "hiddenBlock",
      column(
        width = 12,
        p(
          "The table below summarizes your selections for each of the required endpoints. Values in the selected column are evaluated against formatting requirements. The third column \"Flagged\" will be \"true\" if invalid values were found in the corresponding column and \"false\" otherwise. When you are done reviewing your selections, click 'Run' to run the DASS. If you need to change a selected column, return to the 'Select Data Columns' page. If you need to upload new or updated data, return to the 'Upload Data' page."
        ),
        uiOutput("review_contents_standard_ui")
      )
    ),
    ## Borderline -----
    fluidRow(
      id = "review_contents_blr",
      class = "hiddenBlock",
      column(
        width = 12,
        p(
          "Your selections are summarized below. The KE assay sections show a table with your data column selections. Values in these columns are checked against formatting requirements and flagged if any issues are identified. Chemical identifier columns are flagged if any chemical identifiers do not have enough data to perform the 2o3."
        ),
        p(tags$strong("Note. The data are not checked for the appropriate number of runs. If data from too few or too many runs are provided, an overall outcome will not be returned.")),
        p(
          "The last section lists the unique chemical identifiers from your data and indicates which worksheets contain those identifiers. The identifier is flagged if it does not have values in at least two worksheets."
        ),
        p(
          "Review your selections. When you are done, click 'Run' to run the DASS. If you need to change a selection, return to the 'Select Data Columns' page. If you need to upload new or updated data, return to the 'Upload Data' page."
        ),
        uiOutput("review_contents_blr_ui")
      )
    ),
    actionButton(
      inputId = "run_dass",
      width = "100%",
      label = "Run"
    )
  )
)

# Step 5: Results -----
results_panel <- tabPanel(
  title = tabNames[5],
  fluidRow(
    id = "result_contents",
    class = "bordered-panel hiddenBlock",
    div(
      id = "result_contents_standard",
      class = "hiddenBlock",
      column(
        width = 12,
        p("Results of the DASS App analysis are appended to your data in the table below. By default, the table shows the first three columns of data, your selected input columns, and the DA results. The buttons above the table can be used to hide or show columns. Use the \"Download Results\" button to export your results to an Excel spreadsheet or text file, which may allow for easier viewing.")
      ),
      fluidRow(column(
        12,
        div(
          class = "dropdown",
          id = "dlDropdown",
          style = "margin-bottom: 1em",
          tags$button(
            class = "btn dropbtn btn-default",
            "Download Results",
            HTML("<i class='fas fa-caret-down' role='presentation'> </i>")
          ),
          div(
            class = "dropdown-content",
            downloadButton(
              outputId = "downloadres_xl",
              "Excel (.xlsx)",
              icon = icon("file-excel"),
              class = "btn-dl"
            ),
            downloadButton(
              outputId = "downloadres_txt",
              "Tab-Delimited (.txt)",
              icon = icon("file-alt"),
              class = "btn-dl"
            ),
          )
        ),
        actionButton(inputId = "showTableKey", label = "Table Key")
      )),
      br(),
      dataTableOutput("dt_results"),
      actionButton(inputId = "goToCompare", label = "Compare Results", width = "100%")
    ),
    div(
      id = "result_contents_blr",
      class = "hiddenBlock",
      fluidRow(column(
        width = 12,
        p(
          "Results of the DASS App analysis are shown below. The first table shows overall outcomes from each assay and the 2o3 hazard prediction based on those outcomes. The remaining tables show outcomes from individual runs."
        ),
        p(
          "Use the \"Download Results\" button to export your results to an Excel Spreadsheet which may allow easier viewing. "
        ),
        downloadButton(
          outputId = "downloadres_blr_xl",
          "Download Results (.xlsx)",
          icon = icon("file-excel"),
          class = "btn-dl"
        )
      )),
      fluidRow(
        tags$details(
          open = "open",
          tags$summary("2o3 Results"),
          div(
            class = "detailsBody",
            DT::dataTableOutput("dass_results_blr", width = "fit-content")
          )
        ),
        tags$details(
          open = "open",
          tags$summary("KE1 Assay Run Outcomes"),
          div(
            class = "detailsBody",
            DT::dataTableOutput("ke1_blr_indiv", width = "fit-content")
          )
        ),
        tags$details(
          open = "open",
          tags$summary("KE2 Assay Run Outcomes"),
          div(
            class = "detailsBody",
            p(id = "ks_run_text", "No run outcomes for KeratinoSens input."),
            DT::dataTableOutput("ke2_blr_indiv", width = "fit-content")
          )
        ),
        tags$details(
          open = "open",
          tags$summary("KE3 Assay Run Outcomes"),
          div(
            class = "detailsBody",
            DT::dataTableOutput("ke3_blr_indiv", width = "fit-content")
          )
        )
      )
    )
  )
)

# Compare -----
compare_panel <- tabPanel(
  title = tabNames[6],
  div(
    id = "compare_ui_all",
    fluidRow(
      id = "compare_setup_standard",
      class = "bordered-panel hiddenBlock",
      tags$h1("Input"),
      div(class = "sub-ind",
        p(
        "This section allows you to compare the DA outcomes to reference data from your uploaded input file. You can also compare the DA outcomes to chemical reference lists from the",
        tags$a(
          class = "external-link", 
          target = "_blank", 
          href = "https://ice.ntp.niehs.nih.gov/",
          "Integrated Chemical Environment (ICE)"), 
        ". There must be at least 5 prediction-reference pairs to perform the evaluation."
      ),
      p(
        "Select the DA prediction you want to evaluate and specify the reference data for comparison. When you are done, click \"Compare Data\" to generate performance metrics and figures."
      )),
      radioButtons(
        inputId = "perf_pred_col",
        label = h2("Select DA Prediction"),
        choices = ""
      ),
      div(
        tags$h2(
          "My Reference Data",
          info_button(
            "info_perf_ref_select",
            "Select Reference Columns from My Data Information"
          )
        ),
        div(
          class = "sub-ind",
          checkboxInput(
            inputId = "perf_use_my_data",
            label = "Use reference data from my data.",
            value = TRUE
          ),
          conditionalPanel(
            condition = "input.perf_use_my_data",
            p(
              "Use the dropdown list to select the columns from your input file that contain reference data. You can select more than one column."
            ),
            selectInput(
              inputId = "perf_ref_col",
              label = "Select Reference Column(s)",
              choices = NULL,
              selectize = T,
              multiple = T
            )
          )
        )
      ),
      div(
        tags$h2(
          "ICE Reference Data",
          info_button(
            "info_perf_ice_select",
            "Use Reference Data from ICE Information"
          )
        ),
        div(
          class = "sub-ind",
          checkboxInput(inputId = "perf_use_ice", label = "Use reference data from ICE"),
          conditionalPanel(
            condition = "input.perf_use_ice",
            p("Choose the reference chemical lists you want to use. The data will be matched by a chemical identifier that you specify. Use the dropdown list to select the column in your data containing the appropriate chemical identifiers."),
            checkboxGroupInput(
              inputId = "perf_ice_ref_cql",
              label = "Select ICE Chemical Quick List",
              choiceNames = c(
                "OECD Defined Approach to Skin Sensitization: Human (R)",
                "OECD Defined Approach to Skin Sensitization: LLNA (R)"
              ),
              choiceValues = c("hppt", "llna"),
              width = "100%"
            ),
            radioButtons(
              inputId = "perf_ice_identifier_type",
              label = "Select Identifier Type",
              choiceNames = c("CASRN", "DTSXID", "QSAR-Ready SMILES"),
              choiceValues = c("casrn", "dtsxid", "smiles")
            ),
            selectInput(
              inputId = "perf_ice_user_identifier",
              label = "Select Chemical Identifier Column",
              choices = NULL,
              selectize = T
            )
          )
        )
      ),
      actionButton(
        inputId = "do_compare",
        label = "Compare Data",
        width = "100%"
      )
    ),
    fluidRow(
      id = "compare_explore_standard",
      class = "bordered-panel hiddenBlock",
      tags$h1("Output"),
      column(
        width = 12,
        style = "margin-bottom: 1em;",
        tags$h2(
          "Tables",
          info_button("info_perf_table", "Performance Metrics Information")
        ),
        p(
          "Confusion matrices and performance metrics are shown below. Use the dropdown list to select the comparison you would like to view. Use the 'Download' button to open the download menu."
        ),
        actionButton(inputId = "download_compare_tables", label = "Download Tables")
      ),
      column(
        id = "perf_table_block_1",
        width = 12,
        selectInput(
          inputId = "reference_perf_1",
          label = "Select Reference",
          choices = NULL
        ),
        plotOutput("perf_table_1")
      ),
      column(
        id = "perf_table_block_2",
        class = "hiddenBlock",
        width = 6,
        selectInput(
          inputId = "reference_perf_2",
          label = "Select Reference",
          choices = NULL
        ),
        plotOutput("perf_table_2")
      ),
      column(
        width = 12,
        tags$h2(
          "Create Figures",
          info_button("info_perf_fig", "Performance Figure Information")
        ),
        p("Use the first dropdown list to select the comparison you want to visualize. Use the second dropdown list to select quantitative data columns from your uploaded data set. Hover over the figure and click the camera icon in the top right corner to download the figure."),
        selectInput(
          inputId = "perf_fig_comparison",
          label = "Select Reference to Visualize",
          choices = NULL
        ),
        selectInput(
          inputId = "perf_fig_quant_col",
          label = "Select Quantiative Data to Visualize",
          choices = NULL
        ),
        plotlyOutput("perf_fig", height = "75rem")
      )
    ),
    fluidRow(
      id = "compare_explore_borderline",
      class = "bordered-panel hiddenBlock",
      p("Compare features are not currently available for borderline results.")
      )
  )
)

# Build page -----
ui_dass <- list(
  welcome_panel,
  tabsetPanel(
    id = "step_set",
    select_da_panel,
    upload_data_panel,
    select_columns_panel,
    review_selection_panel,
    results_panel,
    compare_panel
  ),
  info_modals
)
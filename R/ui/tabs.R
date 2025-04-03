# Welcome -----
ui_header <- div(
  class = "header-row",
  div(
    class = "header-details-panel",
    div(
      class = "page-title",
      h1("The DASS App")
    ),
    div(
      class = "page-details",
      p(
        "The DASS App applies defined approaches on skin sensitization (DASS) to predict skin sensitization hazard (sensitizer or non-sensitizer) and potency (based on UN GHS categories). The defined approaches (DA) generate predictions by integrating data from in vitro assays that represent key events in the Adverse Outcome Pathway for Skin Sensitization and in silico hazard predictions."
      ),
      p(
        "For more information about DASS and their regulatory applications, visit the",
        tags$abbr(
          title = "NTP Interagency Center for the Evaluation of Alternative Toxicological Methods",
          "NICEATM"
        ),
        externalLinkTag("niceatm_dass"),
        "webpage."
      )
    )
  ),
  div(
    class = "header-link-panel",
    h2(class = "sr-only", "Links"),
    tags$ul(
      class = "link-list",
      tags$li(
        tags$a(
          class = "btn btn-default external-link",
          id = "btn_user_guide",
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
          tags$span(class = "sr-only", "Opens in new window.")
        )
      ),
      tags$li(
        tags$a(
          class = "btn btn-default external-link",
          href = "https://rstudio.niehs.nih.gov/dass/",
          target = "_blank",
          "Launch App in New Window"
        )
      )
    ),
    div(
      style = "font-size: 12px;",
      externalLinkTag("dass_github", style = "color: white;"),
      p("Last updated: 2025-Mar-27")
    )
  )
)

# Select DA -----
tab_select_da <- tabPanel(
  title = tab_names[[1]],
  tabindex = 0,
  div(
    id = names(tab_names[1]),
    class = "bordered-panel",
    h2(class = "sr-only", "Select Defined Approach"),
    p(
      "To begin, select the DA to be implemented from the choices below. Click on the information buttons next to the DA names to view a description of the DA and the test methods required to implement the DA."
    ),
    # Moved info buttons from input label to a separate div to maintain expected
    # behavior when navigating radio buttons with keyboard.
    div(
      class = "grid-list",
      div(
        radioButtons(
          inputId = "selected_da",
          label = h3(class = "h3-label", "Defined Approaches"),
          width = "fit-content",
          choiceNames = c(
            "2 out of 3 (2o3)",
            "Integrated Testing Strategy (ITS)",
            "Key Event 3/1 Sequential Testing Strategy (KE 3/1 STS)"
          ),
          choiceValues = c("da_2o3", "da_its", "da_ke31")
        )
      ),
      div(
        class = "info-group",
        info_button("info_2o3", "2o3 information"),
        info_button("info_its", "ITS information"),
        info_button("info_ke31", "KE 3/1 STS information")
      )
    ),
    conditionalPanel(
      condition = "input.selected_da=='da_2o3'",
      div(
        h3(class = "h3-label", "2o3 Borderline Evaluation"),
        div(
          class = "ind1 grid-list",
          checkboxInput(
            inputId = "do_da_2o3_bl",
            label = "Flag borderline assay results prior to applying DA 2o3 (Requires data from individual runs)",
            width = "fit-content"
          ),
          div(
            class = "info-group",
            info_button("info_2o3_bl", "Information about borderline results for DA 2o3.")
          )
        )
      )
    ),
    actionButton(
      inputId = "confirm_da",
      label = "Confirm DA Selection",
      width = "100%"
    )
  )
)

# Data -----
tab_upload_data <- tabPanel(
  title = tab_names[[2]],
  tabindex = 0,
  div(
    id = names(tab_names[2]),
    class = "bordered-panel hiddenBlock",
    h2(class = "sr-only", "About Uploading Data"),
    div(
      h2("Selection Summary"),
      uiOutput("user_summary_tab2")
    ),
    div(
      class = "warn-block",
      div(
        `aria-hidden` = T,
        icon("exclamation-sign", lib = "glyphicon")
      ),
      div(
        p(
          style = "margin:auto;",
          "Before uploading your file, ensure that the data meet the data and formatting requirements. More details are available in the user guide."
        )
      )
    ),
    h2(class = "sr-only", "Choose Data File or Demo Data"),
    div(
      id = "ui_upload_data_std",
      class = "hiddenBlock",
      p(
        "A table template is provided in tab-delimited or Excel format. The template contains columns for every possible assay endpoint. If an assay endpoint will not be used, the corresponding column can be deleted but that is not required. Using the template is not required."
      ),
      tags$ul(
        class = "download-list",
        tags$li(
          a(
            href = "DASSApp-dataTemplate.xlsx",
            target = "_blank",
            download = NA,
            "Download Data Template (.xlsx)"
          )
        ),
        tags$li(
          a(
            href = "DASSApp-dataTemplate.txt",
            target = "_blank",
            download = NA,
            "Download Data Template (.txt)"
          )
        )
      ),
      hr(width = "100%")
    ),
    div(
      id = "ui_upload_data_bl",
      class = "hiddenBlock",
      p(
        "A data template is provided in Excel format. The template contains one worksheet template for every assay and each worksheet shows the information needed to perform the borderline evaluation."
      ),
      tags$ul(
        class = "download-list",
        tags$li(
          a(
            href = "DASSApp-dataTemplate-borderline.xlsx",
            target = "_blank",
            download = NA,
            "Download Data Template (.xlsx)"
          )
        )
      ),
      hr(width = "100%")
    ),
    div(
      div(
        id = "ui_upload_data_input",
        h3(class = "h3-label", "Select File to Upload"),
        fileInput_custom("fpath")
      ),
      div(
        class = "ind1 grid-list",
        checkboxInput(
          inputId = "use_demo_data",
          label = "Use demo data",
          width = "fit-content"
        ),
        div(
          class = "info-group",
          info_button("info_demo_data", "Information about demo data.")
        )
      )
    ),
    div(
      id = "ui_data",
      div(
        id = "ui_xl_select",
        class = "hiddenBlock",
        pickerInput(
          inputId = "xl_sheet",
          label = "Select worksheet",
          choices = NULL,
          options = pickerOptions(
            container = "body",
            title = "Choose Worksheet",
          )
        ),
        div(
          id = "bl_xl_warn",
          class = "warningText hiddenBlock",
          tags$strong("WARNING: Expected at least three worksheets in file.")
        ),
      ),
      div(
        style = "margin-top: 0.5rem;",
        id = "ui_view_data_upload",
        class = "hiddenBlock",
        br(),
        DT::dataTableOutput("dt_analyze"),
        br(),
        actionButton(
          inputId = "confirm_data",
          label = "Continue",
          width = "100%"
        )
      )
    )
  )
)

# Select Columns -----
tab_select_columns <- tabPanel(
  title = tab_names[[3]],
  tabindex = 0,
  div(
    id = names(tab_names[3]),
    class = "bordered-panel hiddenBlock",
    div(
      h2("Selection Summary"),
      uiOutput("user_summary_tab3")
    ),
    ## Standard -----
    div(
      id = "ui_sc_std",
      class = "hiddenBlock",
      h2("Select Columns"),
      p("Below, there are sections corresponding to the information sources required for your selected DA. Within each section, use the dropdown lists to select the columns containing the data for the endpoint shown. Columns are automatically selected for an endpoint if the column name matches the corresponding column name in the data template. A column must be selected for each endpoint shown. When you are finished, click \"Done\"."),
      p("Click on the information buttons next to the dropdown list label to view information about the required data and formatting. Values that are incorrectly formatted or invalid will be treated as missing data and may affect the results. More details are given in the User Guide."),
      div(
        ### KE1 -----
        div(
          id = "ui_sc_std_ke1",
          tags$details(
            open = "open",
            tags$summary(h3("Key Event 1")),
            div(
              class = "detailsBody",
              div(
                id = "ui_sc_std_ke1_call",
                class = "sc_endpoint_container",
                h4("KE1 Call", info_button("info_ke1_call", "KE1 Call information")),
                div(
                  class = "ind1",
                  checkboxInput(
                    inputId = "get_ke1_call",
                    label = "Derive call from quantitative data.",
                    value = F
                  ),
                  pickerInput(
                    inputId = "ke1_std_call",
                    label = "Call Column",
                    choices = NULL
                  )
                )
              ),
              div(
                id = "ui_sc_std_ke1_assay",
                class = "sc_endpoint_container",
                h4("KE1 Assay", info_button("info_ke1_std_assay", "KE1 Assay information")),
                div(
                  class = "ind1",
                  radioButtons(
                    inputId = "ke1_std_assay",
                    label = "KE1 Assay Data Source",
                    choiceNames = c("ADRA", "DPRA"),
                    choiceValues = c("adra", "dpra"),
                    inline = T
                  )
                )
              ),
              div(
                id = "ui_sc_std_ke1_dep",
                class = "sc_endpoint_container",
                h4("KE1 Mean Depletion (%)", info_button("info_ke1_mean_dep", "KE1 Mean Depletion Information")),
                div(
                  class = "ind1",
                  checkboxInput(
                    inputId = "get_ke1_mean_dep",
                    label = "Derive mean depletion values from data",
                    value = F,
                    width = "100%"
                  ),
                  pickerInput(
                    inputId = "ke1_std_mean_dep",
                    label = "Mean Depletion Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke1_std_c_dep",
                    label = "Cys/NAC Depletion Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke1_std_k_dep",
                    label = "Lys/NAL Depletion Column",
                    choices = NULL
                  )
                )
              )
            )
          )
        ),
        ### KE2 -----
        div(
          id = "ui_sc_std_ke2",
          tags$details(
            open = "open",
            tags$summary(h3("Key Event 2")),
            div(
              class = "detailsBody",
              div(
                class = "sc_endpoint_container",
                h4("KE2 Call", info_button("info_ke2_std_call", "KE2 Call information")),
                div(
                  class = "ind1",
                  pickerInput(
                    inputId = "ke2_std_call",
                    label = "Call Column",
                    choices = NULL
                  )
                )
              ),
            )
          )
        ),
        ### KE3 -----
        div(
          id = "ui_sc_std_ke3",
          tags$details(
            open = "open",
            tags$summary(h3("Key Event 3")),
            div(
              class = "detailsBody",
              div(
                id = "ui_sc_std_ke3_call",
                class = "sc_endpoint_container",
                h4("KE3 Call", info_button("info_ke3_call", "KE3 Call information")),
                div(
                  class = "ind1",
                  pickerInput(
                    inputId = "ke3_std_call",
                    label = "Call Column",
                    choices = NULL
                  )
                )
              ),
              div(
                id = "ui_sc_std_ke3_assay",
                class = "sc_endpoint_container",
                h4("KE3 Assay", info_button("info_ke3_std_assay", "KE3 Assay information")),
                div(
                  class = "ind1",
                  radioButtons(
                    inputId = "ke3_std_assay",
                    label = "Select KE3 Assay",
                    choiceNames = c("GARDskin", "h-CLAT", "U-SENS"),
                    choiceValues = c("gardskin", "hclat", "usens"),
                    inline = T
                  )
                )
              ),
              div(
                id = "ui_sc_std_ke3_val",
                class = "sc_endpoint_container",
                h4("KE3 Quantitative Endpoint", info_button("info_ke3_std_val", "KE3 Quantitative Endpoint information")),
                div(
                  class = "ind1",
                  pickerInput(
                    inputId = "ke3_std_val",
                    label = "Quantitative Endpoint Column",
                    choices = NULL
                  )
                )
              )
            )
          )
        ),
        div(
          id = "ui_sc_std_insil",
          tags$details(
            open = "open",
            tags$summary(h3("In Silico Model")),
            div(
              class = "detailsBody",
              div(
                class = "sc_endpoint_container",
                h4("In Silico Call Prediction", info_button("info_insil_pred", "In Silico Prediction information")),
                div(
                  class = "ind1",
                  pickerInput(
                    inputId = "insil_call",
                    label = "Call Prediction Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "insil_ad",
                    label = "Applicability Domain Column",
                    choices = NULL
                  )
                )
              )
            )
          )
        )
      )
    ),
    ## Borderline -----
    div(
      id = "ui_sc_bl",
      class = "hiddenBlock",
      h2("Select Columns"),
      p("Below, there are three sections corresponding to the three key events that underly the 2o3 DA. Within each section, specify the assay for which you are providing data and use the dropdown list to select the worksheet that contains the data for the selected assay. Dropdown lists will be displayed for the data columns required for the specific assay. The dropdown lists will be populated with column names from the selected worksheet. Use these dropdown lists to specify the columns containing data for the endpoints shown. When you are finished, click \"Done\""),
      p("Click on the information buttons to view information about the required data and formatting. Values that are incorrectly formatted or invalid will be treated as missing data and may affect the results. More details are given in the User Guide."),
      div(
        ### KE1 -----
        div(
          id = "ui_sc_bl_ke1",
          tags$details(
            open = "open",
            tags$summary(h3("Key Event 1")),
            div(
              class = "detailsBody",
              div(
                class = "sc_endpoint_container",
                h4("KE1 Assay", info_button("info_ke1_bl_assay", "KE1 Assay information")),
                div(
                  class = "ind1",
                  radioButtons(
                    inputId = "ke1_bl_assay",
                    label = "KE1 Assay Data Source",
                    choiceNames = c("ADRA", "DPRA"),
                    choiceValues = c("adra", "dpra"),
                    inline = T,
                    selected = character(0)
                  ),
                  pickerInput(
                    inputId = "ke1_bl_ws",
                    label = "KE1 Worksheet",
                    choices = NULL
                  )
                )
              ),
              div(
                class = "sc_endpoint_container",
                h4("KE1 Data Columns", info_button("info_ke1_bl_columns", "KE1 Data Column Information")),
                div(
                  class = "ind1",
                  pickerInput(
                    inputId = "ke1_bl_cid",
                    label = "Chemical Identifier Columns",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke1_bl_c_dep",
                    label = "Cys/NAC Depletion (%) Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke1_bl_k_dep",
                    label = "Lys/NAL Depletion (%) Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke1_bl_c_only",
                    label = "Cys/NAC-Only Indicator Column",
                    choices = NULL
                  )
                )
              )
            )
          )
        ),
        ### KE2 -----
        div(
          id = "ui_sc_bl_ke2",
          tags$details(
            open = "open",
            tags$summary(h3("Key Event 2")),
            div(
              class = "detailsBody",
              div(
                class = "sc_endpoint_container",
                h4("KE2 Assay", info_button("info_ke2_bl_assay", "KE2 Assay information")),
                div(
                  class = "ind1",
                  radioButtons(
                    inputId = "ke2_bl_assay",
                    label = "KE2 Assay Data Source",
                    choiceNames = c("KeratinoSens", "LuSens"),
                    choiceValues = c("ks", "lusens"),
                    inline = T,
                    selected = character(0)
                  ),
                  pickerInput(
                    inputId = "ke2_bl_ws",
                    label = "KE2 Worksheet",
                    choices = NULL
                  )
                )
              ),
              div(
                class = "sc_endpoint_container",
                h4("KE2 Data Columns", info_button("info_ke2_bl_columns", "KE2 Data Column Information")),
                div(
                  class = "ind1",
                  pickerInput(
                    inputId = "ke2_bl_cid",
                    label = "Chemical Identifier Columns",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke2_bl_ks_call",
                    label = "KeratinoSens Outcome Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke2_bl_lusens_run",
                    label = "Run Identifier Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke2_bl_lusens_conc",
                    label = "Concentration Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke2_bl_lusens_fi",
                    label = "Fold Induction Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke2_bl_lusens_cv",
                    label = "Cell Viability (%) Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke2_bl_lusens_pval",
                    label = "T Test p-value Column",
                    choices = NULL
                  )
                )
              )
            )
          )
        ),
        ### KE3 -----
        div(
          id = "ui_sc_bl_ke3",
          tags$details(
            open = "open",
            tags$summary(h3("Key Event 3")),
            div(
              class = "detailsBody",
              div(
                class = "sc_endpoint_container",
                h4("KE3 Assay", info_button("info_ke3_bl_assay", "KE3 Assay information")),
                div(
                  class = "ind1",
                  radioButtons(
                    inputId = "ke3_bl_assay",
                    label = "KE3 Assay Data Source",
                    choiceNames = c("GARDskin", "h-CLAT", "IL-8 Luc", "U-SENS"),
                    choiceValues = c("gard", "hclat", "il8", "usens"),
                    inline = T,
                    selected = character(0)
                  ),
                  pickerInput(
                    inputId = "ke3_bl_ws",
                    label = "KE3 Worksheet",
                    choices = NULL
                  )
                )
              ),
              div(
                class = "sc_endpoint_container",
                h4("KE3 Data Columns", info_button("info_ke3_bl_columns", "KE3 Data Column Information")),
                div(
                  class = "ind1",
                  pickerInput(
                    inputId = "ke3_bl_cid",
                    label = "Chemical Identifier Columns",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke3_bl_run",
                    label = "Run Identifier Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke3_bl_conc",
                    label = "Concentration Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke3_bl_gard_meanDV",
                    label = "GARDSkin Mean DV Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke3_bl_hclat_cd54",
                    label = "H-CLAT CD54 RFI Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke3_bl_hclat_cd86",
                    label = "H-CLAT CD86 RFI Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke3_bl_hclat_cv",
                    label = "H-CLAT Viability (%) Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke3_bl_usens_cd86",
                    label = "U-SENS CD86 SI Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke3_bl_usens_cv",
                    label = "U-SENS Viability (%) Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke3_bl_il8_ind",
                    label = "IL-8 Luc Ind-IL8LA Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke3_bl_il8_indLCL",
                    label = "IL-8 Luc Ind-IL8LA LCL Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke3_bl_il8_inh",
                    label = "IL-8 Luc Inh-GAPLA Column",
                    choices = NULL
                  ),
                  pickerInput(
                    inputId = "ke3_bl_il8_ws",
                    label = "IL-8 Luc Solubility Column",
                    choices = NULL
                  )
                  
                )
              )
            )
          )
        )
      )
    ),
    actionButton(
      inputId = "review_entries",
      label = "Done",
      width = "100%"
    )
  )
)

# Review -----
tab_review_selection <- tabPanel(
  title = tab_names[[4]],
  tabindex = 0,
  div(
    id = names(tab_names[4]),
    class = "bordered-panel hiddenBlock",
    h2(class = "sr-only", "Review Selection"),
    ## Standard -----
    div(
      id = "ui_rev_std",
      class = "hiddenBlock",
      div(p("The table below summarizes your selections for each of the required endpoints. Values in the selected column are evaluated against formatting requirements. The third column \"Flagged\" will be \"true\" if invalid values were found in the corresponding column and \"false\" otherwise. When you are done reviewing your selections, click 'Run' to run the DASS. If you need to change a selected column, return to the 'Select Data Columns' page. If you need to upload new or updated data, return to the 'Upload Data' page.")),
      uiOutput("ui_review_text_std"),
      DT::dataTableOutput("dt_review_std", width = "fit-content")
    ),
    ## Borderline -----
    div(
      id = "ui_rev_bl",
      class = "hiddenBlock",
      div(
        p("Your selections are summarized below. The KE assay sections show a table with your data column selections. Values in these columns are checked against formatting requirements and flagged if any issues are identified. Chemical identifier columns are flagged if any chemical identifiers do not have enough data to perform the 2o3."),
        p(tags$strong("Note. The data are not checked for the appropriate number of runs. If data from too few or too many runs are provided, an overall outcome will not be returned.")),
        p("The last section lists the unique chemical identifiers from your data and indicates which worksheets contain those identifiers. The identifier is flagged if it does not have values in at least two worksheets."),
        p("Review your selections. When you are done, click 'Run' to run the DASS. If you need to change a selection, return to the 'Select Data Columns' page. If you need to upload new or updated data, return to the 'Upload Data' page."),
      ),
      tags$details(
        open = "open",
        tags$summary(h3("General")),
        div(
          class = "detailsBody",
          uiOutput("ui_review_text_bl_gen")
        )
      ),
      tags$details(
        open = "open",
        tags$summary(h3("KE1 Assay")),
        div(
          class = "detailsBody",
          uiOutput("ui_review_text_bl_ke1"),
          DT::dataTableOutput("dt_review_bl_ke1", width = "fit-content")
        )
      ),
      tags$details(
        open = "open",
        tags$summary(h3("KE2 Assay")),
        div(
          class = "detailsBody",
          uiOutput("ui_review_text_bl_ke2"),
          DT::dataTableOutput("dt_review_bl_ke2", width = "fit-content")
        )
      ),
      tags$details(
        open = "open",
        tags$summary(h3("KE3 Assay")),
        div(
          class = "detailsBody",
          uiOutput("ui_review_text_bl_ke3"),
          DT::dataTableOutput("dt_review_bl_ke3", width = "fit-content")
        )
      ),
      tags$details(
        open = "open",
        tags$summary(h3("Chemical Identifiers")),
        div(
          class = "detailsBody",
          uiOutput("ui_review_text_bl_cid"),
          DT::dataTableOutput("dt_review_bl_cid", width = "fit-content")
        )
      )
    ),
    br(),
    actionButton(
      inputId = "run_dass",
      width = "100%",
      label = "Run"
    )
  )
)

# Results -----
tab_results <- tabPanel(
  title = tab_names[[5]],
  tabindex = 0,
  div(
    id = names(tab_names[5]),
    class = "bordered-panel hiddenBlock",
    h2(class = "sr-only", "Results"),
    ## Standard -----
    div(
      id = "ui_res_std",
      class = "hiddenBlock",
      div(
        p("Results of the DASS App analysis are appended to your data in the table below. By default, the table shows the first three columns of data, your selected input columns, and the DA results. The buttons above the table can be used to hide or show columns. Use the \"Download Results\" button to export your results to an Excel spreadsheet or text file, which may allow for easier viewing.")
      ),
      div(
        id = "dl_container",
        class = "dropdown",
        tags$button(
          class = "btn btn-default dropdown-toggle",
          type = "button", 
          id = "dlDropdown",
          `data-bs-toggle` = "dropdown",
          `aria-expanded` = F,
          "Download Results"
        ),
        tags$ul(
          class = "dropdown-menu",
          `aria-labelledby` = "dlDropdown",
          tags$li(
            downloadLink(
              outputId = "downloadres_xl",
              "Excel (.xlsx)",
              class = "dropdown-item"
            )
          ),
          tags$li(
            downloadLink(
              outputId = "downloadres_zip",
              "Tab-Delimited (.zip)",
              class = "dropdown-item"
            )
          )
        )
      ),
      DT::dataTableOutput("results_std")
    ),
    ## Borderline -----
    div(
      id = "ui_res_bl",
      class = "hiddenBlock",
      div(
        p(
          "Results of the DASS App analysis are shown below. The first table shows overall outcomes from each assay and the 2o3 hazard prediction based on those outcomes. The remaining tables show outcomes from individual runs."
        ),
        p(
          "Use the \"Download Results\" button to export your results to an Excel Spreadsheet which may allow easier viewing. "
        ),
        downloadButton(
          outputId = "downloadres_bl_xl",
          "Download Results (.xlsx)",
          icon = icon("file-excel")
        ),
        div(
          tags$details(
            open = "open",
            tags$summary(h3("2o3 Results")),
            div(
              class = "detailsBody",
              DT::dataTableOutput("results_bl", width = "fit-content")
            )
          ),
          tags$details(
            open = "open",
            tags$summary(h3("KE1 Assay Run Outcomes")),
            div(
              class = "detailsBody",
              DT::dataTableOutput("results_ke1_bl", width = "fit-content")
            )
          ),
          tags$details(
            open = "open",
            tags$summary(h3("KE2 Assay Run Outcomes")),
            div(
              class = "detailsBody",
              p(id = "ks_run_text", "No run outcomes for KeratinoSens input."),
              DT::dataTableOutput("results_ke2_bl", width = "fit-content")
            )
          ),
          tags$details(
            open = "open",
            tags$summary(h3("KE3 Assay Run Outcomes")),
            div(
              class = "detailsBody",
              DT::dataTableOutput("results_ke3_bl", width = "fit-content")
            )
          )
        )
      )
    ),
    actionButton(inputId = "goToCompare", label = "Compare Results", width = "100%")
  )
)

# Compare -----
tab_compare <- tabPanel(
  title = tab_names[[6]],
  tabindex = 0,
    div(
      id = names(tab_names[6]),
      class = "bordered-panel hiddenBlock",
      h2("Evaluate Performance: Input"),
      p(
        "This section allows you to compare the DA outcomes to reference data from your uploaded input file. You can also compare the DA outcomes to chemical reference lists from the",
        tags$a(
          class = "external-link",
          target = "_blank",
          href = "https://ice.ntp.niehs.nih.gov/",
          "Integrated Chemical Environment (ICE)"),
        ". There must be at least 5 prediction-reference pairs to perform the evaluation."
      ),
      p("Select the DA prediction you want to evaluate and specify the reference data for comparison. When you are done, click \"Compare Data\" to generate performance metrics and figures."),
      radioButtons(
        inputId = "compare_type",
        label = h3("Select DA Prediction"),
        choices = ""
      ),
      h3("My Reference Data", info_button("info_compare_ref", "Information about reference data in the original file upload.")),
      div(
        id = "div_compare_usr_dt",
        class = "ind1",
        checkboxInput(
          inputId = "compare_usr_dt",
          label = "Use reference data from my data.",
          value = T,
          width = "100%"
        ),
        conditionalPanel(
          condition = "input.compare_usr_dt",
          div(
            id = "div_compare_bl",
            class = "hiddenBlock",
            pickerInput(
              inputId = "compare_bl_ws",
              label = h4("Select Worksheet with Reference Data"),
              choices = NULL,
            ),
            pickerInput(
              inputId = "compare_bl_cid",
              label = h4("Chemical Identifier Column"),
              choices = NULL,
            )
          ),
          pickerInput(
            inputId = "compare_usr_dt_col",
            label = h4("Select One or More Reference Columns"),
            choices = NULL,
            multiple = T
          )
        )
      ),
      h3("ICE Reference Data", info_button("info_compare_ice", "Information about ICE reference data.")),
      div(
        id = "div_compare_ice",
        class = "ind1",
        checkboxInput(
          inputId = "compare_ice",
          label = "Use reference data from ICE.",
          value = F,
          width = "100%"
        ),
        conditionalPanel(
          condition = "input.compare_ice",
          p("Choose the reference chemical lists you want to use. The data will be matched by a chemical identifier that you specify. Use the dropdown list to select the column in your data containing the appropriate chemical identifiers."),
          checkboxGroupInput(
            inputId = "compare_ice_cql",
            label = h4("Select ICE Chemical Quick List"),
            choiceNames = c(
              "OECD Defined Approach to Skin Sensitization: Human (R)",
              "OECD Defined Approach to Skin Sensitization: LLNA (R)"
            ),
            choiceValues = c("hppt", "llna"),
            width = "100%"
          ),
          radioButtons(
            inputId = "compare_ice_id_type",
            label = h4("Select Identifier Type"),
            choiceNames = c("CASRN", "DTSXID", "QSAR-Ready SMILES"),
            choiceValues = c("casrn", "dtxsid", "smiles")
          ),

          pickerInput(
            inputId = "compare_ice_cid",
            label = h4("Select Chemical Identifier Column"),
            choices = NULL,
            multiple = F
          )
        )
      ),
      actionButton(
        inputId = "do_compare",
        label = "Compare Data",
        width = "100%"
      )
    ),
  div(
    id = "div_compare_results",
    class = "bordered-panel hiddenBlock",
    h2("Evaluate Performance: Output"),
    div(
      class = "ind1",
      h3("Tables", info_button("info_compare_tables", "Performance Metric Information")),
      p("Confusion matrices and performance metrics are shown below. Use the dropdown list to select the comparison you would like to view. Use the 'Download' button to open the download menu."),
      actionButton(inputId = "download_compare_tables", label = "Download Tables", `data-bs-toggle` = "modal", `data-bs-target` = "#download_compare_tables_modal"),
      checkboxInput(inputId = "compare_show_flat", label = "Show flat table")
    ),
    br(),
    div(
      class = "cm-block",
      div(
        id = "cm_block_vert_1",
        class = "cm-block-vert",
        h4(class = "sr-only", "Comparison Table 1"),
        pickerInput(
          inputId = "compare_table_id_1",
          label = "Select Reference",
          choices = NULL
        ),
        uiOutput("compare_table_1")
      ),
      div(
        id = "cm_block_vert_2",
        class = "cm-block-vert",
        h4(class = "sr-only", "Comparison Table 2"),
        pickerInput(
          inputId = "compare_table_id_2",
          label = "Select Reference",
          choices = NULL
        ),
        uiOutput("compare_table_2")
      ),
      div(
        id = "cm_block_full",
        class = "cm-block-vert",
        style = "overflow-x: auto;",
        h4(class = "sr-only", "Comparison Table, Flat"),
        uiOutput("compare_flat")
      )
    ),
    div(
      div(
        class = "ind1",
        h3("Create Figures", info_button("info_comp_fig", "Comparison figures information")),
        p("Use the first dropdown list to select the comparison you want to visualize. Use the second dropdown list to select quantitative data columns from your uploaded data set. Hover over the figure and click the camera icon in the top right corner to download the figure.")
      ),
      div(
        class = "ind1",
        style = "display: flex; flex-wrap: wrap; gap:1rem",
        pickerInput(
          inputId = "comp_fig_comparison",
          label = "Select Reference to Visualize",
          choices = NULL
        ),
        pickerInput(
          inputId = "comp_fig_quant_col",
          label = "Select Quantiative Data to Visualize",
          choices = NULL
        ),
        pickerInput(
          inputId = "comp_fig_chem_col",
          label = "Chemical Identifier Labels for Points",
          choices = NULL
        )
      ),
      plotlyOutput("comp_fig", width = "100%", height = "50vh"),
      uiOutput("compare_table_quant")
    )
  )
)
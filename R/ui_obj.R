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
source("R/modals.R")

# Create page
# Welcome -----
welcome_panel <- fluidRow(
  tags$header(
    class = "header-row",
    div(
      class = "header-details-panel",
      div(
        class = "page-title",
        h1("The DASS App")
      ),
      tags$div(
        class = "page-details",
        tags$section(
        p("The DASS App applies defined approaches on skin sensitization (DASS)",
          "to predict skin sensitization hazard (sensitizer or non-sensitizer)",
          "and potency (based on UN GHS categories). The defined approaches (DA)",
          "generate predictions by integrating data from in vitro assays that",
          "represent key events in the Adverse Outcome Pathway for Skin",
          "Sensitization and in silico hazard predictions."),
        p(
          "For more information about DASS and their regulatory applications, visit the",
          tags$abbr(title = "NTP Interagency Center for the Evaluation of Alternative Toxicological Methods", "NICEATM"),
          tags$a(
            href = "https://ntp.niehs.nih.gov/go/40498",
            "Defined Approaches to Identify Potential Skin Sensitizers",
            tags$span(class = "sr-only", "Opens new window.")
          ), "webpage."
        ),
        )
      )
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
            href = "user_guide.pdf",
            target = "_blank",
            "User Guide",
            tags$span(
              class = "sr-only",
              "Opens PDF in new window."
            )
          )
        ),
        tags$li(
          tags$a(
            class = "btn btn-default",
            href = "mailto:ICE-support@niehs.nih.gov",
            target = "_blank",
            "Contact Us",
            tags$span(
              class = "sr-only",
              "Opens default mail client."
            )
          )
        ),
        tags$li(
          tags$a(
            class = "btn btn-default external-link",
            href = "https://doi.org/10.1186/s12859-023-05617-1",
            target = "_blank",
            "Manuscript",
            tags$span(
              class = "sr-only",
              "External link. Opens in new window."
            )
          )
        ),
        tags$li(
          tags$a(
            class = "btn btn-default external-link",
            href = "https://rstudio.niehs.nih.gov/dass/",
            target = "_blank",
            "Launch App in New Window",
            tags$span(
              class = "sr-only",
              "External link."
            )
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
          tags$span(
            class = "sr-only",
            "External link. Opens in new window."
          )
        ),          br(),
           "Last updated: 2023-Oct-20")
    )
  )
)

# Step 1: Select DAs -----
select_da_panel <- tabPanel(
  title = "Select Defined Approach",
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
            HTML("<span id = '2o3_radio_label'>2 out of 3 (2o3)</span>", info_button("info_2o3", "2o3 information")),
            HTML("<span id = 'its_radio_label'>Integrated Testing Strategy (ITS)</span>", info_button("info_its", "ITS information")),
            HTML("<span id = 'ke31_radio_label'>Key Event 3/1 (KE 3/1) Sequential Testing Strategy (STS)</span>", info_button("info_ke31", "STS information"))
          )
        ),
        conditionalPanel(
          hr(width = "50%"),
          condition = "input.selected_da=='da_2o3'",
          p("Flag borderline results (requires data from individual runs)"),
          div(
            style = "margin-left: 2rem",
          checkboxInput(inputId = "do_da_2o3_bl", 
                        label = HTML(
                          "<span id = '2o3_bl_cb_label'>Flag borderline assay results prior to applying DA 2o3.</span>", 
                                     info_button("info_2o3_bl", "Information about borderline results for DA 2o3.")),
                        width = "100%")
        ))
      )
    ),
    actionButton(
      inputId = "confirm_da",
      label = "Confirm DA Selection",
      width = "100%"
    )
  )
)

# Step 2: Upload Data -----
upload_data_panel <- tabPanel(
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
     ),
     hr(style = "width:50%"),
     div(
       id = "upload_block",
       div(
         class = "form-group shiny-input-container",
         style = "width:100%",
         tags$label(
           class = "control-label",
           `for` = "fpath",
           # id = "fpath-label",
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
       uiOutput("xl_sheet_text_ui")
     ),
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
     ),
     div(
       class = "hiddenBlock",
       id = "data_block",
       hr(style = "width:50%"),
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
 ))

# Step 3: Select Columns -----
select_columns_panel <- tabPanel(
  title = "Select Data Columns",
  div(
    id = "select_col_ui",
    class = "bordered-panel hiddenBlock",
    div(
      p(
        "The assay endpoints that are required for the selected DAs are shown below.",
        "Use the dropdown lists to select the columns from your data",
        "that correspond to the given endpoints. Columns are automatically selected for an endpoint",
        "if the column name matches the corresponding column name in the data template.",
        "A column must be selected for each",
        "endpoint shown. When you are finished, click 'Done'."
      ),
      p(
        "Click on the information buttons next to the assay endpoint names to view",
        "information about the endpoints and data formatting requirements.",
        "Values that are incorrectly formatted or invalid",
        "will be treated as missing data and may affect the results. More details are given",
        "in the User Guide."
      )
    ),
    hr(width = "50%"),
    ## KE1 -----
    div(
      class = "hiddenBlock",
      id = "ke1_select_ui",
      tags$details(
        open = "open",
        tags$summary("Key Event 1 Assay"),
        div(
          class = "detailsBody",
          div(
            id = "ke1_assay_select",
            tags$h2(
              "KE1 Assay",
              info_button("info_ke1Assay", "KE1 Assay Information")
            ),
            div(
              class = "col_assay_endpoint",
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
            id = "ke1_call_select",
            class = "hiddenBlock",
            tags$h2(
              "KE1 Call",
              info_button("info_ke1Call", "KE1 Call information")
            ),
            div(
              class = "col_assay_endpoint",
              checkboxInput(
                inputId = "ke1_call_interpret",
                label = "Derive call from quantiative data.",
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
          div(
            id = "ke1_dep_select",
            class = "hiddenBlock",
            tags$h2(
              "KE1 Mean Depletion Value",
              info_button("info_ke1DepValue", "KE1 Mean Depletion Value Information")
            ),
            div(
              class = "col_assay_endpoint",
              checkboxInput(
                inputId = "ke1_choose_dep",
                label = "Derive mean depletion value from data.",
                value = F
              ),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    inputId = "ke1_mean_c_l_dep_col",
                    label = "Mean Depletion Column",
                    choices = NULL,
                    selectize = F
                  )
                ),
                column(
                  width = 4,
                  selectInput(
                    inputId = "ke1_c_dep_col",
                    label = "Cys/NAC Depletion Column",
                    choices = NULL,
                    selectize = F
                  )
                ),
                column(
                  width = 4,
                  selectInput(
                    inputId = "ke1_l_dep_col",
                    label = "Lys/NAL Depletion Column",
                    choices = NULL,
                    selectize = F
                  )
                )
              )
            )
          )
        )
      )
    ),
    ## KE2 -----
    div(
      class = "hiddenBlock",
      id = "ke2_select_ui",
      tags$details(
        open = "open",
        tags$summary("Key Event 2 Assay"),
        div(
          class = "detailsBody",
          div(
            id = "ke2_assay_select",
            tags$h2(
              "KE2 Assay",
              info_button("info_ke2Assay", "KE2 Assay Information")
            ),
            div(
              class = "col_assay_endpoint",
              radioButtons(
                inputId = "ke2_assay_name",
                label = "Select KE2 Assay",
                choiceNames = c("KeratinoSens", "LuSens"),
                choiceValues = c("keratinosens", "lusens"),
                inline = T
              )
            ),
            tags$h2(
              "KE2 Call",
              info_button("info_ke2Call", "KE2 Call information")
            ),
            div(
              class = "col_assay_endpoint",
              selectInput(
                inputId = "ke2_call_col",
                label = "Call Column",
                choices = NULL,
                selectize = F
              )
            ),
            div(
              id = "ke2_value_select",
              class = "hiddenBlock",
              tags$h2(
                "KE2 Quantitative Endpoint",
                info_button("info_ke2Value", "KE2 Value information")
              ),
              div(
                class = "col_assay_endpoint",
                selectInput(
                  inputId = "ke2_val_col",
                  label = "Quantitative Endpoint Column",
                  choices = NULL,
                  selectize = F
                )
              )
            )
          )
        )
      )
    ),
    ## KE3 -----
    div(
      class = "hiddenBlock",
      id = "ke3_select_ui",
      tags$details(
        open = "open",
        tags$summary("Key Event 3 Assay"),
        div(
          class = "detailsBody",
          div(
            id = "ke3_assay_select",
            tags$h2(
              "KE3 Assay",
              info_button("info_ke3_assay", "KE3 Assay Information")
            ),
            div(
              class = "col_assay_endpoint",
              radioButtons(
                inputId = "ke3_assay_name",
                label = "Select KE3 Assay",
                choiceNames = c("GARDskin", "h-CLAT", "IL-8 Luc", "U-SENS"),
                choiceValues = c("gardskin", "hclat", "il8luc", "usens"),
                inline = T
              )
            )
          ),
          div(
            id = "ke3_call_select",
            class = "hiddenBlock",
            tags$h2(
              "KE3 Call",
              info_button("info_ke3_call", "KE3 Call information")
            ),
            div(
              class = "col_assay_endpoint",
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
            class = "hiddenBlock",
            tags$h2(
              "KE3 Quantitative Endpoint",
              info_button("info_ke3_value", "KE3 Value information")
            ),
            div(
              class = "col_assay_endpoint",
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
    ),
    ## In Silico -----
    div(
      class = "hiddenBlock",
      id = "insil_select_ui",
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
            class = "col_assay_endpoint",
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
    ),
    actionButton(
      inputId = "review_entries",
      label = "Done",
      width = "100%"
    )
  )
)


# Step 4: Review Selection -----
review_selection_panel <- tabPanel(
  title = "Review Selection",
  div(
    id = "review_contents",
    class = "bordered-panel hiddenBlock",
    p(
      "Your selections are summarized below. Review the selected assays and columns.",
      "When you are done, click 'Run' to run the DASS.",
      "If you need to change a selected column, return to the 'Select Data Columns'",
      "page. If you need to upload new or updated data, return to the 'Upload Data' page."
    ),
    div(
      
    ),
    div(
      id = "dupe_col_warning",
      class = "warningText hiddenBlock",
      p(
        strong("Warning: Identical column assigned to more than one endpoint.")
      )
    ),
    div(
      id = "flag_col_warning",
      class = "warningText hiddenBlock",
      p(
        strong("Warning: Selected data columns have been flagged for invalid values."),
        "Invalid values will not be evaluated in the DASS."
      )
    ),
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
      class = "text-block",
      h2("Table Key", style = "font-size:1.2em; font-weight:bold; margin-top:10px;"),
      tags$dl(
        tags$dt("Yellow columns"),
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
    fluidRow(column(
      12,
      div(
        class = "dropdown",
        id = "dlDropdown",
        tags$button(
          class = "btn dropbtn btn-default",
          # style = "padding:1vh;",
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
      actionButton(inputId = "goToCompare",
                   label = "Compare Results")
    )),
    br(),
    dataTableOutput("dt_results")
  )
)

# Compare -----
compare_panel <- tabPanel(
  title = "Compare",
  div(
    id = "compareText",
    class = "bordered-panel",
    # class = "bordered-panel hiddenBlock",
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
    hr(width = "50%"),
    tags$details(
      open = "open",
      tags$summary("Select Data to Compare"),
      div(
        class = "detailsBody",
        tags$h2("DA Prediction"),
        radioButtons(
          inputId = "perf_pred_col",
          label = "Select DA Prediction",
          choices = ""
        ),
        tags$h2("Reference Column", info_button("info_perf_ref_col", "Reference Column Information")),
        checkboxGroupInput(
          inputId = "perf_ref_col_source", 
          label = "Choose Reference Source",
          choiceNames = c("My Data", "Integrated Chemical Environment"),
          choiceValues = c("user_data", "ice"),
          selected = "user_data"
        ),
        conditionalPanel(
          condition = "input.perf_ref_col_source.includes('user_data')",
          selectInput(
            inputId = "perf_ref_col",
            label = "Select Reference Column",
            choices = NULL,
            selectize = T, 
            multiple = T
          )
        ),
        conditionalPanel(
          condition = "input.perf_ref_col_source.includes('ice')",
          checkboxGroupInput(
            inputId = "perf_ice_ref_cql",
            label = "Select ICE Chemical Quick List",
            choiceNames = c(
              "OECD Defined Approach to Skin Sensitization: Human (R)", 
              "OECD Defined Approach to Skin Sensitization: LLNA (R)"),
            choiceValues = c("hppt", "llna"),
            width = "100%"
          )
        )
      )
    ),
    tags$details(
      open = "open",
      tags$summary("Select Options for ICE Data"),
      div(
        class = "detailsBody",
        checkboxInput(inputId = "perf_use_ice_data", 
                      label = HTML(
                        "<span id = 'perf_use_ice_data_label'>Load data from ICE for figures</span>", 
                        info_button("info_perf_use_ice_data", "Information about loading ICE data for figures.")),
                      width = "100%"),
        conditionalPanel(
          condition = "input.perf_ref_col_source.includes('ice')||input.perf_use_ice_data",
          
          tags$h2("Specify Chemical Identifier", info_button("info_chem_identifier", "Information about selecting chemical identifiers")),
          radioButtons(
            inputId = "perf_ice_identifier_type",
            label = "Select Identifier Type",
            choiceNames = c("DTSXID", "CASRN", "QSAR-Ready SMILES"),
            choiceValues = c("dtsxid", "casrn", "smiles")
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
  bsCollapse_dass(
    multiple = T,
    open = c("Tables", "Figures"),
    bsCollapsePanel_dass(
      title = "Tables",
      div(
        class = "hiddenBlock",
        id = "compare_table_body",
        p(
          "Confusion matrices and performance metrics are shown below. Use the dropdown list",
          "to select the comparison you would like to view. Use the 'Download' button to open",
          "the download menu."
        ),
        actionButton(inputId = "download_compare_tables",
                     label = "Download Tables"),
        br(),
        fluidRow(
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
          )
        ),
        hr(width = "50%"),
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
        
      )
    ),
    bsCollapsePanel_dass(
      title = "Figures",
      div(
        # class = "hiddenBlock",
        selectInput(
          inputId = "perf_fig_comparison",
          label = "Select Comparison(s) to Visualize",
          choices = NULL,
          multiple = T
        ),
        conditionalPanel(
          condition = "input.perf_use_ice_data",
          radioButtons(
            inputId = "quant_data_source",
            label = "Select Data Source",
            choiceNames = c("My Data", "Integrated Chemical Environment"),
            choiceValues = c("user_data", "ice"),
            selected = "user_data"
          )
        ),
        selectInput(
          inputId = "perf_fig_quant_col",
          label = "Select Quantitative Data to Visualize",
          choices = NULL,
          multiple = T
        ),
        selectInput(
          inputId = "perf_fig_id_col",
          label = "Select Identifiers for Tool Tips (Optional)",
          choices = NULL,
          multiple = T
        ),
        actionButton(
          inputId = "create_perf_figs",
          label = "Create Figures"
        ),
        
        # fluidRow(
        #   column(
        #     id = "perf_fig_block_1",
        #     width = 12,
        #     selectInput(
        #       inputId = "perf_fig_comp_select",
        #       label = "Select Comparison",
        #       choices = NULL
        #     ),
        #     plotlyOutput("perf_fig_1", height = "75rem")
        #   ),
        #   column(
        #     id = "perf_fig_block_2",
        #     class = "hiddenBlock",
        #     width = 6,
        #     selectInput(
        #       inputId = "reference_perf_2",
        #       label = "Select Reference",
        #       choices = NULL
        #     ),
        #     plotlyOutput("perf_fig_2", height = "75rem")
        #   )
        # ),
        
        
        
        
        selectInput(
          inputId = "perf_fig_comp_select",
          label = "Select Figure to Show",
          choices = NULL
        ),
        selectInput(
          inputId = "perf_fig_quant_select",
          label = "Select Figure to Show",
          choices = NULL
        ),
        plotlyOutput("perf_fig", height = "75rem")
      )
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
    # selected = "Select Data Columns"
  ),
  da_modals,
  data_modals,
  select_modals,
  review_modals,
  results_modals,
  compare_modals
)
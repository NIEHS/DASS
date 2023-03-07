# =============================================================================#
# File Name: Step3-SelectColumns.R
# Original Creator: ktto
# Contact Information: ICE-support@niehs.nih.gov
# Date Created: 2021-02-10
# License: MIT
# Description: server file for module with column selection
# Required Packages:
# - data.table, DT
# - shiny, shinyBS, shinyjs
# =============================================================================#

# Step 3: Select Columns -----
## Reactive Values -----
# selected tests ordered as: 2o3, its, ke 3/1 sts
dass_choice <- reactiveVal()

# used to set selections based on template column names
template_col_select <- reactiveValues(
  dpra_call = "",
  dpra_pC = "",
  dpra_pK = "",
  hclat_call = "",
  hclat_mit = "",
  ks_call = "",
  ks_imax = "",
  insilico_call = "",
  insilico_ad = ""
)

## Set Up Panel 3 -----
observeEvent(input$confirm_data, {
  req(usr_dt())
  if (all(!input$do_da_2o3 &
          !input$do_da_its & !input$do_da_ke31)) {
    showNotification(
      type = "error",
      ui = "No defined approaches selected.",
      duration = 10
    )
  } else {
    # set up values for dropdown lists
    col_select_input <- c("", colnames(usr_dt()))
    template_cols <- c("dpra_call","dpra_pC","dpra_pK","hclat_call","hclat_mit",
                       "ks_call","ks_imax","insilico_call","insilico_ad")
    for (i in template_cols) {
      out <- col_select_input[grep_ci(paste0("^", i, "$"), trimws(col_select_input))]
      if (length(out) > 0) {
        template_col_select[[i]] <- out
      }
    }

    # Names of DASS options
    dass_opts <- c("da_2o3", "da_its", "da_ke31")
    # Variables needed for each DASS
    dass_vars <- list(
      da_2o3 = c("ks_call", "dpra_call", "hclat_call"),
      da_its = c("hclat_mit", "dpra_pC", "dpra_pK", "insilico"),
      da_ke31 = c("hclat_mit", "dpra_call")
    )
    # Get user DASS selection as logical vector
    dass_selected <- c(
      input$do_da_2o3,
      input$do_da_its,
      input$do_da_ke31
    )
    # Filter DASS options and DASS variable vectors based on selection
    dass_opts <- dass_opts[dass_selected]
    dass_choice(dass_opts)
    dass_vars <- dass_vars[dass_selected]
    dass_vars <- sort(unique(unlist(dass_vars)))
    
    # Set up UI for Panel 3
    # List of ui objects
    dt_col_ui <- list()

    dt_col_ui$intro <-  HTML(
      "<p>The assay endpoints that are required for the selected DAs are shown below.",
      "Use the dropdown lists to select the columns from your data",
      "that correspond to the given endpoints. Columns are automatically selected for an endpoint",
      "if the column name matches the corresponding column name in the data template.",
      "A column must be selected for each",
      "endpoint shown. When you are finished, click 'Done'.<br><br>",
      "Click on the in information buttons next to the assay endpoint names to view",
      "information about the endpoints and data formatting requirements.",
      "Values that are incorrectly formatted or invalid",
      "will be treated as missing data and may affect the results. More details are given",
      "in the User Guide.</p>"
    )

    # DPRA %C- and %K-depletion
    # For DPRA call, if %C- and %K depletion are provided, user may choose
    # how to determine call - as the DPRA call column, or evaluated from
    # the assay values
    if (all(c("dpra_pC", "dpra_pK") %in% dass_vars)) {
      dt_col_ui$dpra_depl <- fluidRow(div(
        style = "margin-left:25px",
        HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>DPRA % Depletion</span>"),
        actionLink(inputId = "info_dpradep", class = "btn-qs", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
        HTML("</p>"),
        div(
          style = "margin-left:25px",
          selectInput(
            inputId = "dpra_pC_col",
            label = "DPRA %-Cysteine Depletion Column",
            choices = col_select_input,
            selected = template_col_select$dpra_pC,
            selectize = FALSE
          ),
          selectInput(
            inputId = "dpra_pK_col",
            label = "DPRA %-Lysine Depletion Column",
            choices = col_select_input,
            selected = template_col_select$dpra_pK,
            selectize = FALSE
          )
        )
      ))
      if ("dpra_call" %in% dass_vars) {
        dt_col_ui$dpra_call <- fluidRow(div(
          style = "margin-left:25px",
          HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>DPRA Binary Call</span>"),
          actionLink(inputId = "info_dpracall_1", class = "btn-qs", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
          HTML("</p>"),
          div(
            style = "margin-left:25px",
            radioButtons(
              inputId = "dpra_call_choice",
              label = NULL,
              choiceNames = c(
                "Use DPRA Binary Call",
                "Use %-Depletion Values"
              ),
              choiceValues = c(
                "call",
                "pdepl"
              )
            ),
            div(
              style = "margin-left:25px",
              conditionalPanel(
                condition = "input.dpra_call_choice=='call'",
                selectInput(
                  inputId = "dpra_call_col",
                  label = "DPRA Binary Call Column",
                  choices = col_select_input,
                  selected = template_col_select$dpra_call,
                  selectize = FALSE
                )
              )
            )
          )
        ))
      }
    } else if ("dpra_call" %in% dass_vars) {
      dt_col_ui$dpra_call <- fluidRow(div(
        style = "margin-left:25px",
        HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>DPRA Binary Call</span>"),
        actionLink(inputId = "info_dpracall_2", class = "btn-qs", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
        HTML("</p>"),
        div(
          style = "margin-left:25px",
          radioButtons(
            inputId = "dpra_call_choice",
            label = NULL,
            choiceNames = c(
              "Use DPRA Binary Call",
              "Use %-Depletion Values"
            ),
            choiceValues = c(
              "call",
              "pdepl"
            )
          ),
          div(
            style = "margin-left:25px",
            conditionalPanel(
              condition = "input.dpra_call_choice=='call'",
              selectInput(
                inputId = "dpra_call_col",
                label = "DPRA Binary Call Column",
                choices = col_select_input,
                selected = template_col_select$dpra_call,
                selectize = FALSE
              )
            ),
            conditionalPanel(
              condition = "input.dpra_call_choice=='pdepl'",
              selectInput(
                inputId = "dpra_pC_col",
                label = "DPRA %-Cysteine Depletion Column",
                choices = col_select_input,
                selected = template_col_select$dpra_pC,
                selectize = FALSE
              ),
              selectInput(
                inputId = "dpra_pK_col",
                label = "DPRA %-Lysine Depletion Column",
                choices = col_select_input,
                selected = template_col_select$dpra_pK,
                selectize = FALSE
              )
            )
          )
        )
      ))
    }

    # h-CLAT sens/nonsens
    if ("hclat_call" %in% dass_vars) {
      dt_col_ui$hclat_call <- fluidRow(div(
        style = "margin-left:25px",
        HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>h-CLAT Binary Call</span>"),
        actionLink(inputId = "info_hclatcall", class = "btn-qs", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
        HTML("</p>"),
        div(
          style = "margin-left:25px",
          selectInput(
            inputId = "hclat_call_col",
            label = "h-CLAT Binary Call Column",
            choices = col_select_input,
            selected = template_col_select$hclat_call,
            selectize = FALSE
          )
        )
      ))
    }
    
    # h-CLAT MIT
    if ("hclat_mit" %in% dass_vars) {
      dt_col_ui$hclat_mit <- fluidRow(div(
        style = "margin-left:25px",
        HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>h-CLAT MIT</span>"),
        actionLink(inputId = "info_hclatmit", class = "btn-qs", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
        HTML("</p>"),
        div(
          style = "margin-left:25px",
          selectInput(
            inputId = "hclat_mit_col",
            label = "h-CLAT Minimum Induction Threshold (MIT) Column",
            choices = col_select_input,
            selected = template_col_select$hclat_mit,
            selectize = FALSE
          )
        )
      ))
    }
    
    # KS Call - User selects how it should be loaded, as the call column,
    # or as KS iMax to be evaluated as Positive/Negative
    if ("ks_call" %in% dass_vars) {
      dt_col_ui$ks <- fluidRow(div(
        style = "margin-left:25px",
        HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>KeratinoSens&trade; Binary Call</span>"),
        actionLink(inputId = "info_kscall", class = "btn-qs", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
        HTML("</p>"),
        div(
          style = "margin-left:25px",
          selectInput(
            inputId = "ks_call_col",
            label = "KS Binary Call Column",
            choices = col_select_input,
            selected = template_col_select$ks_call,
            selectize = FALSE
          )
        )
      ))
    }
    # in silico
    if ("insilico" %in% dass_vars) {
      dt_col_ui$insilico <- fluidRow(div(
        style = "margin-left:25px",
        HTML("<p><span style='font-size:16px; font-weight:bold; line-height:3;'>In Silico Binary Call</span>"),
        actionLink(inputId = "info_insilico_call", class = "btn-qs", label = NULL, icon = icon("question-sign", lib = "glyphicon")),
        HTML("</p>"),
        div(
          style = "margin-left:25px",
          selectInput(
            inputId = "insilico_call_col",
            label = "In Silico Binary Call Column",
            choices = col_select_input,
            selected = template_col_select$insilico_call,
            selectize = FALSE
          ),
          selectInput(
            inputId = "insilico_ad_col",
            label = "In Silico Applicability Domain",
            choices = col_select_input,
            selected = template_col_select$insilico_ad,
            selectize = FALSE
          )
        )
      ))
    }
    dt_col_ui$done_button <- actionButton(inputId = "review_entries",
                                          label = "Done",
                                          width = "100%")
    
    output$selectcol_ui <- renderUI({
      dt_col_ui
    })
    show("selectcol_ui")
    updateCollapse(session,
                   id = "panels",
                   open = "panel_col_options"
    )
    shinyjs::disable("do_da_2o3")
    shinyjs::disable("do_da_its")
    shinyjs::disable("do_da_ke31")
    shinyjs::disable("fpath")
    shinyjs::disable("button_upload")
    shinyjs::hide("user_data_block_confirm")
    shinyjs::show("user_data_block_reload")
  }
})

## Reload app -----
observeEvent(input$reload_button, {
  showModal(
    modalDialog(
      title = NULL,
      footer = NULL,
      p("Are you sure you want to reload the app? All selections will be reset."),
      actionButton(inputId = "confirm_reload", label = "Reload App"),
      actionButton(inputId = "cancel_reload", label = "Cancel")
    )
  )
}
)

observeEvent(input$confirm_reload, {
  session$reload()
})

observeEvent(input$cancel_reload, {
  removeModal()
})

## Questions -----
### DPRA -----
observeEvent(input$info_dpradep, {
  toggleModal(session, "dpra_dep_modal", toggle = "open")
})

observeEvent(input$info_dpracall_1, {
  toggleModal(session, "dpra_call_modal", toggle = "open")
})

observeEvent(input$info_dpracall_2, {
  toggleModal(session, "dpra_call_modal", toggle = "open")
})

### h-CLAT -----
observeEvent(input$info_hclatcall, {
  toggleModal(session, "hclat_call_modal", toggle = "open")
})

observeEvent(input$info_hclatmit, {
  toggleModal(session, "hclat_mit_modal", toggle = "open")
})

### KeratinoSens -----
observeEvent(input$info_kscall, {
  toggleModal(session, "ks_call_modal", toggle = "open")
})

### In Silico -----
observeEvent(input$info_insilico_call, {
  toggleModal(session, "insilico_modal", toggle = "open")
})
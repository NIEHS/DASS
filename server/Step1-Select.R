# =============================================================================#
# File Name: Step1-Select.R                                                    #
# Original Creator: ktto                                                       #
# Contact Information: comptox@ils-inc.com                                     #
# Date Created: 2021-02-10                                                     #
# License: MIT                                                                 #
# Description: server object for module with da selection                      #
# Required Packages:                                                           #
# - data.table, DT                                                             #
# - openxlsx                                                                   #
# - readxl                                                                     #
# - shiny shinyBS shinyjs                                                      #
# =============================================================================#

# Step 1: Select Approaches -----
# link to select all strategies
observeEvent(input$dass_all, {
  updateCheckboxInput(
    inputId = "do_da_2o3",
    value = TRUE
  )
  updateCheckboxInput(
    inputId = "do_da_its",
    value = TRUE
  )
  updateCheckboxInput(
    inputId = "do_da_ke31",
    value = TRUE
  )
})

# link to select no strategies
observeEvent(input$dass_none, {
  updateCheckboxInput(
    inputId = "do_da_2o3",
    value = FALSE
  )
  updateCheckboxInput(
    inputId = "do_da_its",
    value = FALSE
  )
  updateCheckboxInput(
    inputId = "do_da_ke31",
    value = FALSE
  )
})

## Questions -----
# text for info popups
observeEvent(input$info_2o3, {
  showModal(modalDialog(
    title = "2 out of 3",
    HTML(
      "<p>The 2 out of 3 (2o3) Defined Approach is a sequential testing strategy",
      "for predicting hazard (sensitizers or non-sensitizers) based on at",
      "least 2 concordant results among the direct peptide reactivity assay (DPRA),",
      "KeratinoSens&trade;, and human cell-line activiation test (h-CLAT).",
      "<br><br>",
      "For more details, see <em>OECD Guideline No. 497: Defined Approaches on Skin",
      "Sensitisation</em>[<a href='https://doi.org/https://doi.org/10.1787/b92879a4-en'",
      "target = '_blank'>1</a>]</p>"
    ),
    easyClose = T
  ))
})

observeEvent(input$info_its, {
  showModal(modalDialog(
    title = "Integrated Testing Strategy",
    HTML(
      "The Integrated Testing Strategy (ITS) Defined Approach predicts chemical",
      "hazard and GHS potency category by",
      "scoring results from the the direct peptide reactivity assay (DPRA),",
      "human cell-line activiation test (h-CLAT), and <em>in silico</em> predictions",
      "from either <a href='https://www.lhasalimited.org/products/skin-sensitisation-assessment-using-derek-nexus.htm'",
      "target = '_blank'>Derek Nexus</a> or the OECD QSAR Toolbox[<a href='https://doi.org/10.1016/j.comtox.2019.01.006'",
      "target = '_blank'>7</a>].<br><br>",
      "For more details, see <em>OECD Guideline No. 497: Defined Approaches on Skin",
      "Sensitisation</em>[<a href='https://doi.org/https://doi.org/10.1787/b92879a4-en'",
      "target = '_blank'>1</a>]</p>"
    ),
    easyClose = T
  ))
})

observeEvent(input$info_ke31, {
  showModal(modalDialog(
    title = "Key Event 3/1 (KE 3/1) Sequential Testing Strategy (STS)",
    HTML(
      "<p>The Key Event 3/1 Sequential Testing Strategy is a sequential testing strategy",
      "that predicts chemical hazard and",
      "GHS potency category based on results from the the direct",
      "peptide reactivity assay (DPRA) and human cell-line activiation test (h-CLAT).",
      "<br><br>",
      "For more details, see EPA's <em>Interim Science Policy: Use of Alternative",
      "Approaches for Skin Sensitization as a Replacement for Laboratory Animal",
      "Testing Draft for Public Comment</em>[<a",
      "href='https://www.regulations.gov/document/EPA-HQ-OPP-2016-0093-0090'",
      "target = '_blank'>2</a>]</p>"
    ),
    easyClose = T
  ))
})
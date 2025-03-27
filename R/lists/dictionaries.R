tab_names <- list(
  select_defined_approach = "Select Defined Approach",
  upload_data = "Upload Data",
  select_data_columns = "Select Data Columns",
  review_selection = "Review Selection",
  results = "Results",
  compare = "Compare"
)

da_dict <- list(
  da_2o3 = list(
    full_name = "2 out of 3",
    abbrev = "2o3",
    col_req_std_ui = c(
      "ke1_std"
    ),
    col_req_std = c(
      "ke1_call",
      "ke2_call",
      "ke3_call"
    )
  ),
  da_its = list(
    full_name = "Integrated Testing Strategy",
    abbrev = "ITS",
    col_req_std = c(
      "ke1_mean_dep",
      "ke3_val",
      "insil_call",
      "insil_ad"
    )
  ),
  da_ke31 = list(
    full_name = "Key Event 3/1 Sequential Testing Strategy",
    abbrev = "KE 3/1 STS",
    col_req_std = c(
      "ke1_call",
      "ke3_val"
    )
  )
)

assay_dict <- list(
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

external_links <- list(
  niceatm_dass = list(
    url   = "https://ntp.niehs.nih.gov/go/40498",
    label = "Defined Approaches to Identify Potential Skin Sensitizers"
  ),
  dass_github = list(
    url   = "https://github.com/NIEHS/DASS",
    label = "Source Code"
  )
)
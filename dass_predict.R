# ============================================================================#
# File Name: dass_predict.r                                                   #
# Original Creator: ktto                                                      #
# Contact Information: comptox@ils-inc.com                                    #
# Date Created: 2021-12-03                                                    #
# License: MIT                                                                #
# Description: description                                                    #
# Required Packages:                                                          #
# - data.table, DT                                                            #
# - openxlsx                                                                  #
# - readxl                                                                    #
# - shiny shinyBS shinyjs                                                     #
# ============================================================================#

# Function to read in data
# `fpath` - user-supplied path to data file
read_data <- function(fpath, sheet = 1) {
  if (grepl("txt$|tsv$|csv$", fpath)) {
    fread(fpath, colClasses = "character", na.strings = c("NA", ""))
  } else if (grepl("xls$|xlsx$", fpath)) {
    data.table(readxl::read_excel(fpath, sheet = sheet, na = c("NA", "")))
  } else {
    stop("Incorrect file extension.")
  }
}

# grep functions for case-insensitive matching
grep_ci <- function(str, x) grep(pattern = str, x = x, ignore.case = T)
grepl_ci <- function(str, x) grepl(pattern = str, x = x, ignore.case = T)

# Function to produce the columns needed to calculate the predictions
# the user has requested.
# `dass` - a vector or character string to indicate which defined approaches
#          will be calculated
# `ks_call_method` - a character string that indicates if KeratinoSens Call
#                    will be provided as a call value or if call needs to be
#                    derived from iMax
# `dpra_call_method` - a character string that indicates if DPRA Call will
#                      be provided as a call value or if call needs to be
#                      derived from % depletion values
# Output: Returns a vector with the column labels for columns required
#         to implement the selected defined approaches
check_cols <- function(dass = c("da_2o3", "da_its", "da_ke31"),
                       ks_call_method = NULL,
                       dpra_call_method = NULL) {
  # Create list to store column labels
  cols_to_check <- vector(mode = "list", length = 3)
  names(cols_to_check) <- c("da_2o3", "da_its", "da_ke31")
  # 2o3 requires KeratinoSens Call and DPRA Call
  if ("da_2o3" %in% dass) {
    cols_to_check[["da_2o3"]] <- "hclat_call"
    if (ks_call_method == "call") {
      cols_to_check[["da_2o3"]] <- c(cols_to_check[["da_2o3"]], "ks_call")
    } else if (ks_call_method == "imax") {
      cols_to_check[["da_2o3"]] <- c(cols_to_check[["da_2o3"]], "ks_imax")
    }

    if (dpra_call_method == "call") {
      cols_to_check[["da_2o3"]] <- c(cols_to_check[["da_2o3"]], "dpra_call")
    } else if (dpra_call_method == "pdepl") {
      cols_to_check[["da_2o3"]] <- c(cols_to_check[["da_2o3"]], "dpra_pC", "dpra_pK")
    }
  }
  # ITS requires h-CLAT MIT, dpra %C-depletion, DPRA %K-depletion,
  # in silico hazard identification and applicablity domain
  if ("da_its" %in% dass) {
    cols_to_check[["da_its"]] <- c("hclat_mit", "dpra_pC", "dpra_pK", "insilico_call", "insilico_ad")
  }

  # KE 3/1 STS requires h-CLAT MIT and DPRA Call
  if ("da_ke31" %in% dass) {
    cols_to_check[["da_ke31"]] <- "hclat_mit"
    if (dpra_call_method == "call") {
      cols_to_check <- c(cols_to_check, "dpra_call")
    } else if (dpra_call_method == "pdepl") {
      cols_to_check <- c(cols_to_check, "dpra_pC", "dpra_pK")
    }
  }
  # Returns character vector
  sort(unique(unlist(cols_to_check)))
}

# Function that performs the requested DASS by calling the functions for each
# individual DASS
# `dt` - A data table containing all required columns for the requested DASS.
#        Column names must be the column labels used throughout the app:
#        'dpra_pC' = DPRA %C-Depletion (numeric)
#        'dpra_pK' = DPRA %K-Depletion (numeric)
#        'dpra_call' = DPRA Hazard ID (0 or 1)
#        'hclat_call' = h-CLAT Hazard ID (0 or 1)
#        'hclat_mit' = h-CLAT MIT (character)
#        'ks_call' = KeratinoSens Hazard ID (0 or 1)
#        'ks_imax' = KeratinoSens iMax (quantitative)
#        'insilico_call' = In silico Hazard ID (0 or 1) from OECD QSAR Toolbox or Derek Nexus
#        'insilico_ad' = In silico Applicability Domain (0 or 1) from OECD QSAR Toolbox or Derek Nexus
# `dass` - a vector or character string to indicate which defined approaches
#          will be calculated
# `ks_call_method` - a character string that indicates if KeratinoSens Call
#                    will be provided as a call value or if call needs to be
#                    derived from iMax
# `dpra_call_method` - a character string that indicates if DPRA Call will
#                      be provided as a call value or if call needs to be
#                      derived from % depletion values
# Output: Returns a data table with rows corresponding to rows in
#         the original user data and `dt`. Data table contains DASS
#         predictions
dass_predict <- function(dt, dass = c("da_2o3", "da_its", "da_ke31"),
                         ks_call_method = NULL,
                         dpra_call_method = NULL) {
  # Set KS Call based on user selection
  if (!is.null(ks_call_method)) {
    if (ks_call_method == "call") {
      ks_call <- dt$ks_call
    } else if (ks_call_method == "imax") {
      ks_call <- ifelse(dt$ks_imax >= 1.5, 1, 0)
      dt[, ks_call_calculated := ks_call]
    }
  }

  # Calculate DPRA mean
  if ("da_its" %in% dass) {
    dt[, id := 1:.N]
    dt[!is.na(dpra_pC) & !is.na(dpra_pK),
      dpra_mean_calculated := mean(c(max(0, dpra_pC), max(0, dpra_pK))),
      by = id
    ]
    dt[, id := NULL]
  }

  # Set DPRA Call based on user selection
  if (!is.null(dpra_call_method)) {
    if (dpra_call_method == "call") {
      dpra_call <- dt[, dpra_call]
    } else if (dpra_call_method == "pdepl") {
      # Calculate dpra mean if not already calculated
      if (!"da_its" %in% dass) {
        dt[, id := 1:.N]
        dt[!is.na(dpra_pC) & !is.na(dpra_pK),
          dpra_mean_calculated := mean(c(max(0, dpra_pC), max(0, dpra_pK))),
          by = id
        ]
        dt[, id := NULL]
      }

      # Call is set as a variable
      dpra_call <- dt[, fcase(
        # If %C-dep is not given, then no evaluation
        is.na(dpra_pC), as.numeric(NA),
        # If %K-dep is not given, use %C-dep
        is.na(dpra_pK), fifelse(dpra_pC <= 13.89, 0, 1),
        # If both %C and %K dep given, use mean
        !is.na(dpra_pC) & !is.na(dpra_pK), fifelse(dpra_mean_calculated <= 6.38, 0, 1)
      )]
      dt[, dpra_call_calculated := ..dpra_call]
    }
  }

  # Set up list of results
  res_list <- vector(mode = "list", length = 0)
  if ("da_2o3" %in% dass) {
    temp <- da_2o3(
      ks_call = ks_call,
      hclat_call = dt[, hclat_call],
      dpra_call = dpra_call
    )
    res_list$DA_2o3 <- data.table(DA_2o3_Call = temp)
  }

  if ("da_its" %in% dass) {
    temp <- da_its(
      hclat_mit = dt[, hclat_mit],
      dpra_pC = dt[, dpra_pC],
      dpra_pK = dt[, dpra_pK],
      dpra_mean = dt[, dpra_mean_calculated],
      insilico_call = dt[, insilico_call],
      insilico_domain = dt[, insilico_ad]
    )
    res_list$DA_IT2 <- data.table(
      ITS_hCLAT_Score = temp$ITS_hCLAT_Score,
      ITS_DPRA_Score = temp$ITS_DPRA_Score,
      ITS_inSilico_Score = temp$ITS_inSilico_Score,
      ITS_TotalScore = temp$ITS_TotalScore,
      DA_ITS_Call = temp$ITS_Call,
      DA_ITS_Potency = temp$ITS_Potency
    )
  }

  if ("da_ke31" %in% dass) {
    temp <- da_ke31(
      hclat_mit = dt[, hclat_mit],
      dpra_call = dpra_call
    )
    res_list$DA_KE31STS <- data.table(
      DA_KE31STS_Call = temp$KE31STS_Call,
      DA_KE31STS_Potency = temp$KE31STS_Potency
    )
  }
  # Merge results into datatable
  names(res_list) <- NULL
  res <- do.call("cbind", res_list)
  dt_names <- sort(colnames(dt))
  dt <- dt[, ..dt_names]
  cbind(dt, res)
}

# Performs DASS 2 out of 3
# `ks_call` - a numeric vector for KeratinoSens call, where '0' indicates a
#             negative call and '1' indicates a positive call
# `hclat_call` - a numeric vector for h-CLAT call, where '0' indicates a
#                negative call and '1' indicates a positive call
# `dpra_call` - a numeric vector for DPRA call, where '0' indicates a
#               negative call and '1' indicates a positive call
da_2o3 <- function(ks_call, hclat_call, dpra_call) {
  # Combine data
  temp <- data.table(ks_call, hclat_call, dpra_call)
  temp[, id := 1:nrow(temp)]
  # Count the number of positive, negative, and missing
  temp[, n_pos := sum(ks_call == 1, hclat_call == 1, dpra_call == 1, na.rm = T), by = id]
  temp[, n_neg := sum(ks_call == 0, hclat_call == 0, dpra_call == 0, na.rm = T), by = id]
  temp[, n_miss := sum(is.na(ks_call), is.na(hclat_call), is.na(dpra_call)), by = id]
  temp[, DA_2o3_Call := fcase(
    n_miss >= 2, as.character(NA),
    n_pos >= 2, "1",
    n_neg >= 2, "0",
    n_pos < 2 & n_neg < 2, "Inconclusive"
  ), by = id]

  temp[, DA_2o3_Call]
}

# Performs DASS ITS
# `hclat_mit` - numeric vector containing MIT values. Negative results are 'Inf'
# `dpra_pC` - numeric vector for %C-depletion
# `dpra_pK` - numeric vector for %K-depletion
# `insilico_call` - a numeric vector for in silico call prediction from either oecd
#               qsar toolbox or derek nexus. '0' indicates a
#               negative call and '1' indicates a positive call
# `insilico_domain` - a numeric vector for in silico applicability domain from either
#                 oecd qsar toolbox or derek nexus.
#                 '0' indicates a prediction outside the
#                 applicability domain and '1' indicates a prediction within
#                 the applicability domain
# Returns a Call and Potency prediction

da_its <- function(hclat_mit, dpra_pC, dpra_pK, dpra_mean, insilico_call, insilico_domain) {
  temp <- data.table(hclat_mit, dpra_pC, dpra_pK, dpra_mean, insilico_call, insilico_domain)
  temp[, id := 1:nrow(temp)]

  # Calculate h-CLAT score
  temp[, hclat_score := fcase(
    hclat_mit == Inf, 0,
    is.na(hclat_mit), as.numeric(NA),
    hclat_mit <= 10, 3,
    hclat_mit > 10 & hclat_mit <= 150, 2,
    hclat_mit > 150 & hclat_mit <= 5000, 1
  ), by = id]

  # Calculate DPRA score
  temp[, dpra_score := fcase(
    # No %C-dep
    is.na(dpra_pC), as.numeric(NA),
    # No %K-dep
    is.na(dpra_pK), fcase(
      dpra_pC >= 98.24, 3,
      dpra_pC >= 23.09, 2,
      dpra_pC >= 13.89, 1,
      dpra_pC < 13.89, 0
    ),
    # Both given
    !is.na(dpra_pC) & !is.na(dpra_pK), fcase(
      dpra_mean >= 42.47, 3,
      dpra_mean >= 22.62, 2,
      dpra_mean >= 6.38, 1,
      dpra_mean < 6.38, 0
    )
  ), by = id]

  # Calculate in silico tool SCORE
  temp[, insilico_score := fcase(
    # outside AD
    insilico_domain == 0, as.numeric(NA),
    # no information provided
    is.na(insilico_call) | is.na(insilico_domain), as.numeric(NA),
    # Positive
    insilico_call == 1, 1,
    # Negative
    insilico_call == 0, 0
  ), by = id]

  # Different scoring schemes are used depending on available data sources
  # Label each row based on available data
  temp[, flow := fcase(
    # All 3
    all(!is.na(c(hclat_score, dpra_score, insilico_score))), "all",
    # No in silico prediction
    all(!is.na(hclat_score) & !is.na(dpra_score) & is.na(insilico_score)), "no_ins",
    # Only h-CLAT and in silico
    all(!is.na(c(hclat_score, insilico_score))) & is.na(dpra_score), "ins",
    # Only DPRA and in silico
    all(!is.na(c(dpra_score, insilico_score))) & is.na(hclat_score), "ins"
  ), by = id]

  # Calculate total ITS score if at least 2 of 3 assays have data
  temp[(is.na(hclat_score) + is.na(dpra_score) + is.na(insilico_score)) < 2,
    its_score := sum(c(hclat_score, dpra_score, insilico_score), na.rm = T),
    by = id
  ]

  # Get potency scores
  # All assays available
  temp[flow == "all", its_cat := fcase(
    its_score %in% 6:7, "1A",
    its_score %in% 2:5, "1B",
    its_score %in% 0:1, "NC"
  )]
  # DPRA, h-CLAT, but no in silico
  temp[flow == "no_ins", its_cat := fcase(
    its_score == 6, "1A",
    its_score == 5, "1*",
    its_score %in% 2:4, "1B",
    its_score == 1, "Inconclusive",
    its_score == 0, "NC"
  )]
  # in silico and (DPRA or h-CLAT), with either (DPRA or h-CLAT) not available
  temp[flow == "ins", its_cat := fcase(
    its_score %in% 3:4, "1*",
    its_score == 2, "1B",
    its_score %in% 0:1, "Inconclusive"
  )]

  # Use potency scores to derive calls
  temp[, its_call := fcase(
    its_cat %in% c("1A", "1*", "1B"), "1",
    its_cat == "Inconclusive", "Inconclusive",
    its_cat == "NC", "0"
  )]

  # Update potency score for 1*
  temp[its_cat == "1*", its_cat := "Inconclusive"]

  list(
    ITS_hCLAT_Score = temp$hclat_score,
    ITS_DPRA_Score = temp$dpra_score,
    ITS_inSilico_Score = temp$insilico_score,
    ITS_TotalScore = temp$its_score,
    ITS_Call = temp$its_call,
    ITS_Potency = temp$its_cat
  )
}
# Performs DASS KE 3/1 STS
# `hclat_mit` - numeric vector containing MIT values. Negative results are 'Inf'
# `dpra_call` - a numeric vector for DPRA call, where '0' indicates a
#               negative call and '1' indicates a positive call
# Returns a Call and Potency prediction
da_ke31 <- function(hclat_mit, dpra_call) {
  # Combine data
  temp <- data.table(hclat_mit, dpra_call)
  temp[, id := 1:nrow(temp)]

  temp[, KE31STS_Call := fcase(
    hclat_mit == Inf, dpra_call, # if negative h-clat, use dpra
    is.na(hclat_mit), as.numeric(NA), # if no h-clat, NA
    !is.na(hclat_mit), 1 # if MIT exists and != Inf, positive
  ), by = id]

  temp[, KE31STS_Potency := fcase(
    is.na(hclat_mit), as.character(NA), # if no h-clat, NA
    hclat_mit == Inf & dpra_call == 1, "1B", # if neg hclat and pos dpra, weak sensitizer
    hclat_mit == Inf & dpra_call == 0, "NC", # if neg hclat and neg dpra, not classified(?)
    hclat_mit == Inf & is.na(dpra_call), as.character(NA), # if neg hclat and no dpra, NA
    hclat_mit <= 10, "1A", # strong sensitizer
    hclat_mit > 10 & hclat_mit < 5000, "1B" # weak sensitizer
  ), by = id]

  list(
    KE31STS_Call = temp$KE31STS_Call,
    KE31STS_Potency = temp$KE31STS_Potency
  )
}

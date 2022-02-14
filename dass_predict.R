#=============================================================================#
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
#=============================================================================#

# Function to read in data
# `fpath` - user-supplied path to data file
read_data <- function(fpath) {
  if (grepl("txt$|tsv$|csv$", fpath)) {
    fread(fpath, colClasses = "character", na.strings = c("NA", ""))
  } else if (grepl("xls$|xlsx$", fpath)) {
    data.table(readxl::read_excel(fpath, na = c("NA", "")))
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
check_cols <- function(dass = c("da_2o3", "da_itsv2", "da_ke31"),
                       ks_call_method = NULL,
                       dpra_call_method = NULL) {
  cols_to_check <- vector(mode = "list", length = 3)
  names(cols_to_check) <- c("da_2o3", "da_itsv2", "da_ke31")
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
  # ITSv2 requires h-CLAT MIT, dpra %C-depletion, DPRA %K-depletion, 
  # oecd qsar tb Call, and OECD QSAR TB Applicability Domain
  if ("da_itsv2" %in% dass) {
    cols_to_check[["da_itsv2"]] <- c("hclat_mit", "dpra_pC", "dpra_pK", "oecd_tb_call", "oecd_tb_ad")
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
#        'oecd_tb_call' = OECD QSAR Toolbox Hazard ID (0 or 1)
#        'oecd_tb_ad' = OECD QSAR Toolbox Applicability Domain (0 or 1)
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
dass_predict <- function(dt, dass = c("da_2o3", "da_itsv2", "da_ke31"),
                         ks_call_method = NULL,
                         dpra_call_method = NULL) {
  # Set KS Call based on user selection
  if (!is.null(ks_call_method)) {
    if (ks_call_method == "call") {
      ks_call <- dt$ks_call
    } else if (ks_call_method == "imax") {
      ks_call <- ifelse(dt$ks_imax >= 1.5, 1, 0)
      dt[,ks_call_calculated := ks_call]
    }
  }
  
  if ("da_itsv2" %in% dass) {
    dt[,id := 1:.N]
    dt[!is.na(dpra_pC) & !is.na(dpra_pK),
       dpra_mean_calculated := mean(c(max(0, dpra_pC), max(0, dpra_pK))),
       by = id]
    dt[,id := NULL]
  }
  
  # Set DPRA Call based on user selection
  if (!is.null(dpra_call_method)) {
    if (dpra_call_method == "call") {
      dpra_call <- dt[,dpra_call]
    } else if (dpra_call_method == "pdepl") {
      # Calculate dpra mean
      if (!"da_itsv2" %in% dass) {
        dt[,id := 1:.N]
        dt[!is.na(dpra_pC) & !is.na(dpra_pK),
           dpra_mean_calculated := mean(c(max(0, dpra_pC), max(0, dpra_pK))),
           by = id]
        dt[,id := NULL]
      }
      
      # Call is set as a variable
      dpra_call <- dt[,fcase(
        # If %C-dep is not given, then no evaluation
        is.na(dpra_pC), as.numeric(NA),
        # If %K-dep is not given, use %C-dep
        is.na(dpra_pK), fifelse(dpra_pC <= 13.89, 0, 1),
        # If both %C and %K dep given, use mean
        !is.na(dpra_pC) & !is.na(dpra_pK), fifelse(dpra_mean_calculated <= 6.38, 0, 1)
      )]
      dt[,dpra_call_calculated := ..dpra_call]
    }
  }
  
  # Set up list of results
  res_list <- vector(mode = "list", length = 0)
  if ("da_2o3" %in% dass) {
    temp <- da_2o3(
      ks_call = ks_call,
      hclat_call = dt[,hclat_call],
      dpra_call = dpra_call
    )
    res_list$DA_2o3 <- data.table(DA_2o3_Call = temp)
  }
  
  if ("da_itsv2" %in% dass) {
    temp <- da_itsv2(
      hclat_mit = dt[,hclat_mit],
      dpra_pC = dt[,dpra_pC],
      dpra_pK = dt[,dpra_pK],
      dpra_mean = dt[,dpra_mean_calculated],
      oecd_call = dt[,oecd_tb_call],
      oecd_domain = dt[,oecd_tb_ad]
    )
    res_list$DA_ITSv2 <- data.table(
      ITS_hCLAT_Score = temp$ITS_hCLAT_Score,
      ITS_DPRA_Score = temp$ITS_DPRA_Score,
      ITS_OECDQSARTB_Score = temp$ITS_OECDQSARTB_Score,
      ITS_TotalScore = temp$ITS_TotalScore,
      DA_ITS_Call = temp$ITS_Call,
      DA_ITS_Potency = temp$ITS_Potency
    )
  }

  if("da_ke31" %in% dass) {
    temp <- da_ke31(
      hclat_mit = dt[,hclat_mit],
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
  dt <- dt[,..dt_names]
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
  temp[,id := 1:nrow(temp)]
  temp[,n_pos := sum(ks_call == 1, hclat_call == 1, dpra_call == 1, na.rm = T), by = id]
  temp[,n_neg := sum(ks_call == 0, hclat_call == 0, dpra_call == 0, na.rm = T), by = id]
  temp[,n_miss := sum(is.na(ks_call), is.na(hclat_call), is.na(dpra_call)), by = id]
  temp[,DA_2o3_Call := fcase(
    n_miss >= 2, as.character(NA),
    n_pos >= 2, "1",
    n_neg >= 2, "0",
    n_pos < 2 & n_neg < 2,"Inconclusive"
  ), by = id]
  
  temp[,DA_2o3_Call]
}

# Performs DASS ITSv2
# `hclat_mit` - character vector containing MIT values or a character string 
#               'n', 'neg', or 'negative' (case insensitive) for negative
#               test outcome
# `dpra_pC` - numeric vector for %C-depletion
# `dpra_pK` - numeric vector for %K-depletion
# `oecd_call` - a numeric vector for OECD QSAR TB call, where '0' indicates a 
#               negative call and '1' indicates a positive call
# `oecd_domain` - a numeric vector for OECD QSAR TB applicability domain, where 
#                 '0' indicates a prediction outside the OECD QSAR TB 
#                 applicability domain and '1' indicates a prediction within 
#                 the OECD QSAR TB applicability domain
# Returns a Call and Potency prediction

da_itsv2 <- function(hclat_mit, dpra_pC, dpra_pK, dpra_mean, oecd_call, oecd_domain){
  temp <- data.table(hclat_mit, dpra_pC, dpra_pK, dpra_mean, oecd_call, oecd_domain)
  temp[,id := 1:nrow(temp)]

  # Calculate h-CLAT score
  temp[,hclat_score := fcase(
    hclat_mit == Inf, 0,
    is.na(hclat_mit), as.numeric(NA),
    hclat_mit <= 10, 3,
    hclat_mit > 10 & hclat_mit <= 150,2,
    hclat_mit >150 & hclat_mit <= 5000,1
  ), by = id]

  # Calculate DPRA score
  temp[,dpra_score := fcase(
    # No %C-dep
    is.na(dpra_pC), as.numeric(NA),
    # No %K-dep
    is.na(dpra_pK), fcase(dpra_pC >= 98.24, 3,
                          dpra_pC >= 23.09, 2,
                          dpra_pC >= 13.89, 1,
                          dpra_pC < 13.89, 0),
    # Both given
    !is.na(dpra_pC) & !is.na(dpra_pK), fcase(dpra_mean >= 42.47, 3,
                                             dpra_mean >= 22.62, 2, 
                                             dpra_mean >= 6.38, 1,
                                             dpra_mean < 6.38, 0)
  ), by = id]

  # Calculate OECD QSAR TB SCORE
  temp[,oecd_score := fcase(
    # outside AD
    oecd_domain == 0, as.numeric(NA),
    # no AD information provided
    is.na(oecd_call) | is.na(oecd_domain), as.numeric(NA),
    # Positive
    oecd_call == 1, 1,
    # Negative
    oecd_call == 0, 0
  ), by = id]
  
  # Different scoring schemes are used depending on available data sources
  # Label each row based on available data

  temp[,flow := fcase(
    all(!is.na(c(hclat_score, dpra_score, oecd_score))), "all",
    all(!is.na(hclat_score) & !is.na(dpra_score) & is.na(oecd_score)), "no_ins",
    all(!is.na(c(hclat_score, oecd_score))) & is.na(dpra_score), "ins",
    all(!is.na(c(dpra_score, oecd_score))) & is.na(hclat_score), "ins"
  ), by = id]

  # Calculate total ITSv2 score
  # temp[,itsv2_score := sum(c(hclat_score, dpra_score, oecd_score), na.rm = T), by = id]
  temp[(is.na(hclat_score) + is.na(dpra_score) + is.na(oecd_score)) < 2,
       itsv2_score := sum(c(hclat_score, dpra_score, oecd_score), na.rm = T), by = id]
  
  # Get potency scores
  # All assays available
  temp[flow == "all",itsv2_cat := fcase(
    itsv2_score %in% 6:7, "1A",
    itsv2_score %in% 2:5, "1B",
    itsv2_score %in% 0:1, "NC"
  )]
  # DPRA, h-CLAT, but no QSAR
  temp[flow == "no_ins",itsv2_cat := fcase(
    itsv2_score == 6, "1A",
    itsv2_score == 5, "1*",
    itsv2_score %in% 2:4, "1B",
    itsv2_score == 1, "Inconclusive",
    itsv2_score == 0, "NC"
  )]
  # QSAR and (DPRA or h-CLAT), with either (DPRA or h-CLAT) not available
  temp[flow == "ins",itsv2_cat := fcase(
    itsv2_score %in% 3:4, "1*",
    itsv2_score == 2, "1B",
    itsv2_score %in% 0:1, "Inconclusive"
  )]
  
  # Use potency scores to derive calls
  temp[,itsv2_call := fcase(
    itsv2_cat %in% c("1A", "1*", "1B"), "1",
    itsv2_cat == "Inconclusive", "Inconclusive",
    itsv2_cat == "NC", "0"
  )]
  
  # Update potency score for 1*
  temp[itsv2_cat == "1*", itsv2_cat := "Inconclusive"]
  
  list(
    ITS_hCLAT_Score = temp$hclat_score,
    ITS_DPRA_Score = temp$dpra_score,
    ITS_OECDQSARTB_Score = temp$oecd_score,
    ITS_TotalScore = temp$itsv2_score,
    ITS_Call = temp$itsv2_call,
    ITS_Potency = temp$itsv2_cat
  )
}

# Performs DASS KE 3/1 STS
# `hclat_mit` - character vector containing MIT values or a character string 
#               'n', 'neg', or 'negative' (case insensitive) for negative
#               test outcome
# `dpra_call` - a numeric vector for DPRA call, where '0' indicates a 
#               negative call and '1' indicates a positive call
# Returns a Call and Potency prediction
da_ke31 <- function(hclat_mit, dpra_call) {
  temp <- data.table(hclat_mit, dpra_call)
  temp[,id := 1:nrow(temp)]
  
  temp[,KE31STS_Call := fcase(
    hclat_mit == Inf, dpra_call, 
    is.na(hclat_mit), as.numeric(NA),
    !is.na(hclat_mit), 1
  ), by = id]
  
  temp[,KE31STS_Potency := fcase(
    is.na(hclat_mit), as.character(NA),
    hclat_mit == Inf & dpra_call == 1, "1B",
    hclat_mit == Inf & dpra_call == 0, "NC",
    hclat_mit == Inf & is.na(dpra_call), as.character(NA),
    hclat_mit <= 10, "1A",
    hclat_mit >10 & hclat_mit < 5000, "1B"
  ), by = id]
  
  list(
    KE31STS_Call = temp$KE31STS_Call,
    KE31STS_Potency = temp$KE31STS_Potency
  )
}
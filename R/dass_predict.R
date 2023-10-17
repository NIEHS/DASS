# ============================================================================#
# File Name: dass_predict.r
# Original Creator: ktto
# Contact Information: ICE-support@niehs.nih.gov
# Date Created: 2021-12-03
# License: MIT
# Description: Functions used within the DASS app
# Required Packages:
# - data.table
# - readxl
# ============================================================================#

# Function to read in data
# `fpath` - user-supplied path to data file
read_data <- function(fpath, sheet = 1) {
  if (grepl("txt$|tsv$|csv$", fpath)) {
    fread(fpath, colClasses = "character", na.strings = c("NA", ""))
  } else if (grepl("xls$|xlsx$", fpath)) {
    # Read columns in as list to prevent displaying converted floats
    tmp <- data.table(read_excel(fpath, sheet = sheet, na = c("NA", ""), col_types = "list"))
    tmp[,lapply(.SD, function(x) as.character(unlist(x)))]
  } else {
    stop("Incorrect file extension.")
  }
}

# grep functions for case-insensitive matching
grep_ci <- function(str, x, ...) grep(pattern = str, x = x, ignore.case = T, ...)
grepl_ci <- function(str, x, ...) grepl(pattern = str, x = x, ignore.case = T, ...)

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
# `dpra_mean` - numeric vector with the average of `dpra_pc` and `dpra_pk`
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

# Creates exact match for multiple strings for grepping
# `vector` - character vector
# Returns a query string for mattching multiple patterns
concatOrString <- function(vector) {
  paste(sprintf("^%s$", vector), collapse = "|")
}

roundPercent <- function(x, digits = 0) {
  paste0(round(x * 100, digits = digits), "%")
}

# `pred` - factor vector with 0/1
# `ref` - factor vector with 0/1. reference for comparing against pred
# Calculates binary performance metrics
compareBinary <- function(pred, ref) {
  if (!all(na.omit(pred) %in% c(0,1, "Inconclusive"))) {
    stop("`pred` should be a factor with levels = c(0,1, Inconclusive)")
  }
  if (!all(na.omit(ref) %in% c(0,1))) {
    stop("`ref` should be a factor with levels = c(0,1)")
  }
  if (length(pred) != length(ref)) {
    stop("`pred` and `ref` should be the same length")
  }
  
  # Keep only cases with reference data
  refNA <- is.na(ref)
  ref <- ref[!refNA]
  pred <- pred[!refNA]
  
  # Check number of pairs
  n <- length(ref)
  if (n <= 5) {
    stop("Fewer than 5 non-missing pairs")
  }

  pred <- factor(pred, levels = c("1", "0", "Inconclusive"), labels = c("Positive", "Negative", "Inconclusive"))
  pred <- droplevels(pred)
  ref <- factor(ref, levels = c("1", "0"), labels = c("Positive", "Negative"))
  ref <- droplevels(ref)
  
  cm <- table(pred, ref, useNA = "ifany")
  tp <- cm[1,1]
  fp <- cm[1,2]
  tn <- cm[2,2]
  fn <- cm[2,1]

  acc <- (tp + tn)/n
  tpr <- tp/(sum(ref == "Positive"))
  fpr <- fp/(sum(ref == "Negative"))
  tnr <- tn/(sum(ref == "Negative"))
  fnr <- fn/(sum(ref == "Positive"))
  balAcc <- (tpr + tnr)/2
  f1 <- (2*tp)/((2*tp) + fp + fn)
  
  vals <- list(
    N = sum(cm),
    truePositive = tp,
    falsePositive = fp,
    trueNegative = tn,
    falseNegative = fn,
    accuracy = acc,
    balancedAccuracy = balAcc,
    f1Score = f1,
    truePositiveRate = tpr,
    falsePositiveRate = fpr,
    trueNegativeRate = tnr,
    falseNegativeRate = fnr
  )
  
  figs <- list(
    cm = draw_CM(cm),
    mets = draw_metTab(list(
      N = sum(cm),
      accuracy = roundPercent(acc),
      balancedAccuracy = roundPercent(balAcc),
      f1Score = roundPercent(f1),
      truePositiveRate = roundPercent(tpr),
      falsePositiveRate = roundPercent(fpr),
      trueNegativeRate = roundPercent(tnr),
      falseNegativeRate = roundPercent(fnr)))
  )

  return(list(vals = vals, figs = figs))
}
# `pred` - factor vector with 1A/1B/NC
# `ref` - factor vector with 1A/1B/NC. reference for comparing against pred
# Calculates categorical performance metrics
compareCat <- function(pred, ref) {
  if (!all(na.omit(pred) %in% c("1A", "1B", "NC", "Inconclusive"))) {
    stop("`pred` should be a factor with levels = c('1A', '1B', 'NC', 'Inconclusive')")
  }
  if (!all(na.omit(ref) %in% c("1A", "1B", "NC"))) {
    stop("`ref` should be a factor with levels = c('1A', '1B', 'NC')")
  }
  if (length(pred) != length(ref)) {
    stop("`pred` and `ref` should be the same length")
  }
  
  # Keep only cases with reference data
  refNA <- is.na(ref)
  ref <- ref[!refNA]
  pred <- pred[!refNA]
  
  # Check number of pairs
  n <- length(ref)
  if (n <= 5) {
    stop("Fewer than 5 non-missing pairs")
  }
  
  if (!is.ordered(pred)) pred <- factor(pred, levels = c("1A", "1B", "NC", "Inconclusive"), ordered = T)
  if (!is.ordered(ref)) ref <- factor(ref, levels = c("1A", "1B", "NC"), ordered = T)

  predLev <- factor(pred, levels = c("1A", "1B", "NC"), ordered = T)
  
  acc <- mean(predLev == ref, na.rm = T)
  under <- mean(predLev > ref, na.rm = T)
  over <- mean(predLev < ref, na.rm = T)

  pred <- droplevels(pred)
  ref <- droplevels(ref)
  
  cm <- table(pred, ref)
  
  vals <- list(
    confusionMatrix = cm,
    N = n,
    accuracy = acc,
    overpredicted = over,
    underpredicted = under
  )
  
  figs <- list(
    cm = draw_CM(cm),
    mets = draw_metTab(list(
      N = n,
      accuracy = roundPercent(acc),
      overpredicted = roundPercent(over),
      underpredicted = roundPercent(under)
    ))
  )

  return(list(vals = vals, figs = figs))
}

draw_CM <- function(cm) {
  colnames(cm)[is.na(colnames(cm))] <- "NA"
  rownames(cm)[is.na(rownames(cm))] <- "NA"
  
  ref_labs <- colnames(cm)
  pred_labs <- rownames(cm)
  
  cm <- as.data.table(cm)
  cmNames <- names(cm)
  cm <- data.frame(dcast(cm, as.formula(paste(cmNames[1], "~", cmNames[2])), value.var = cmNames[3]), check.names = F)
  
  cm <- cm[match(pred_labs, cm[[cmNames[1]]]),]
  cm <- cm[c(cmNames[1], ref_labs)]
  
  nCol <- ncol(cm) + 1
  nRow <- nrow(cm) + 2
  
  df <- rbind(
    c(rep("", nCol - 2), "Reference"),
    names(cm),
    cm
  )

  df <- cbind(c(rep("", nRow - 1), "Predicted"), df)
  
  df[1:2,1:2] <- ""
  tab <- tableGrob(df, cols = NULL, rows = NULL)
  
  blankCells <- tab$layout$t %in% 1:2 & tab$layout$r %in% 1:2
  refCell <- tab$layout$t == 1 & tab$layout$r == nCol
  predCell <- tab$layout$l == 1 & tab$layout$b == nRow
  pnCell <- (tab$layout$t == 2 & tab$layout$r %in% 3:nCol) | (tab$layout$t %in% c(3:nRow) & tab$layout$r == 2)
  valCells <- tab$layout$t %in% 3:nRow & tab$layout$r %in% 3:nCol
  
  rectCells <- sapply(tab$grobs, function(x) grepl("rect", x))
  textCells <- sapply(tab$grobs, function(x) grepl("text", x))
  
  tab$grobs[(refCell | predCell | pnCell) & textCells] <- lapply(tab$grobs[(refCell | predCell | pnCell) & textCells], function(x) {
    x$gp$font <- as.integer(2)
    return(x)
  })
  
  tab$grobs[(refCell | predCell | pnCell) & rectCells] <- lapply(tab$grobs[(refCell | predCell | pnCell) & rectCells], function(x) {
    x$gp$fill <- "#dae6ee"
    x$gp$col <- "black"
    return(x)
  })
  
  tab$layout[refCell, "l"] <- 3
  tab$layout[predCell, "t"] <- 3
  
  tab$grobs[blankCells & rectCells] <- lapply(tab$grobs[blankCells & rectCells], function (x) {
    x$gp$fill <- NULL
    return(x)
  })
  
  tab$grobs[valCells & rectCells] <- lapply(tab$grobs[valCells & rectCells], function(x) {
    x$gp$fill <- "white"
    x$gp$col <- "black"
    return(x)
  })
  
  tab$widths <- rep(max(tab$widths), nCol)
  
  return(tab)
}

draw_metTab <- function(named_list, fixNames = T) {
  metricLabels <- gsub("([[:upper:]])", "_\\1", names(named_list)) |>
    strsplit(split = "_")
  metricLabels <- sapply(X = metricLabels, paste, collapse = " ")
  metricLabels <- gsub("^(.){1}", "\\U\\1", x = metricLabels, perl = T) |>
    trimws()
  
  df <- data.frame(Metric = metricLabels, Value = unlist(named_list))
  tab <- tableGrob(df, rows = NULL, cols = c("Metric", "Value"))
  metCol <- tab$layout$l == 1 & tab$layout$t != 1
  valCol <- tab$layout$r == 2 & tab$layout$t != 1
  header <- tab$layout$t == 1
  
  rectCells <- sapply(tab$grobs, function(x) grepl("rect", x))
  
  tab$grobs[(metCol | header) & rectCells] <- lapply(tab$grobs[(metCol | header) & rectCells], function(x) {
    x$gp$fill <- x$gp$fill <- "#dae6ee"
    return(x)
  })
  
  tab$grobs[rectCells] <- lapply(tab$grobs[rectCells], function(x) {
    x$gp$col <- "black"
    return(x)
  })
  
  tab$grobs[valCol & rectCells] <- lapply(tab$grobs[valCol & rectCells], function(x) {
    x$gp$fill <- "white"
    return(x)
  })
  
  return(tab)
}










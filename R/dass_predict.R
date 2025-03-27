# Description: Functions used within the DASS app

#' Apply the 2 out of 3 defined approach
#'
#' Predict skin sensitization hazard using the 2o3 method.
#' @param assayA_call A numeric vector containing the assay call from the first
#' assay; 0 indicates a negative call and 1 indicates a positive call.
#' @param assayB_call A numeric vector containing the assay call from the second
#' assay; 0 indicates a negative call and 1 indicates a positive call.
#' @param assayC_call A numeric vector containing the assay call from the third
#' assay; 0 indicates a negative call and 1 indicates a positive call.
#'
#' @return Skin sensitization hazard prediction.
da2o3 <- function(assayA_call, assayB_call, assayC_call = NULL) {
  if (is.null(assayC_call)) {
    assayC_call <- rep(0, length(assayB_call))
  }
  
  # Are inputs the same length?
  if ((length(assayA_call) != length(assayB_call)) |
      (length(assayB_call) != length(assayC_call))) {
    stop("Input vectors must be the same length.")
  }
  
  # Is there anything that is not a 0 or 1?
  if (
    !all(na.omit(assayA_call) %in% c(0,1)) |
    !all(na.omit(assayB_call) %in% c(0,1)) |
    !all(na.omit(assayC_call) %in% c(0,1))
  ) {
    warning("Values not equal to 0 or 1 will be treated as missing data.")
  }
  
  assayA_call[!assayA_call %in% c(0,1)] <- NA
  assayB_call[!assayB_call %in% c(0,1)] <- NA
  assayC_call[!assayC_call %in% c(0,1)] <- NA
  
  tmp <- data.frame(
    assayA_call, assayB_call, assayC_call
  )
  
  tmp$hazard <- apply(tmp, 1, function(x) {
    x <- na.omit(x)
    out <- NA
    if (length(x) >= 2) {
      if (sum(x == 1) >= 2) {
        out <- "Positive"
      } else if (sum(x == 0) >= 2) {
        out <- "Negative"
      } else {
        out <- "Inconclusive"
      }
    }
    return(out)
  })
  
  return(tmp)
}

# ITS Scores
da_its_ke_thresholds <- list(
  ke1 = list(
    adra = list(
      mean_c_k_dep = list(
        score_breaks = c(-Inf, 4.9, 15.5, 46.4, Inf),
        score = 0:3
      ),
      c_dep = list(
        score_breaks = c(-Inf, 5.6, 17.5, 67.4, Inf),
        score = 0:3
      )
    ),
    dpra = list(
      mean_c_k_dep = list(
        score_breaks = c(-Inf, 6.38, 22.62, 42.47, Inf),
        score = 0:3
      ),
      c_dep = list(
        score_breaks = c(-Inf, 13.89, 23.09, 98.24, Inf),
        score = 0:3
      )
    )
  ),
  ke3 = list(
    gardskin = list(
      score_breaks = c(-Inf, 13.03, 56.44, .Machine$integer.max, Inf),
      score = 3:0
    ),
    hclat = list(
      score_breaks = c(-Inf, 10, 150, 5000, Inf),
      score = 3:0
    ),
    usens = list(
      score_breaks = c(-Inf, 3, 35, 200, Inf),
      score = 3:0
    )
  )
)

#' Apply the Integrated Testing Strategy defined approach
#' Assign call and potency using ITS scores
#' @param ke1_assay A character string 'dpra' or 'adra'
#' @param ke1_mean_c_k_dep A numeric vector of mean percent depletion values from the ke1 assay
#' @param ke1_c_dep A numeric vector of cys/nac percent depletion values from the ke1 assay
#' @param ke3_assay A character string 'gardskin', 'hclat', or 'usens'
#' @param ke3_value A numeric vector with the quantiative endpoint values from the ke3 assay
#' @param insil_prediction A vector of 0s and 1s indicating a negative or positive skin sensitization prediction from an in silico model
#' @param insil_ad A vector of 0s and 1s indicating if the corresponding chemical is within the in silico model's applicability domain
#'
#' @return Data frame with ITS scores and hazard and potency predictions
daITS <- function(
    ke1_assay, ke1_mean_c_k_dep = NULL, ke1_c_dep = NULL,
    ke3_assay, ke3_value = NULL,
    insil_prediction = NULL, insil_ad = NULL) {
  
  values <- list(ke1_mean_c_k_dep = ke1_mean_c_k_dep, 
                 ke1_c_dep = ke1_c_dep,
                 ke3_value = ke3_value, 
                 insil_prediction = insil_prediction, 
                 insil_ad = insil_ad)
  arg_len <- sapply(values, length)
  nonzero <- arg_len[arg_len != 0]
  
  if (length(nonzero) == 0) {
    stop ("Insufficient assay data.")
  }
  
  if (length(unique(nonzero)) > 1) {
    stop ("Inputs should be the same length.")
  }
  
  nz <- nonzero[1]
  
  values <- lapply(values, function(x) {
    if (is.null(x)) {
      rep(NA, nz)
    } else {
      x
    }
  })
  
  ke1_use_mean <- !is.na(values$ke1_mean_c_k_dep)
  ke1_use_c <- is.na(values$ke1_mean_c_k_dep) & !is.na(values$ke1_c_dep)
  ke1_score <- rep(NA, nz)
  
  if (any(ke1_use_mean)) {
    ke1_score[ke1_use_mean] <- as.numeric(as.character(cut(
      values$ke1_mean_c_k_dep[ke1_use_mean],
      breaks = da_its_ke_thresholds$ke1[[ke1_assay]]$mean_c_k_dep$score_breaks,
      labels = da_its_ke_thresholds$ke1[[ke1_assay]]$mean_c_k_dep$score)))
  }
  
  if (any(ke1_use_c)) {
    ke1_score[ke1_use_c] <- as.numeric(as.character(cut(
      values$ke1_c_dep[ke1_use_c],
      breaks = da_its_ke_thresholds$ke1[[ke1_assay]]$c_dep$score_breaks,
      labels = da_its_ke_thresholds$ke1[[ke1_assay]]$c_dep$score)))
  }

  if (!all(is.na(values$ke3_value))) {
    ke3_score <- as.numeric(as.character(cut(
      values$ke3_value,
      breaks = da_its_ke_thresholds$ke3[[ke3_assay]]$score_breaks,
      labels = da_its_ke_thresholds$ke3[[ke3_assay]]$score)))
  }

  insil_score <- values$insil_prediction
  insil_score[is.na(values$insil_ad) | values$insil_ad == 0] <- NA
  
  scores <- data.frame(ke1_score, ke3_score, insil_score)
  scores$total_score<- rowSums(scores, na.rm = T)
  
  scores$total_score[sum(!is.na(scores$total_score)) < 2] <- NA
  scores$hazard <- NA
  scores$potency <- NA
  
  score_1 <- !is.na(ke1_score) & !is.na(ke3_score) & !is.na(insil_score)  
  score_2 <- !is.na(ke1_score) & !is.na(ke3_score) & is.na(insil_score)  
  score_3 <- (is.na(ke1_score) & !is.na(ke3_score) & !is.na(insil_score)) | (!is.na(ke1_score) & is.na(ke3_score) & !is.na(insil_score))  
  
  scores$potency[score_1] <- as.character(cut(
    scores$total_score[score_1],
    breaks = c(0,2,6,7),
    labels = c("NC", "1B", "1A"),
    include.lowest = T,
    right = F
  ))
  
  scores$potency[score_2] <- as.character(cut(
    scores$total_score[score_2],
    breaks = c(0,1,2,5,6,6.01),
    labels = c("NC", "Inconclusive", "1B", "1*", "1A"),
    include.lowest = T,
    right = F
  ))
  
  scores$potency[score_3] <- as.character(cut(
    scores$total_score[score_3],
    breaks = c(0,2,3,5),
    labels = c("Inconclusive", "1B", "1*"),
    include.lowest = T,
    right = F
  ))

  scores$hazard[scores$potency == "NC"] <- "Negative"
  scores$hazard[scores$potency %in% c("1A", "1B", "1*")] <- "Positive"
  scores$hazard[scores$potency == "Inconclusive"] <- "Inconclusive"
  scores$potency[scores$potency == "1*"] <- "Inconclusive"

  out <- data.frame(
    as.data.frame(values),
    scores
  )
  
  return(out)
}

#' Apply the Key Event 3/1 Sequential Testing Strategy defined approach
#' @param ke1_call A vector of 0s and 1s indicating a negative or positive outcome from DPRA
#' @param ke3_value A numeric vector of MIT values from the h-CLAT
#' 
#' @return Data frame with skin sensitization hazard and potency prediction.
daKE31 <- function(ke1_call, ke3_value) {
  
  out <- data.frame(
    ke1_call,
    ke3_value,
    hazard = NA,
    potency = NA
  )
  
  out$potency[ke3_value == Inf & ke1_call == 0] <- "NC"
  out$potency[ke3_value == Inf & ke1_call == 1] <- "1B"
  out$potency[ke3_value <= 10] <- "1A"
  out$potency[ke3_value > 10 & ke3_value < Inf] <- "1B"
  
  out$hazard[out$potency == "NC"] <- "Negative"
  out$hazard[out$potency %in% c("1A", "1B")] <- "Positive"
  
  return(out)
}

# `pred` - factor vector with 0/1
# `ref` - factor vector with 0/1. reference for comparing against pred
# Calculates binary performance metrics
compareBinary <- function(pred, ref, predCol = NULL, refCol = NULL) {
  # if (!all(na.omit(pred) %in% c(0,1, "Inconclusive"))) {
  #   stop("`pred` should be a factor with levels = c(0,1, Inconclusive)")
  # }
  # if (!all(na.omit(ref) %in% c(0,1))) {
  #   stop("`ref` should be a factor with levels = c(0,1)")
  # }
  if (length(pred) != length(ref)) {
    stop("`pred` and `ref` should be the same length")
  }
  
  # Count non-missing
  anyMiss <- is.na(ref) | is.na(pred)
  n <- sum(!anyMiss)
  if (n < 5) {
    stop("Fewer than 5 non-missing pairs")
  }
  
  # pred <- factor(pred, levels = c("1", "0", "Inconclusive"), labels = c("Positive", "Negative", "Inconclusive"))
  pred <- droplevels(pred)
  # ref <- factor(ref, levels = c("1", "0"), labels = c("Positive", "Negative"))
  ref <- droplevels(ref)
  
  ref_pred_comp <- rep(NA, length = length(ref))
  ref_pred_comp[ref == "Positive" & pred == "Positive"] <- "TP"
  ref_pred_comp[ref == "Negative" & pred == "Positive"] <- "FP"
  ref_pred_comp[ref == "Positive" & pred == "Negative"] <- "FN"
  ref_pred_comp[ref == "Negative" & pred == "Negative"] <- "TN"
  

  N <- sum(!is.na(ref_pred_comp))
  tp <- sum(na.omit(ref_pred_comp == "TP"))
  fp <- sum(na.omit(ref_pred_comp == "FP"))
  fn <- sum(na.omit(ref_pred_comp == "FN"))
  tn <- sum(na.omit(ref_pred_comp == "TN"))
  
  acc <- (tp + tn)/N
  sens <- tp/(tp + fn)
  spec <- tn/(tn + fp)
  balAcc <- (sens + spec)/2
  f1 <- (2*tp)/((2*tp) + fp + fn)
  
  perf_tab <- list(
    N = N, 
    `True Positive` = tp,
    `False Positive` = fp,
    `False Negative` = fn,
    `True Negative` = tn,
    Sensitivity = sens,
    Specificity = spec,
    `Balanced Accuracy` = balAcc,
    Accuracy = acc,
    `F1 Score` = f1
  )
  perf_tab <- lapply(perf_tab, function(x) {names(x) <- "Value"; return(x)})

  cm <- draw_CM(table(pred, ref, dnn = NULL))
  perf_fig_tmp <- draw_metTab(perf_tab)
  perf_fig <- perf_fig_tmp$tab
  ref_pred_comp <- factor(ref_pred_comp, levels = c("NA", "TP", "TN", "FP", "FN"), labels = c("NA", "True Positive", "True Negative", "False Positive", "False Negative"))
  ref_pred_comp[is.na(ref_pred_comp)] <- "NA"
  ref_pred_comp <- droplevels(ref_pred_comp)
  
   return(list(
    indiv = ref_pred_comp,
    perf_list = perf_tab,
    fig = tableArrange(tab_list = list(cm, perf_fig),
                       refCol = refCol,
                       predCol = predCol),
    fig_alt = paste(cm$alttext, perf_fig_tmp$alttext, collapse = ". ")
  ))
}

# `pred` - factor vector with 1A/1B/NC
# `ref` - factor vector with 1A/1B/NC. reference for comparing against pred
# Calculates categorical performance metrics
compareCat <- function(pred, ref, predCol = NULL, refCol = NULL) {
  if (!all(na.omit(pred) %in% c("1A", "1B", "NC", "Inconclusive"))) {
    stop("`pred` should be a factor with levels = c('1A', '1B', 'NC', 'Inconclusive')")
  }
  if (!all(na.omit(ref) %in% c("1A", "1B", "NC"))) {
    stop("`ref` should be a factor with levels = c('1A', '1B', 'NC')")
  }
  if (length(pred) != length(ref)) {
    stop("`pred` and `ref` should be the same length")
  }
  
  # Count non-missing
  anyMiss <- is.na(ref) | is.na(pred)
  n <- sum(!anyMiss)
  if (n < 5) {
    stop("Fewer than 5 non-missing pairs")
  }
  
  # pred <- factor(pred, levels = c("1A", "1B", "NC", "Inconclusive"))
  pred <- droplevels(pred)
  # ref  <- factor(ref, levels = c("1A", "1B", "NC"))
  ref <- droplevels(ref)

  ref_pred_comp <- rep(NA, length = length(ref))
  ref_pred_comp[ref == "1A" & pred == "1A"] <- "1A"
  ref_pred_comp[ref == "1A" & (pred == "1B" | pred == "NC")] <- "UP"
  ref_pred_comp[ref == "1B" & pred == "1B"] <- "1B"
  ref_pred_comp[ref == "1B" & pred == "NC"] <- "UP"
  ref_pred_comp[ref == "1B" & pred == "1A"] <- "OP"
  ref_pred_comp[ref == "NC" & pred == "NC"] <- "NC"
  ref_pred_comp[ref == "NC" & (pred == "1A" | pred == "1B")] <- "OP"

  N <- length(na.omit(ref_pred_comp))
  acc <- mean(na.omit(ref_pred_comp) %in% c("1A", "1B", "NC"))
  under <- mean(na.omit(ref_pred_comp) == "UP")
  over <- mean(na.omit(ref_pred_comp) == "OP")
  
  perf_tab <- list(
    N = N,
    Accuracy = acc,
    Overpredicted = over,
    Underpredicted = under
  )
  perf_tab <- lapply(perf_tab, function(x) {names(x) <- "Value"; return(x)})
  
  class_perf <- lapply(c("1A", "1B", "NC"), function(i) {
    sensitivity <- mean(ref_pred_comp[!is.na(ref_pred_comp) & ref == i] %in% c("1A", "1B", "NC"))
    specificity <- mean(pred[!is.na(ref_pred_comp) & ref != "1A"] != "1A")
    list(
      Sensitivity = sensitivity,
      Specificity = specificity,
      `Balanced Accuracy` = (sensitivity + specificity)/2
    )
  })
  names(class_perf) <- c("1A", "1B", "NC")
  class_perf <- list(Sensitvity = sapply(class_perf, function(x) x[["Sensitivity"]]),
                     Specificity = sapply(class_perf, function(x) x[["Specificity"]]),
                     `Balanced Accuracy` = sapply(class_perf, function(x) x[["Balanced Accuracy"]]))

  cm <- draw_CM(table(pred, ref, dnn = NULL))
  perf_fig <- draw_metTab(perf_tab)
  perf_class_fig <- draw_metTab(class_perf, class_perf = T)
  
  ref_pred_comp <- factor(ref_pred_comp, levels = c("NA", "NC", "1B", "1A", "OP", "UP"), labels = c("NA", "True NC", "True 1B", "True 1A", "Overpredicted", "Underpredicted"))
  ref_pred_comp[is.na(ref_pred_comp)] <- "NA"
  ref_pred_comp <- droplevels(ref_pred_comp)

  return(
    list(
      indiv = ref_pred_comp,
      perf_list = perf_tab,
      fig = tableArrange(
        tab_list = list(cm, perf_fig$tab, perf_class_fig$tab),
        refCol = refCol,
        predCol = predCol
      ),
      fig_alt = paste(cm$alttext, perf_fig$alttext, perf_class_fig$alttext, sep = " ")
    )
  )
}

draw_CM <- function(cm) {

  colnames(cm)[is.na(colnames(cm))] <- "NA"
  rownames(cm)[is.na(rownames(cm))] <- "NA"
  
  df <- data.frame(matrix(cm, nrow = nrow(cm), dimnames = list(rownames(cm), colnames(cm))), check.names = F)
  df <- cbind(
    a = c(rep("", nrow(df) -1), "Predicted"),
    b = rownames(df), 
    df
  ) 
  df <- rbind(
    c(rep("", ncol(df) - 1), "Reference"),
    names(df),
    df
  )
  df[1:2,1:2] <- ""

  nCol <- ncol(df)
  nRow <- nrow(df)
  
  tab <- tableGrob(df, cols = NULL, rows = NULL, theme = ttheme_default(base_size = 14))
  
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
  
  tab$alttext <- paste(sprintf("Table 1. Confusion Matrix. %i rows, %i columns.", nrow(cm), ncol(cm)),
                                       paste(sapply(1:nrow(cm), function(x) {
                                         paste(sapply(1:ncol(cm), function(y) {
                                           sprintf("Row %i, Column %i. Predicted = %s, Reference = %s, Count = %i", x, y, rownames(cm)[x], colnames(cm)[y], cm[x,y])
                                         }), collapse = ". ")
                                       }), collapse = ". "))
  
  return(tab)
}

draw_metTab <- function(named_list, fixNames = F, class_perf = F) {
  metricLabels <- names(named_list)
  if (fixNames) {
    metricLabels <- gsub("([[:upper:]])", "_\\1", metricLabels) |>
      strsplit(split = "_")
    metricLabels <- sapply(X = metricLabels, paste, collapse = " ")
    metricLabels <- gsub("^(.){1}", "\\U\\1", x = metricLabels, perl = T) |>
      trimws()
  }
  
  named_list <- lapply(names(named_list), function(x) {
    tmp <- named_list[[x]]
    x_names <- names(tmp)
    if (typeof(tmp) == "double") tmp <- roundPercent(tmp)
    tmp <- structure(as.character(tmp), names = x_names)
    return(c(Metric = x, tmp))
  })
  
  df <- data.frame(do.call("rbind", named_list), check.names = F)
  
  if (class_perf) {
    celltext <- NULL
    for (i in 1:nrow(df)) {
      for (j in 2:ncol(df)) {
        celltext <- c(celltext, paste(sprintf("Row %i, Column %i:  Metric GHS %s %s = %s.", i, j-1, names(df)[j], df$Metric[i], df[i,j]), collapse = " "))
      }
    }
    alttext <- paste(
      sprintf("Table 3. Performance by Class Table with %i rows and %i columns.", nrow(df), ncol(df)-1),
      paste(celltext, collapse = " ")
    )
  } else {
    alttext <- paste(
      sprintf("Table 2. Performance Table with %i rows and %i columns.", nrow(df), ncol(df)-1),
      paste(sprintf("Row %i: Metric %s Value %s.", 1:nrow(df), df$Metric, df$Value), collapse = " ")
    )
  }

  tab <- tableGrob(df, rows = NULL, cols = names(df), theme = ttheme_default(base_size = 14))
  
  metCol <- tab$layout$l == 1 & tab$layout$t != 1
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
  
  tab$grobs[!(header | metCol) & rectCells] <- lapply(tab$grobs[!(header | metCol) & rectCells], function(x) {
    x$gp$fill <- "white"
    return(x)
  })
  return(list(tab=tab, alttext = alttext))
}

tableArrange <- function(tab_list, refCol = NULL, predCol = NULL) {
  if (is.null(refCol)) refCol <- "Reference Column"
  if (is.null(predCol)) predCol <- "Prediction Column"
  
  lay_mat <- unlist(lapply(1:length(tab_list), function(x) {
    rep(x, ceiling(length(tab_list[[x]]$heights) * 1.5))
  }))
  lay_mat <- matrix(c(1:3, lay_mat + 3))

  tabFig <- arrangeGrob(
    grobs =   c(
      list(
        textGrob(label = "Confusion Matrix and Performance Metrics", gp = gpar(font = 2, fontsize = 16)),
        textGrob(label = paste("Reference Column: ", refCol), gp = gpar(fontsize = 14)),
        textGrob(label = paste("Prediction Column:", predCol), gp = gpar(fontsize = 14))
      ), tab_list
    ),
      layout_matrix = lay_mat
  )
  
  return(tabFig)
}
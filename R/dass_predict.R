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
  
  # Is there anything that is not a -1, 0 or 1?
  if (
    !all(na.omit(assayA_call) %in% c(-1,0,1)) |
    !all(na.omit(assayB_call) %in% c(-1,0,1)) |
    !all(na.omit(assayC_call) %in% c(-1,0,1))
  ) {
    warning("Values not equal to -1, 0 or 1 will be treated as missing data.")
  }
  
  assayA_call[!assayA_call %in% c(-1,0,1)] <- NA
  assayB_call[!assayB_call %in% c(-1,0,1)] <- NA
  assayC_call[!assayC_call %in% c(-1,0,1)] <- NA
  
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
      } else if (sum(x == -1) >= 2) {
        out <- "Borderline"
      }else {
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

  if (is.null(predCol)) predCol <- "Prediction Column"
  if (is.null(refCol)) refCol <- "Reference Column"
  
  if (length(pred) != length(ref)) {
    stop("`pred` and `ref` should be the same length")
  }
  
  # Count non-missing
  anyMiss <- is.na(ref) | is.na(pred)
  n <- sum(!anyMiss)
  if (n < 5) {
    stop("Fewer than 5 non-missing pairs")
  }
  
  pred <- droplevels(pred)
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
  
  bl <- sum(pred == "Borderline", na.rm = T)
  perf_tab <- list(
    N = N, 
    `True Positive` = tp,
    `False Positive` = fp,
    `False Negative` = fn,
    `True Negative` = tn,
    Borderline = ifelse(bl > 0, bl, NA),
    Inconclusive = sum(pred == "Inconclusive", na.rm = T),
    Sensitivity = sens,
    Specificity = spec,
    `Balanced Accuracy` = balAcc,
    Accuracy = acc,
    `F1 Score` = f1
  )
  if (bl == 0) perf_tab["Borderline"] <- NULL
  perf_tab <- lapply(perf_tab, function(x) {names(x) <- "Value"; return(x)})
  
  cm <- draw_CM(table(pred, ref))
  perf_fig <- draw_metTab(perf_tab)
  
  ref_pred_comp[pred=="Inconclusive"] <- "Inc."
  ref_pred_comp[pred=="Borderline"] <- "BL"
  ref_pred_comp <- factor(ref_pred_comp, levels = c("NA", "Inc.", "BL", "TP", "TN", "FP", "FN"), labels = c("NA", "Inconclusive", "Borderline", "True Positive", "True Negative", "False Positive", "False Negative"))
  ref_pred_comp[is.na(ref_pred_comp)] <- "NA"
  ref_pred_comp <- droplevels(ref_pred_comp)
  
   return(list(
    indiv = ref_pred_comp,
    perf_list = perf_tab,
    fig = div(
      class = "met-tables", 
      tags$h4("Comparison Tables", tags$br(), "Reference Column: ", refCol, tags$br(), "Prediction Column: ", predCol),
      cm$html, perf_fig$html
      ),
    fig_gg = tableArrange(list(cm$gg, perf_fig$gg), refCol, predCol)
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
  
  class_perf <- lapply(c("1A", "1B", "NC"), function(i) {
    sensitivity <- mean(ref_pred_comp[!is.na(ref_pred_comp) & ref == i] %in% c("1A", "1B", "NC"))
    specificity <- mean(pred[!is.na(ref_pred_comp) & ref != i] != i)
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

  ref_pred_comp[pred == "Inconclusive"] <- "Inc."
  perf_tab <- list(
    N = N,
    Accuracy = acc,
    Overpredicted = over,
    Underpredicted = under,
    Inconclusive = sum(pred == "Inconclusive", na.rm = T),
    `NA` = sum(is.na(ref_pred_comp))
  )
  perf_tab <- lapply(perf_tab, function(x) {names(x) <- "Value"; return(x)})
  
  cm <- draw_CM(table(pred, ref, useNA = "ifany"))
  perf_fig <- draw_metTab(perf_tab)
  perf_class_fig <- draw_metTab_byClass(class_perf)
  
  ref_pred_comp <- factor(ref_pred_comp, levels = c("NA", "Inc.", "NC", "1B", "1A", "OP", "UP"), labels = c("NA", "Inconclusive", "True NC", "True 1B", "True 1A", "Overpredicted", "Underpredicted"))
  ref_pred_comp[is.na(ref_pred_comp)] <- "NA"
  ref_pred_comp <- droplevels(ref_pred_comp)

  return(
    list(
      indiv = ref_pred_comp,
      perf_list = perf_tab,
      fig = div(
        class = "met-tables", 
        tags$h4("Comparison Tables", tags$br(), "Reference Column: ", refCol, tags$br(), "Prediction Column: ", predCol),
        cm$html, perf_fig$html, perf_class_fig$html
      ),
      fig_gg = tableArrange(list(cm$gg, perf_fig$gg, perf_class_fig$gg), refCol, predCol)
    )
  )
}

draw_CM <- function(cm) {
  df <- data.frame(cm)

  gg_tab <- ggplot(df, aes(x = 1, y = 1)) + 
    geom_text(aes(label = Freq), size = 10.5/.pt) + 
    facet_grid(pred ~ ref,  switch = "y") +
    scale_x_discrete(name = "Reference", position = "top", expand = c(0,0)) +
    scale_y_discrete(name = "Predicted", position = "left", expand = c(0,0)) +
    ggtitle("Confusion Matrix") + 
    theme(
      axis.title.x.top = element_text(face = "bold", size = 11, vjust = 0.5, margin = margin(5.5,5.5,5.5,5.5)),
      axis.title.y = element_text(face = "bold", size = 11, vjust = 0.5, angle = 0, margin = margin(5.5,5.5,5.5,5.5)),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      
      strip.text = element_text(face = "bold", size = 11),
      strip.text.y.left = element_text(angle = 0),
      strip.background = element_rect(fill = "#dae6ee", color = "black"),
      panel.spacing = unit(0, "cm"),
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(color = "black", fill = NA),

      plot.title = element_text(hjust = 0.5, size = 12),
      plot.title.position = "plot"
  )
  
  gg_tab <- ggplot_gtable(ggplot_build(gg_tab))
  yloc <- grep("ylab-l", gg_tab$layout$name)
  xloc <- grep("xlab-t", gg_tab$layout$name)
  
  gg_tab <- gtable::gtable_add_grob(gg_tab, rectGrob(gp = gpar(fill = "#dae6ee")), gg_tab$layout$t[yloc], gg_tab$layout$r[yloc], gg_tab$layout$b[yloc], gg_tab$layout$l[yloc], z = 0)
  gg_tab <- gtable::gtable_add_grob(gg_tab, rectGrob(gp = gpar(fill = "#dae6ee")), gg_tab$layout$t[xloc], gg_tab$layout$r[xloc], gg_tab$layout$b[xloc], gg_tab$layout$l[xloc], z = 0)
  
  lay_t <- unique(gg_tab$layout$t[grep("panel", gg_tab$layout$name)])
  gg_tab$heights[lay_t] <- unit(1.25, "lines")
  
  lay_r <- unique(gg_tab$layout$r[grep("panel", gg_tab$layout$name)])
  gg_tab$widths[lay_r] <- unit(1.25, "strwidth", "Inconclusive")

  head_cells <- list(
    tags$tr(
      tags$td(class = "blank-cell", rowspan = 2, colspan = 2),
      tags$th(colspan = ncol(cm), scope = "colgroup", "Reference")
    ),
    tags$tr(
      lapply(colnames(cm), function(x) tags$th(scope = "col", x))
  ))
  
  cm_cells <- lapply(1:nrow(cm), function(i) {
    tmp_list <- list(
      tags$th(scope = "row", rownames(cm)[i]),
      lapply(1:ncol(cm), function(j) {
        tags$td(cm[i,j])
      })
    )
    
    if (i == 1) {
      tmp_list <- c(list(tags$th(rowspan = nrow(cm), scope = "rowgroup", "Predicted")), tmp_list)
    }

    return(tags$tr(tmp_list))
  })
  
  html_tab <- tags$table(
    class = "cm-table",
    tags$caption("Confusion Matrix"),
    tags$col(),
    tags$col(),
    tags$colgroup(span = ncol(cm)),
    head_cells, 
    cm_cells
  )
  return(list(html = html_tab, gg = gg_tab))
}

draw_metTab <- function(named_list) {
  
  df <- Map(function(met, val) {
    if (typeof(val) == "double") val <- roundPercent(val)
    data.frame(
      grp = met,
      ctype = c("Metric", "Value"),
      val = c(met, unname(val))
    )
  }, names(named_list), named_list)
 
  df <- do.call("rbind", df)
  df$grp <- factor(df$grp, levels = names(named_list))
  df$ctype <- factor(df$ctype, levels = c("Metric", "Value"))

  gg_tab <- ggplot(df, aes(x = 1, y = 1)) + 
    geom_rect(aes(fill = ctype), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, show.legend = F) +
    geom_text(aes(label = val), size = 10.5/.pt, fontface = ifelse(df$ctype == "Metric", 2, 1)) +
    scale_fill_manual(breaks = c("Metric", "Value"), values = c("#dae6ee", "white")) +
    ggtitle("Performance Metrics") +
    facet_grid(grp ~ ctype, switch = "y") + 
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      strip.text.x = element_text(face = "bold", size = 11),
      strip.background.x = element_rect(fill = "#dae6ee", color = "black"),
      strip.text.y = element_blank(),
      strip.background.y = element_blank(),
      panel.spacing = unit(0, "cm"),
      panel.border = element_rect(color = "black", fill = NA),
      plot.title = element_text(hjust = 0.5, size = 12)
    )

  gg_tab <- ggplot_gtable(ggplot_build(gg_tab))
  lay_t <- unique(gg_tab$layout$t[grep("panel", gg_tab$layout$name)])
  gg_tab$heights[lay_t] <- unit(1.25, "lines")
  
  lay_l <- unique(gg_tab$layout$l[grep("panel", gg_tab$layout$name)])
  gg_tab$widths[lay_l] <- unit(1.25, "strwidth", "Balanced Accuracy")

  html_tab <- tags$table(
    class = "cm-table",
    tags$caption("Performance Metrics"),
    tags$thead(
      tags$tr(
        tags$th(scope = "col", "Metric"),
        tags$th(scope = "col", "Value")
      )
    ),
    tags$tbody(
      Map(function(met,val) {
        if (typeof(val) == "double") val <- roundPercent(val)
        tags$tr(tags$th(scope = "row", met), tags$td(val))
      }, names(named_list), named_list)
    )
  )
  
  return(list(html = html_tab, gg = gg_tab))
}

draw_metTab_byClass <- function(named_list) {

  df <- Map(function(met, val) {
    data.frame(Metric = met, GHS = names(val), Value = val)
  }, names(named_list), named_list)
  df <- do.call("rbind", df)
  df$Value <- roundPercent(df$Value)
  df$Metric <- factor(df$Metric, levels = names(named_list))
  
  gg_tab <- ggplot(df, aes(x = 1, y = 1)) + 
    geom_text(aes(label = Value), size = 10.5/.pt) +
    facet_grid(Metric ~ GHS, switch = "y") + 
    ggtitle("Performance Metrics by Class") +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      
      strip.text = element_text(face = "bold", size = 11),
      strip.text.y.left = element_text(angle = 0),
      strip.background = element_rect(fill = "#dae6ee", color = "black"),

      panel.spacing = unit(0, "cm"),
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(color = "black", fill = NA),
      
      plot.title = element_text(hjust = 0.5, size = 12), 
      plot.title.position = "plot"
    )
  
  gg_tab <- ggplot_gtable(ggplot_build(gg_tab))
  
  lay_t <- unique(gg_tab$layout$t[grep("panel", gg_tab$layout$name)])
  gg_tab$heights[lay_t] <- unit(1.25, "lines")
  
  lay_r <- unique(gg_tab$layout$l[grep("panel", gg_tab$layout$name)])
  gg_tab$widths[lay_r] <- unit(1.25, "strwidth", "1A1BNC")
  
  df <- do.call("rbind", named_list)
  html_tab <- tags$table(
    class = "cm-table",
    tags$caption("Performance Metrics by Class"),
    tags$thead(
      tags$tr(
        tags$th(scope = "col", "Metric"),
        lapply(1:ncol(df), function(j) tags$th(scope = "col", colnames(df)[j]))
      )
    ),
    tags$tbody(
      lapply(1:nrow(df), function(i) {
        tags$tr(
          tags$th(scope = "row", rownames(df)[i]),
          lapply(1:ncol(df), function(j) {
            tags$td(roundPercent(df[i,j]))
          })
        )
      })
    )
  )
  return(list(html = html_tab, gg = gg_tab))
}

tableArrange <- function(tab_list, refCol, predCol) {

  tab_title <- textGrob(label = sprintf("Comparison Tables\nReference Column: %s\nPrediction Column: %s", refCol, predCol), gp = gpar(font = 2, fontsize = 12))
  tabFig <- tab_list[[length(tab_list)]]
  
  for (i in rev(seq_along(tab_list))[-1]) {
    tabFig <- tabFig %>%
      gtable::gtable_add_rows(heights = grobHeight(tab_list[[i]]) + unit(3,"line"), 0) %>%
      gtable::gtable_add_grob(tab_list[[i]], t = 1, l = 1, r = ncol(tabFig), clip = "off")
  }
  tabFig <- tabFig %>% 
    gtable::gtable_add_rows(heights = grobHeight(tab_title) + unit(1,"line"), pos = 0) %>%
    gtable::gtable_add_grob(tab_title, t = 1, l = 1, r = ncol(tabFig), clip = "off")

  return(tabFig)
}

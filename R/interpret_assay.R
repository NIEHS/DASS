# KE1 -----
ke1_call_thresholds <- list(
  adra = list(
    mean_c_l_dep = c(
      call = 4.9,
      concl_neg_ul = 3,
      bl_neg_ul = 4.06,
      bl_ul = 5.94,
      bl_pos_ul = 10
    ),
    c_dep = c(
      call = 5.9,
      concl_neg_ul = 4,
      bl_neg_ul = 4.67,
      bl_ul = 6.83,
      bl_pos_ul = 11
    )
  ),
  dpra = list(
    mean_c_l_dep = c(
      call = 6.38,
      concl_neg_ul = 3,
      bl_neg_ul = 4.95,
      bl_ul = 8.32,
      bl_pos_ul = 10
    ),
    c_dep = c(
      call = 13.89,
      concl_neg_ul = 9,
      bl_neg_ul = 10.56,
      bl_ul = 17,
      bl_pos_ul = 18.47
    )
  )
)

ke1Call <- function(assay, mean_c_l_dep = NULL, c_dep = NULL, borderline = F) {

  if (is.null(mean_c_l_dep) & is.null(c_dep)) {
    stop("No data provided")
  }

  if (is.null(c_dep)) {
    c_dep <- rep(NA, length(mean_c_l_dep))
  }

  if (is.null(mean_c_l_dep)) {
    mean_c_l_dep <- rep(NA, length(c_dep))
  }

  if (length(c_dep) != length(mean_c_l_dep)) {
    stop("Vectors are not the same length.")
  }

  use_c <- is.na(mean_c_l_dep) & !is.na(c_dep)

  out <- data.frame(
    mean_c_l_dep,
    c_dep,
    outcome = NA
  )
  
  c_thresh <- ke1_call_thresholds[[assay]][["c_dep"]]
  mean_thresh <- ke1_call_thresholds[[assay]][["mean_c_l_dep"]]
  
  if (borderline) {
    
    out$outcome[use_c] <- ifelse(c_dep[use_c] <= c_thresh[["concl_neg_ul"]], "Negative",
                                 ifelse(c_dep[use_c] <= c_thresh[["bl_neg_ul"]], "BL Negative",
                                        ifelse(c_dep[use_c] <= c_thresh[["bl_ul"]], "Borderline",
                                               ifelse(c_dep[use_c] <= c_thresh[["bl_pos_ul"]], "BL Positive",
                                                      ifelse(c_dep[use_c] > c_thresh[["bl_pos_ul"]], "Positive", "Inconclusive")))))
    
    out$outcome[!use_c] <- ifelse(mean_c_l_dep[!use_c] <= mean_thresh[["concl_neg_ul"]], "Negative",
                                 ifelse(mean_c_l_dep[!use_c] <= mean_thresh[["bl_neg_ul"]], "BL Negative",
                                        ifelse(mean_c_l_dep[!use_c] <= mean_thresh[["bl_ul"]], "Borderline",
                                               ifelse(mean_c_l_dep[!use_c] <= mean_thresh[["bl_pos_ul"]], "BL Positive",
                                                      ifelse(mean_c_l_dep[!use_c] > mean_thresh[["bl_pos_ul"]], "Positive", "Inconclusive")))))
    
  } else if (!borderline) {
    out$outcome[use_c] <- ifelse(c_dep[use_c] >= c_thresh[["call"]], "Positive", "Negative")
    out$outcome[!use_c] <- ifelse(mean_c_l_dep[!use_c] >= mean_thresh[["call"]], "Positive", "Negative")
  }

  return(out)
}

# KE2 -----
## LuSens -----
lusensCall <- function(conc, fold_induction, relative_viability, ttest_p) {
  test <- unique(c(
    length(conc),
    length(fold_induction),
    length(relative_viability),
    length(ttest_p)
  ))
  
  if (length(test) > 1) {
    stop("Inputs are not the same length.")
  }
  
  new_order <- order(conc)
  conc <- conc[new_order]
  fold_induction <- fold_induction[new_order]
  relative_viability <- relative_viability[new_order]
  ttest_p <- ttest_p[new_order]
  
  # Positive
  rv_ge70 <- relative_viability >= 70
  
  pos_test1 <- fold_induction[rv_ge70] >= 1.76 & ttest_p[rv_ge70] <= 0.05
  pos_test1 <- pos_test1[1:(length(pos_test1) - 1)] + pos_test1[2:length(pos_test1)]
  pos_test1 <- any(pos_test1 == 2)
  pos_test2 <- sum(rv_ge70) >= 3

  if (pos_test1 & pos_test2) {
    out <- "Positive"
  } else if (pos_test1 & !pos_test2) {
    out <- "Invalid"
  } else {
    neg_test1 <- any(fold_induction[rv_ge70] < 1.28) & any(!rv_ge70)
    # neg_test2 <- all(conc <= 2000) & all(rv_ge70)
    neg_test2 <- any(conc >= 2000) & all(rv_ge70)
    
    if (neg_test1 | neg_test2) {
      out <- "Negative"
    } else {
      bl_test1 <- (fold_induction[rv_ge70] < 1.76 & fold_induction[rv_ge70] >= 1.28) & ttest_p[rv_ge70] <= 0.05
      bl_test1 <- bl_test1[1:(length(bl_test1) - 1)] + bl_test1[2:length(bl_test1)]
      bl_test1 <- any(bl_test1 == 2)
      if (bl_test1 & pos_test2) {
        out <- "Borderline"
      } else {
        out <- "Invalid"
      }
    }
  }
  return(out)
}

# KE3 -----
## GARD -----
gardskinCall <- function(mean_dv) {
  ifelse(
    mean_dv < -0.450, "Negative", ifelse(
      mean_dv > 0.450, "Positive", ifelse(
        mean_dv >= -0.450 & mean_dv <= 0.450, "Borderline", NA
      )
    )
  )
}

## hCLAT
hclatCall <- function(cd54, cd86, viability) {
  via50 <- viability >= 50
  via50[is.na(via50)] <- FALSE
  if (any(cd54[via50] > 255) | any(cd86[via50] > 184)) {
    out <- "Positive"
  } else if (all(cd54 <= 157 & cd86 <= 122)) {
    out <- "Negative"
  } else {
    bl_test1 <- any(cd54[via50] > 157 & cd54[via50] <= 255)
    bl_test2 <- any(cd86[via50] > 122 & cd86[via50] <= 284)
    if (bl_test1 | bl_test2) {
      out <- "Borderline"
    } else {
      out <- "Negative"
    }
  }
  return(out)
}

il8Call <- function(ind_il8la, ind_il8la_ll, inh_gapla, ws) {
  ws_test <- na.omit(ws)[1] == "y"
  pos_test <- any(ind_il8la >= 1.67 & ind_il8la_ll < 1)
  if (pos_test) {
    return("Positive")
  } else {
    if (any(ind_il8la < 1.25) | any(ind_il8la_ll < 1)) {
      if (ws_test) {
        return("Negative")
      } else if (!ws_test) {
        neg_test1 <- any(ind_il8la < 1.25 & inh_gapla < 0.8)
        neg_test2 <- any(ind_il8la_ll < 1 & inh_gapla < 0.8)
        if (neg_test1 | neg_test2) {
          return("Negative")
        } else {
          return("Inconclusive")
        }
      }
    } else {
      return("Borderline")
    }
  }
}

usensCall <- function(conc, viability, si_cd86) {
  pos_test1 <- any(viability[conc <= 10] < 70)
  pos_test2 <- any(si_cd86 > 169)

  if (pos_test1 | pos_test2) {
    return("Positive")
  } else {
    bl_test1 <- any(si_cd86 >= 150 & si_cd86 <= 169)
    if (bl_test1) {
      return("Borderline")
    } else {
      neg_test1 <- all(si_cd86[viability >= 70] <= 133)
      if (neg_test1) {
        return("Negative")
      } else {
        bl_test2 <- si_cd86 > 133 & si_cd86 < 150
        max_conc <- max(conc[viability >= 70])
        bl_test2 <- bl_test2[conc == max_conc]
        if (bl_test2) {
          return("Borderline")
        } else if (!bl_test2) {
          return("Negative")
        }
      }
    }
  }
}

# ============================================================================#
# File Name: interpret_assay.r
# Original Creator: ktto
# Contact Information: ICE-support@niehs.nih.gov
# Date Created: 2024-04-22
# License: MIT
# Description: Interpret quantitative results from SS assays
# Required Packages:

# ============================================================================#

# KE1 -----
ke1_call_thresholds <- list(
  adra = list(
    mean_c_l_dep = c(call = 4.9, bl_ll = 3, bl_ul = 10),
    c_dep = c(call = 5.6, bl_ll = 4, bl_ul = 11)
  ),
  dpra = list(
    mean_c_l_dep = c(call = 6.38, bl_ll = 4.95, bl_ul = 8.32),
    c_dep = c(call = 13.89, bl_ll = 0.56, bl_ul = 18.47)
  )
)

ke1Call <- function(assay, mean_c_l_dep = NULL, c_dep = NULL, l_dep = NULL) {
  if (is.null(mean_c_l_dep)) {
    if (is.null(c_dep)) {
      stop ("Insufficient arguments supplied.")
    } else {
      if (is.null(l_dep)) {
        mean_c_l_dep <- rep(NA, length(c_dep))
        warning("Calls will be made with only the C_dep parameter.")
      } else {
        mean_c_l_dep <- (c_dep + l_dep)/2
      }
    }
  } else {
    if (is.null(c_dep)) {
      c_dep <- rep(NA, length(mean_c_l_dep))
    }
  }
  
  if (is.null(l_dep)) l_dep <- rep(NA, length(mean_c_l_dep))
  
  use_mean <- !is.na(mean_c_l_dep)
  use_c <- is.na(mean_c_l_dep) & !is.na(c_dep)
  
  call <- rep(NA, length(mean_c_l_dep))
  call[use_mean] <- as.numeric(mean_c_l_dep[use_mean] < ke1_call_thresholds[[assay]][["mean_c_l_dep"]][["call"]])
  call[use_c] <- as.numeric(c_dep[use_c] < ke1_call_thresholds[[assay]][["c_dep"]][["call"]])
  
  return(
    data.frame(
      mean_c_l_dep = mean_c_l_dep,
      c_dep = c_dep,
      l_dep = l_dep,
      call = call)
  )
}

ke1BL <- function(assay, call, mean_c_l_dep = NULL, c_dep = NULL) {
  if (is.null(mean_c_l_dep) & is.null(c_dep)) {
    stop ("Insufficient arguments supplied.")
  } else if (is.null(mean_c_l_dep)) {
    mean_c_l_dep <- rep(NA, length(c_dep))
  } else if (is.null(c_dep)) {
    c_dep <- rep(NA, length(mean_c_l_dep))
  }
  
  thresh_ll <- thresh_ul <- rep(NA, length(mean_c_l_dep))
  thresh_ll[!is.na(mean_c_l_dep)] <- ke1_call_thresholds[[assay]][["mean_c_l_dep"]][["bl_ll"]]
  thresh_ul[!is.na(mean_c_l_dep)] <- ke1_call_thresholds[[assay]][["mean_c_l_dep"]][["bl_ul"]]
  thresh_ll[is.na(mean_c_l_dep) & !is.na(c_dep)] <- ke1_call_thresholds[[assay]][["c_dep"]][["bl_ll"]]
  thresh_ul[is.na(mean_c_l_dep) & !is.na(c_dep)] <- ke1_call_thresholds[[assay]][["c_dep"]][["bl_ul"]]

  call_bl <- as.character(call)
  call_bl[call > thresh_ll & call <= thresh_ul] <- "BL"
  
  return(call_bl)
}

# KE2 -----
ke2_call_thresholds <- list(
  keratinosens = c(call = 1.5, bl_ll = 1.35, bl_ul = 1.67),
  lusens = c(call = 1.5, bl_ll = 1.28, bl_ul = 1.76)
)

ke2BL <- function(assay, call, value) {
  thresh_ll <- ke2_call_thresholds[[assay]][["bl_ll"]]
  thresh_ul <- ke2_call_thresholds[[assay]][["bl_ul"]]
  
  call_bl <- as.character(call)
  call_bl[call > thresh_ll & call <= thresh_ul] <- "BL"
  
  return(call_bl)
}

# KE3 -----
ke3_call_thresholds <- list(
  gardskin = c(call = 0, bl_ll = -0.450, bl_ul = 0.450),
  hclat = list(
    cd54 = c(call = 200, bl_ll = 157, bl_ul = 255),
    cd86 = c(call = 150, bl_ll = 122, bl_ul = 184)
  ),
  il8luc = c(call = 1.4, bl_ll = 1.25, bl_ul = 1.57),
  usens = c(call = 150, bl_ll = 133, bl_ul = 169)
)

# ke3BL <- function(assay, call, value, value2 = NULL) {
#   if (assay == "hclat") {
#     call_bl <- as.character(call)
#     
#     thresh_ll_cd54 <- ke3_call_thresholds[[assay]][["cd54"]][["bl_ll"]]
#     thresh_ul_cd54 <- ke3_call_thresholds[[assay]][["cd54"]][["bl_ul"]]
#     thresh_ll_cd86 <- ke3_call_thresholds[[assay]][["cd86"]][["bl_ll"]]
#     thresh_ul_cd86 <- ke3_call_thresholds[[assay]][["cd86"]][["bl_ul"]]
#     
#     (value > thresh_ll & value <= value2) | ()
#     
#   } else {
#     
#   }
# }


# confirm about cell viability
# confirm how to handle one bl/one not
# hclat_BL <- function(cd54, cd86, hazard) {
#   cd54_ll <- 157
#   cd54_ul <- 255
#   
#   cd86_ll <- 122
#   cd86_ul <- 184
#   
#   out <- data.frame(
#     cd54,
#     cd86,
#     hazard,
#     hazard_BR = hazard
#   )
#   
#   out$cd54_tmp <- ifelse(cd54 > cd54_ll & cd54 <= cd54_ul, NA, cd54)
#   out$cd86_tmp <- ifelse(cd86 > cd86_ll & cd86 <= cd86_ul, NA, cd86)
#   
# }
# 
# 
# hclat_BL <- function(cd54, cd86, hazard) {
#   cd54_ll <- 157
#   cd54_ul <- 255
#   
#   cd86_ll <- 122
#   cd86_ul <- 184
#   
#   out <- data.frame(
#     cd54,
#     cd86,
#     hazard,
#     hazard_BR = hazard
#   )
#   
#   out$cd54_tmp <- ifelse(cd54 > cd54_ll & cd54 <= cd54_ul, NA, cd54)
#   out$cd86_tmp <- ifelse(cd86 > cd86_ll & cd86 <= cd86_ul, NA, cd86)
#   
#   return(out)
#   
# }
# 
# hclat_BL(c(200,3.8499241,300.3,NA, 74.2, NA), c(22.2,1.599999,259.9,0.55,NA,NA), c(1,1,1,1,1,0))

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
    c_dep = c(call = 13.89, bl_ll = 10.56, bl_ul = 18.47)
  )
)

ke1Call <- function(assay, cid, c_dep, l_dep = NULL, only_c = NULL) {
  
  if (length(cid) != length(c_dep)) {
    stop("cid and c_dep vectors must be the same length.")
  }
  
  if (is.null(l_dep)) {
    l_dep <- mean_c_l_dep <- rep(NA, length(c_dep))
    
    if (!is.null(only_c)) {
      warning("NULL l_dep. Setting only_c to TRUE for all values.")
    }
    only_c <- rep(T, length(c_dep))
    
  } else {
    if (length(c_dep) != length(l_dep)) {
      stop("c_dep and l_dep vectors must be the same length")
    } else {
      
      if (is.null(only_c)) {
        message("NULL only_c. Setting only_c to TRUE for all values.")
        only_c <- rep(T, length(c_dep))
      }
      
      mean_c_l_dep <- (c_dep + l_dep)/2
    }
  }
  
  run_call <- bl_call <- rep(NA, length(c_dep))
  
  use_mean <- which(!only_c)
  use_c <- which(only_c)
  
  run_call[use_mean] <- as.numeric(mean_c_l_dep[use_mean] > ke1_call_thresholds[[assay]][["mean_c_l_dep"]][["call"]])
  bl_call[use_mean] <- as.numeric(
    mean_c_l_dep[use_mean] > ke1_call_thresholds[[assay]][["mean_c_l_dep"]][["bl_ll"]] & 
    mean_c_l_dep[use_mean] < ke1_call_thresholds[[assay]][["mean_c_l_dep"]][["bl_ul"]])
  
  run_call[use_c] <- as.numeric(c_dep[use_c] > ke1_call_thresholds[[assay]][["c_dep"]][["call"]])
  bl_call[use_c] <- as.numeric(
    c_dep[use_c] > ke1_call_thresholds[[assay]][["c_dep"]][["bl_ll"]] & 
    c_dep[use_c] < ke1_call_thresholds[[assay]][["c_dep"]][["bl_ul"]])
  
  run_call[run_call == 0] <- "Negative"
  run_call[run_call == 1] <- "Positive"

  bl_call[bl_call == 1] <- "Borderline"
  bl_call[bl_call == 0] <- run_call[bl_call == 0]
  
  bl_call_all <- aggregate(bl_call, list(cid), function(x) {
    if (length(x) == 1) {
      return(x)
    } else {
      n_call <- table(x)
      max_call <- max(n_call)
      
      overall_call <- names(n_call)[n_call == max_call]
      
      if (length(overall_call) == 1) {
        return(overall_call)
      } else {
        return("Inconclusive")
      }
    }
  })
  names(bl_call_all) <- c("chem_id", "ke1_call")
  
  run_call_all <- data.frame(
    chem_id = cid,
    c_dep = c_dep,
    l_dep = l_dep,
    mean_c_l_dep = mean_c_l_dep,
    cys_only = only_c,
    run_call = run_call,
    bl_call = bl_call
  )

  return(list(run_call = run_call_all, overall_call = bl_call_all))
}

# KE2 -----
lusensCall <- function(lusens_df) {
  
  # Checks
  
  lusens_list <- split(lusens_df, list(lusens_df$run, lusens_df$cid), drop = T)
  run_call_all <- lapply(lusens_list, function(df) {
    df <- df[order(df$conc),]
    
    out <- df[1,c("cid", "run")]
    out$bl_call <- NA
    
    cv70 <- df$cv >= 70
    
    pos <- grepl("TRUETRUE", paste0(df$fi[cv70] >= 1.76, collapse = "")) & grepl("TRUETRUE", paste0(df$pval[cv70] <= 0.05, collapse = ""))
    if (pos) {
      out$bl_call <- ifelse(sum(!cv70) >= 3, "Positive", "Invalid")
    } else {
      neg <- grepl("TRUETRUE", paste0(df$fi[cv70] < 1.28, collapse = ""))
      
      if (neg) {
        out$bl_call <- ifelse(any(!cv70) | ((max(df$conc) >= 2000) & all(cv70)), "Negative", "Invalid")
      } else {
        
        bl <- grepl("TRUETRUE", paste0(df$fi[cv70] >= 1.28 & df$fi[cv70] < 1.76, collapse = "")) & grepl("TRUETRUE", paste0(df$pval[cv70] <= 0.05, collapse = ""))
        
        if (bl) {
          out$bl_call <- ifelse(sum(!cv70) >= 3, "Borderline", "Invalid")
        } else {
          out$bl_call <- "Invalid"
        }
        
      }
      
    }
    return(out)
  })
  
  run_call_all <- do.call("rbind", run_call_all)
  rownames(run_call_all) <- NULL
  
  bl_call_all <- aggregate(run_call_all$bl_call, list(run_call_all$cid), function(x) {
    if (length(x) == 1) {
      return(x)
    } else {
      n_call <- table(x)
      max_call <- max(n_call)
      
      overall_call <- names(n_call)[n_call == max_call]
      
      if (length(overall_call) == 1) {
        return(overall_call)
      } else {
        return("Inconclusive")
      }
    }
  })
  names(bl_call_all) <- c("chem_id", "ke2_call")
  return(list(run_call = run_call_all, overall_call = bl_call_all))
}



# ke2_call_thresholds <- list(
#   keratinosens = c(call = 1.5, bl_ll = 1.35, bl_ul = 1.67),
#   lusens = c(call = 1.5, bl_ll = 1.28, bl_ul = 1.76)
# )
# 
# ke2BL <- function(assay, call, value) {
#   thresh_ll <- ke2_call_thresholds[[assay]][["bl_ll"]]
#   thresh_ul <- ke2_call_thresholds[[assay]][["bl_ul"]]
#   
#   call_bl <- as.character(call)
#   call_bl[call > thresh_ll & call <= thresh_ul] <- "BL"
#   
#   return(call_bl)
# }

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

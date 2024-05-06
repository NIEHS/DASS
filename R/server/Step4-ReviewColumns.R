# =============================================================================#
# File Name: Step4-ReviewColumns.R
# Original Creator: ktto
# Contact Information: ICE-support@niehs.nih.gov
# Date Created: 2021-02-10
# License: MIT
# Description: server file for module with selection review
# Required Packages:
# - data.table, DT
# - shiny shinyBS shinyjs
# =============================================================================#

# Step 4: Review Selections -----
## Reactives -----
ke1_calc_mean <- reactiveVal(F)
ke1_get_call <- reactiveVal(F)
data_select <- reactiveVal()
dt_review <- reactiveVal()
flagged <- reactiveVal()

## Dictionaries -----
# Column IDs by type
check_call_cols <- c("ke1_call_col", "ke2_call_col", "ke3_call_col", "insil_call_col")
check_numeric_cols <- c("ke1_mean_c_l_dep_col", "ke1_c_dep_col", "ke1_l_dep_col", "ke2_val_col")
check_ke3_cols <- "ke3_val_col"
check_ad_cols <- "insil_ad_col"

# Strings for pattern matching
call0_str <- c("0", "i", "inactive", "n", "neg", "negative", "non-sensitizer", "non-sensitiser", "nonsensitizer", "nonsensitiser")
call1_str <- c("1", "a", "active", "p", "pos", "positive", "sensitizer", "sensitiser")
ke3_0_str <- c("NI", "Inf", "i", "inactive", "n", "neg", "negative", "non-sensitizer", "non-sensitiser", "nonsensitizer", "nonsensitiser")

observeEvent(input$review_entries, {
  # Get required information sources
  col_req <- NULL
  data_select <- data_select_template
  
  if (input$do_da_2o3) {
    tmp <- c( "ke2_call_col", "ke3_call_col")
    if (input$do_da_2o3_BL) tmp <- c(tmp, "ke2_val_col", "ke3_val_col")
    if (input$ke1_call_interpret) {
      ke1_get_call(T)
    } else {
      tmp <- c(tmp, "ke1_call_col")
    }
    if (input$ke1_call_interpret | input$do_da_2o3_BL) {
      if (input$ke1_choose_dep) {
        tmp <- c(tmp, "ke1_c_dep_col", "ke1_l_dep_col")
        ke1_calc_mean(T)
      } else {
        tmp <- c(tmp, "ke1_mean_c_l_dep_col")
      }
    }
    data_select[tmp] <- lapply(data_select[tmp], function(x) {
      x[["da_2o3"]] <- "x"
      return(x)
    })
    col_req <- c(col_req, tmp)
  }

  if (input$do_da_its) {
    if (input$ke1_choose_dep) {
      tmp <- c("ke1_c_dep_col", "ke1_l_dep_col", "ke3_val_col", "insil_call_col", "insil_ad_col")
      ke1_calc_mean(T)
    } else {
      tmp <- c("ke1_mean_c_l_dep_col", "ke3_val_col", "insil_call_col", "insil_ad_col")
    }
    data_select[tmp] <- lapply(data_select[tmp], function(x) {
      x[["da_its"]] <- "x"
      return(x)
    })
    col_req <- c(col_req, tmp)
  }
  
  if (input$do_da_ke31) {
    tmp <- "ke3_val_col"
    if (input$ke1_call_interpret) {
      ke1_get_call(T)
      if (input$ke1_choose_dep | input$do_da_2o3_BL) {
        tmp <- c(tmp, "ke1_c_dep_col", "ke1_l_dep_col")
        ke1_calc_mean(T)
      } else {
        tmp <- c(tmp, "ke1_mean_c_l_dep_col")
      }
    } else {
      tmp <- c(tmp, "ke1_call_col")
    }
    data_select[tmp] <- lapply(data_select[tmp], function(x) {
      x[["da_ke31"]] <- "x"
      return(x)
    })
    col_req <- c(col_req, tmp)
  }
  
  col_req <- as.character(sort(factor(unique(col_req), levels = names(data_select))))
  
  no_select <- any(sapply(col_req, function(x) ifelse(is.null(input[[x]]), T, is.na(input[[x]]) | input[[x]] == "")))
  if (no_select) {
    showNotification(
      type = "error",
      ui = "Missing required column selections.",
      duration = 10
    )
  }
  req(!no_select)
  
  shinyjs::enable("tab_review_columns")
  
  call_cols <- col_req[col_req %in% check_call_cols]
  if (length(call_cols) > 0) {
    for (i in call_cols) {
      col_name <- input[[i]]
      data_select[[i]][["col_name"]] <- col_name
      
      vals <- dt_analyze()[[col_name]]
      data_select[[i]][["values"]] <- vals
      
      converted_values <- rep(NA, length(vals))
      converted_values[grepl_ci(concatOrString(call1_str), vals)] <- 1
      converted_values[grepl_ci(concatOrString(call0_str), vals)] <- 0
      data_select[[i]][["converted_values"]] <- converted_values
      
      data_select[[i]][["flagged"]] <- !all(is.na(vals[is.na(converted_values)]))
    }
  }
  
  numeric_cols <- col_req[col_req %in% check_numeric_cols]
  if (length(numeric_cols) > 0) {
    for (i in numeric_cols) {
      col_name <- input[[i]]
      data_select[[i]][["col_name"]] <- col_name
      
      vals <- dt_analyze()[[col_name]]
      data_select[[i]][["values"]] <- vals
      
      converted_values <- suppressWarnings(as.numeric(vals))
      data_select[[i]][["converted_values"]] <- converted_values
      
      data_select[[i]][["flagged"]] <- !all(is.na(vals[is.na(converted_values)]))
    }
  }
  
  if (check_ke3_cols %in% col_req) {
    col_name <- input[[check_ke3_cols]]
    data_select[[check_ke3_cols]][["col_name"]] <- col_name
    
    vals <- dt_analyze()[[col_name]]
    data_select[[check_ke3_cols]][["values"]] <- vals
    
    converted_values <- rep(NA, length(vals))
    isNeg <- grepl_ci(concatOrString(ke3_0_str), vals)
    converted_values[isNeg] <- Inf
    converted_values[!isNeg] <- suppressWarnings(as.numeric(vals[!isNeg]))
    data_select[[check_ke3_cols]][["converted_values"]] <- converted_values
    
    data_select[[check_ke3_cols]][["flagged"]] <- !all(is.na(vals[is.na(converted_values)]))
  }
  
  if (check_ad_cols %in% col_req) {
    col_name <- input[[check_ad_cols]]
    data_select[[check_ad_cols]][["col_name"]] <- col_name
    
    vals <- dt_analyze()[[col_name]]
    data_select[[check_ad_cols]][["values"]] <- vals
    
    converted_values <- rep(NA, length(vals))
    converted_values[grepl_ci(concatOrString(c("1", "in")), vals)] <- 1
    converted_values[grepl_ci(concatOrString(c("0", "out")), vals)] <- 0
    data_select[[check_ad_cols]][["converted_values"]] <- converted_values
    
    data_select[[check_ad_cols]][["flagged"]] <- !all(is.na(vals[is.na(converted_values)]))
  }
  
  data_select(data_select)
  
  dt_review <- lapply(data_select[col_req], function(x) x[c("da_2o3", "da_its", "da_ke31", "display_name", "col_name", "flagged")])
  dt_review <- do.call("rbind.data.frame", dt_review)
  names(dt_review) <- c("DA 2o3", "DA ITS", "DA KE3/1", "Endpoint", "Selected Column", "Flagged")
  rm_col <- colMeans(dt_review[,1:3] == "") == 1
  rm_col <- names(rm_col)[rm_col]
  dt_review[rm_col] <- NULL
  dt_review$Flagged <- ifelse(dt_review$Flagged, "FLAG", "")
  
  dt_review(dt_review)
  
  shinyjs::show("review_contents")
  shinyjs::enable("tab_review_columns")
  updateTabsetPanel(inputId = "step_set", selected = "Review Selection")

  anyFlag <- any(dt_review$Flagged == "FLAG")
  
  if (anyFlag) {
    shinyjs::show("flag_col_warning")
    flagged(1)
  } else {
    flagged(0)
  }
  anyDupe <- any(duplicated(dt_review$`Selected Column`))
  if(anyDupe) {
    shinyjs::show("dupe_col_warning")
  }
})

output$dt_review <- DT::renderDataTable({
  req(dt_review())
  dt_review <- dt_review()
  flag_row <- which(dt_review$Flagged == "FLAG")
  dt_review <- data.frame(lapply(dt_review, as.factor), check.names = F)
  i <- grep("Endpoint|Selected", names(dt_review)) - 1
  
  dt_review <- datatable(dt_review,
            class = "table-bordered stripe",
            rownames = F,
            escape = F,
            selection = "none",
            filter = "top",
            options = list(
              autoWidth = TRUE,
              dom = "t",
              ordering = F,
              columnDefs = list(
                list(width = "30%", targets = i)
              )
            ))
    if (length(flag_row) > 0) {
      dt_review <- formatStyle(dt_review, 0, target = "row", color = styleRow(flag_row, rep("#C25400", length = length(flag_row))))
    }
    dt_review
})
# =============================================================================#
# File Name: utils.R
# Original Creator: Kim To
# Contact Information: ICE-support@niehs.nih.gov
# Date Created: 2024-04-23
# License: MIT
# Description: Functions used in the DASS App
# Required Packages:
# 
# =============================================================================#

# Function to read in data
# `fpath` - user-supplied path to data file
read_data <- function(fpath, sheet = 1) {
  if (grepl("txt$|tsv$|csv$", fpath)) {
    fread(fpath, colClasses = "character", na.strings = c("", "na", "NA"))
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


# Creates exact match for multiple strings for grepping
# `vector` - character vector
# Returns a query string for mattching multiple patterns
concatOrString <- function(vector) {
  paste(sprintf("^%s$", vector), collapse = "|")
}

roundPercent <- function(x, digits = 0) {
  paste0(round(x * 100, digits = digits), "%")
}

info_button <- function(button_id,
                        aria_label) {
  HTML(
    sprintf(
      "<button id = '%s' type='button' class='btn action-link btn-qs' aria-label='%s'><i class='glyphicon glyphicon-question-sign' role='presentation'></i></button>",
      button_id,
      aria_label
    )
  )
}
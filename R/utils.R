# Function to read in data
# `fpath` - user-supplied path to data file
read_excel_dass <- function(fpath, sheet = 1) {
  # Read columns in as list to prevent displaying converted floats
  tryCatch({
    tmp <- read_excel(fpath, sheet = sheet, na = c("NA","na", ""), col_types = "list")
    data.frame(lapply(tmp, unlist))
  }, error = function(e) {e})
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

showHide <- function(show = NULL, hide = NULL) {
  if (!is.null(show)) {
    for (i in show) {
      shinyjs::show(i)
    }
  }
  
  if (!is.null(hide)) {
    for (i in hide) {
      shinyjs::hide(i)
    }
  }
}
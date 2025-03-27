# UI -----
## Custom UI -----
#' Create info button
#' @param button_id
#' @param aria_label
#' @param ... Additional button attributes or ui
#' 
#' @return info button that controls {button_id}_modal
info_button <- function(button_id, aria_label,...) {
  tags$button(
    id = button_id,
    class = "btn action-button btn-qs",
    `aria-label` = aria_label,
    icon("question-sign", lib = "glyphicon"),
    `data-bs-toggle`="modal",
    `data-bs-target`=sprintf("#%s_modal", button_id),
    ...
  )
}

#' Custom file input
#' @description modification of built-in fileInput to resolve accessibility flags.
#' @param inputId
#' @param buttonText
#' @param placeholder 
fileInput_custom <- function(inputId, buttonText = "Choose File", placeholder = "No file selected.") {
  tags$form(
      style = "display: flex; gap:0;",
      action = "#",
      enctype = "multipart/form-data",
      # Button
      tags$label(
        `for` = inputId,
        id = sprintf("%s-label", inputId),
        class = "btn-default",
        style = "margin-bottom: 0;",
        span(
          style = "display: block; padding-left: 1rem; padding-right:2rem; white-space: nowrap;",
          role = "button",
          `aria-controls` = sprintf("%s-fname", inputId),
          tabindex = "0",
          buttonText
        )
      ),
      tags$input(
        id = inputId,
        type = "file",
        style = "display: none;"
      ),
      tags$label(
        `for` = sprintf("%s-fname", inputId),
        class = "sr-only",
        "Selected File"
      ),
      tags$input(
        id = sprintf("%s-fname", inputId),
        class = "form-control",
        type = "text",
        autocomplete = "off",
        readonly = "",
        style = "margin-left: -8px; width:100%; border-radius: 0 8px 8px 0; height: inherit;",
        placeholder = placeholder
      )
  )
}

#' create external link
#' @param x identifier corresponding to the external_links dictionary
externalLinkTag <- function(x, ...) {
  a(
    class = "external-link",
    href = external_links[[x]]$url,
    target = "_blank",
    external_links[[x]]$label,
    tags$span(class = "sr-only", "Opens in new window."),
    ...
  )
}

roundPercent <- function(x, digits = 0) {
  paste0(round(x * 100, digits = digits), "%")
}

## Visibility Toggles -----
# Show or hide ui elements by ID
show_hide <- function(show = NULL, hide = NULL) {
  if (!is.null(show)) for (i in show) shinyjs::show(i)
  if (!is.null(hide)) for (i in hide) shinyjs::hide(i)
}

# Update tab and set focus
tab_change <- function(tabset_id, tab_name) {
  updateTabsetPanel(inputId = tabset_id, selected = tab_name)
  runjs(sprintf("tabFocus('%s')", tab_name))
}

# Strings -----
# Create concatenated string for exact matching of multiple strings
concatOrString <- function(vector) {
  paste(sprintf("^%s$", vector), collapse = "|")
}

# grep functions for case-insensitive matching
grep_ci <- function(str, x, ...) grep(pattern = str, x = x, ignore.case = T, ...)
grepl_ci <- function(str, x, ...) grepl(pattern = str, x = x, ignore.case = T, ...)
# =============================================================================#
# File Name: modifyCollapses.R
# Original Creator: Kim To
# Contact Information: ICE-support@niehs.nih.gov
# Date Created: 2023-08-0
# License: MIT
# Description: Modification of collapse aria attributes. Copy pasted
# functions from package source and edited lines.
# Required Packages:
# - shinyBS
# =============================================================================#

bsCollapse_dass <- function (..., id = NULL, multiple = FALSE, open = NULL) 
{
  if (is.null(id)) 
    id = paste0("collapse", sprintf("%07i", as.integer(stats::runif(1, 
                                                                    1, 1e+06))))
  if (!multiple & length(open) > 1) {
    open <- open[1]
  }
  panels <- list(...)
  for (i in seq(length(panels))) {
    if (getAttribs(panels[[i]])$value %in% open) {
      panels[[i]]$children[[2]] <- addClass(panels[[i]]$children[[2]], 
                                            "in")
    }
    if (!multiple) {
      panels[[i]]$children[[1]]$children[[1]]$children[[1]] <- addAttribs(panels[[i]]$children[[1]]$children[[1]]$children[[1]], 
                                                                          `data-parent` = paste0("#", id))
    }
  }
  bsTag <- shiny::tags$div(class = "panel-group sbs-panel-group", 
                           `data-sbs-multi` = multiple, id = id, 
                           # role = "tablist", 
                           panels)
  htmltools::attachDependencies(bsTag, shinyBSDep)
}
environment(bsCollapse_dass) <- asNamespace("shinyBS")

bsCollapsePanel_dass <- function (title, ..., value = title, style = NULL) 
{
  content <- list(...)
  id <- paste0("cpanel", sprintf("%07i", as.integer(stats::runif(1, 
                                                                 1, 1e+06))))
  if (is.null(value)) {
    value = title
  }
  if (is.null(style)) {
    style = "default"
  }
  bsTag <- shiny::tags$div(
    class = paste0("panel panel-", style), 
    value = value, 
    shiny::tags$div(
      class = "panel-heading",
      id = paste0("heading_", id),
      shiny::tags$a(
        `data-toggle` = "collapse", 
        `aria-controls` = id,
        role = "button",
        href = paste0("#", id),
        div(
          class = "panel-heading-text",
          title
          )
      )
    ),
    shiny::tags$div(
      id = id, 
      class = "panel-collapse collapse", 
      role = "region", 
      `aria-labelledby` = paste0("heading_", id), 
      shiny::tags$div(class = "panel-body", 
                      content)))
  htmltools::attachDependencies(bsTag, shinyBSDep)
}
environment(bsCollapsePanel_dass) <- asNamespace("shinyBS")
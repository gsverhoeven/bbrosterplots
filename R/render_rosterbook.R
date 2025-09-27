#' Render rosterbook as PDF
#'
#' @returns Nothing.

#' @export
render_rosterbook <- function(target_dir = "."){
  file.copy(from = system.file("extdata", "roster_book_template.Rmd", package = "bbrosterplots"), to = "roster_book.Rmd")
  rmarkdown::render("roster_book.Rmd")
}

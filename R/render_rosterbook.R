#' Render rosterbook as PDF
#'
#' @returns Nothing.

#' @export
render_rosterbook <- function(target_dir = ".", params, refresh_rmd = TRUE){
  if(refresh){
    file.copy(from = system.file("extdata", "roster_book_template.Rmd", package = "bbrosterplots"), to = "roster_book.Rmd", overwrite = TRUE)
  }
  rmarkdown::render("roster_book.Rmd", params = params)
}

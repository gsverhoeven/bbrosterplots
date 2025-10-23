#' Render rosterbook as PDF
#'
#' @returns Nothing.

#' @export
render_rosterbook <- function(target_dir = ".", params, refresh_rmd = TRUE){
  if(refresh_rmd){
    file.copy(from = system.file("extdata", "roster_book_template.Rmd", package = "bbrosterplots"), to = "output/roster_book.Rmd", overwrite = TRUE)
  }
  rmarkdown::render("output/roster_book.Rmd", params = params)
}

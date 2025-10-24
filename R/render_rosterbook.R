#' Render rosterbook as PDF
#'
#' @param target_dir dir where the roster book is rendered.
#' @param params A list of parameters to customize the roster book.
#' @param refresh_rmd Controls if the template is refreshed.
#' @returns Nothing.

#' @export
render_rosterbook <- function(target_dir = "output/", params, refresh_rmd = TRUE){
  if(refresh_rmd){
    file.copy(from = system.file("extdata", "roster_book_template.Rmd", package = "bbrosterplots"), to = paste0(target_dir, "roster_book.Rmd"), overwrite = TRUE)
  }
  rmarkdown::render(paste0(target_dir, "roster_book.Rmd"), params = params)
}

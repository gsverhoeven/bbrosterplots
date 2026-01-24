#' Render rosterbook as PDF
#'
#' @param target_dir dir where the roster book is rendered.
#' @param params A list of parameters to customize the roster book. See example below for the available parameters.
#' @param refresh_rmd Controls if the template is refreshed.
#' @returns Nothing.
#' @examples
#'\dontrun{
#' render_rosterbook(params = list(group_name = "Super League S6",
#'                                 tournament_ruleset = "EB25",
#'                                 cover_image = "EB25.jpg",
#'                                 scale_cover_perc = "100%",
#'                                 races = c("Khorne", "Dwarf"),
#'                                 perc = FALSE,
#'                                 team = FALSE), refresh_rmd = TRUE)
#'}

#' @export
render_rosterbook <- function(target_dir = "output/", params, refresh_rmd = TRUE){
  if(refresh_rmd){
    file.copy(from = system.file("extdata", "roster_book_template.Rmd", package = "bbrosterplots"), to = paste0(target_dir, "roster_book.Rmd"), overwrite = TRUE)
  }
  rmarkdown::render(paste0(target_dir, "roster_book.Rmd"), params = params)
}

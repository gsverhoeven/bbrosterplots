#' Add metadata to roster
#'
#' @param df_rosters A (set of) roster.
#' @returns A set of rosters with color, cost and sort order added

#'@export
add_roster_metadata <- function(df_rosters){
  skill_colors <- readr::read_csv2(system.file("extdata" , "ref_bb_skill_colors.csv", package = "bbrosterplots"), show_col_types = FALSE)
  # fix data error
  skill_colors <- skill_colors[!duplicated(skill_colors$skill_name),]
  # match input data
  skill_colors <- skill_colors %>%
    dplyr::mutate(skill_name = ifelse(is.na(skill_name), "", skill_name))

  df_rosters <- df_rosters %>%
    dplyr::left_join(skill_colors %>% dplyr::select(skill_name, color), by = "skill_name")

  rosters_cost <- readr::read_csv2(system.file("extdata" , "ref_bb_rosters_cost.csv", package = "bbrosterplots"), show_col_types = FALSE)

  df_rosters <- df_rosters %>%
    dplyr::left_join(rosters_cost %>% dplyr::select(position, roster.name, sort_order, cost), by = c("position", "roster.name") )

  # add player_id (remove this later, now needed downstream)
  df_rosters <- df_rosters %>%
    dplyr::mutate(player_id = 1:nrow(df_rosters))
  return(df_rosters)
}

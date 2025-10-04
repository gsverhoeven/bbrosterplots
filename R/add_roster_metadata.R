#' Add metadata to roster
#'
#' @param df_rosters A (set of) roster.
#' @returns A set of rosters with color, cost and sort order added

#'@export
add_roster_metadata <- function(df_rosters){
  skill_colors <- readr::read_delim(system.file("extdata" , "ref_bb_skill_colors.csv", package = "bbrosterplots"), show_col_types = FALSE)

  # match input data
  skill_colors <- skill_colors %>%
    dplyr::mutate(skill_name = ifelse(is.na(skill_name), "", skill_name))

  # create tmp skill name variable without spaces and caps in both tables before joining
  skill_colors <- skill_colors %>%
    mutate(skill_name2 = stringr::str_replace_all(skill_name, fixed(" "), "")) %>%
    mutate(skill_name2 = stringr::str_to_lower(skill_name2))

  df_rosters <- df_rosters %>%
    mutate(skill_name2 = stringr::str_replace_all(skill_name, fixed(" "), "")) %>%
    mutate(skill_name2 = stringr::str_to_lower(skill_name2))

  # join then drop tmp variable
  df_rosters <- df_rosters %>%
    dplyr::left_join(skill_colors %>% dplyr::select(skill_name2, color), by = "skill_name2") %>%
    dplyr::select(-skill_name2)

  rosters_cost <- readr::read_delim(system.file("extdata" , "ref_bb_rosters_cost.csv", package = "bbrosterplots"), show_col_types = FALSE)

  rosters_cost <- rosters_cost %>%
    mutate(position2 = stringr::str_replace_all(position, fixed(" "), "")) %>%
    mutate(position2 = stringr::str_replace_all(position2, "-", "")) %>%
    mutate(position2 = stringr::str_to_lower(position2))

  df_rosters <- df_rosters %>%
    mutate(position2 = stringr::str_replace_all(position, fixed(" "), "")) %>%
    mutate(position2 = stringr::str_replace_all(position2, "-", "")) %>%
    mutate(position2 = stringr::str_to_lower(position2))

  df_rosters <- df_rosters %>%
    dplyr::left_join(rosters_cost %>% dplyr::select(position2, roster.name, sort_order, cost), by = c("position2", "roster.name") ) %>%
    select(-position2)

  # add player_id (remove this later, now needed downstream)
  df_rosters <- df_rosters %>%
    dplyr::mutate(player_id = 1:nrow(df_rosters))
  return(df_rosters)
}

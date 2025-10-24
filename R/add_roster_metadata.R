#' Add metadata to roster
#'
#' @param df_rosters A (set of) roster.
#' @returns A set of rosters with color, cost and sort order added
#'@export
add_roster_metadata <- function(df_rosters){
  df_rosters <- df_rosters %>%
    dplyr::select_if(!names(.) %in% c('color', 'sort_order', 'cost'))

  skill_colors <- readr::read_delim(system.file("extdata" , "ref_bb_skill_colors.csv", package = "bbrosterplots"), show_col_types = FALSE)
  # remove existing grouping from tibble
  df_rosters <- df_rosters %>% ungroup()

  # match input data
  skill_colors <- skill_colors %>%
    dplyr::mutate(skill_name = ifelse(is.na(.data$skill_name), "", .data$skill_name))

  # create tmp skill name variable without spaces and caps in both tables before joining
  skill_colors <- skill_colors %>%
    mutate(skill_name2 = stringr::str_replace_all(.data$skill_name, stringr::fixed(" "), "")) %>%
    mutate(skill_name2 = stringr::str_to_lower(.data$skill_name2))

  df_rosters <- df_rosters %>%
    mutate(skill_name2 = stringr::str_replace_all(.data$skill_name, stringr::fixed(" "), "")) %>%
    mutate(skill_name2 = stringr::str_to_lower(.data$skill_name2))

  # join then drop tmp variable
  df_rosters <- df_rosters %>%
    dplyr::left_join(skill_colors %>% dplyr::select(.data$skill_name2, .data$color), by = "skill_name2") %>%
    dplyr::select(-.data$skill_name2)

  rosters_cost <- readr::read_delim(system.file("extdata" , "ref_bb_rosters_cost.csv", package = "bbrosterplots"), show_col_types = FALSE)

  rosters_cost <- rosters_cost %>%
    mutate(position2 = stringr::str_replace_all(.data$position, stringr::fixed(" "), "")) %>%
    mutate(position2 = stringr::str_replace_all(.data$position2, "-", "")) %>%
    mutate(position2 = stringr::str_to_lower(.data$position2))

  df_rosters <- df_rosters %>%
    mutate(position2 = stringr::str_replace_all(.data$position, stringr::fixed(" "), "")) %>%
    mutate(position2 = stringr::str_replace_all(.data$position2, "-", "")) %>%
    mutate(position2 = stringr::str_to_lower(.data$position2))

  df_rosters <- df_rosters %>%
    dplyr::left_join(rosters_cost %>% dplyr::select(.data$position2, .data$roster.name, .data$sort_order, .data$cost), by = c("position2", "roster.name") ) %>%
    select(-.data$position2)

  # add player_id if column does not exist (remove this later, now needed downstream)
  if (!("player_id" %in% names(df_rosters))){
    df_rosters <- df_rosters %>%
      dplyr::mutate(player_id = 1:nrow(df_rosters))
  }
  return(df_rosters)
}
utils::globalVariables(".")

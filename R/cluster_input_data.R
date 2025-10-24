cluster_input_data <- function(df_rosters, group_name, race_name){

  df <- df_rosters %>%
    filter(.data$roster.name == .data$race_name) %>%
    group_by(.data$team_id, .data$coach_name, .data$player_id, .data$position, .data$sort_order, .data$number, .data$skill_name, .data$color) %>% # skill stacking
    summarise(cnt = max(.data$cnt), cost = max(.data$cost)) %>%
    group_by(.data$team_id, .data$coach_name, .data$player_id, .data$position, .data$sort_order, .data$skill_name, .data$color) %>%
    summarise(n = sum(.data$cnt), cost = .data$cost * sum(.data$cnt)) %>%
    group_by(.data$team_id, .data$coach_name, .data$position, .data$sort_order, .data$n) %>%
    summarise(nr = row_number(), skill_name = .data$skill_name, color = .data$color, cost = .data$cost)

  # need at least two teams for clustering
  if(n_distinct(df$team_id) > 1 & n_distinct(df$cost) > 1){ # do clustering
    df <- df %>%
      mutate(position_unique = paste0(.data$position, "_", .data$nr)) %>%
      mutate(team_id_char = as.character(.data$team_id)) %>%
      ungroup() %>%
      hclust_order(xvar = "team_id_char",
                               yvar = "position_unique",
                               value_var = "cost", dcast_fill = 1) %>%
      select(.data$team_id, .data$cluster_order) %>%
      distinct() %>%
      right_join(df, by = "team_id")
  } else { # if cost is missing or only 1 team
    df <- df %>%
      mutate(cluster_order = as.integer(as.factor(.data$coach_name)))
  }
  df$coach_team_id <- paste0(df$coach_name, "_", stringr::str_sub(as.character(df$team_id)))


  # Wrap title to ensure it does not overlap with caption (e.g. for Imperial Nobility)
  plot_title <- stringr::str_wrap(paste0(group_name, " ", race_name, " rosters"), width = 35)

  return(list(df, plot_title))

}

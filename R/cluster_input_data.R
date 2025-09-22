cluster_input_data <- function(df_rosters, group_name, race_name){

  df <- df_rosters %>%
    filter(is.na(banned_due_to_star_player) & is.na(banned_due_to_inducements) & is.na(banned_due_to_number_of_skills)) %>%
    filter(roster.name == race_name) %>%
    filter(!(position %in% c("cheerleaders", "assistantCoaches"))) %>%
    group_by(team_id, coach_name, player_id, position, sort_order, number, skill_name, color) %>% # skill stacking
    summarise(cnt = max(cnt), cost = max(cost)) %>%
    group_by(team_id, coach_name, player_id, position, sort_order, skill_name, color) %>%
    summarise(n = sum(cnt), cost = cost * sum(cnt)) %>%
    group_by(team_id, coach_name, position, sort_order, n) %>%
    summarise(nr = row_number(), skill_name = skill_name, color = color, cost = cost)

  # need at least two teams for clustering
  if(n_distinct(df$team_id) > 1 & n_distinct(df$cost) > 1){ # do clustering
    df <- df %>%
      mutate(position_unique = paste0(position, "_", nr)) %>%
      mutate(team_id_char = as.character(team_id)) %>%
      ungroup() %>%
      ggoheatmap::hclust_order(xvar = "team_id_char",
                               yvar = "position_unique",
                               value_var = "cost", dcast_fill = 1) %>%
      select(team_id, cluster_order) %>%
      distinct() %>%
      right_join(df, by = "team_id")
  } else {
    df <- df %>%
      mutate(cluster_order = as.integer(as.factor(coach_name)))
  }
  df$coach_team_id <- paste0(df$coach_name, "_", stringr::str_sub(as.character(df$team_id)))


  # Wrap title to ensure it does not overlap with caption (e.g. for Imperial Nobility)
  plot_title <- stringr::str_wrap(paste0("FUMBBL ", group_name, " ", race_name, " rosters"), width = 35)

  return(list(df, plot_title))

}

cluster_input_data <- function(df, group_name, race_name){

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
  } else { # if cost is missing and/or only 1 team: no clustering possible or needed
    df <- df %>%
      mutate(cluster_order = as.integer(as.factor(.data$coach_name)))
  }
  df$coach_team_id <- paste0(df$coach_name, "_", stringr::str_sub(as.character(df$team_id)))

  # Wrap title to ensure it does not overlap with caption (e.g. for Imperial Nobility)
  plot_title <- stringr::str_wrap(paste0(group_name, " ", race_name, " rosters"), width = 35)

  return(list(df, plot_title))

}

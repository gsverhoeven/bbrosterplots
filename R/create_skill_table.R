#' Make Skill tables
#'
#' @param df_rosters A roster.
#' @param tournament_ruleset A ruleset name.
#' @param group_name A group name.
#' @param return_table If a table is returned.
#' @returns A tag list.

#' @export
create_skill_table <- function(df_rosters, tournament_ruleset = "my_ruleset", group_name = "my_group", return_table = FALSE) {
  races <- unique(df_rosters$roster.name)
  for(i in 1:length(races)){
    race_name <- races[i]
    print(race_name)
    df <- df_rosters %>%
      filter(.data$roster.name == race_name & .data$position != "" & .data$number != 99) %>%
      # Drop rows with no skill if at least one other skill is filled for the player (to solve a bug with multiple rows for some players with only 1 skill)
      group_by(.data$player_id) %>%
      mutate(all_empty = all(.data$name == "" | is.na(.data$name))) %>%
      filter(
        # Case 1: all rows empty → keep only the first
        (.data$all_empty & row_number() == 1) |
          # Case 2: otherwise → keep only non-empty rows
          (!.data$all_empty & .data$name != "")
      ) %>%
      ungroup() %>%
      mutate(name = ifelse(.data$name == "", "No skill", .data$name),
             team_count = n_distinct(.data$team_id)) %>%
      group_by(.data$position) %>%
      mutate(positional_count = n_distinct(.data$player_id)) %>%
      ungroup() %>%
      group_by(.data$position, .data$name) %>%
      summarise(percentage_picks = 100*n() / first(.data$positional_count),
                team_picks = n() / first(.data$team_count),
                .groups = "drop") %>%
      rename("Skill" = .data$name)

    percentage_table <- df %>%
      select(-.data$team_picks) %>%
      tidyr::pivot_wider(
        names_from = .data$position,
        values_from = .data$percentage_picks,
        values_fill = list(percentage_picks = 0)
      )

    team_table <- df %>%
      select(-.data$percentage_picks) %>%
      filter(.data$Skill != "No skill") %>%
      tidyr::pivot_wider(
        names_from = .data$position,
        values_from = .data$team_picks,
        values_fill = list(team_picks = 0)
      )
    table1 <- build_table(data = percentage_table, type = "percentage", group_name, race_name, tournament_ruleset, save = TRUE)
    table2 <- build_table(data = team_table, type = "team", group_name, race_name, tournament_ruleset, save = TRUE)
  }
  if(return_table){
    return(  htmltools::tagList(table1, table2)
             )
  }
}

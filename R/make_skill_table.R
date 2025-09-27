#' Make Skill tables
#'
#' @param df_rosters A roster.
#' @param race_name A race name.
#' @returns A tag list.

#' @export
make_skill_table <- function(race_name, df_rosters, return_table = FALSE) {
  df <- df_rosters %>%
    filter(roster.name == race_name & position != "" & number != 99) %>%
    # Drop rows with no skill if at least one other skill is filled for the player (to solve a bug with multiple rows for some players with only 1 skill)
    group_by(player_id) %>%
    mutate(all_empty = all(name == "" | is.na(name))) %>%
    filter(
      # Case 1: all rows empty → keep only the first
      (all_empty & row_number() == 1) |
        # Case 2: otherwise → keep only non-empty rows
        (!all_empty & name != "")
    ) %>%
    ungroup() %>%
    mutate(name = ifelse(name == "", "No skill", name),
           team_count = n_distinct(team_id)) %>%
    group_by(position) %>%
    mutate(positional_count = n_distinct(player_id)) %>%
    ungroup() %>%
    group_by(position, name) %>%
    summarise(percentage_picks = 100*n() / first(positional_count),
              team_picks = n() / first(team_count),
              .groups = "drop") %>%
    rename("Skill" = name)

  percentage_table <- df %>%
    select(-team_picks) %>%
    tidyr::pivot_wider(
      names_from = position,
      values_from = percentage_picks,
      values_fill = list(percentage_picks = 0)
    )

  team_table <- df %>%
    select(-percentage_picks) %>%
    filter(Skill != "No skill") %>%
    tidyr::pivot_wider(
      names_from = position,
      values_from = team_picks,
      values_fill = list(team_picks = 0)
    )
  table1 <- build_table(data = percentage_table, type = "percentage", save = TRUE)
  table2 <- build_table(data = team_table, type = "team", save = TRUE)

  if(return_table){
    return(  htmltools::tagList(table1, table2)
             )
  }
}

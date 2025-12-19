#' Create rosterplot
#'
#' @param df_rosters A roster.
#' @param group_name A group name.
#' @param tournament_ruleset A ruleset name.
#' @param write whether to write the plot to disk or not.
#' @returns A plot object.

#' @export
create_rosterplot <- function(df_rosters, group_name = "my_group", write = TRUE, tournament_ruleset = "my_ruleset"){
  races <- unique(df_rosters$roster.name)
  for(i in 1:length(races)){
    race_name <- races[i]
    print(race_name)
    # datawrangling -> move to separate function?
    df <- df_rosters %>%
      filter(.data$roster.name == race_name) %>% # aggregate to remove duplicate rows
      group_by(.data$team_id, .data$coach_name, .data$player_id, .data$position, .data$sort_order, .data$number, .data$skill_name, .data$color) %>%
      summarise(cnt = max(.data$cnt), cost = max(.data$cost)) %>% # sum cnt and cnt*cost
      group_by(.data$team_id, .data$coach_name, .data$player_id, .data$position, .data$sort_order, .data$skill_name, .data$color) %>%
      summarise(n = sum(.data$cnt), cost = .data$cost * sum(.data$cnt)) %>% # add positional numbering, use reframe()
      group_by(.data$team_id, .data$coach_name, .data$position, .data$sort_order, .data$n) %>%
      reframe(nr = row_number(), skill_name = .data$skill_name, color = .data$color, cost = .data$cost)

    out_list <- cluster_input_data(df, group_name, race_name)
    df <- out_list[[1]]
    plot_title <- out_list[[2]]

    main_plot <- create_main_plot(df, plot_title)

    # Calculate the number of skills displayed to adapt the height for the width of the legend and saving the plot
    n_legend_items <- n_distinct(df$skill_name)

    legend_plot <- create_legend_plot(df, n_legend_items)
    #
    # Combine the main plot and custom legend
    final_plot <- gridExtra::arrangeGrob(
      main_plot,
      legend_plot,
      ncol = 2,
      widths = c(3, 1)
    )
    plotname <- paste0('output/', tournament_ruleset, "/", group_name, "_roster_plot_", race_name, ".png")
    write_rosterplot(final_plot, plotname, n_legend_items)
  }
  # draw plot
  final_plot <- gridExtra::grid.arrange(final_plot)
  return(final_plot)

}

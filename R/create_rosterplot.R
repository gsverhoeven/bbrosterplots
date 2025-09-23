#' Create rosterplot
#'
#' @param df_rosters A roster.
#' @param group_name A group name.
#' @returns A plot object.

#' @export
create_rosterplot <- function(df_rosters, group_name, race_name, write = TRUE){

  out_list <- cluster_input_data(df_rosters, group_name, race_name)
  df <- out_list[[1]]
  plot_title <- out_list[[2]]

  main_plot <- create_main_plot(df, plot_title)

  # Calculate the number of skills displayed to adapt the height for the width of the legend and saving the plot
  n_legend_items <- n_distinct(df$skill_name)

  legend_plot <- create_legend_plot(df, n_legend_items)

  # Combine the main plot and custom legend
  final_plot <- gridExtra::arrangeGrob(
    main_plot,
    legend_plot,
    ncol = 2,
    widths = c(3, 1)
  )
  plotname <- paste0(tournament_ruleset, "/", group_name, "_roster_plot_", race_name, ".png")
  write_rosterplot(final_plot, plotname, n_legend_items)
  return(gridExtra::grid.arrange(final_plot))
}

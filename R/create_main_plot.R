#' @import ggplot2
create_main_plot <- function(df, plot_title){
  main_plot <- ggplot(df, aes(x = reorder(factor(.data$coach_team_id), .data$cluster_order), y = reorder(paste(.data$position, .data$nr), -.data$sort_order))) +
    geom_tile(aes(fill = .data$color), color = "black") +
    geom_text(aes(label = .data$n), color = "white") +
    scale_fill_identity(breaks = c(df$color),
                        labels = c(df$skill_name)) +
    coord_fixed() +
    theme(legend.position = "none") +
    ggtitle(plot_title) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(panel.background = element_rect(fill = "white")) +
    theme(plot.margin = ggplot2::margin(l = 0)) + # Set left margin to 0 to try to move to the left
    labs(x = "", y = "")
  return(main_plot)
}

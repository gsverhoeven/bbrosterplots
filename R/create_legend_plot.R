create_legend_plot <- function(df, n_legend_items){
  # Sort skills alphabetically to display them sorted in the caption
  df <- df %>%
    arrange(skill_name)

  # Create a data frame for the legend
  legend_df <- df  %>%
    ungroup() %>% 
    select(skill_name, color) %>% 
    distinct(skill_name, color) 
  

  # Generate a custom legend using ggplot
  legend_plot <- ggplot(legend_df, aes(x = 1, y = skill_name, fill = color)) +
    # The width is proportional to the number of items to display because for some reasons, the more the items, the smaller the width becomes in default settings
    geom_tile(color = "black", width = 2.8*n_legend_items/11) +
    # Add a shadow to make white (added on the following line) readable on light background. The nudge_x offset is there because for some reasons, the text is not aligned left, despite hjust = 0. It is proportional to the number of items because the bigger the width (proportional to the number of items), the bigger the disalignment
    geom_text(aes(label = skill_name), hjust = 0, color = "black", nudge_x = -1.4*n_legend_items/11-0.02, nudge_y = -0.02) + 
    geom_text(aes(label = skill_name), hjust = 0, color = "white", nudge_x = -1.4*n_legend_items/11) +
    scale_fill_identity() +
    coord_fixed() +
    theme_void() +
    theme(
      legend.position = "none",
      axis.text.y = element_blank(),
      axis.title = element_blank()
    )
  return(legend_plot)
}
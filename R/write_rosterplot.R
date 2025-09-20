write_rosterplot <- function(plot, plotname, n_legend_items, base_height = 6.3, extra_height_per_item = 0.25, verbose = FALSE){
  # Set dynamic height: Base height + extra height per legend item
  # 1 = 300 px, usually a race requires 1500px to be displayed properly
  # A skill requires about 75px or 0.25 to be properly displayed
  # The base height of 6.25 allows to display properly about 20 skills (+ "Skills")
  
  dynamic_height <- base_height + max((n_legend_items-20), 0) * extra_height_per_item
  
  ggsave(gsub(" ", "_", plotname), plot, width = 10, height = dynamic_height)
  if (verbose)
    print(plotname)
}
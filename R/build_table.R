build_table <- function(data, type, save) {
  data <- data %>%
    janitor::adorn_totals("col") %>%
    janitor::adorn_totals("row") %>%
    # custom ordering
    arrange(
      case_when(Skill == "No skill" ~ 1,
                Skill == "Total" ~ 3,
                TRUE ~ 2),
      desc(Total)
    )

  # Round because save_reactable() otherwise messes up the formating of digits
  if (type == "percentage") {
    data <- data %>% mutate(across(where(is.numeric), ~ round(., 0)))
  } else {
    data <- data %>% mutate(across(where(is.numeric), ~ round(., 1)))
  }

  default_col_format <- if (type == "percentage") {
    reactable::colFormat(digits = 0, suffix = "%")
  } else if (type == "team") {
    reactable::colFormat(digits = 1) # no % suffix
  } else {
    reactable::colFormat() # fallback
  }

  table <- reactable::reactable(
    data %>% select(-Total), # select to remove for team_table (to be done)
    # searchable = TRUE,
    sortable = TRUE,
    # filterable = TRUE,
    striped = TRUE,
    highlight = TRUE,
    columns = list(
      Skill = reactable::colDef(align = "left"),  # no % suffix here
      format = reactable::colFormat(suffix = "") # does not work at this stage
    ),
    defaultColDef = reactable::colDef(           # <- not .default !
      align = "center",
      format = default_col_format

    ),
    defaultPageSize = 20,
    bordered = TRUE,
    rowStyle = function(index) {
      if (index == nrow(data)) {
        list(fontWeight = "bold")
      } else {
        NULL
      }
    },
    theme = reactable::reactableTheme(
      borderColor = "#e0e0e0",
      highlightColor = "#f0f8ff"
    )
  )

  if(save == TRUE) {
    save_reactable(table,
                   gsub(" ", "_", paste0(tournament_ruleset, "/", group_name, "_", type, "-stat_plot_", race_name, ".png")))
  }

  return(table)
}


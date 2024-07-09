
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(shadowtext)
library(scales)


# Import Data -------------------------------------------------------------

population_projection <-
  read_rds("population_projection.rds")


# Create Function ---------------------------------------------------------

population_projection_plot <- function(town_to_plot, county_to_plot) {
  population_projection |>
    filter(location %in% c(town_to_plot, county_to_plot, "Connecticut")) |>
    mutate(location = fct(
      location,
      levels = c("Connecticut", county_to_plot, town_to_plot)
    )) |>
    ggplot(aes(
      x = year,
      y = pct,
      color = location,
      group = location
    )) +
    geom_point(size = 2) +
    geom_line(show.legend = FALSE) +
    labs(color = NULL) +
    facet_wrap(
      vars(age_group),
      nrow = 1
    ) +
    scale_y_continuous(
      limits = c(0, 0.4),
      labels = percent_format(1)
    ) +
    scale_color_manual(
      values = c(
        # town_to_plot = "#9f3515",
        # county_to_plot = "#fbbfb8",
        # "Connecticut" = "#c4c4c4"
        "#c4c4c4",
        "#fbbfb8",
        "#9f3515"
      )
    ) +
    guides(color = guide_legend(reverse = TRUE)) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(
        face = "italic",
        size = 13,
        color = "grey40"
      ),
      legend.text = element_text(
        size = 13,
        color = "grey40"
      ),
      axis.title = element_blank(),
      axis.text = element_text(
        size = 13,
        color = "grey40"
      )
    )
}


# Plot Hartford -----------------------------------------------------------

population_projection_plot(
  town_to_plot = "Hartford",
  county_to_plot = "Hartford County"
) +
  geom_text(
    data = population_projection |> filter(location == "Hartford"),
    nudge_y = 0.03,
    aes(
      label = pct_formatted
    )
  )


# Plot Stamford -----------------------------------------------------------

population_projection_plot(
  town_to_plot = "Stamford",
  county_to_plot = "Fairfield County"
) +
  geom_text(
    data = population_projection |> filter(location == "Stamford"),
    nudge_y = 0.03,
    aes(
      label = pct_formatted
    )
  )

population_projection_plot(
  town_to_plot = "Stamford",
  county_to_plot = "Fairfield County"
) +
  geom_shadowtext(
    data = population_projection |> filter(location == "Stamford"),
    bg.color = "white",
    nudge_y = 0.03,
    aes(
      label = pct_formatted
    )
  )

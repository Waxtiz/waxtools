#' Wax theme for ggplot2
#' Personal theme for ggplot2
#'
#' @param theme sets the overall style of the chart
#' @param key is a list of predefined styles
#'
#' @import ggplot2
#'
#' @export
theme_wax <- function(theme = "defaut", key = list(
                    defaut = list(
                      base_size = 13,
                      family_title = "sans",
                      family_text = "sans",
                      primary_color = "#33302C",
                      secondary_color = "#59544e",
                      color_grid = "#59544e"),
                    wax = list(
                      base_size = 13,
                      family_title = "roboto-slab",
                      family_text = "fira-sans",
                      primary_color = "#33302C",
                      secondary_color = "#59544e",
                      color_grid = "#E8E3DC")
                    )) {

  ggplot2::theme_light(base_size = key[[theme]]$base_size) %+replace%
    ggplot2::theme(
      # Text
      text = element_text(family = key[[theme]]$family_text,
                          size = key[[theme]]$base_size,
                          color = key[[theme]]$secondary_color),
      axis.text = element_text(size = key[[theme]]$base_size),
      axis.title = element_text(family = key[[theme]]$family_text, face = "bold", size = rel(1),
                                color = key[[theme]]$primary_color),
      plot.title = element_text(family = key[[theme]]$family_title, face = "bold",
                                size = rel(1.67), hjust = 0, color = key[[theme]]$primary_color),
      plot.subtitle = element_text(family = key[[theme]]$family_title, size = 16,
                                   margin = margin(0.25, 0, .5, 0, unit = "cm"),
                                   hjust = 0, color = key[[theme]]$primary_color),
      plot.caption = element_text(size = rel(.75), margin = margin(.25, 0, 0, 0, unit = "cm"), hjust = 1),
      legend.title = element_text(family = key[[theme]]$family_text, size = rel(1), face = "bold", hjust = 0,
                                  color = key[[theme]]$primary_color),
      legend.text = element_text(size=rel(.9)),
      # fill, grid, background
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.grid.minor = element_line(color = alpha(key[[theme]]$secondary_color, .25), size = rel(.25)),
      # panel.grid.minor = element_blank(),
      panel.grid.major =  element_line(color = alpha(key[[theme]]$secondary_color, .25), size =  rel(.75)),
      axis.line = element_line(size = rel(1.5), alpha(key[[theme]]$secondary_color, .5)),
      # panel.border  = element_rect(colour = key[[theme]]$color_grid, fill = NA, size = rel(2)),
      panel.border = element_blank(),
      strip.background = element_blank(),
      legend.background = element_blank(),
      # Margin, position
      plot.margin = margin(.5, .5, .5, .5, unit = "cm"),
      axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")),
      axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm"), angle =90),
      # legend.position = "bottom",
      legend.direction = 'horizontal',
      legend.box = "horizontal",
      legend.title.align = 0.5,
      legend.box.just = "center",
      panel.spacing.x = grid::unit(2, "lines"), # Space between facets
      # Other
      legend.key.size = unit(.5, 'cm'),
      axis.ticks = element_blank(),
      strip.text = element_text(size = rel(1), face = "bold", margin = margin(0, 0, .25, 0, unit = "cm"))
    )
}
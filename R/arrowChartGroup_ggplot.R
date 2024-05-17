#' Helper function for Arrow Chart by Group for Blackstone Research and Evaluation
#'
#' [arrowChartGroup_ggplot()] creates an pre-post arrow chart of group averages and returns a ggplot object.
#'
#' @param df_gg Required, a [tibble][tibble::tibble-package] or data frame of **numeric** data that has items with the prefix of `pre_` and `post_`; and has a categorical group
#'  variable to split up the data (e.g. role, gender, education level, etc.).
#'
#' @param group Required, the name of the grouping variable in a quoted character string (e.g. "role", "gender", "edu_level", etc.).
#'
#' @param fill_gg Passed character vector of color codes from main function, that corresponds to the colors for each scale label and bar in the chart.
#'
#' @param scale_labels_gg Required, a character vector of labels for the response scale, must be in the desired order,
#'    e.g. if you have a 5 item scale of minimal to extensive it should look like this: `levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")`.
#'
#' @return A [ggplot2][ggplot2::ggplot2-package] object that plots the items into a arrow bar chart.
#' @noRd
arrowChartGroup_ggplot <- function(df_gg, group, fill_gg, scale_labels_gg) {

    # Load fonts:
    extrafont::loadfonts("all", quiet = TRUE)
    . <- NULL # to stop check() from bringing up "."
    # Create a font family character var so that it is easy to change, could also be a new arg:
    font_family <- c("Arial")

    # ggplot call:
    arrow <- {{ df_gg }} %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$score_avg, y = forcats::fct_rev(.data[[group]]), color = forcats::fct_rev(.data[[group]]),
        label = scales::number(.data$score_avg, accuracy = 0.01), group = .data[[group]]
      )) +
      ggplot2::geom_line(
        lineend = "round", linejoin = "round", linewidth = 1,
        arrow = grid::arrow(type = "closed", length = ggplot2::unit(0.1, "inches"))
      ) +
      ggplot2::geom_text(
        data = dplyr::filter(df_gg, .data$timing == "pre"), nudge_x = -0.075, hjust = 1, show.legend = FALSE,
        family = font_family, size = 3.5
      ) +
      ggplot2::geom_text(
        data = dplyr::filter(df_gg, .data$timing == "post"), nudge_x = 0.075, hjust = 0, show.legend = FALSE,
        family = font_family, size = 3.5
      ) +
      ggplot2::facet_wrap(~question, ncol = 1, strip.position = "left") +
      ggplot2::scale_color_manual(values = fill_gg, labels = function(group) stringr::str_to_title(group)) +
      ggplot2::scale_x_continuous(limits = c(1, length(scale_labels_gg)), labels = scale_labels_gg) +
      ggplot2::labs(tag = NULL, color = NULL) +
      ggplot2::theme_void(base_family = font_family, base_size = 12) +
      ggplot2::theme(
        axis.text.x = ggtext::element_markdown(
          color = "#767171", size = 12, family = font_family,
          margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
        ),
        strip.text.y.left = ggtext::element_markdown(
          angle = 0, hjust = 1, color = "black", size = 12, family = font_family,
          margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 0, unit = "pt")
        ),
        plot.margin = ggplot2::margin(t = 5, r = 25, b = 5, l = 5, unit = "pt"),
        legend.position = "top"
      )

    return(arrow)
}

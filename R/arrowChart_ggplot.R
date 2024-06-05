#' Helper function for Arrow Chart for single items
#'
#' [arrowChart_ggplot()] creates an pre-post arrow chart of averages and returns a ggplot object.
#'
#' @param df_gg Required, a [tibble][tibble::tibble-package] or data frame of **numeric** data that has items with the prefix of `pre_` and `post_`; and has a categorical group
#'  variable to split up the data (e.g. role, gender, education level, etc.).
#'
#' @param fill_gg Passed character vector of color codes from main function, that corresponds to the colors for each scale label and bar in the chart.
#'
#' @param scale_labels_gg Required, a character vector of labels for the response scale, must be in the desired order,
#'    e.g. if you have a 5 item scale of minimal to extensive it should look like this: `levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")`.
#'
#' @param font_family Character value to set the font family for all text in the chart, defaults to "Arial".
#'
#' @param font_size Numeric value to set the font size in points for all text in the chart, defaults to size 10.
#'
#' @return A [ggplot2][ggplot2::ggplot2-package] object that plots the items into a arrow bar chart.
#' @noRd
arrowChart_ggplot <- function(df_gg, fill_gg, scale_labels_gg, font_family = "Arial", font_size = 10 ) {
        # Load fonts:
        extrafont::loadfonts("all", quiet = TRUE)
        . <- NULL # to stop check() from bringing up .

        arrow <- {{ df_gg }} %>%
            ggplot2::ggplot(ggplot2::aes(
                x = .data[["score_avg"]], y = forcats::fct_rev(.data[["question"]]), color = .data[["question"]],
                label = scales::number(.data[["score_avg"]], accuracy = 0.01), group = .data[["question"]]
            )) +
            ggplot2::geom_line(
                lineend = "round", linejoin = "round", linewidth = 2.5,
                arrow = grid::arrow(type = "closed", length = ggplot2::unit(0.2, "inches"))
            ) +
            ggplot2::geom_text(
                data = dplyr::filter(df_gg, .data[["timing"]] == "pre"), nudge_x = -0.075, hjust = 1, show.legend = FALSE,
                family = font_family, size = font_size, size.unit = "pt"
            ) +
            ggplot2::geom_text(
                data = dplyr::filter(df_gg, .data[["timing"]] == "post"), nudge_x = 0.075, hjust = 0, show.legend = FALSE,
                family = font_family, size = font_size, size.unit = "pt"
            ) +
            ggplot2::scale_color_manual(values = fill_gg) +
            ggplot2::scale_x_continuous(limits = c(1, length(scale_labels_gg)), labels = scale_labels_gg) +
            ggplot2::labs(tag = NULL, color = NULL) +
            ggplot2::theme_void(base_family = font_family, base_size = font_size) +
            ggplot2::theme(
                axis.text.x = ggtext::element_markdown(
                    color = "black", size = font_size, family = font_family,
                    margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
                ),
                axis.text.y = ggtext::element_markdown(
                    angle = 0, hjust = 1, color = "black", size = font_size, family = font_family,
                    margin = ggplot2::margin(t = 5, r = -15, b = 5, l = 0, unit = "pt")
                ),
                plot.margin = ggplot2::margin(t = 5, r = 25, b = 5, l = 5, unit = "pt"),
                legend.position = "none" # no legend
            )

        return(arrow)

}

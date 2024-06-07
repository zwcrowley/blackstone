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
arrowChart_ggplot <- function(df_gg, fill_gg, scale_labels_gg, font_family = "Arial", font_size = 10) {
        # Load fonts:
        extrafont::loadfonts("all", quiet = TRUE)
        . <- NULL # to stop check() from bringing up .
        # Calculate the nudge_x and hjust for the geom_text()
        # Pre
        pre_diff_score_avg <- arrow_df %>% dplyr::filter(.data[["timing"]] == "pre") %>% dplyr::select(.data[["diff_score_avg"]]) %>% tibble::deframe()
        nudge_x_pre <- dplyr::if_else(pre_diff_score_avg > 0, -0.075, 0.075)
        hjust_pre <- dplyr::if_else(pre_diff_score_avg > 0, 1, 0)
        # Post
        post_diff_score_avg <- arrow_df %>% dplyr::filter(.data[["timing"]] == "post") %>% dplyr::select(.data[["diff_score_avg"]]) %>% tibble::deframe()
        nudge_x_post <- dplyr::if_else(pre_diff_score_avg > 0, 0.075, -0.075)
        hjust_post <- dplyr::if_else(pre_diff_score_avg > 0, 0, 1)

        arrow <- {{ df_gg }} %>%
            ggplot2::ggplot(ggplot2::aes(
                x = .data[["score_avg"]], y = forcats::fct_rev(.data[["question"]]), color = .data[["question"]],
                label = scales::number(.data[["score_avg"]], accuracy = 0.01), group = .data[["question"]]
            )) +
            ggplot2::geom_path(
                lineend = "round", linejoin = "round", linewidth = 1.5,
                arrow = grid::arrow(type = "closed", length = ggplot2::unit(0.15, "inches"))
            ) +
            ggplot2::geom_text(
                data = dplyr::filter(df_gg, .data[["timing"]] == "pre"), show.legend = FALSE,
                nudge_x = nudge_x_pre, hjust = hjust_pre, # calculated above.
                family = font_family, size = font_size, size.unit = "pt"
            ) +
            ggplot2::geom_text(
                data = dplyr::filter(df_gg, .data[["timing"]] == "post"), show.legend = FALSE,
                nudge_x = nudge_x_post, hjust = hjust_post, # calculated above.
                family = font_family, size = font_size, size.unit = "pt"
            ) +
            ggplot2::scale_color_manual(values = fill_gg) +
            ggplot2::scale_x_continuous(limits = c(0.75, length(scale_labels_gg)),
                                        breaks = c(1:length(scale_labels_gg)),
                                        labels = scale_labels_gg) +
            ggplot2::theme_void(base_family = font_family, base_size = font_size) +
            ggplot2::theme(
                legend.position = "none", # no legend
                axis.text.x = ggplot2::element_text( # x axis response labels on bottom scale
                    color = "black", size = font_size, family = font_family,
                    margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
                ),
                axis.text.y = ggtext::element_markdown(
                    angle = 0, hjust = 1, color = "black", size = font_size, family = font_family,
                    margin = ggplot2::margin(t = 5, r = -15, b = 5, l = 5, unit = "pt")
                ),
                plot.margin = ggplot2::margin(t = 5, r = 20, b = 5, l = 5, unit = "pt")
            )

        return(arrow)

}

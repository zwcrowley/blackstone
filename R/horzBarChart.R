#' Horizontal Bar Chart for Blackstone Research and Evaluation
#'
#' [horzBarChart()] creates a horizontal bar chart and returns a ggplot object with Blackstone Research and Evaluation branding.
#'
#' @param df Required, a [tibble][tibble::tibble-package]/data frame that has been pre-processed with dataSumm().
#'
#' @param scale_colors Required, a character vector of the colors for the scale items.
#'
#' @param width Input a value between 0.3 and 0.8 to set the thickness of the bars. Default is NULL.
#'
#' @return A [ggplot2][ggplot2::ggplot2-package] object that plots the items into a horizontal bar chart and can be exported.
#' @export
#'
#' @examples
#' data <- dplyr::tibble(
#'   role = c(
#'     "Faculty", "Postdoc", "Undergraduate student", "Graduate student",
#'     "Graduate student", "Postdoc", "Postdoc", "Faculty",
#'     "Faculty", "Graduate student", "Graduate student", "Postdoc",
#'     "Faculty", "Faculty", "Faculty", "Faculty", "Faculty", "Graduate student",
#'     "Undergraduate student", "Undergraduate student"
#'   )
#' )
#'
#' role_summ <- data %>%
#'   dplyr::select(role) %>%
#'   bre::dataSumm()
#'
#' role_color <- c("#2C2C4F", "#4B9FA6", "#79AB53", "#767171")
#'
#' horzBarChart(df = role_summ, scale_colors = role_color, width = 0.6)
horzBarChart <- function(df, scale_colors, width = NULL) {
    extrafont::loadfonts("all", quiet = TRUE)
    # Create a font family character var so that it is easy to change, could also be a new arg:
    font_family <- c("Arial")

    bar_chart <- {{ df }} %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$n_answers, y = forcats::fct_rev(.data$response),
        label = .data$percent_answers_label, fill = .data$response
      )) +
      ggplot2::geom_col(width = width, color = "black") +
      ggplot2::scale_fill_manual(values = scale_colors) +
      ggrepel::geom_text_repel(
        nudge_x = .1, fontface = "bold", size = 4,
        min.segment.length = Inf
      ) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.025, 0.2))) +
      ggplot2::theme_void(base_family = font_family, base_size = 12) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.y = ggplot2::element_text(
          angle = 0, hjust = 1, color = "black",
          margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
        ),
        axis.text.x = ggplot2::element_text(
          angle = 0, hjust = 1, color = "#767171",
          margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
        ),
        plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
      )

    return(bar_chart)
}

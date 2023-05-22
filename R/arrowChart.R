#' Arrow Chart for The Mark USA, Inc.
#'
#' `arrowChart()` creates an pre-post arrow chart of group averages and returns
#' a ggplot object.
#'
#' @param df A tibble or data frame of numeric data that has a categorical group
#'   variable to split up the data, e.g. role, gender, education level, etc.
#'
#' @param scale_labels character vector of labels for the x-axis, usually a
#'   response to a set of likert items, needs to match the number of response
#'   items in the data.
#'
#' @param group_colors character vector of hex codes for colors to associate
#'   each group to, e.g. this data has two groups and this function creates an
#'   overall group so this function will need a 'group_colors' character vector of
#'   three colors. 'group_colors' need to be in the order you want them associated to
#'   the group based on the factor levels for the group variable, last color
#'   will be the overall group of "all".
#'
#' @return A ggplot object that plots the items into a arrow bar chart.
#'
#' @examples
#' items <- dplyr::tibble(
#'   Pre_Organization = c(1, 2, 3, 4, 5, 4, 3, 2, 1),
#'   Post_Organization = dplyr::if_else(Pre_Organization < 5, Pre_Organization + 1, Pre_Organization),
#'   Pre_Source = c(2, 2, 3, 5, 4, 3, 2, 1, 2),
#'   Post_Source = dplyr::if_else(Pre_Source < 4, Pre_Source + 2, Pre_Source),
#'   Pre_Publish = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   Post_Publish = Pre_Publish + 2,
#'   Pre_Write = c(2, 2, 2, 3, 3, 3, 4, 4, 4),
#'   Post_Write = Pre_Write + 1,
#'   Pre_Research = c(1, 1, 2, 2, 3, 3, 4, 4, 4),
#'   Post_Research = Pre_Research + 1,
#'   group = c(
#'     "grad", "grad", "grad", "grad",
#'     "undergrad", "undergrad", "undergrad", "undergrad", "undergrad"
#'   )
#' )
#' levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")
#' items <- items %>% dplyr::mutate(
#'   group = factor(group, levels = c("grad", "undergrad"))
#' )
#' threeScale_theMark_colors <- c("#79AB53", "#4B9FA6", "#2C2C4F")
#' arrowChart(df = items, scale_labels = levels_min_ext, group_colors = threeScale_theMark_colors)
#' @export
arrowChart <- function(df, scale_labels, group_colors) {
  extrafont::loadfonts("all", quiet = TRUE)

  N_df <- {{ df }} %>% nrow()

  arrow_df_group <- {{ df }} %>%
    dplyr::group_by(.data$group) %>%
    tidyr::pivot_longer(-"group", names_to = "question", values_to = "response") %>%
    dplyr::mutate(group = factor(.data$group)) %>%
    tidyr::separate(.data$question, into = c("timing", "question"), sep = "_") %>%
    dplyr::group_by(.data$group, .data$question, .data$timing) %>%
    dplyr::mutate(timing = factor(.data$timing, levels = c("Pre", "Post"))) %>%
    dplyr::summarize(score_avg = mean(.data$response, na.rm = TRUE), .groups = "keep") %>%
    dplyr::ungroup()

  arrow_df_all <- {{ df }} %>%
    dplyr::select(!"group") %>%
    tidyr::pivot_longer(tidyselect::everything(), names_to = "question", values_to = "response") %>%
    tidyr::separate(.data$question, into = c("timing", "question"), sep = "_") %>%
    dplyr::group_by(.data$question, .data$timing) %>%
    dplyr::mutate(timing = factor(.data$timing, levels = c("Pre", "Post"))) %>%
    dplyr::summarize(score_avg = mean(.data$response, na.rm = TRUE), .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(group = "all")

  arrow_df <- dplyr::full_join(arrow_df_group, arrow_df_all, by = dplyr::join_by("group", "question", "timing", "score_avg"))

  arrow_df <- arrow_df %>%
    dplyr::group_by(.data$group, .data$question, .data$timing) %>%
    dplyr::mutate(
      question = factor(.data$question, levels = .data$question),
      group = forcats::fct_rev(factor(.data$group))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(.data$timing), dplyr::desc(.data$score_avg)) %>%
    dplyr::mutate(question = forcats::fct_reorder2(.data$question, .data$timing, .data$score_avg, .desc = TRUE))

  arrow <- arrow_df %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$score_avg, y = forcats::fct_rev(.data$group), color = forcats::fct_rev(.data$group),
      label = scales::number(.data$score_avg, accuracy = 0.01), group = .data$group
    )) +
    ggplot2::geom_line(
      lineend = "round", linejoin = "round", linewidth = 1,
      arrow = grid::arrow(type = "closed", length = ggplot2::unit(0.1, "inches"))
    ) +
    ggplot2::geom_text(
      data = dplyr::filter(arrow_df, .data$timing == "Pre"), nudge_x = -0.075, hjust = 1, show.legend = FALSE,
      family = "Gill Sans MT", size = 3.5
    ) +
    ggplot2::geom_text(
      data = dplyr::filter(arrow_df, .data$timing == "Post"), nudge_x = 0.075, hjust = 0, show.legend = FALSE,
      family = "Gill Sans MT", size = 3.5
    ) +
    ggplot2::facet_wrap(~question, ncol = 1, strip.position = "left") +
    ggplot2::scale_color_manual(values = group_colors, labels = function(group) stringr::str_to_title(group)) +
    ggplot2::scale_x_continuous(limits = c(1, 5), labels = scale_labels) +
    ggplot2::labs(tag = parse(text = paste0("(", expression(italic(n)), "==", N_df, ")")), color = NULL) +
    ggplot2::theme_void(base_family = "Gill Sans MT", base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        color = "#767171", size = 12, family = "Gill Sans MT",
        margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
      ),
      strip.text.y.left = ggplot2::element_text(
        angle = 0, hjust = 1, color = "black", size = 12, family = "Gill Sans MT",
        margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 0, unit = "pt")
      ),
      plot.margin = ggplot2::margin(t = 5, r = 25, b = 5, l = 5, unit = "pt"),
      legend.position = "top"
    )

  return(arrow)
}

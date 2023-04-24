#' Stacked Bar Chart for The Mark USA, Inc.
#'
#' @param df A tibble/data frame of survey items that are categorical/character
#'   variables, in 5 point scales and pre-post, that will be inserted into a
#'   stacked bar chart with The Mark USA branding.
#'
#' @param set_5_levels character vector of 5 levels to set the scale for the
#'   plot
#'
#' @return A ggplot object that plots the items into a stacked bar chart and can
#'   be exported.
#' @export
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
#'   Post_Research = Pre_Research + 1
#' )
#' levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")
#'
#' # Recode the numeric to factor variables using the levels from levels_min_ext:
#' cat_items<- TheMarkUSA::recodeFiveCat(items, levels_min_ext)
#'
#' # Select the factor variables:
#' cat_items <- cat_items %>% dplyr::select(dplyr::where(is.factor))
#'
#' # Pass the factor variables and the levels to 'stackedBarChart()':
#' stackedBarChart(cat_items, levels_min_ext)
stackedBarChart <- function(df, set_5_levels) {
  extrafont::loadfonts(quiet = TRUE)

  fiveScale_theMark_colors <- c("#767171", "#FFE699", "#79AB53", "#4B9FA6", "#2C2C4F")

  new_df <- {{ df }} %>%
    tidyr::pivot_longer(tidyselect::everything(), names_to = "question", values_to = "response") %>%
    dplyr::mutate(question = stringr::str_remove(.data$question, "cat_")) %>%
    tidyr::separate(.data$question, into = c("timing", "question"), sep = "_") %>%
    dplyr::group_by(.data$question, .data$timing, .data$response) %>%
    dplyr::summarize(n_answers = dplyr::n(), .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$question, .data$timing) %>%
    dplyr::mutate(
      percent_answers = .data$n_answers / sum(.data$n_answers),
      percent_answers_label = scales::percent(.data$percent_answers, accuracy = 1),
      label_color = dplyr::if_else(.data$response == levels(.data$response)[2], "black", "white"),
      pos_valence_post = dplyr::case_when(
        .data$response == levels(.data$response)[4] & .data$timing == "Post" ~ percent_answers,
        .data$response == levels(.data$response)[5] & timing == "Post" ~ percent_answers,
        TRUE ~ 0
      ),
      response = forcats::fct_relevel(.data$response, set_5_levels),
      timing = factor(.data$timing, levels = c("Pre", "Post"))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$response)

  question_order <- new_df %>%
    dplyr::group_by(.data$question, .data$timing) %>%
    dplyr::summarize(n_pos_valence_post = sum(.data$pos_valence_post), .groups = "keep") %>%
    dplyr::arrange(dplyr::desc(.data$n_pos_valence_post)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$timing == "Post") %>%
    dplyr::select("question") %>%
    dplyr::mutate(question = as.character(.data$question)) %>%
    unlist()

  new_df <- new_df %>% dplyr::mutate(question = forcats::fct_relevel(.data$question, question_order))

  N_df <- {{ df }} %>% nrow()

  width <- dplyr::if_else(dplyr::n_distinct(new_df$question) < 4, 0.5,
    dplyr::if_else(dplyr::n_distinct(new_df$question) < 7, 0.75, 0.95)
  )

  stacked_bar_chart <- new_df %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$percent_answers, y = forcats::fct_rev(.data$timing), fill = .data$response,
      label = .data$n_answers, group = .data$question
    )) +
    ggplot2::geom_col(width = width, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(ggplot2::aes(color = .data$label_color),
      family = "Gill Sans MT",
      fontface = "bold", position = ggplot2::position_stack(vjust = .5, reverse = TRUE), size = 3
    ) +
    ggplot2::scale_color_manual(values = c("black", "white")) +
    ggplot2::facet_wrap(~question, ncol = 1, strip.position = "left") +
    ggplot2::scale_fill_manual(values = fiveScale_theMark_colors, drop = FALSE, labels = function(response) stringr::str_wrap(response, width = 10)) +
    ggplot2::guides(color = "none", fill = ggh4x::guide_stringlegend(
      size = 12, family = "Gill Sans MT", face = "bold", hjust = 0, vjust = 0, ncol = 5,
      spacing.x = 14, spacing.y = 0
    )) +
    ggplot2::labs(title = NULL, fill = NULL, y = NULL, x = NULL, tag = paste("N=", N_df, sep = "")) +
    ggplot2::theme_void(base_family = "Gill Sans MT", base_size = 12) +
    ggplot2::theme(
      strip.placement = "outside",
      axis.text.y = ggplot2::element_text(
        angle = 0, hjust = 1, color = "black", size = 10, family = "Gill Sans MT",
        margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
      ),
      strip.text.y.left = ggplot2::element_text(
        angle = 0, hjust = 1, color = "black", size = 12, family = "Gill Sans MT",
        margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 0, unit = "pt")
      ),
      plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
      legend.position = "top"
    )

  return(stacked_bar_chart)
}

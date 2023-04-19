#' Diverging and Stacked Bar Chart for The Mark
#'
#' @param df A tibble/data frame of survey items that are categorical/character variables, in 5 point scales and pre-post, that will be inserted into a stacked bar chart with The Mark USA branding.
#'
#' @param set_5_levels character vector of 5 levels to set the scale for the plot
#'
#' @return A ggplot object that plots the items into a diverging and stacked bar chart as a ggplot object. The chart is sorted by the highest positive valence, post items at the top by the post.
#' @export
#'
#' @examples
#' items <- dplyr::tibble(
#'   cat_Pre_Sources =
#'     c("Good", "Moderate", "Minimal", "Slight", "Slight", "Moderate", "Good"),
#'   cat_Post_Sources =
#'     c("Good", "Good", "Minimal", "Moderate", "Moderate", "Good", "Extensive"),
#'   cat_Pre_Orgs =
#'     c("Good", "Moderate", "Minimal", "Slight", "Slight", "Moderate", "Moderate"),
#'   cat_Post_Orgs =
#'     c("Good", "Moderate", "Minimal", "Moderate", "Moderate", "Good", "Extensive")
#' )
#' levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")
#' divBarChart(items, levels_min_ext)
divBarChart <- function(df, set_5_levels) {
  extrafont::loadfonts(quiet = TRUE)

  fiveScale_theMark_colors <- c("#767171", "#FFE699", "#79AB53", "#4B9FA6", "#2C2C4F")

  new_df <- {{ df }} %>%
    tidyr::pivot_longer(tidyselect::everything(), names_to = "question", values_to = "response") %>%
    dplyr::mutate(question = stringr::str_remove(.data$question, "cat_")) %>%
    tidyr::separate(.data$question, into = c("timing", "question"), sep = "_") %>%
    dplyr::mutate(response = factor(.data$response, levels = set_5_levels)) %>%
    dplyr::group_by(.data$question, .data$timing, .data$response) %>%
    dplyr::summarize(n_answers = dplyr::n(), .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$question, .data$timing) %>%
    dplyr::mutate(
      percent_answers = .data$n_answers / sum(.data$n_answers),
      percent_answers = dplyr::case_when(
        .data$response == levels(.data$response)[4] ~ percent_answers,
        .data$response == levels(.data$response)[5] ~ percent_answers,
        TRUE ~ -percent_answers
      ),
      percent_answers_label = scales::percent(abs(.data$percent_answers), accuracy = 1),
      label_color = dplyr::if_else(.data$response == levels(.data$response)[2], "black", "white"),
      pos_valence_post = dplyr::case_when(.data$response == levels(.data$response)[4] & .data$timing == "Post" ~ n_answers,
                                          .data$response == levels(.data$response)[5] & timing == "Post" ~ n_answers,
                                                                                                    TRUE ~ 0),
      response = forcats::fct_relevel(.data$response, c(
        levels(.data$response)[3], levels(.data$response)[2], levels(.data$response)[1],
        levels(.data$response)[4], levels(.data$response)[5]
      )),
      timing = factor(.data$timing, levels = c("Pre", "Post"))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$response)

  question_order <- new_df %>% dplyr::group_by(.data$question, .data$timing) %>%
    dplyr::summarize(n_pos_valence_post = sum(.data$pos_valence_post), .groups = "keep") %>%
    dplyr::arrange(dplyr::desc(.data$n_pos_valence_post))  %>% dplyr::ungroup() %>% dplyr::filter(.data$timing == "Post") %>%
    dplyr::select(.data$question) %>% dplyr::mutate(question = as.character(.data$question)) %>% unlist()

  new_df <-  new_df %>% dplyr::mutate(question = forcats::fct_relevel(.data$question, question_order))

  N_df <- {{ df }} %>% nrow()

  width <- dplyr::if_else(dplyr::n_distinct(new_df$question) < 4, 0.3, 0.75)

  diverging_bar_chart <- new_df %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$percent_answers, y = forcats::fct_rev(.data$timing), fill = .data$response,
      label = .data$n_answers, group = .data$question
    )) +
    ggplot2::geom_col(width = width, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(ggplot2::aes(color = .data$label_color),
      family = "Gill Sans MT", fontface = "bold",
      position = ggplot2::position_stack(vjust = .5, reverse = T), size = 3
    ) +
    ggplot2::scale_color_manual(values = c("black", "white")) +
    ggplot2::facet_wrap(~question, ncol = 1, strip.position = "left") +
    ggplot2::scale_fill_manual(
      breaks = set_5_levels, values = fiveScale_theMark_colors, drop = FALSE,
      labels = function(response) stringr::str_wrap(response, width = 10)
    ) +
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
      panel.spacing.y = ggplot2::unit(10, "pt"),
      plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
      legend.position = "top"
    )

  return(diverging_bar_chart)
}

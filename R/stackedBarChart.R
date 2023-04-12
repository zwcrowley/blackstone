#' Stacked Bar Chart for The Mark
#'
#' @param df A tibble/data frame of survey items that are categorical/character variables, in 5 point scales and pre-post, that will be inserted into a stacked bar chart with The Mark USA branding.
#'
#' @param set_5_levels character vector of 5 levels to set the scale for the plot
#'
#' @return A ggplot object that plots the items into a stacked bar chart and can be exported.
#' @export
#'
#' @examples
#' items <- dplyr::tibble(
#' cat_Pre_Sources =
#' c("Good","Moderate","Minimal","Slight","Slight","Moderate","Good"),
#' cat_Post_Sources =
#' c("Good","Good","Minimal","Moderate","Moderate","Good","Extensive"),
#' cat_Pre_Orgs =
#' c("Good","Moderate","Minimal","Slight","Slight","Moderate","Moderate"),
#' cat_Post_Orgs =
#' c("Good","Moderate","Minimal","Moderate","Moderate","Good","Extensive"))
#' levels_min_ext <- c('Minimal', 'Slight', 'Moderate', 'Good', 'Extensive')
#' stackedBarChart(items,levels_min_ext)
stackedBarChart <- function(df, set_5_levels) {
    extrafont::loadfonts(quiet = TRUE)

    fiveScale_theMark_colors <-  c("#767171", "#FFE699", "#79AB53", "#4B9FA6", "#2C2C4F")

    texts <- list(ggplot2::element_text(size = 14, family = "Gill Sans MT", hjust = 1),
                  ggplot2::element_text(size = 11, family = "Gill Sans MT",  hjust = 0.6))

    new_df <-  {{df}} %>%
        tidyr::pivot_longer(tidyselect::everything(), names_to = "question", values_to = "response") %>%
        dplyr::mutate(question = stringr::str_remove(.data$question, "cat_")) %>%
        tidyr::separate(.data$question, into = c("timing", "question"), sep = "_") %>%
        dplyr::group_by(.data$question, .data$timing, .data$response) %>%
        dplyr::count(name = "n_answers") %>%
        dplyr::ungroup() %>%
        dplyr::group_by(.data$question, .data$timing) %>%
        dplyr::mutate(percent_answers = .data$n_answers / sum(.data$n_answers)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(percent_answers_label = scales::percent(.data$percent_answers, accuracy = 1),
                      timing = factor(.data$timing, levels = c("Pre","Post")),
                      response = factor(.data$response, levels = set_5_levels),
                      label_color = dplyr::if_else(.data$response == levels(.data$response)[2], "black", "white"))

    N_df <- new_df %>% dplyr::select(-c(.data$percent_answers,.data$percent_answers_label)) %>%
        tidyr::pivot_wider(names_from = c(.data$question,.data$timing), values_from = c(.data$n_answers)) %>%
        dplyr::summarise(dplyr::across(tidyselect::where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% max()

    stacked_bar_chart <- new_df %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$question, y = .data$percent_answers, fill = .data$response, label = .data$n_answers)) +
        ggplot2::geom_col(width = 3, position = ggplot2::position_stack(reverse = TRUE)) +
        ggplot2::geom_text(ggplot2::aes(y = .data$percent_answers, label = .data$n_answers, group = .data$response, color = .data$label_color),
                           family = "Gill Sans MT", fontface = "bold",
                  position = ggplot2::position_stack(vjust = .5,reverse = T), size = 3) +
        ggplot2::scale_color_manual(values = c('black','white')) +
        ggplot2::coord_flip() +
        ggh4x::facet_nested_wrap(dplyr::vars(.data$question,.data$timing), ncol = 1, scales = "free", strip.position = "left",
                          strip = ggh4x::strip_nested(text_y = texts, by_layer_y = TRUE)) +
        ggplot2::scale_fill_manual(values = fiveScale_theMark_colors, drop = FALSE, labels = function(response) stringr::str_wrap(response, width = 10)) +
        ggplot2::guides(color = "none",fill = ggh4x::guide_stringlegend(size = 12, family = "Gill Sans MT", face = "bold", hjust = 0, vjust = 0, ncol = 5,
                                                        spacing.x = 14, spacing.y = 0)) +
        ggplot2::labs(title = NULL, fill = NULL, y = NULL, x = NULL, tag = paste("N=",N_df,sep = "")) +
        ggplot2::scale_y_discrete(expand = c(0,0)) +
        ggh4x::force_panelsizes(cols = c(10, 10), rows = c(.5, .5), respect = TRUE) +
        ggplot2::labs(title = NULL, fill = NULL, y = NULL, x = NULL, tag = paste("N=",N_df,sep = "")) +
        ggplot2::theme_void(base_family = "Gill Sans MT", base_size = 12) +
        ggplot2::theme(plot.margin = ggplot2::margin(.5, .5, .5, 0, "cm"),
              panel.spacing.y = ggplot2::unit(.1, "inches"),
              legend.position = "top")

    return(stacked_bar_chart)

}

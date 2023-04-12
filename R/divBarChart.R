#' Divering and Stacked Bar Chart for The Mark
#'
#' @param df A tibble/data frame of survey items that are categorical/character variables, in 5 point scales and pre-post, that will be inserted into a stacked bar chart with The Mark USA branding.
#'
#' @param set_5_levels character vector of 5 levels to set the scale for the plot
#'
#' @return A ggplot object that plots the items into a diverging and stacked bar chart and can be exported.
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
#' divBarChart(items,levels_min_ext)
divBarChart <- function(df, set_5_levels) {
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
        dplyr::mutate(response = factor(.data$response, levels = set_5_levels),
                percent_answers = dplyr::case_when(.data$response == levels(.data$response)[4] ~ percent_answers,
                                                         .data$response == levels(.data$response)[5] ~ percent_answers,
                                                                                                TRUE ~ -percent_answers))  %>%
        dplyr::mutate(percent_answers_label = scales::percent(abs(.data$percent_answers), accuracy = 1),
               timing = factor(.data$timing, levels = c("Pre","Post")),
               label_color = dplyr::if_else(.data$response == levels(.data$response)[2], "black", "white"))

    N_df <- new_df %>% dplyr::select(-c(.data$percent_answers,.data$percent_answers_label)) %>%
        tidyr::pivot_wider(names_from = c(.data$question,.data$timing), values_from = c(.data$n_answers)) %>%
        dplyr::summarise(dplyr::across(tidyselect::where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% max()


    df_pos <- dplyr::filter(new_df, .data$response %in% c(levels(.data$response)[4], levels(.data$response)[5]))
    df_neg <- dplyr::filter(new_df, .data$response %in% c(levels(.data$response)[1], levels(.data$response)[2],levels(.data$response)[3]))

    diverging_bar_chart <- ggplot2::ggplot() +
        ggplot2::geom_col(data = df_pos, ggplot2::aes(x = .data$question, y = .data$percent_answers, fill = .data$response),
                          width = 4, position = ggplot2::position_stack(reverse = TRUE)) +
        ggplot2::geom_text(data = df_pos, ggplot2::aes(x = .data$question, y = .data$percent_answers,
                                                       label = .data$n_answers, group = .data$response, color = .data$label_color),
                  family = "Gill Sans MT", fontface = "bold", position = ggplot2::position_stack(vjust = .5,reverse = T), size = 3) +
        ggplot2::geom_col(data = df_neg, ggplot2::aes(x = .data$question, y = .data$percent_answers, fill = .data$response),
                          width = 4, position = ggplot2::position_stack()) +
        ggplot2::geom_text(data = df_neg, ggplot2::aes(x = .data$question, y = .data$percent_answers,
                                                       label = .data$n_answers, group = .data$response, color = .data$label_color),
                  family = "Gill Sans MT", fontface = "bold", position = ggplot2::position_stack(vjust = .5,reverse = F), size = 3) +
        ggplot2::scale_color_manual(values = c('black','white')) +
        ggplot2::coord_flip() +
        ggh4x::facet_nested(dplyr::vars(.data$question, .data$timing), scales = "free_y", switch = "y", space = "free_y",
                     strip = ggh4x::strip_nested(text_y = texts,by_layer_y = TRUE)) +
        ggplot2::guides(color = "none",fill = ggh4x::guide_stringlegend(size = 12, family = "Gill Sans MT", face = "bold", hjust = 0, vjust = 0, ncol = 5,
                                                        spacing.x = 14, spacing.y = 0)) +
        ggplot2::scale_fill_manual(values = fiveScale_theMark_colors, drop = FALSE, labels = function(response) stringr::str_wrap(response, width = 10)) +
        ggplot2::scale_y_discrete(expand = c(0,0)) +
        ggh4x::force_panelsizes(cols = c(8, 8), rows = c(.45, .45) , respect = TRUE) +
        ggplot2::labs(title = NULL, fill = NULL, y = NULL, x = NULL, tag = paste("N=",N_df,sep = "")) +
        ggplot2::theme_void(base_family = "Gill Sans MT", base_size = 12) +
        ggplot2::theme(plot.margin = ggplot2::margin(.5, .5, .5, 0, "cm"),
              panel.spacing.y = ggplot2::unit(.1, "inches"),
              legend.position = "top")

    return(diverging_bar_chart)

}

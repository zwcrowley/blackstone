#' Stacked Bar Chart for The Mark USA
#'
#' `stackedBarChart()` creates a stacked bar chart and returns a ggplot object with The Mark USA branding.
#'
#' @param df Required, A [tibble][tibble::tibble-package]/data frame of survey items that are categorical/character
#'   variables, in 3 to 7 point scales, that will be inserted into a stacked bar chart with The Mark USA branding.
#'
#' @param scale_labels Required, a character vector of labels for the response scale, must be in the desired order,
#'    e.g. if you have a 5 item scale of minimal to extensive it should look like this: `levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")`.
#'    This argument accepts a character vector of 3 to 7 items.
#'
#' @param pre_post Logical, default is FALSE. If true, returns a pre-post stacked bar chart.
#'
#' @param overall_n Logical, default is FALSE. If TRUE, returns an overall *n* for all questions that is in the upper left tag of the plot.
#'    If False, adds *n* to each question/item after the respective labels.
#'
#' @param percent_label Logical, default is TRUE. If FALSE, labels the bars with the number of answers per response.
#'
#' @param question_labels Default is NULL. Takes in a named character vector to both supply labels the questions and sort the order of the questions.
#'    The named character vector should have the new labels as the "name" and the old labels as the "variable" sorted in the
#'    desired order of appearing in the plot, first item will appear at the top of the plot. See examples.
#'
#' @param question_order Logical, default is FALSE. If TRUE, the question order will be taken from the user supplied named character vector passed to
#'    question_labels, where the first item will be at the top of the plot and so on. If FALSE, the question order will be the questions with highest
#'    positive valenced response options on the top of the plot descending.
#'
#' @param width Input a value between 0.3 and 0.8 to set the thickness of the bars. Default is NULL.
#'
#' @return A [ggplot2][ggplot2::ggplot2-package] object that plots the items into a stacked bar chart and can be exported.
#' @export
#'
#' @examples
#' items <- dplyr::tibble(
#'   pre_Organization = c(1, 2, 3, 4, 5, 4, 3, 2, 1),
#'   post_Organization = dplyr::if_else(pre_Organization < 5, pre_Organization + 1, pre_Organization),
#'   pre_Source = c(2, 2, 3, 5, 4, 3, 2, 1, 2),
#'   post_Source = dplyr::if_else(pre_Source < 4, pre_Source + 2, pre_Source),
#'   pre_Publish = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   post_Publish = pre_Publish + 2,
#'   pre_Write = c(2, 2, 2, 3, 3, 3, 4, 4, 4),
#'   post_Write = pre_Write + 1,
#'   pre_Research = c(1, 1, 2, 2, 3, 3, 4, 4, 4),
#'   post_Research = pre_Research + 1
#' )
#'
#' items_single <- dplyr::tibble(
#'   Organization = c(1, 2, 3, 4, 5, 4, 3, 2, 1),
#'   Source = c(2, 2, 3, 5, 4, 3, 2, 1, 2),
#'   Publish = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   Write = c(2, 2, 2, 3, 3, 3, 4, 4, 4),
#'   Research = c(1, 1, 2, 2, 3, 3, 4, 4, 4),
#' )
#'
#' # Set scale_labels:
#' levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")
#'
#' # Question labels as a named vector with the naming structure
#' # like this: c("new label" = "original variable name"):
#' question_labels <- c("Publish a lot of high quality papers" =  "Publish",
#'                      "Write a lot of research papers" = "Write",
#'                      "Research in a lab with faculty" = "Research",
#'                      "Organization of a large research project" = "Organization",
#'                      "Source work for a research paper" = "Source")
#'
#' # Recode the numeric to factor variables using the levels from levels_min_ext:
#' cat_items <- TheMarkUSA::recodeCat(items, levels_min_ext)
#' cat_items_single <- TheMarkUSA::recodeCat(items_single, levels_min_ext)
#'
#' # Select the factor variables:
#' cat_items <- cat_items %>% dplyr::select(dplyr::where(is.factor))
#' cat_items_single <- cat_items_single %>% dplyr::select(dplyr::where(is.factor))
#'
#' # Pass the factor variables and the levels to stackedBarChart:
#' stackedBarChart(
#'   df = cat_items, pre_post = TRUE, scale_labels = levels_min_ext,
#'   question_labels= NULL, percent_label = TRUE, width = NULL
#' )
#' stackedBarChart(
#'   df = cat_items_single, pre_post = FALSE, scale_labels = levels_min_ext,
#'   question_labels= NULL, percent_label = TRUE, width = NULL
#' )
#' stackedBarChart(
#'   df = cat_items, pre_post = TRUE, scale_labels = levels_min_ext,
#'   question_labels= question_labels, question_order = FALSE, percent_label = TRUE, width = NULL
#' )
#' stackedBarChart(
#'   df = cat_items_single, pre_post = FALSE, scale_labels = levels_min_ext,
#'   question_labels= question_labels, question_order = FALSE, percent_label = TRUE, width = NULL
#' )
stackedBarChart <- function(df, scale_labels, pre_post = FALSE, overall_n = FALSE, percent_label = TRUE, question_labels = NULL, question_order= FALSE, width = NULL) {
  extrafont::loadfonts("all", quiet = TRUE)

  . <- NULL

  # Changes scale_labels to tibble pulls out index and saves that as a vector, gets number of levels from scale_labels:
  number_levels <- scale_labels %>%
    tibble::enframe() %>%
    dplyr::select("name") %>%
    tibble::deframe()

  if (isTRUE(pre_post)) {
    # Sets up new_df if pre_post is TRUE:
    new_df <- {{ df }} %>%
      tidyr::pivot_longer(tidyselect::everything(), names_to = "question", values_to = "response") %>%
      dplyr::mutate(question = stringr::str_remove(.data$question, "cat_")) %>%
      tidyr::separate(.data$question, into = c("timing", "question"), sep = "_") %>%
      dplyr::group_by(.data$question, .data$timing, .data$response) %>%
      dplyr::summarize(n_answers = dplyr::n(), .groups = "keep") %>%
      dplyr::ungroup() %>%
      tidyr::drop_na() %>%
      dplyr::group_by(.data$question, .data$timing) %>%
      dplyr::mutate(
        percent_answers = .data$n_answers / sum(.data$n_answers),
        percent_answers_label = scales::percent(.data$percent_answers, accuracy = 1),
        response = forcats::fct_relevel(.data$response, scale_labels),
        timing = factor(.data$timing, levels = c("pre", "post"))
      ) %>%
      dplyr::ungroup()

    # If the user supplies a named vector for questions labels:
    if (!is.null(question_labels)) {
      names(question_labels) <- names(question_labels) %>%
        stringr::str_wrap(., width = 30) %>%
        gsub("\n", "<br>", .)
      new_df <- new_df %>%
        dplyr::mutate(question = forcats::fct_recode(.data$question, !!!question_labels)) %>%
        dplyr::ungroup()
    }

    # Get total n for each question, grouped by question and timing:
    totals_new_df <- new_df %>%
      dplyr::group_by(.data$question, .data$timing) %>%
      dplyr::summarize(total = sum(.data$n_answers), .groups = "keep") %>%
      dplyr::ungroup()

    # IF/ELSE statement, first if number_levels equals 3, sets up the label_color and fill color:
    if (length(number_levels) == 3) {
      new_df <- new_df %>%
        dplyr::group_by(.data$question, .data$timing) %>%
        dplyr::mutate(
          label_color = "black",
          pos_valence_post = dplyr::case_when(
            .data$response == levels(.data$response)[3] & timing == "post" ~ percent_answers,
            TRUE ~ 0
          )
        ) %>%
        dplyr::ungroup()

      # 3 colors for chart:
      fill_colors <- c("#79AB53", "#4B9FA6", "#2C2C4F")

      # If number_levels) == 4
    } else if (length(number_levels) == 4) {
      new_df <- new_df %>%
        dplyr::group_by(.data$question, .data$timing) %>%
        dplyr::mutate(
          label_color = dplyr::if_else(.data$response == levels(.data$response)[1], "black", "white"),
          pos_valence_post = dplyr::case_when(
            .data$response == levels(.data$response)[3] & timing == "post" ~ percent_answers,
            .data$response == levels(.data$response)[4] & timing == "post" ~ percent_answers,
            TRUE ~ 0
          )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(.data$response)

      # 4 colors for chart:
      fill_colors <- c("#FFE699", "#79AB53", "#4B9FA6", "#2C2C4F")

      # If number_levels) == 5
    } else if (length(number_levels) == 5) {
      new_df <- new_df %>%
        dplyr::group_by(.data$question, .data$timing) %>%
        dplyr::mutate(
          label_color = dplyr::if_else(.data$response == levels(.data$response)[2], "black", "white"),
          pos_valence_post = dplyr::case_when(
            .data$response == levels(.data$response)[4] & timing == "post" ~ percent_answers,
            .data$response == levels(.data$response)[5] & timing == "post" ~ percent_answers,
            TRUE ~ 0
          )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(.data$response)

      # 5 colors for chart:
      fill_colors <- c("#767171", "#FFE699", "#79AB53", "#4B9FA6", "#2C2C4F")

      # If number_levels) == 6
    } else if (length(number_levels) == 6) {
      new_df <- new_df %>%
        dplyr::group_by(.data$question, .data$timing) %>%
        dplyr::mutate(
          label_color = dplyr::if_else(.data$response == levels(.data$response)[2], "black", "white"),
          pos_valence_post = dplyr::case_when(
            .data$response == levels(.data$response)[4] & timing == "post" ~ percent_answers,
            .data$response == levels(.data$response)[5] & timing == "post" ~ percent_answers,
            .data$response == levels(.data$response)[6] & timing == "post" ~ percent_answers,
            TRUE ~ 0
          )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(.data$response)

      # 6 colors for chart:
      fill_colors <- c("#767171", "#FFE699", "#79AB53", "#4B9FA6", "#2C2C4F", "gray")

      # If number_levels) == 7
    } else if (length(number_levels) == 7) {
      new_df <- new_df %>%
        dplyr::group_by(.data$question, .data$timing) %>%
        dplyr::mutate(
          label_color = dplyr::if_else(.data$response == levels(.data$response)[2], "black", "white"),
          pos_valence_post = dplyr::case_when(
            .data$response == levels(.data$response)[5] & timing == "post" ~ percent_answers,
            .data$response == levels(.data$response)[6] & timing == "post" ~ percent_answers,
            .data$response == levels(.data$response)[7] & timing == "post" ~ percent_answers,
            TRUE ~ 0
          )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(.data$response)

      # 7 colors for chart:
      fill_colors <- c("#767171", "#FFE699", "#79AB53", "#4B9FA6", "#37546d", "#2C2C4F", "gray")
    }

    # Set up a new question order if not supplied by the user after finding the most positive valenced items for post
    # (top levels depending on total response levels):
    if (isFALSE(question_order)) {
      question_order <- new_df %>%
        dplyr::group_by(.data$question, .data$timing) %>%
        dplyr::summarize(n_pos_valence_post = sum(.data$pos_valence_post), .groups = "keep") %>%
        dplyr::arrange(dplyr::desc(.data$n_pos_valence_post)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(.data$timing == "post") %>%
        dplyr::select("question") %>%
        dplyr::mutate(question = as.character(.data$question)) %>%
        tibble::deframe()

      new_df <- new_df %>% dplyr::mutate(question = factor(.data$question, levels = question_order))
    } else {
      # If FALSE, use user supplied by order based on the set up the levels for question using- names(question_labels):
      new_df <- new_df %>% dplyr::mutate(question = factor(.data$question, levels = names(question_labels)))
    }

    # Return N_df that will be an overall n for all the items, only if all totals_new_df$total are equal:
    if (length(unique(totals_new_df$total)) == 1) {
      # Get overall n if it is the same for each item:
      N_df <- totals_new_df %>%
        dplyr::summarize(N = mean(.data$total)) %>%
        tibble::deframe()
    }

    if (is.null(width)) {
      width <- dplyr::if_else(dplyr::n_distinct(new_df$question) < 4, 0.5,
        dplyr::if_else(dplyr::n_distinct(new_df$question) < 7, 0.75, 0.95)
      )
    }

    if (isTRUE(overall_n)) {
      if (isTRUE(percent_label)) {
        stacked_bar_chart <- new_df %>% ggplot2::ggplot(ggplot2::aes(
          x = .data$percent_answers, y = forcats::fct_rev(.data$timing), fill = .data$response,
          label = .data$percent_answers_label, group = .data$question
        ))
      } else {
        stacked_bar_chart <- new_df %>% ggplot2::ggplot(ggplot2::aes(
          x = .data$percent_answers, y = forcats::fct_rev(.data$timing), fill = .data$response,
          label = .data$n_answers, group = .data$question
        ))
      }

      stacked_bar_chart <- stacked_bar_chart +
        ggplot2::geom_col(width = width, position = ggplot2::position_stack(reverse = TRUE), color = "black") +
        ggplot2::geom_text(ggplot2::aes(color = .data$label_color),
          family = "Gill Sans MT",
          fontface = "bold", position = ggplot2::position_stack(vjust = .5, reverse = TRUE), size = 3
        ) +
        ggplot2::scale_color_manual(values = c("black", "white")) +
        ggplot2::facet_wrap(~question, ncol = 1, strip.position = "left") +
        ggplot2::scale_fill_manual(
          values = fill_colors, drop = FALSE,
          labels = function(response) stringr::str_wrap(response, width = 10)
        ) +
        ggplot2::guides(color = "none", fill = ggh4x::guide_stringlegend(
          size = 12, family = "Gill Sans MT", face = "bold", hjust = 0, vjust = 0, ncol = 5,
          spacing.x = 14, spacing.y = 0
        )) +
        ggplot2::labs(
          title = NULL, fill = NULL, y = NULL, x = NULL,
          tag = parse(text = paste0("(", expression(italic(n)), "==", N_df, ")"))
        ) +
        ggplot2::theme_void(base_family = "Gill Sans MT", base_size = 12) +
        ggplot2::theme(
          strip.placement = "outside",
          axis.text.y = ggtext::element_markdown(
            angle = 0, hjust = 1, color = "black", size = 10, family = "Gill Sans MT",
            margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
          ),
          strip.text.y.left = ggtext::element_markdown(
            angle = 0, hjust = 1, color = "black", size = 12, family = "Gill Sans MT",
            margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 0, unit = "pt")
          ),
          plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
          legend.position = "top"
        )
      # Otherwise, if overall_n == FALSE, return a stacked_bar_chart with n for each question appended to the question label:
    } else {
      # Change the label of the variable "question" by adding n of each to the end of the character string and add string wrap of 20:
      labels_n_questions <- new_df %>%
        dplyr::mutate(
          labels = paste0(.data$question, " ", "(*n* = ", totals_new_df$total, ")"),
          labels = factor(.data$labels)
        ) %>%
        dplyr::arrange(.data$question) %>%
        dplyr::distinct(.data$labels) %>%
        tibble::deframe()

      # Change the label of the variable "question" by adding n of each to the end of the character string and
      # Set factor labels for question to labels:
      new_df <- new_df %>%
        dplyr::mutate(question = factor(.data$question, labels = labels_n_questions))

      if (isTRUE(percent_label)) {
        stacked_bar_chart <- new_df %>% ggplot2::ggplot(ggplot2::aes(
          x = .data$percent_answers, y = forcats::fct_rev(.data$timing), fill = .data$response,
          label = .data$percent_answers_label, group = .data$question
        ))
      } else {
        stacked_bar_chart <- new_df %>% ggplot2::ggplot(ggplot2::aes(
          x = .data$percent_answers, y = forcats::fct_rev(.data$timing), fill = .data$response,
          label = .data$n_answers, group = .data$question
        ))
      }

      stacked_bar_chart <- stacked_bar_chart +
        ggplot2::geom_col(width = width, position = ggplot2::position_stack(reverse = TRUE), color = "black") +
        ggplot2::geom_text(ggplot2::aes(color = .data$label_color),
          family = "Gill Sans MT",
          fontface = "bold", position = ggplot2::position_stack(vjust = .5, reverse = TRUE), size = 3
        ) +
        ggplot2::scale_color_manual(values = c("black", "white")) +
        ggplot2::facet_wrap(~question, ncol = 1, strip.position = "left") +
        ggplot2::scale_fill_manual(
          breaks = scale_labels, values = fill_colors, drop = FALSE,
          labels = function(response) stringr::str_wrap(response, width = 10)
        ) +
        ggplot2::guides(color = "none", fill = ggh4x::guide_stringlegend(
          size = 12, family = "Gill Sans MT", face = "bold", hjust = 0, vjust = 0, ncol = 5,
          spacing.x = 14, spacing.y = 0
        )) +
        ggplot2::labs(title = NULL, fill = NULL, y = NULL, x = NULL, tag = NULL) +
        ggplot2::theme_void(base_family = "Gill Sans MT", base_size = 12) +
        ggplot2::theme(
          strip.placement = "outside",
          axis.text.y = ggtext::element_markdown(
            angle = 0, hjust = 1, color = "black", size = 10, family = "Gill Sans MT",
            margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
          ),
          strip.text.y.left = ggtext::element_markdown(
            angle = 0, hjust = 1, color = "black", size = 12, family = "Gill Sans MT",
            margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 0, unit = "pt")
          ),
          plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
          legend.position = "top"
        )
    }
    # If pre_post == FALSE:
  } else {
    # If pre_post is FALSE, set up new_df:
    new_df <- {{ df }} %>%
      tidyr::pivot_longer(tidyselect::everything(), names_to = "question", values_to = "response") %>%
      dplyr::mutate(question = stringr::str_remove(.data$question, "cat_")) %>%
      dplyr::group_by(.data$question, .data$response) %>%
      dplyr::summarize(n_answers = dplyr::n(), .groups = "keep") %>%
      dplyr::ungroup() %>%
      tidyr::drop_na() %>%
      dplyr::group_by(.data$question) %>%
      dplyr::mutate(
        percent_answers = .data$n_answers / sum(.data$n_answers),
        percent_answers_label = scales::percent(.data$percent_answers, accuracy = 1),
        response = forcats::fct_relevel(.data$response, scale_labels)
      ) %>%
      dplyr::ungroup()

    # If the user supplies a named vector for questions labels:
    if (!is.null(question_labels)) {
      names(question_labels) <- names(question_labels) %>%
        stringr::str_wrap(., width = 30) %>%
        gsub("\n", "<br>", .)
      new_df <- new_df %>%
        dplyr::mutate(question = forcats::fct_recode(.data$question, !!!question_labels)) %>%
        dplyr::ungroup()
    }

    # Get total n for each question, grouped by question and timing:
    totals_new_df <- new_df %>%
      dplyr::group_by(.data$question) %>%
      dplyr::summarize(total = sum(.data$n_answers), .groups = "keep") %>%
      dplyr::ungroup()

    # IF/ELSE statement, first if number_levels equals 3, sets up the label_color and fill color:
    if (length(number_levels) == 3) {
      new_df <- new_df %>%
        dplyr::group_by(.data$question) %>%
        dplyr::mutate(
          label_color = "black",
          pos_valence_post = dplyr::case_when(
            .data$response == levels(.data$response)[3] ~ percent_answers,
            TRUE ~ 0
          )
        ) %>%
        dplyr::ungroup()

      # 3 colors for chart:
      fill_colors <- c("#79AB53", "#4B9FA6", "#2C2C4F")

      # If number_levels) == 4
    } else if (length(number_levels) == 4) {
      new_df <- new_df %>%
        dplyr::group_by(.data$question) %>%
        dplyr::mutate(
          label_color = dplyr::if_else(.data$response == levels(.data$response)[1], "black", "white"),
          pos_valence_post = dplyr::case_when(
            .data$response == levels(.data$response)[3] ~ percent_answers,
            .data$response == levels(.data$response)[4] ~ percent_answers,
            TRUE ~ 0
          )
        ) %>%
        dplyr::ungroup()

      # 4 colors for chart:
      fill_colors <- c("#FFE699", "#79AB53", "#4B9FA6", "#2C2C4F")

      # If number_levels) == 5
    } else if (length(number_levels) == 5) {
      new_df <- new_df %>%
        dplyr::group_by(.data$question) %>%
        dplyr::mutate(
          label_color = dplyr::if_else(.data$response == levels(.data$response)[2], "black", "white"),
          pos_valence_post = dplyr::case_when(
            .data$response == levels(.data$response)[4] ~ percent_answers,
            .data$response == levels(.data$response)[5] ~ percent_answers,
            TRUE ~ 0
          )
        ) %>%
        dplyr::ungroup()

      # 5 colors for chart:
      fill_colors <- c("#767171", "#FFE699", "#79AB53", "#4B9FA6", "#2C2C4F")

      # If number_levels) == 6
    } else if (length(number_levels) == 6) {
      new_df <- new_df %>%
        dplyr::group_by(.data$question) %>%
        dplyr::mutate(
          label_color = dplyr::if_else(.data$response == levels(.data$response)[2], "black", "white"),
          pos_valence_post = dplyr::case_when(
            .data$response == levels(.data$response)[4] ~ percent_answers,
            .data$response == levels(.data$response)[5] ~ percent_answers,
            .data$response == levels(.data$response)[6] ~ percent_answers,
            TRUE ~ 0
          )
        ) %>%
        dplyr::ungroup()

      # 6 colors for chart:
      fill_colors <- c("#767171", "#FFE699", "#79AB53", "#4B9FA6", "#2C2C4F", "gray")

      # If number_levels) == 7
    } else if (length(number_levels) == 7) {
      new_df <- new_df %>%
        dplyr::group_by(.data$question) %>%
        dplyr::mutate(
          label_color = dplyr::if_else(.data$response == levels(.data$response)[2], "black", "white"),
          pos_valence_post = dplyr::case_when(
            .data$response == levels(.data$response)[5] ~ percent_answers,
            .data$response == levels(.data$response)[6] ~ percent_answers,
            .data$response == levels(.data$response)[7] ~ percent_answers,
            TRUE ~ 0
          )
        ) %>%
        dplyr::ungroup()

      # 7 colors for chart:
      fill_colors <- c("#767171", "#FFE699", "#79AB53", "#4B9FA6", "#37546d", "#2C2C4F", "gray")
    }

    # Set up a new question order if not supplied by the user after finding the most positive valenced items for post
    # (top levels depending on total response levels):
    if (isFALSE(question_order)) {
      question_order <- new_df %>%
        dplyr::group_by(.data$question) %>%
        dplyr::summarize(n_pos_valence_post = sum(.data$pos_valence_post), .groups = "keep") %>%
        dplyr::arrange(dplyr::desc(.data$n_pos_valence_post)) %>%
        dplyr::ungroup() %>%
        dplyr::select("question") %>%
        dplyr::mutate(question = as.character(.data$question)) %>%
        tibble::deframe()

      new_df <- new_df %>% dplyr::mutate(question = factor(.data$question, levels = question_order))
    } else {
      # If supplied by user set up the levels for question using the user supplied order = question_order (not NULL):
      new_df <- new_df %>% dplyr::mutate(question = factor(.data$question, levels = names(question_labels)))
    }

    # Return N_df that will be an overall n for all the items, only if all totals_new_df$total are equal:
    if (length(unique(totals_new_df$total)) == 1) {
      # Get overall n if it is the same for each item:
      N_df <- totals_new_df %>%
        dplyr::summarize(N = mean(.data$total)) %>%
        tibble::deframe()
    }

    # Set default width for geom_col() bars if not supplied by user:
    if (is.null(width)) {
      width <- dplyr::if_else(dplyr::n_distinct(new_df$question) < 4, 0.5,
        dplyr::if_else(dplyr::n_distinct(new_df$question) < 7, 0.75, 0.95)
      )
    }

    # If overall_n == TRUE and pre_post == FALSE::
    if (isTRUE(overall_n)) {
      # Set labels to percent or n_answers:
      if (isTRUE(percent_label)) {
        stacked_bar_chart <- new_df %>% ggplot2::ggplot(ggplot2::aes(
          x = .data$percent_answers, y = forcats::fct_rev(.data$question), fill = .data$response,
          label = .data$percent_answers_label, group = .data$question
        ))
      } else {
        stacked_bar_chart <- new_df %>% ggplot2::ggplot(ggplot2::aes(
          x = .data$percent_answers, y = forcats::fct_rev(.data$question), fill = .data$response,
          label = .data$n_answers, group = .data$question
        ))
      }

      stacked_bar_chart <- stacked_bar_chart +
        ggplot2::geom_col(width = width, position = ggplot2::position_stack(reverse = TRUE), color = "black") +
        ggplot2::geom_text(ggplot2::aes(color = .data$label_color),
          family = "Gill Sans MT",
          fontface = "bold", position = ggplot2::position_stack(vjust = .5, reverse = TRUE), size = 3
        ) +
        ggplot2::scale_color_manual(values = c("black", "white")) +
        ggplot2::scale_fill_manual(values = fill_colors, drop = FALSE, labels = function(response) stringr::str_wrap(response, width = 20)) +
        ggplot2::guides(color = "none", fill = ggh4x::guide_stringlegend(
          size = 12, family = "Gill Sans MT", face = "bold", hjust = 0, vjust = 0, ncol = 6,
          spacing.x = 25, spacing.y = 0
        )) +
        ggplot2::labs(title = NULL, fill = NULL, y = NULL, x = NULL, tag = parse(text = paste0("(", expression(italic(n)), "==", N_df, ")"))) +
        ggplot2::theme_void(base_family = "Gill Sans MT", base_size = 12) +
        ggplot2::theme(
          axis.text.y = ggtext::element_markdown(angle = 0, hjust = 1, color = "black", size = 10, family = "Gill Sans MT"),
          plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
          panel.spacing.x = ggplot2::unit(1, "in"),
          legend.position = "top"
        )
      # Otherwise, if overall_n == FALSE, return a diverging_bar_chart with n for each question appended to the question label:
    } else {
      # Change the label of the variable "question" by adding n of each to the end of the character string and add string wrap of 20:
      labels_n_questions <- new_df %>%
        dplyr::mutate(
          labels = paste0(.data$question, " ", "(*n* = ", totals_new_df$total, ")"),
          labels = factor(.data$labels)
        ) %>%
        dplyr::arrange(.data$question) %>%
        dplyr::distinct(.data$labels) %>%
        tibble::deframe()

      # Change the label of the variable "question" by adding n of each to the end of the character string and
      # Set factor labels for question to labels:
      new_df <- new_df %>%
        dplyr::mutate(question = factor(.data$question, labels = labels_n_questions))

      # Set labels to percent or n_answers:
      if (isTRUE(percent_label)) {
        stacked_bar_chart <- new_df %>% ggplot2::ggplot(ggplot2::aes(
          x = .data$percent_answers, y = forcats::fct_rev(.data$question), fill = .data$response,
          label = .data$percent_answers_label, group = .data$question
        ))
      } else {
        stacked_bar_chart <- new_df %>% ggplot2::ggplot(ggplot2::aes(
          x = .data$percent_answers, y = forcats::fct_rev(.data$question), fill = .data$response,
          label = .data$n_answers, group = .data$question
        ))
      }

      stacked_bar_chart <- stacked_bar_chart +
        ggplot2::geom_col(width = width, position = ggplot2::position_stack(reverse = TRUE), color = "black") +
        ggplot2::geom_text(ggplot2::aes(color = .data$label_color),
          family = "Gill Sans MT",
          fontface = "bold", position = ggplot2::position_stack(vjust = .5, reverse = TRUE), size = 3
        ) +
        ggplot2::scale_color_manual(values = c("black", "white")) +
        ggplot2::scale_fill_manual(values = fill_colors, drop = FALSE, labels = function(response) stringr::str_wrap(response, width = 20)) +
        ggplot2::guides(color = "none", fill = ggh4x::guide_stringlegend(
          size = 12, family = "Gill Sans MT", face = "bold", label.hjust = 0, label.vjust = 0, ncol = 6,
          spacing.x = 25, spacing.y = 0
        )) +
        ggplot2::labs(title = NULL, fill = NULL, y = labels, x = NULL, tag = NULL) +
        ggplot2::theme_void(base_family = "Gill Sans MT", base_size = 12) +
        ggplot2::theme(
          axis.text.y = ggtext::element_markdown(
            angle = 0, hjust = 1, color = "black", size = 10, family = "Gill Sans MT",
            margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 0, unit = "pt")
          ),
          plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
          panel.spacing.x = ggplot2::unit(1, "in"),
          legend.position = "top"
        )
    }
  }

  return(stacked_bar_chart)
}

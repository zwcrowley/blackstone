#' Diverging Bar Chart for Blackstone Research and Evaluation
#'
#' [divBarChart()] creates a diverging bar chart and returns a ggplot object with Blackstone Research and Evaluation branding.
#'
#' @param df Required, A [tibble][tibble::tibble-package]/data frame of survey items that are categorical/character
#'   variables, in 3 to 7 point scales, that will be inserted into a diverging bar chart with Blackstone Research and Evaluation branding.
#'
#' @param scale_labels Required, a character vector of labels for the response scale, must be in the desired order,
#'    e.g. if you have a 5 item scale of minimal to extensive it should look like this: `levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")`.
#'    This argument accepts a character vector of 3 to 7 items.
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
#' @return A [ggplot2][ggplot2::ggplot2-package] object that plots the items into a diverging bar chart and can be exported.
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
#' # Set scale_labels for recodeCat function:
#' # scale_labels as a named character vector, items in correct order:
#' levels_min_ext <- c("Minimal" = "1", "Slight" = "2", "Moderate" = "3",
#'                     "Good" = "4", "Extensive" = "5")
#'
#' # bar_scale_labels as just the names from levels_min_ext:
#' bar_scale_labels <- names(levels_min_ext)
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
#' cat_items <- bre::recodeCat(items, levels_min_ext)
#'
#' # Select the factor variables:
#' cat_items <- cat_items %>% dplyr::select(dplyr::where(is.factor))
#'
#' # Pass the factor variables and the levels to divBarChart:
#' divBarChart(
#'   df = cat_items, scale_labels = bar_scale_labels,
#'   question_labels= NULL, percent_label = TRUE, width = NULL
#' )
#' divBarChart(
#'   df = cat_items, scale_labels = bar_scale_labels,
#'   question_labels= question_labels, question_order = FALSE, percent_label = TRUE, width = NULL
#' )
divBarChart  <- function(df, scale_labels, overall_n = FALSE, percent_label = TRUE, question_labels = NULL,
                                        question_order= FALSE, width = NULL) {
  # Load all fonts:
  extrafont::loadfonts("all", quiet = TRUE)
  # Set . to NULL to stop message when using dot notation in mutate:
  . <- NULL

  # Make sure the all vars in df are factors with scale_labels as their levels:
  new_df <- {{ df }} %>% dplyr::mutate(dplyr::across(tidyselect::everything(), ~ factor(., levels = scale_labels)))

  # Changes scale_labels to tibble pulls out index and saves that as a vector, gets number of levels from scale_labels:
  number_levels <- scale_labels %>%
    tibble::enframe() %>%
    dplyr::select("name") %>%
    tibble::deframe()

  # Error messages if number_levels is less than 3:
  if (length(number_levels) < 3) {
    stop("Error: the response options in scale_labels are less than 3, they must be between 3 and 7 for this function.")
  }
  # Error messages if number_levels is greater than 7:
  if (length(number_levels) > 7) {
    stop("Error: the response options in scale_labels are greater than 7, they must be between 3 and 7 for this function.")
  }

  # Sets up new_df:
  new_df <- new_df %>%
    tidyr::pivot_longer(tidyselect::everything(), names_to = "question", values_to = "response") %>%
    dplyr::mutate(question = stringr::str_remove(.data$question, "cat_")) %>%
    tidyr::separate(.data$question, into = c("timing", "question"), sep = "_", extra = "merge") %>%
    dplyr::mutate(response = factor(.data$response, levels = scale_labels)) %>%
    dplyr::group_by(.data$question, .data$timing, .data$response) %>%
    dplyr::summarize(n_answers = dplyr::n(), .groups = "keep") %>%
    dplyr::ungroup() %>%
    tidyr::drop_na() %>%
    dplyr::group_by(.data$question, .data$timing) %>%
    dplyr::mutate(
      percent_answers = .data$n_answers / sum(.data$n_answers),
      timing = stringr::str_to_title(.data$timing),
      timing = factor(.data$timing, levels = c("Pre", "Post"))
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

  # IF/ELSE statement, first if number_levels equals 3, sets up the label_color and fill color:
  if (length(number_levels) == 3) {
    new_df <- new_df %>%
      dplyr::group_by(.data$question, .data$timing) %>%
      dplyr::mutate(
        label_color = "black",
        percent_answers = dplyr::case_when(
          .data$response == levels(.data$response)[2] ~ percent_answers,
          .data$response == levels(.data$response)[3] ~ percent_answers,
          TRUE ~ -percent_answers
        ),
        percent_answers_label = scales::percent(abs(.data$percent_answers), accuracy = 1),
        # Sets response levels to: 1,2,3 = this is so LHS of chart is reversed to create the diverging bars:
        response = forcats::fct_relevel(.data$response, c(
          levels(.data$response)[1], levels(.data$response)[2], levels(.data$response)[3]
        )),
        pos_valence_post = dplyr::case_when(
          .data$response == levels(.data$response)[3] & .data$timing == levels(.data$timing)[2] ~ percent_answers,
          .data$response == levels(.data$response)[2] & .data$timing == levels(.data$timing)[2] ~ percent_answers,
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
        percent_answers = dplyr::case_when(
          .data$response == levels(.data$response)[3] ~ percent_answers,
          .data$response == levels(.data$response)[4] ~ percent_answers,
          TRUE ~ -percent_answers
        ),
        percent_answers_label = scales::percent(abs(.data$percent_answers), accuracy = 1),
        # Sets response levels to: 2,1,3,4 = this is so LHS of chart is reversed to create the diverging bars: "
        response = forcats::fct_relevel(.data$response, c(
          levels(.data$response)[2], levels(.data$response)[1],
          levels(.data$response)[3], levels(.data$response)[4]
        )),
        # yellow is now: levels(.data$response)[2]
        label_color = dplyr::if_else(.data$response == levels(.data$response)[2], "black", "white"),
        pos_valence_post = dplyr::case_when(
          .data$response == levels(.data$response)[3] & .data$timing == levels(.data$timing)[2] ~ percent_answers,
          .data$response == levels(.data$response)[4] & .data$timing == levels(.data$timing)[2] ~ percent_answers,
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
        percent_answers = dplyr::case_when(
          .data$response == levels(.data$response)[5] ~ percent_answers,
          .data$response == levels(.data$response)[4] ~ percent_answers,
          TRUE ~ -percent_answers
        ),
        percent_answers_label = scales::percent(abs(.data$percent_answers), accuracy = 1),
        # Sets response levels to: 3,2,1,4,5 = this is so LHS of chart is reversed to create the diverging bars:
        response = forcats::fct_relevel(.data$response, c(
          levels(.data$response)[3], levels(.data$response)[2], levels(.data$response)[1],
          levels(.data$response)[4], levels(.data$response)[5]
        )),
        # yellow is now: levels(.data$response)[3]
        label_color = dplyr::if_else(.data$response == levels(.data$response)[3], "black", "white"),
        pos_valence_post = dplyr::case_when(
          .data$response == levels(.data$response)[4] & .data$timing == levels(.data$timing)[2] ~ percent_answers,
          .data$response == levels(.data$response)[5] & .data$timing == levels(.data$timing)[2] ~ percent_answers,
          TRUE ~ 0
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data$response)

    # 5 colors for chart:
    fill_colors <- c("#FFE699", "#79AB53","#767171", "#4B9FA6","#2C2C4F")

    # If number_levels) == 6
  } else if (length(number_levels) == 6) {
    new_df <- new_df %>%
      dplyr::group_by(.data$question, .data$timing) %>%
      dplyr::mutate(
        percent_answers = dplyr::case_when(
          .data$response == levels(.data$response)[4] ~ percent_answers,
          .data$response == levels(.data$response)[5] ~ percent_answers,
          .data$response == levels(.data$response)[6] ~ percent_answers,
          TRUE ~ -percent_answers
        ),
        percent_answers_label = scales::percent(abs(.data$percent_answers), accuracy = 1),
        # Sets response levels to: 4,3,2,1,5,6 = this is so LHS of chart is reversed to create the diverging bars:
        response = forcats::fct_relevel(.data$response, c(
          levels(.data$response)[4], levels(.data$response)[3], levels(.data$response)[2], levels(.data$response)[1],
          levels(.data$response)[5], levels(.data$response)[6]
        )),
        # yellow is now: levels(.data$response)[2]
        label_color = dplyr::if_else(.data$response == levels(.data$response)[2], "black", "white"),
        pos_valence_post = dplyr::case_when(
          .data$response == levels(.data$response)[4] & .data$timing == levels(.data$timing)[2] ~ percent_answers,
          .data$response == levels(.data$response)[5] & .data$timing == levels(.data$timing)[2] ~ percent_answers,
          .data$response == levels(.data$response)[6] & .data$timing == levels(.data$timing)[2] ~ percent_answers,
          TRUE ~ 0
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data$response)

    # 6 colors for chart:
    fill_colors <- c("gray","#FFE699", "#79AB53","#767171", "#4B9FA6", "#2C2C4F")

    # If number_levels) == 7
  } else if (length(number_levels) == 7) {
    new_df <- new_df %>%
      dplyr::group_by(.data$question, .data$timing) %>%
      dplyr::mutate(
        percent_answers = dplyr::case_when(
          .data$response == levels(.data$response)[5] ~ percent_answers,
          .data$response == levels(.data$response)[6] ~ percent_answers,
          .data$response == levels(.data$response)[7] ~ percent_answers,
          TRUE ~ -percent_answers
        ),
        percent_answers_label = scales::percent(abs(.data$percent_answers), accuracy = 1),
        # Sets response levels to: 4,3,2,1,5,6,7 = this is so LHS of chart is reversed to create the diverging bars:
        response = forcats::fct_relevel(.data$response, c(
          levels(.data$response)[4], levels(.data$response)[3], levels(.data$response)[2], levels(.data$response)[1],
          levels(.data$response)[5], levels(.data$response)[6], levels(.data$response)[7]
        )),
        # yellow is now: levels(.data$response)[3]
        label_color = dplyr::if_else(.data$response == levels(.data$response)[2], "black", "white"),
        pos_valence_post = dplyr::case_when(
          .data$response == levels(.data$response)[5] & .data$timing == levels(.data$timing)[2] ~ percent_answers,
          .data$response == levels(.data$response)[6] & .data$timing == levels(.data$timing)[2] ~ percent_answers,
          .data$response == levels(.data$response)[7] & .data$timing == levels(.data$timing)[2] ~ percent_answers,
          TRUE ~ 0
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data$response)

    # 7 colors for chart:
    fill_colors <- c("gray","#FFE699", "#79AB53","#767171", "#4B9FA6", "#37546d", "#2C2C4F")
  }

  # Set up a new question order if not supplied by the user after finding the most positive valenced items for post
  # (top levels depending on total response levels):
  if (isFALSE(question_order)) {
    question_order <- new_df %>%
      dplyr::group_by(.data$question, .data$timing) %>%
      dplyr::summarize(n_pos_valence_post = sum(.data$pos_valence_post), .groups = "keep") %>%
      dplyr::arrange(dplyr::desc(.data$n_pos_valence_post)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$timing == levels(.data$timing)[2]) %>%
      dplyr::select("question") %>%
      dplyr::mutate(question = as.character(.data$question)) %>%
      tibble::deframe()

    new_df <- new_df %>% dplyr::mutate(question = factor(.data$question, levels = question_order))
  } else {
    # If FALSE, use user supplied by order based on the set up the levels for question using- names(question_labels):
    new_df <- new_df %>% dplyr::mutate(question = factor(.data$question, levels = names(question_labels)))
  }

  # Get total n for each question, grouped by question and timing:
  totals_new_df <- new_df %>%
    dplyr::group_by(.data$question, .data$timing) %>%
    dplyr::summarize(total = sum(.data$n_answers), .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$question) %>%
    dplyr::distinct(.data$question, .data$total) %>%
    dplyr::ungroup()

  if (is.null(width)) {
    width <- dplyr::if_else(dplyr::n_distinct(new_df$question) < 4, 0.5,
                            dplyr::if_else(dplyr::n_distinct(new_df$question) < 7, 0.75, 0.95)
    )
  }

  # If overall_n == TRUE:
  if (isTRUE(overall_n)) {

    # Return N_df that will be an overall n for all the items, only if all totals_new_df$total are equal to each other:
    N_df <- NULL
    if (length(unique(totals_new_df$total)) == 1) {
      # Get overall n if it is the same for each item:
      N_df <- totals_new_df %>%
        dplyr::summarize(N = mean(.data$total)) %>%
        tibble::deframe()
    }

    # Error messages if N_df is null, not filled by last if statement above:
    if (is.null(N_df)) {
      stop("Error: Can not use `overall_n` for this function, responses for variables are not of equal length. Use argument: `overall_n = FALSE`.")
    }

    # Add percent_answers_label as label in aes() if percent_label == TRUE:
    if (isTRUE(percent_label)) {
      diverging_bar_chart <- new_df %>%
        ggplot2::ggplot(ggplot2::aes(
          x = .data$percent_answers, y = forcats::fct_rev(.data$timing), fill = .data$response,
          label = .data$percent_answers_label, group = .data$question
        ))
      # Otherwise n_answers as label in aes() if percent_label == FALSE:
    } else{
      diverging_bar_chart <- new_df %>%
        ggplot2::ggplot(ggplot2::aes(
          x = .data$percent_answers, y = forcats::fct_rev(.data$timing), fill = .data$response,
          label = .data$n_answers, group = .data$question
        ))
    }

    diverging_bar_chart <- diverging_bar_chart +
      ggplot2::geom_col(width = width, position = ggplot2::position_stack(reverse = TRUE), color = "black") +
      ggplot2::geom_text(ggplot2::aes(color = .data$label_color),
                         family = "Gill Sans MT",
                         fontface = "bold", position = ggplot2::position_stack(vjust = .5, reverse = TRUE), size = 3.5
      ) +
      ggplot2::scale_color_identity() +
      ggplot2::facet_wrap(~question, ncol = 1, strip.position = "left") +
      ggplot2::scale_fill_manual(breaks = scale_labels, values = fill_colors, drop = FALSE,
                                 labels = paste("<span style='color:", fill_colors, "'>",
                                                stringr::str_wrap(scale_labels, width = 10) %>% gsub("\n", "<br>", .), "</span>"),
                                 guide = ggplot2::guide_legend(override.aes = ggplot2::aes(color = NA, fill = NA))
      ) +
      ggplot2::guides(color = "none") +
      ggplot2::labs(title = NULL, y = labels, x = NULL, tag = paste0("(*n* = ", N_df, ")")) +
      ggplot2::theme_void(base_family = "Gill Sans MT", base_size = 11) +
      ggplot2::theme(
        strip.placement = "outside",
        axis.text.y = ggtext::element_markdown(
          angle = 0, hjust = 1, color = "black", size = 11, family = "Gill Sans MT",
          margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
        ),
        strip.text.y.left = ggtext::element_markdown(
          angle = 0, hjust = 1, color = "black", size = 11, family = "Gill Sans MT",
          margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 0, unit = "pt")
        ),
        plot.tag = ggtext::element_markdown(color = "black", size = 10, family = "Gill Sans MT"),
        plot.tag.position = "topleft",
        plot.margin = ggplot2::margin(t = 35, r = 5, b = 35, l = 5, unit = "pt"),
        legend.text = ggtext::element_markdown(angle = 0, hjust = 0.5, vjust = 0, halig	= 0.5, valign = 0,
                                               size = 11, family = "Gill Sans MT", face = "bold",
                                               margin = ggplot2::margin(t = 5, r = 10, b = 5, l = 10, unit = "pt")
        ),
        legend.justification = c("right", "top"),
        legend.position = "top",
        legend.key = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank()
      )

    # Otherwise, if overall_n == FALSE, return a diverging_bar_chart with n for each question appended to the question label:
  } else {
    # Change the label of the variable "question" by adding n of each to the end of the character string:
    labels_n_questions <- paste0(totals_new_df$question, " ", "(*n* = ", totals_new_df$total, ")")

    # Set factor labels for question to labels:
    new_df <- new_df %>%
      dplyr::mutate(question = factor(.data$question, levels = levels(.data$question), labels = labels_n_questions))

    # Add percent_answers_label as label in aes() if percent_label == TRUE:
    if (isTRUE(percent_label)) {
      diverging_bar_chart <- new_df %>%
        ggplot2::ggplot(ggplot2::aes(
          x = .data$percent_answers, y = forcats::fct_rev(.data$timing), fill = .data$response,
          label = .data$percent_answers_label, group = .data$question
        ))
      # Otherwise n_answers as label in aes() if percent_label == FALSE:
    } else{
      diverging_bar_chart <- new_df %>%
        ggplot2::ggplot(ggplot2::aes(
          x = .data$percent_answers, y = forcats::fct_rev(.data$timing), fill = .data$response,
          label = .data$n_answers, group = .data$question
        ))
    }

    diverging_bar_chart <- diverging_bar_chart +
      ggplot2::geom_col(width = width, position = ggplot2::position_stack(reverse = TRUE), color = "black") +
      ggplot2::geom_text(ggplot2::aes(color = .data$label_color),
                         family = "Gill Sans MT",
                         fontface = "bold", position = ggplot2::position_stack(vjust = .5, reverse = TRUE), size = 3.5
      ) +
      ggplot2::scale_color_identity() +
      ggplot2::facet_wrap(~question, ncol = 1, strip.position = "left") +
      ggplot2::scale_fill_manual(breaks = scale_labels, values = fill_colors, drop = FALSE,
                                 labels = paste("<span style='color:", fill_colors, "'>",
                                                stringr::str_wrap(scale_labels, width = 10) %>% gsub("\n", "<br>", .), "</span>"),
                                 guide = ggplot2::guide_legend(override.aes = ggplot2::aes(color = NA, fill = NA))
      ) +
      ggplot2::guides(color = "none") +
      ggplot2::labs(title = NULL, y = labels, x = NULL, tag = NULL) +
      ggplot2::theme_void(base_family = "Gill Sans MT", base_size = 11) +
      ggplot2::theme(
        strip.placement = "outside",
        axis.text.y = ggtext::element_markdown(
          angle = 0, hjust = 1, color = "black", size = 11, family = "Gill Sans MT",
          margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
        ),
        strip.text.y.left = ggtext::element_markdown(
          angle = 0, hjust = 1, color = "black", size = 11, family = "Gill Sans MT",
          margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 0, unit = "pt")
        ),
        plot.margin = ggplot2::margin(t = 35, r = 5, b = 35, l = 5, unit = "pt"),
        legend.text = ggtext::element_markdown(angle = 0, hjust = 0.5, vjust = 0, halig	= 0.5, valign = 0,
                                               size = 11, family = "Gill Sans MT", face = "bold",
                                               margin = ggplot2::margin(t = 5, r = 10, b = 5, l = 10, unit = "pt")
        ),
        legend.justification = c("right", "top"),
        legend.position = "top",
        legend.key = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank()
      )

  }

  return(diverging_bar_chart)
}

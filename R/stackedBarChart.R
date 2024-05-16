#' Stacked Bar Chart for Blackstone Research and Evaluation
#'
#' [stackedBarChart()] creates a stacked bar chart and returns a ggplot object with Blackstone Research and Evaluation branding.
#'
#' @param df Required, A [tibble][tibble::tibble-package]/data frame of survey items that are categorical/character
#'   variables, in 3 to 7 point scales, that will be inserted into a stacked bar chart with Blackstone Research and Evaluation branding.
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
#' # Set scale_labels for recodeCat function:
#' # scale_labels as a named character vector, items in correct order:
#' levels_min_ext <- c(
#'   "Minimal" = "1", "Slight" = "2", "Moderate" = "3",
#'   "Good" = "4", "Extensive" = "5"
#' )
#'
#' # bar_scale_labels as just the names from levels_min_ext:
#' bar_scale_labels <- names(levels_min_ext)
#'
#' # Question labels as a named vector with the naming structure
#' # like this: c("new label" = "original variable name"):
#' question_labels <- c(
#'   "Publish a lot of high quality papers" = "Publish",
#'   "Write a lot of research papers" = "Write",
#'   "Research in a lab with faculty" = "Research",
#'   "Organization of a large research project" = "Organization",
#'   "Source work for a research paper" = "Source"
#' )
#'
#' # Recode the numeric to factor variables using the levels from levels_min_ext:
#' cat_items <- bre::recodeCat(items, levels_min_ext)
#' cat_items_single <- bre::recodeCat(items_single, levels_min_ext)
#'
#' # Select the factor variables:
#' cat_items <- cat_items %>% dplyr::select(dplyr::where(is.factor))
#' cat_items_single <- cat_items_single %>% dplyr::select(dplyr::where(is.factor))
#'
#' # Pass the factor variables and the levels to stackedBarChart:
#' stackedBarChart(
#'   df = cat_items, pre_post = TRUE, scale_labels = bar_scale_labels,
#'   question_labels = NULL, percent_label = TRUE, width = NULL
#' )
#' stackedBarChart(
#'   df = cat_items_single, pre_post = FALSE, scale_labels = bar_scale_labels,
#'   question_labels = NULL, percent_label = TRUE, width = NULL
#' )
#' stackedBarChart(
#'   df = cat_items, pre_post = TRUE, scale_labels = bar_scale_labels,
#'   question_labels = question_labels, question_order = FALSE, percent_label = TRUE, width = NULL
#' )
#' stackedBarChart(
#'   df = cat_items_single, pre_post = FALSE, scale_labels = bar_scale_labels,
#'   question_labels = question_labels, question_order = FALSE, percent_label = TRUE, width = NULL
#' )
stackedBarChart <- function(df, scale_labels, pre_post = FALSE, overall_n = FALSE, percent_label = TRUE,
                            question_labels = NULL, question_order= FALSE, width = NULL) {
    # Load all fonts:
    extrafont::loadfonts("all", quiet = TRUE)
    # Create a font family character var so that it is easy to change, could also be a new arg:
    font_family <- c("Arial")

    # Set . to NULL to stop message when using dot notation in mutate:
    . <- NULL

    # Function to build ggplot:
    stackedBar_ggplot <- function(df_gg, x_gg , y_gg, fill_gg, group_gg, label_gg, label_color_gg, scale_labels_gg, width_gg, fill_colors_gg,
                                  overall_n_gg, N_df_gg, pre_post = FALSE, font_family = font_family) {

      stacked_bar_chart_gg <- {{df_gg}} %>%
        ggplot2::ggplot(ggplot2::aes(
          x = {{x_gg}}, y = forcats::fct_rev({{y_gg}}), fill = {{fill_gg}},
          label = {{label_gg}}, group = {{group_gg}})) +
        ggplot2::geom_col(width = {{width_gg}}, position = ggplot2::position_stack(reverse = TRUE), color = "black") +
        ggplot2::geom_text(ggplot2::aes(color = {{label_color_gg}}),
                           family = font_family,
                           fontface = "bold", position = ggplot2::position_stack(vjust = .5, reverse = TRUE), size = 3.5
        ) +
        ggplot2::scale_color_identity()
      if (isTRUE(pre_post)) {
        stacked_bar_chart_gg <-  stacked_bar_chart_gg + ggplot2::facet_wrap(~question, ncol = 1, strip.position = "left")
      }
      stacked_bar_chart_gg <-  stacked_bar_chart_gg +
        ggplot2::scale_fill_manual(breaks = {{scale_labels_gg}}, values = {{fill_colors_gg}}, drop = FALSE,
                                   labels = paste("<span style='color:", {{fill_colors_gg}}, "'>",
                                                  stringr::str_wrap({{scale_labels_gg}}, width = 10) %>% gsub("\n", "<br>", .), "</span>"),
                                   guide = ggplot2::guide_legend(override.aes = ggplot2::aes(color = NA, fill = NA))
      ) + ggplot2::guides(color = "none")

      if (isTRUE(pre_post)) {
        stacked_bar_chart_gg <- stacked_bar_chart_gg +
          ggplot2::theme_void(base_family = font_family, base_size = 11) +
          ggplot2::theme(strip.placement = "outside",
                         strip.text.y.left = ggtext::element_markdown(
                           angle = 0, hjust = 1, color = "black", size = 11, family = font_family,
                           margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 0, unit = "pt")),
                         axis.text.y = ggtext::element_markdown(
                           angle = 0, hjust = 1, color = "black", size = 11, family = font_family,
                           margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
                         ),
                         plot.margin = ggplot2::margin(t = 35, r = 5, b = 35, l = 5, unit = "pt"),
                         legend.text = ggtext::element_markdown(angle = 0, hjust = 0.5, vjust = 0, halig	= 0.5, valign = 0,
                                                                size = 11, family = font_family, face = "bold",
                                                                margin = ggplot2::margin(t = 5, r = 10, b = 5, l = 10, unit = "pt")
                         ),
                         legend.justification = c("right", "top"),
                         legend.position = "top",
                         legend.key = ggplot2::element_blank(),
                         legend.title = ggplot2::element_blank()
          )
      } else {
        stacked_bar_chart_gg <- stacked_bar_chart_gg +
          ggplot2::theme_void(base_family = font_family, base_size = 11) +
          ggplot2::theme(
            axis.text.y = ggtext::element_markdown(
              angle = 0, hjust = 1, color = "black", size = 11, family = font_family,
              margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
            ),
            plot.margin = ggplot2::margin(t = 35, r = 5, b = 35, l = 5, unit = "pt"),
            legend.text = ggtext::element_markdown(angle = 0, hjust = 0.5, vjust = 0, halig	= 0.5, valign = 0,
                                                   size = 11, family = font_family, face = "bold",
                                                   margin = ggplot2::margin(t = 5, r = 10, b = 5, l = 10, unit = "pt")
            ),
            legend.justification = c("right", "top"),
            legend.position = "top",
            legend.key = ggplot2::element_blank(),
            legend.title = ggplot2::element_blank()
          )
      }

      # Set tag to N_df if overall_n_gg == TRUE
      if (isTRUE(overall_n_gg)) {
        stacked_bar_chart_gg <-  stacked_bar_chart_gg +  ggplot2::labs(title = NULL, y = labels, x = NULL, tag = paste0("(*n* = ",  N_df_gg , ")")) +
          ggplot2::theme(plot.tag = ggtext::element_markdown(color = "black", size = 10, family = font_family),
                         plot.tag.position = "topleft")
      } else {
        stacked_bar_chart_gg <-  stacked_bar_chart_gg +  ggplot2::labs(title = NULL, y = labels, x = NULL, tag = NULL)
      }

      return(stacked_bar_chart_gg)

    } # end of stackedBar_ggplot()

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

    if (isTRUE(pre_post)) {
      # Sets up new_df if pre_post is TRUE:
      new_df <- new_df %>%
        tidyr::pivot_longer(tidyselect::everything(), names_to = "question", values_to = "response") %>%
        dplyr::mutate(question = stringr::str_remove(.data$question, "cat_")) %>%
        tidyr::separate(.data$question, into = c("timing", "question"), sep = "_", extra = "merge") %>%
        dplyr::group_by(.data$question, .data$timing, .data$response) %>%
        dplyr::summarize(n_answers = dplyr::n(), .groups = "keep") %>%
        dplyr::ungroup() %>%
        tidyr::drop_na() %>%
        dplyr::group_by(.data$question, .data$timing) %>%
        dplyr::mutate(
          percent_answers = .data$n_answers / sum(.data$n_answers),
          percent_answers_label = scales::percent(.data$percent_answers, accuracy = 1),
          response = forcats::fct_relevel(.data$response, scale_labels),
          timing = stringr::str_to_title(.data$timing),
          timing = factor(.data$timing, levels = c("Pre", "Post"))
        ) %>%
        dplyr::ungroup()

      # Set up a new question order if not supplied by the user after finding the most positive valenced items for post
      # (top levels depending on total response levels):
      if (isFALSE(question_order)) {
        question_order <- new_df %>% dplyr::filter(., .data$timing == "Post")  %>% tidyr::complete(.data$question, .data$response) %>%
          tidyr::pivot_wider(id_cols = -c("timing", "percent_answers", "percent_answers_label"), names_from = "response", values_from = "n_answers") %>%
          dplyr::group_by(.data$question) %>% rev() %>%
          dplyr::arrange(dplyr::across(-c("question"), dplyr::desc)) %>%
          dplyr::select("question") %>%
          unique() %>%
          tibble::deframe()
        # change the factor levels of question to be ordered by the question_order:
        new_df <- new_df %>% dplyr::mutate(question = forcats::fct_relevel(.data$question, question_order))
      } else {
        # If question_order == TRUE,set up the levels for question using the user supplied order = question_labels:
        new_df <- new_df %>% dplyr::mutate(question = factor(.data$question, levels = question_labels))
      }
      # If the user supplies a named vector for questions labels:
      if (!is.null(question_labels)) {
        names(question_labels) <- names(question_labels) %>%
          stringr::str_wrap(., width = 30) %>%
          gsub("\n", "<br>", .)
        new_df <- new_df %>%
          dplyr::mutate(question = forcats::fct_recode(.data$question, !!!question_labels))
      }
      # Get total n for each question, grouped by question and timing:
      totals_new_df <- new_df %>%
        dplyr::group_by(.data$question, .data$timing) %>%
        dplyr::summarize(total = sum(.data$n_answers), .groups = "keep") %>%
        dplyr::ungroup() %>%
        dplyr::group_by(.data$question) %>%
        dplyr::distinct(.data$question, .data$total) %>%
        dplyr::ungroup()
      # End of if pre_post == TRUE
    } else {
      # If pre_post is FALSE, set up new_df:
      new_df <- new_df %>%
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

      # Set up a new question order if not supplied by the user after finding the most positive valenced items for post
      # (top levels depending on total response levels):
      if (isFALSE(question_order)) {
        question_order <- new_df %>% tidyr::complete(.data$question, .data$response) %>%
          tidyr::pivot_wider(id_cols = -c("percent_answers", "percent_answers_label"), names_from = "response", values_from = "n_answers") %>%
          dplyr::group_by(.data$question) %>% rev() %>%
          dplyr::arrange(dplyr::across(-c("question"), dplyr::desc)) %>%
          dplyr::select("question") %>%
          unique() %>%
          tibble::deframe()
        # change the factor levels of question to be ordered by the question_order:
        new_df <- new_df %>% dplyr::mutate(question = forcats::fct_relevel(.data$question, question_order))
      } else {
        # If question_order == TRUE,set up the levels for question using the user supplied order = question_labels:
        new_df <- new_df %>% dplyr::mutate(question = factor(.data$question, levels = question_labels))
      }
      # If the user supplies a named vector for questions labels:
      if (!is.null(question_labels)) {
        names(question_labels) <- names(question_labels) %>%
          stringr::str_wrap(., width = 30) %>%
          gsub("\n", "<br>", .)
        new_df <- new_df %>%
          dplyr::mutate(question = forcats::fct_recode(.data$question, !!!question_labels))
      }
      # Get total n for each question, grouped by question:
      totals_new_df <- new_df %>%
        dplyr::group_by(.data$question) %>%
        dplyr::summarize(total = sum(.data$n_answers), .groups = "keep") %>%
        dplyr::ungroup()
    } # End of if pre_post == FALSE

    # IF/ELSE statement, first if number_levels equals 3, sets up the label_color and fill color:
    if (length(number_levels) == 3) {
      new_df <- new_df %>% dplyr::mutate(label_color = "black")
      # 3 colors for chart:
      fill_colors <- c("#79AB53", "#4B9FA6", "#2C2C4F")
      # If number_levels) == 4
    } else if (length(number_levels) == 4) {
      new_df <- new_df %>% dplyr::mutate(label_color = dplyr::if_else(.data$response == levels(.data$response)[1], "black", "white"))
      # 4 colors for chart:
      fill_colors <- c("#FFE699", "#79AB53", "#4B9FA6", "#2C2C4F")
      # If number_levels) == 5
    } else if (length(number_levels) == 5) {
      new_df <- new_df %>% dplyr::mutate(label_color = dplyr::if_else(.data$response == levels(.data$response)[1], "black", "white"))
      # 5 colors for chart:
      fill_colors <- c("#FFE699", "#79AB53","#767171", "#4B9FA6", "#2C2C4F")
      # If number_levels) == 6
    } else if (length(number_levels) == 6) {
      new_df <- new_df %>% dplyr::mutate(label_color = dplyr::if_else(.data$response == levels(.data$response)[2], "black", "white"))
      # 6 colors for chart:
      fill_colors <- c("gray","#FFE699", "#79AB53","#767171", "#4B9FA6", "#2C2C4F")
      # If number_levels) == 7
    } else if (length(number_levels) == 7) {
      new_df <- new_df %>% dplyr::mutate(label_color = dplyr::if_else(.data$response == levels(.data$response)[2], "black", "white"))
      # 7 colors for chart:
      fill_colors <- c("gray","#FFE699", "#79AB53","#767171", "#4B9FA6", "#37546d", "#2C2C4F")
    }

    # Set default width for geom_col() bars if not supplied by user:
    if (is.null(width)) {
      width <- dplyr::if_else(dplyr::n_distinct(new_df$question) < 4, 0.5,
                              dplyr::if_else(dplyr::n_distinct(new_df$question) < 7, 0.75, 0.95)
      )
    }

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
    }

    # Otherwise, if overall_n == FALSE, return a stacked_bar_chart with n for each question appended to the question label:
    if (isFALSE(overall_n)) {
      # Change the label of the variable "question" by adding n of each to the end of the character string:
      labels_n_questions <- paste0(totals_new_df$question, " ", "(*n* = ", totals_new_df$total, ")")

      # Set factor labels for question to labels:
      new_df <- new_df %>%
        dplyr::mutate(question = factor(.data$question, levels = levels(.data$question), labels = labels_n_questions))
    }

    # Call stackedBar_ggplot for either to set labels to percent or n_answers:
    if (isTRUE(percent_label)) {
      label_gg <- new_df$percent_answers_label
    } else {
      label_gg <- new_df$n_answers
    }

    # Final call to stackedBar_ggplot() for pre_post == TRUE:
    if (isTRUE(pre_post)) {
      stacked_bar_chart <- stackedBar_ggplot(df_gg = new_df, x_gg = .data$percent_answers , y_gg = .data$timing, fill_gg = .data$response, group_gg = .data$question,
                                             label_gg = label_gg, label_color_gg = .data$label_color, scale_labels_gg = scale_labels,
                                             width_gg = width, fill_colors_gg = fill_colors, overall_n_gg = overall_n, N_df_gg = N_df, pre_post = TRUE)
      # Final call to stackedBar_ggplot() if pre_post == FALSE:
    } else {
      stacked_bar_chart <- stackedBar_ggplot(df_gg = new_df, x_gg = .data$percent_answers , y_gg = .data$question, fill_gg = .data$response, group_gg = .data$question,
                                             label_gg = label_gg, label_color_gg = .data$label_color, scale_labels_gg = scale_labels,
                                             width_gg = width, fill_colors_gg = fill_colors, overall_n_gg = overall_n, N_df_gg = N_df, pre_post = FALSE)
    }

    return(stacked_bar_chart)

}

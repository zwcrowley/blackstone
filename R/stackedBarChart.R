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
#' @param fill_colors Default is "seq", If "seq", the color scale for the fill for each bar is set to 'cividis'. If set to "div", it is 'Blue Red 3',
#'    otherwise the user can input a character vector of hex codes at least a long as the `scale_labels` arg.
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
#'
#' @export
#'
#' @examples
#' items <- tibble::tibble(
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
#' items_single <- tibble::tibble(
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
stackedBarChart <- function(df, scale_labels, fill_colors = "seq", pre_post = FALSE, overall_n = FALSE, percent_label = TRUE,
                            question_labels = NULL, question_order= FALSE, width = NULL) {
    # Load all fonts:
    extrafont::loadfonts("all", quiet = TRUE)

    # Set . to NULL to stop message when using dot notation in mutate:
    . <- NULL

    # Start of data manipulation: ----
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
          stringr::str_wrap(., width = 15) %>%
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
          if (isFALSE(overall_n)) {
                names(question_labels) <- names(question_labels)
           } else if (isTRUE(overall_n)) {
               names(question_labels) <- names(question_labels) %>%
                   stringr::str_wrap(., width = 15) %>%
                   gsub("\n", "<br>", .)
           }
        new_df <- new_df %>%
          dplyr::mutate(question = forcats::fct_recode(.data$question, !!!question_labels))
      }
      # Get total n for each question, grouped by question:
      totals_new_df <- new_df %>%
        dplyr::group_by(.data$question) %>%
        dplyr::summarize(total = sum(.data$n_answers), .groups = "keep") %>%
        dplyr::ungroup()
    } # End of if pre_post == FALSE

    # If statement to handle the value(s) of `fill_colors`:
    if (length(fill_colors) > 1) {
        if (length(fill_colors) >= length(scale_labels)) {
            new_fill_colors <- fill_colors # sets the fill colors to the hex codes passed in by the user.
        } else {
            stop("Error: the length of `fill_colors` needs to be greater than or equal to the length of `scale_labels.`")
        }
    } else if (fill_colors == "seq") {
        new_fill_colors <- seqFillColors(length(scale_labels)) # sets the fill colors to the default sequential palette of `cividis`.
    } else if (fill_colors == "div") {
        new_fill_colors <- divFillColors(length(scale_labels)) # sets the fill colors to the default diverging palette of `Blue Red 3` from namespace `colorspace`.
    }

    # Use the internal function labelColorMaker(), to create text color labels of black or white, see `helpers.R`:
    label_colors <- labelColorMaker(new_fill_colors, names = scale_labels)
    # create a new col `label_color` using the named vector `label_colors` to map the text color to the variable response
    new_df <- new_df %>% dplyr::mutate(., label_color = label_colors[.data$response]) # create a new col `label_color` using the named vector `label_colors` to map the text color to the variable response

    # Set default width for geom_col() bars if not supplied by user:
    if (is.null(width)) {
        width <- 0.9
      # width <- dplyr::if_else(dplyr::n_distinct(new_df$question) < 4, 0.5,
      #                         dplyr::if_else(dplyr::n_distinct(new_df$question) < 7, 0.75, 0.95)
      # )
    }

    if (isTRUE(overall_n)) {
      # Return N_df that will be an overall n for all the items, only if all totals_new_df$total are equal to each other: ----
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
      labels_n_questions <- paste0(totals_new_df$question, " ", "(*n* = ", totals_new_df$total, ")") %>%
                            stringr::str_wrap(., width = 15) %>%
                            gsub("\n", "<br>", .)

      # Set factor labels for question to labels:
      new_df <- new_df %>%
        dplyr::mutate(question = factor(.data$question, levels = levels(.data$question), labels = labels_n_questions))
    }

    # Call stackedBar_ggplot ----
    # Set labels to percent or n_answers:
    if (isTRUE(percent_label)) {
      label_gg <- new_df$percent_answers_label
    } else {
      label_gg <- new_df$n_answers
    }

    # Final call to stackedBar_ggplot() for pre_post == TRUE:
    if (isTRUE(pre_post)) {
      stacked_bar_chart <- stackedBar_ggplot(df_gg = new_df, x_gg = .data$percent_answers , y_gg = .data$timing, fill_gg = .data$response, group_gg = .data$question,
                                             label_gg = label_gg, label_color_gg = .data$label_color, scale_labels_gg = scale_labels,
                                             width_gg = width, fill_colors_gg = new_fill_colors, overall_n_gg = overall_n, N_df_gg = N_df, pre_post = TRUE)
      # Final call to stackedBar_ggplot() if pre_post == FALSE:
    } else {
      stacked_bar_chart <- stackedBar_ggplot(df_gg = new_df, x_gg = .data$percent_answers , y_gg = .data$question, fill_gg = .data$response, group_gg = .data$question,
                                             label_gg = label_gg, label_color_gg = .data$label_color, scale_labels_gg = scale_labels,
                                             width_gg = width, fill_colors_gg = new_fill_colors, overall_n_gg = overall_n, N_df_gg = N_df, pre_post = FALSE)
    }

    return(stacked_bar_chart)

}

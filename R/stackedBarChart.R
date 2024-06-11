#' Stacked Bar Chart for Blackstone Research and Evaluation
#'
#' [stackedBarChart()] creates a stacked bar chart and returns a ggplot object with Blackstone Research and Evaluation branding.
#'
#' @param df Required, A [tibble][tibble::tibble-package]/data frame of survey items that are categorical/character
#'   variables, that will be inserted into a stacked bar chart with Blackstone Research and Evaluation branding.
#'
#' @param scale_labels Required, a character vector of labels for the response scale, must be in the desired order,
#'    e.g. if you have a 5 item scale of minimal to extensive it should look like this:
#'    `levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")`.
#'
#' @param fill_colors Default is "seq", If "seq", the color scale for the fill for each bar is set to blue sequential palette.
#'      If set to "div", it is the blue-red diverging color palette, otherwise the user can input a character vector of hex codes
#'      at least a long as the character vector passed to the `scale_labels` argument.
#'
#' @param pre_post Logical, default is FALSE. If true, returns a pre-post stacked bar chart.
#'
#' @param overall_n Logical, default is TRUE. If TRUE, returns an overall *n* for all questions that is in the upper left tag of the plot.
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
#' # Recode the numeric to factor variables using the levels from levels_min_ext and
#' # select the factor variables::
#' cat_items <- bre::recodeCat(items, levels_min_ext) %>%
#'                  dplyr::select(dplyr::where(is.factor))
#' cat_items_single <- bre::recodeCat(items_single, levels_min_ext) %>%
#'                         dplyr::select(dplyr::where(is.factor))
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
stackedBarChart <- function(df, scale_labels, fill_colors = "seq", pre_post = FALSE, overall_n = TRUE, percent_label = TRUE,
                            question_labels = NULL, question_order= FALSE, width = NULL) {
    # Load all fonts:
    extrafont::loadfonts("all", quiet = TRUE)

    # Set . to NULL to stop message when using dot notation in mutate:
    . <- NULL

    # Start of data manipulation: ----
    # Make sure the all vars in df are factors with scale_labels as their levels:
    new_df <- {{ df }} %>% dplyr::mutate(dplyr::across(tidyselect::everything(), ~ factor(., levels = scale_labels)))

    if (isTRUE(pre_post)) {
        # If pre_post is TRUE, set up new_df with dataVizCleaning():
        new_df <- dataVizCleaning(df = new_df, pre_post = TRUE, scale_labels = scale_labels, na_remove = TRUE)

        # Get total n for each question, grouped by question and timing:
        # totals_new_df <- new_df %>%
        #   dplyr::group_by(.data[["question"]], .data[["timing"]]) %>%
        #   dplyr::summarize(total = sum(.data[["n_answers"]]), .groups = "keep") %>%
        #   dplyr::ungroup() %>%
        #   dplyr::group_by(.data[["question"]]) %>%
        #   dplyr::distinct(.data[["question"]], .data[["total"]]) %>%
        #   dplyr::ungroup()
        totals_new_df <- {{ df }} %>%
            tidyr::pivot_longer(tidyselect::contains(c("pre_", "post_")), names_to = "question", values_to = "response") %>%
            dplyr::mutate(question = stringr::str_remove(.data$question, "cat_")) %>%
            tidyr::separate(.data$question, into = c("timing", "question"), sep = "_", extra = "merge") %>%
            dplyr::group_by(.data[["question"]], .data[["timing"]]) %>%
            dplyr::mutate(timing = stringr::str_to_title(.data$timing), # capitalize timing
                          timing = factor(.data$timing, levels = c("Pre", "Post"))
            ) %>%
            dplyr::summarize(total = dplyr::n(), .groups = "keep") %>%
            dplyr::ungroup()
        # Join the `total` column to arrow_df
        new_df <- new_df %>% dplyr::full_join(totals_new_df,by = dplyr::join_by("question", "timing"))
        # End of if pre_post == TRUE
    } else if (isFALSE(pre_post)) {
        # If pre_post is FALSE, set up new_df with dataVizCleaning():
        new_df <- dataVizCleaning(df = new_df, pre_post = FALSE, scale_labels = scale_labels, na_remove = TRUE)

        # Get total n for each question, grouped by question:
        # totals_new_df <- new_df %>%
        #   dplyr::group_by(.data[["question"]]) %>%
        #   dplyr::summarize(total = sum(.data[["n_answers"]]), .groups = "keep") %>%
        #   dplyr::ungroup()
        totals_new_df <- {{ df }} %>%
            tidyr::pivot_longer(tidyselect::everything(), names_to = "question", values_to = "response") %>%
            dplyr::mutate(question = stringr::str_remove(.data$question, "cat_")) %>%
            dplyr::group_by(.data[["question"]]) %>%
            dplyr::summarize(total = dplyr::n(), .groups = "keep") %>%
            dplyr::ungroup()
        # Join the `total` column to arrow_df
        new_df <- new_df %>% dplyr::full_join(totals_new_df,by = dplyr::join_by("question"))
    } # End of if pre_post == FALSE

    # If the user supplies a named vector for questions labels:
    if (!is.null(question_labels)) {
        names(question_labels) <- names(question_labels) %>%
            stringr::str_wrap(., width = 20) %>%
            gsub("\n", "<br>", .)
        new_df <- new_df %>%
            dplyr::mutate(question = forcats::fct_recode(.data[["question"]], !!!question_labels))
    }

    # Set up a new question order if not supplied by the user after finding the most positive valenced items for post
    # if pre_post is TRUE, otherwise use questions if pre_post is FALSE (top levels depending on total response levels):
    if (isFALSE(question_order)) {
        if (isTRUE(pre_post)) {
            new_question_order <- questionOrder(df = new_df, pre_post = TRUE)
        } else if (isFALSE(pre_post)) {
            new_question_order <- questionOrder(df = new_df, pre_post = FALSE)
        }
        # change the factor levels of question to be ordered by the question_order:
        new_df <- new_df %>% dplyr::mutate(question = forcats::fct_relevel(.data[["question"]], new_question_order))
    } else if (isTRUE(question_order) && !is.null(question_labels)) {
        # If question_order == TRUE, set up the levels for question using the user supplied order = question_labels:
        new_df <- new_df %>% dplyr::mutate(question = factor(.data[["question"]], levels = names(question_labels)))
    }

    # If statement to handle the value(s) of `fill_colors`:
    if (length(fill_colors) > 1) {
        if (length(fill_colors) >= length(scale_labels)) {
            new_fill_colors <- fill_colors # sets the fill colors to the hex codes passed in by the user.
        } else {
            stop("Error: the length of `fill_colors` needs to be greater than or equal to the length of `scale_labels`")
        }
    } else if (fill_colors == "seq") {
        new_fill_colors <- seqFillColors(length(scale_labels)) # sets the fill colors to the default sequential palette of `cividis`.
    } else if (fill_colors == "div") {
        new_fill_colors <- divFillColors(length(scale_labels)) # sets the fill colors to the default diverging palette of `Blue Red 3` from namespace `colorspace`.
    }

    # Use the internal function labelColorMaker(), to create text color labels of black or white, see `helpers.R`:
    label_colors_named <- labelColorMaker(new_fill_colors, names = scale_labels)
    # create a new col `label_color` using the named vector `label_colors_named` to map the text color to the variable response
    new_df <- new_df %>% dplyr::mutate(., label_color = label_colors_named[.data[["response"]]])

    # Named vector created by new color palette named by the scale_labels:
    new_fill_colors_named <- new_fill_colors # new color palette as values
    names(new_fill_colors_named) <- stringr::str_wrap(scale_labels, width = 10) # str wrapped for legend

    # Named vector created by new color palette named by the scale_labels:
    fill_colors_named <- new_fill_colors # new color palette as values
    names(fill_colors_named) <- scale_labels # pass to scale_fill_manual()

    # Named vector created by new color palette named by the scale_labels:
    fill_colors_legend <- new_fill_colors # new color palette as values
    names(fill_colors_legend) <- stringr::str_wrap(scale_labels, width = 10) # names as str_wrap scale_labels

    # Calculate the width and height of legend keys using `names(new_fill_colors_named)` (i.e. str_wrap scale_labels)
    # Legend key width = maximum label strwidth: PASS TO guide_legend keywidth
    key_width <- grid::unit(max(sapply(names(fill_colors_legend), graphics::strwidth, units = "inches")) * 0.9, "in")
    # Create a minimum default of key_width being at least 0.7 inches:
    key_width <- dplyr::if_else(as.numeric(key_width) > 0.7, as.numeric(key_width), 0.7) %>% grid::unit(., "in")
    # Legend key height = maximum label strheight: PASS TO guide_legend keyheight
    key_height <- grid::unit(max(sapply(names(fill_colors_legend), graphics::strheight, units = "inches")) * 0.95, "in")
    # Create a minimum default of key_height being at least 0.35 inches:
    key_height <- dplyr::if_else(as.numeric(key_height) > 0.35, as.numeric(key_height), 0.35) %>% grid::unit(., "in")

    # Set default width for geom_col() bars if not supplied by user:
    if (is.null(width)) {
        width <- dplyr::if_else(dplyr::n_distinct(new_df[["question"]]) < 4, 0.7, 0.9)
    }

    if (isTRUE(overall_n)) {
        # Return N_df that will be an overall n for all the items, only if all totals_new_df$total are equal to each other: ----
        N_df <- NULL
        if (length(unique(totals_new_df[["total"]])) == 1) {
          # Get overall n if it is the same for each item:
          N_df <- totals_new_df %>%
            dplyr::summarize(N = mean(.data[["total"]])) %>%
            tibble::deframe()
        }
        # Error messages if N_df is null, not filled by last if statement above:
        if (is.null(N_df)) {
          stop("Error: Can not use `overall_n` for this function, responses for variables are not of equal length. Use argument: `overall_n = FALSE`.")
        }
    } else if (isFALSE(overall_n)) { # Otherwise, if overall_n == FALSE, return a stacked_bar_chart with n for each question appended to the question label:
        # # Change the label of the variable "question" by adding n of each to the end of the character string:
        # labels_n_questions <- paste0(totals_new_df[["question"]], " ", "(*n* = ", totals_new_df[["total"]], ")")
        #
        # # Set factor labels for question to labels:
        # new_df <- new_df %>%
        #   dplyr::mutate(question = factor(.data[["question"]], levels = levels(.data[["question"]]), labels = labels_n_questions))
        # Change the label of the variable "question" by adding n of each to the end of the character string:
        # Set up labels for question:
        labels_n_questions <- new_df %>%
            dplyr::mutate(
                labels = paste0(.data[["question"]], "<br>(*n* = ", .data[["total"]], ")"),
                question = as.character(.data[["question"]]),
            ) %>%
            dplyr::distinct(labels, .keep_all = TRUE) %>%
            dplyr::select("labels", "question") %>%
            tibble::deframe()
        # Set factor labels for question to labels = labels_n_questions using the named vector string `labels_n_questions` in fct_recode():
        new_df <- new_df %>%
            dplyr::mutate(question = forcats::fct_recode(.data[["question"]], !!!labels_n_questions))
    }

    # Call stackedBar_ggplot ----
    # Set labels to percent or n_answers:
    if (isTRUE(percent_label)) {
        label_gg <- new_df[["percent_answers_label"]]
    } else {
        label_gg <- new_df[["n_answers"]]
    }


    # Final call to stackedBar_ggplot() for pre_post == TRUE:
    if (isTRUE(pre_post)) {
        stacked_bar_chart <- stackedBar_ggplot(df_gg = new_df, y_gg = "timing", pre_post = TRUE,
                                               label_gg = label_gg, label_colors_named = label_colors_named,
                                               legend_labels = names(fill_colors_legend),
                                               width_gg = width, fill_colors_gg = fill_colors_named,
                                               overall_n_gg = overall_n, N_df_gg = N_df,
                                               key_width = key_width, key_height = key_height)
    # Final call to stackedBar_ggplot() if pre_post == FALSE:
    } else if (isFALSE(pre_post)) {
        stacked_bar_chart <- stackedBar_ggplot(df_gg = new_df, y_gg = "question", pre_post = FALSE,
                                               label_gg = label_gg, label_colors_named = label_colors_named,
                                               legend_labels = names(fill_colors_legend),
                                               width_gg = width, fill_colors_gg = fill_colors_named,
                                               overall_n_gg = overall_n, N_df_gg = N_df,
                                               key_width = key_width, key_height = key_height)
    }

    return(stacked_bar_chart)

}

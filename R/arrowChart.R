#' Arrow Chart for Blackstone Research and Evaluation
#'
#' [arrowChart()] creates an pre-post arrow chart of averages and returns a ggplot object.
#'
#' @param df Required, a [tibble][tibble::tibble-package] or data frame of **numeric** data that has items with the prefix of `pre_` and `post_`.
#'
#' @param scale_labels Required, a character vector of labels for the response scale, must be in the desired order,
#'    e.g. if you have a 5 item scale of minimal to extensive it should look like this: `levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")`.
#'
#' @param arrow_colors Required, defaults to dark blue BRE color code "#283251" for all values, a character vector of hex codes for colors to associate
#'   each item, needs to be the same length or longer than the items to place in the same chart.
#'
#' @param overall_n Logical, default is TRUE. If TRUE, returns an overall *n* for all questions that is in the upper left tag of the plot.
#'    If False, adds *n* to each question/item after the respective labels.
#'
#' @param question_labels Default is NULL. Takes in a named character vector to both supply labels the questions and sort the order of the questions.
#'    The named character vector should have the new labels as the "name" and the old labels as the "variable" sorted in the
#'    desired order of appearing in the plot, first item will appear at the top of the plot. See examples.
#'
#' @param question_order Logical, default is FALSE. If TRUE, the question order will be taken from the user supplied named character vector passed to
#'    question_labels, where the first item will be at the top of the plot and so on. If FALSE, the question order will be the questions with highest
#'    post score average on the top of the plot descending.
#'
#' @param font_family Character value to set the font family for all text in the chart, defaults to "Arial".
#'
#' @param font_size Numeric value to set the font size in points for all text in the chart, defaults to size 10.
#'
#' @return A [ggplot2][ggplot2::ggplot2-package] object that plots the items into a arrow bar chart.
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
#' # Labels for response scales to recode the numeric variables to on the plot:
#' levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")
#' # Question labels as a named vector with the naming structure
#' # like this: c("new label" = "original variable name"):
#' question_labels <- c("Publish a lot of high quality papers" =  "Publish",
#'                      "Write a lot of research papers" = "Write",
#'                      "Research in a lab with faculty" = "Research",
#'                      "Organization of a large research project" = "Organization",
#'                      "Source work for a research paper" = "Source")
#'
#' # Example with n for each question and original labels:
#' arrowChart(df = items, scale_labels = levels_min_ext, overall_n = FALSE,
#'            question_labels = NULL, question_order = FALSE)
#' # With new labels, question_order = FALSE, and overall_n set to TRUE:
#' arrowChart(df = items, scale_labels = levels_min_ext, overall_n = TRUE,
#'            question_labels = question_labels, question_order = FALSE)
#' # With new labels and order taken from question_labels argument, and overall_n set to FALSE:
#' arrowChart(df = items, scale_labels = levels_min_ext, overall_n = FALSE,
#'            question_labels = question_labels, question_order = TRUE)
arrowChart <- function(df, scale_labels, arrow_colors = "#283251", overall_n = TRUE, question_labels = NULL, question_order = FALSE,
                       font_family = "Arial", font_size = 10) {
    # Load fonts:
    extrafont::loadfonts("all", quiet = TRUE)
    . <- NULL # to stop check() from bringing up .

    #  Start of data manipulation: ----
    # Data wrangling to long format, three cols: question, timing and score_avg:
    arrow_df <- {{ df }} %>%
        tidyr::pivot_longer(tidyselect::everything(), names_to = "question", values_to = "response") %>%
        dplyr::mutate(question = stringr::str_remove(.data$question, "_num")) %>%
        tidyr::separate(.data[["question"]], into = c("timing", "question"), sep = "_", extra = "merge") %>%
        dplyr::group_by(.data[["question"]], .data[["timing"]]) %>%
        dplyr::mutate(timing = factor(.data[["timing"]], levels = c("pre", "post"))) %>%
        dplyr::summarize(score_avg = mean(.data[["response"]], na.rm = TRUE), .groups = "keep") %>% # drops NA's
        dplyr::ungroup() %>%
        tidyr::pivot_wider( # pivot wider to add a difference in `score_avg` column
            id_cols = tidyselect::everything(),
            names_from = tidyselect::all_of(c("timing")),
            values_from = tidyselect::all_of(c("score_avg")),
            names_glue = "{timing}_score_avg"
        ) %>%
        dplyr::mutate(diff_score_avg = .data[["post_score_avg"]] - .data[["pre_score_avg"]]) %>%
        tidyr::pivot_longer(tidyselect::all_of(c("pre_score_avg","post_score_avg")),
                            names_to = c("timing", ".value"),
                            names_pattern = "([A-Za-z]+)_(.*)" # puts prefix of timing and then "score_avg" as the value
        )

    # Get total n for each question, grouped by question and timing: ----
    totals_new_df <- {{ df }}  %>%
        tidyr::pivot_longer(tidyselect::everything(), names_to = "question", values_to = "response") %>%
        dplyr::mutate(question = stringr::str_remove(.data$question, "_num")) %>%
        tidyr::separate(.data[["question"]], into = c("timing", "question"), sep = "_", extra = "merge") %>%
        dplyr::group_by(.data[["question"]], .data[["timing"]]) %>%
        dplyr::mutate(timing = factor(.data[["timing"]], levels = c("pre", "post"))) %>%
        dplyr::summarize(total = dplyr::n(), .groups = "keep") %>%
        dplyr::ungroup()
    # Join the `total` column to arrow_df
    arrow_df <- arrow_df %>% dplyr::full_join(totals_new_df,by = dplyr::join_by("question", "timing"))

    # If the user supplies a named vector for questions labels: ----
    if (!is.null(question_labels)) {
        names(question_labels) <- names(question_labels) %>%
            stringr::str_wrap(., width = 20) %>%
            gsub("\n", "<br>", .)
        arrow_df <- arrow_df %>%
            dplyr::mutate(question = forcats::fct_recode(.data[["question"]], !!!question_labels))
    }

    # Set up a new question order if not supplied by the user by using the highest post score_avg: ----
    if (isFALSE(question_order)) {
        # Set up question as a factor and arrange by the top post `score_avg`:
        new_question_order <- arrow_df %>% dplyr::filter(., .data$timing == "post") %>%
            dplyr::arrange(dplyr::desc(.data[["score_avg"]])) %>%
            dplyr::select("question") %>%
            tibble::deframe()
        arrow_df <- arrow_df %>% dplyr::mutate(question = factor(.data[["question"]], levels = new_question_order))
    } else if (isTRUE(question_order) && !is.null(question_labels)) {
        # If FALSE, use user supplied by order based on the set up the levels for question using- names(question_labels):
        arrow_df <- arrow_df %>% dplyr::mutate(question = factor(.data[["question"]], levels = names(question_labels)))
    }


    # Return N_df that will be an overall n for all the items, only if all totals_new_df[["total are equal"]]:
    if (length(unique(totals_new_df[["total"]])) == 1) {
        # Get overall n if it is the same for each item:
        N_df <- totals_new_df %>%
            dplyr::summarize(N = mean(.data[["total"]])) %>%
            tibble::deframe()
    }

    # Set up value of total_question_items
    total_question_items <- nrow(dplyr::distinct(arrow_df, .data[["question"]]))
    # Set up the right length of arrow color fill vector if set to defualt single BRE dark blue color:
    if (length(arrow_colors) > 1) {
        if (length(arrow_colors) >= total_question_items) {
            new_arrow_colors <- arrow_colors # sets the fill colors to the hex codes passed in by the user.
        } else {
            stop("Error: the length of `arrow_colors` needs to be greater than or equal to the length of unique question items")
        }
    } else if (arrow_colors == "#283251") { # default
        new_arrow_colors <- rep("#283251", length.out = total_question_items)
    } else if (length(arrow_colors) == 1) { # if only one color is passed to arrow colors
        new_arrow_colors <- rep(arrow_colors, length.out = total_question_items)
    }

    # Set up scale_labels so that they include the number as well as the likert scale item for each response:
    new_scale_labels <- sapply(seq_along(scale_labels), \(x) paste0(scale_labels[x], "\n(",x,")"))

    # Main calls to ggplot function arrowChartGroup_ggplot(): -----
    # If overall_n == TRUE:
    if (isTRUE(overall_n)) {
        arrow_new <- arrowChart_ggplot(df_gg = arrow_df, fill_gg = new_arrow_colors, scale_labels_gg = new_scale_labels) +
                        addPlotTag(n = N_df, font_size = font_size, font_family = font_family, plot_tag_position = c(0, 0.98)) # see 'gg_helpers.R', re-position with plot_tag_position arg

        return(arrow_new)

    # Otherwise, if overall_n == FALSE and, return an arrow chart with n for each question appended to the question label:
    } else if (isFALSE(overall_n)) {
        # Change the label of the variable "question" by adding n of each to the end of the character string:
        # Set up labels for question:
        labels_n_questions <- arrow_df %>%
            dplyr::mutate(
                labels = paste0(.data[["question"]],"<br>(*n* = ", .data[["total"]], ")"),
                question = as.character(.data[["question"]]),
            ) %>%
            dplyr::distinct(labels, .keep_all = TRUE) %>%
            dplyr::select("labels", "question") %>%
            tibble::deframe()
        # Set factor labels for question to labels = labels_n_questions:
        arrow_df <- arrow_df %>%
            dplyr::mutate(question = forcats::fct_recode(.data[["question"]], !!!labels_n_questions))
        # ggplot call for overall_n == FALSE
        arrow_new <- arrowChart_ggplot(df_gg = arrow_df, fill_gg = new_arrow_colors, scale_labels_gg = new_scale_labels)

        return(arrow_new)
    }

}

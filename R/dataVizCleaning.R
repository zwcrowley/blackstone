#' Creates a data frame that is a summary table of counts and percentages
#'
#' @param df Required, A [tibble][tibble::tibble-package]/data frame of survey items that are categorical/character
#'      variables.
#'
#' @param scale_labels Required, a character vector of labels for the response scale, must be in the desired order,
#'    e.g. if you have a 5 item scale of minimal to extensive it should look like this:
#'    `levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")`.
#'
#' @param pre_post Logical, default is FALSE. If true, returns a [tibble][tibble::tibble-package] with an additional column of `timing`
#'      that is a factor variable of either `Pre` or `Post`.
#'
#' @param na_remove Logical, defaults to TRUE. If TRUE, Drops NA values; if FALSE, turns NA's into "Missing" and adds it as a factor
#'      in the first position of `scale_labels`.
#'
#' @return A [tibble][tibble::tibble-package] with the data in 5 columns: `question`, `response`, `n_answers`, `percent_answers` and `percent_answers_label.`
#'      `question` is the name of the original item, `response` is all of the categorical responses possible for the item. `n_answers` is the count of each response,
#'      `percent_answers` is the percentage of each response and `percent_answers_label` is a character variable of percentage labelled with percent sign for use as
#'      text label. If `pre_post` arg is TRUE, an column of `timing` is added that is a factor variable of either `Pre` or `Post`.
#'
#' @export
#'
#' @examples
#' # Fake data for examples, first are single items and the second has pre-post data with correct prefixes in variable names:
#' items_single <- tibble::tibble(
#'     Organization = c("Minimal", "Slight", "Moderate", "Good", "Extensive", "Good", "Moderate", "Slight", "Minimal"),
#'     Source = c("Slight", "Slight", "Moderate", "Extensive", "Good", "Moderate", "Slight", "Minimal", "Slight"),
#'     Publish = c("Minimal", "Minimal", "Minimal", "Slight", "Slight", "Slight", "Moderate", "Moderate", "Moderate"),
#'     Write = c("Slight", "Slight", "Slight", "Moderate", "Moderate", "Moderate", "Good", "Good", "Good"),
#'     Research = c("Minimal", "Minimal", "Slight", "Slight", "Moderate", "Moderate", "Good", "Good", "Good")
#' )
#'
#' items_pre_post <- tibble::tibble(
#'     pre_Organization = c("Minimal", "Slight", "Moderate", "Good", "Extensive", "Good", "Moderate", "Slight", "Minimal"),
#'     post_Organization = c("Slight", "Moderate", "Good", "Extensive", "Extensive", "Extensive", "Good", "Moderate", "Slight"),
#'     pre_Source = c("Slight", "Slight", "Moderate", "Extensive", "Good", "Moderate", "Slight", "Minimal", "Slight"),
#'     post_Source = c("Good", "Good", "Extensive", "Extensive", "Good", "Extensive", "Good", "Moderate", "Good"),
#'     pre_Publish = c("Minimal", "Minimal", "Minimal", "Slight", "Slight", "Slight", "Moderate", "Moderate", "Moderate"),
#'     post_Publish = c("Moderate", "Moderate", "Moderate", "Good", "Good", "Good", "Extensive", "Extensive", "Extensive"),
#'     pre_Write = c("Slight", "Slight", "Slight", "Moderate", "Moderate", "Moderate", "Good", "Good", "Good"),
#'     post_Write = c("Moderate", "Moderate", "Moderate", "Good", "Good", "Good", "Extensive", "Extensive", "Extensive"),
#'     pre_Research = c("Minimal", "Minimal", "Slight", "Slight", "Moderate", "Moderate", "Good", "Good", "Good"),
#'     post_Research = c("Slight", "Slight", "Moderate", "Moderate", "Good", "Good", "Extensive", "Extensive", "Extensive")
#' )
#' # Add a row of NA values to each fake data set:
#' items_pre_post_na <- rows_append(items_pre_post, as_tibble_row(setNames(rep(NA, NCOL(items_pre_post)), names(items_pre_post))))
#' items_single_na <- rows_append(items_single, as_tibble_row(setNames(rep(NA, NCOL(items_single)), names(items_single))))
#'
#' # Likert scale to pass to `scale_labels` that is the order to arrange each variable:
#' levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")
#'
#' dataVizCleaning(df = items_single, pre_post = FALSE, scale_labels = levels_min_ext, na_remove = TRUE)
#' dataVizCleaning(df = items_single_na, pre_post = FALSE, scale_labels = levels_min_ext, na_remove = FALSE)
#' dataVizCleaning(df = items_pre_post, pre_post = TRUE, scale_labels = levels_min_ext, na_remove = TRUE)
#' dataVizCleaning(df = items_pre_post_na, pre_post = TRUE, scale_labels = levels_min_ext, na_remove = FALSE)
dataVizCleaning <- function(df, scale_labels, pre_post = TRUE, na_remove = TRUE) {

    if (isTRUE(na_remove)) { # NA's are dropped in this first section:
        # Start of data manipulation: ----
        # Make sure the all vars in df are factors with scale_labels as their levels:
        new_df <- {{ df }} %>% dplyr::mutate(dplyr::across(tidyselect::everything(), ~ factor(., levels = scale_labels)))

        if (isTRUE(pre_post)) { # Processes the data with "pre_" and "post_" prefixes, adds a `timing` var:
            # For pre-post data:
            # Test if all vars contain c("pre_", "post_"), if not then stop and return an error message:
            test_names <- {{ df }} %>% names() %>% str_detect(., paste(c("pre_", "post_"), collapse = "|"))
            if (any(test_names == FALSE)) {
                stop("the variables do not have `pre_` and/or `post_` prefixes, makes sure all variables have the correct prefixes.")
            }

            # Sets up new_df:
            new_df <- new_df %>%
                tidyr::pivot_longer(contains(c("pre_", "post_")), names_to = "question", values_to = "response") %>%
                dplyr::mutate(question = stringr::str_remove(.data$question, "cat_")) %>%
                tidyr::separate(.data$question, into = c("timing", "question"), sep = "_", extra = "merge") %>%
                dplyr::mutate(response = factor(.data$response, levels = scale_labels)) %>%
                dplyr::group_by(.data$question, .data$timing, .data$response) %>%
                dplyr::summarize(n_answers = dplyr::n(), .groups = "keep") %>%
                dplyr::ungroup() %>%
                tidyr::drop_na() %>% # drops NA's
                dplyr::group_by(.data$question, .data$timing) %>%
                dplyr::mutate(
                    percent_answers = .data$n_answers / sum(.data$n_answers),
                    percent_answers_label = scales::percent(.data$percent_answers, accuracy = 1),
                    timing = stringr::str_to_title(.data$timing), # capitalize timing
                    timing = factor(.data$timing, levels = c("Pre", "Post")),
                    response = factor(.data$response, levels = scale_labels)
                ) %>%
                dplyr::ungroup() %>%
                dplyr::arrange(.data$question, .data$timing, .data$response)
            # end of if pre_post == TRUE
        } else if (isFALSE(pre_post)) {
            # If pre_post is FALSE, set up new_df:
            new_df <- new_df %>%
                tidyr::pivot_longer(tidyselect::everything(), names_to = "question", values_to = "response") %>%
                dplyr::mutate(question = stringr::str_remove(.data$question, "cat_"),
                              response = factor(.data$response, levels = scale_labels)) %>%
                dplyr::group_by(.data$question, .data$response) %>%
                dplyr::summarize(n_answers = dplyr::n(), .groups = "keep") %>%
                dplyr::ungroup() %>%
                tidyr::drop_na() %>% # drops NA's
                dplyr::group_by(.data$question) %>%
                dplyr::mutate(
                    percent_answers = .data$n_answers / sum(.data$n_answers),
                    percent_answers_label = scales::percent(.data$percent_answers, accuracy = 1)
                ) %>%
                dplyr::ungroup() %>%
                dplyr::arrange(.data$question, .data$response)
        } # end of if pre_post == FALSE
        # end of if na_remove == TRUE
    } else if (isFALSE(na_remove)) { # Code section to keep NA's: recode as `Missing` and count as the lowest factor:
        # Add missing as the lowest level of `scale_labels`:
        scale_labels <- append(scale_labels, "Missing", after = 0)
        # # Start of data manipulation: ----
        # changes NA to "Missing" and then all vars in df are factors with scale_labels as their levels:
        new_df <- {{ df }} %>% dplyr::mutate(dplyr::across(tidyr::everything(), ~ case_when(is.na(.) ~ "Missing", TRUE ~ .)),
                                             dplyr::across(tidyselect::everything(), ~ factor(., levels = scale_labels)))

        if (isTRUE(pre_post)) { # Processes the data with "pre_" and "post_" prefixes, adds a `timing` var:
            # For pre-post data:
            # Test if all vars contain c("pre_", "post_"), if not then stop and return an error message:
            test_names <- {{ df }} %>% names() %>% str_detect(., paste(c("pre_", "post_"), collapse = "|"))
            if (any(test_names == FALSE)) {
                stop("the variables do not have `pre_` and/or `post_` prefixes, makes sure all variables have the correct prefixes.")
            }
            # Process `new_df` for pre_post == TRUE
            new_df <- new_df %>%
                tidyr::pivot_longer(contains(c("pre_", "post_")), names_to = "question", values_to = "response") %>%
                dplyr::mutate(question = stringr::str_remove(.data$question, "cat_")) %>%
                tidyr::separate(.data$question, into = c("timing", "question"), sep = "_", extra = "merge") %>%
                dplyr::mutate(response = factor(.data$response, levels = scale_labels)) %>%
                dplyr::group_by(.data$question, .data$timing, .data$response) %>%
                dplyr::summarize(n_answers = dplyr::n(), .groups = "keep") %>%
                dplyr::ungroup() %>%
                dplyr::group_by(.data$question, .data$timing) %>%
                dplyr::mutate(
                    percent_answers = .data$n_answers / sum(.data$n_answers),
                    percent_answers_label = scales::percent(.data$percent_answers, accuracy = 1),
                    timing = stringr::str_to_title(.data$timing), # capitalize timing
                    timing = factor(.data$timing, levels = c("Pre", "Post"))
                ) %>%
                dplyr::ungroup() %>%
                dplyr::arrange(.data$question, .data$timing, .data$response)
            # end of if pre_post == TRUE
        } else if (isFALSE(pre_post)) {
            # If pre_post is FALSE, set up new_df, which already has NA's recoded to missing from above:
            new_df <- new_df  %>%
                tidyr::pivot_longer(tidyselect::everything(), names_to = "question", values_to = "response") %>%
                dplyr::mutate(question = stringr::str_remove(.data$question, "cat_"),
                              response = factor(.data$response, levels = scale_labels)) %>%
                dplyr::group_by(.data$question, .data$response) %>%
                dplyr::summarize(n_answers = dplyr::n(), .groups = "keep") %>%
                dplyr::ungroup() %>%
                dplyr::group_by(.data$question) %>%
                dplyr::mutate(
                    percent_answers = .data$n_answers / sum(.data$n_answers),
                    percent_answers_label = scales::percent(.data$percent_answers, accuracy = 1)
                ) %>%
                dplyr::ungroup() %>%
                dplyr::arrange(.data$question, .data$response)
        } # end of if pre_post == FALSE
    } # end of if na_remove == FALSE

    return(new_df)
}

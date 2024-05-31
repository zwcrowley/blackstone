#' Helper Functions for the `bre` package
#'
#'
#' Helper function to get question order for bar charts
#'
#' @description Helper function to get question order for bar charts when not supplied by user. Takes the
#'      highest post value of the highest `response` columns.
#'
#' @return A vector of variable names in order of highest valenced response items.
#'
#' @noRd
questionOrder <- function(df, pre_post = TRUE) {
    . <- NULL # Set . to NULL to stop message when using dot notation in mutate:
    if (isTRUE(pre_post)) {
        var_order <- {{ df }} %>% dplyr::filter(., .data$timing == "Post") %>% tidyr::complete(.data$question, .data$response) %>%
            tidyr::pivot_wider(id_cols = -c("timing", "percent_answers", "percent_answers_label"), names_from = "response", values_from = "n_answers") %>%
            dplyr::group_by(.data$question) %>% rev() %>%
            dplyr::arrange(dplyr::across(-c("question"), dplyr::desc)) %>%
            dplyr::select("question") %>%
            unique() %>%
            tibble::deframe()
    } else if (isFALSE(pre_post)) {
        var_order <- {{ df }} %>% tidyr::complete(.data$question, .data$response) %>%
            tidyr::pivot_wider(id_cols = -c("percent_answers", "percent_answers_label"), names_from = "response", values_from = "n_answers") %>%
            dplyr::group_by(.data$question) %>% rev() %>%
            dplyr::arrange(dplyr::across(-c("question"), dplyr::desc)) %>%
            dplyr::select("question") %>%
            unique() %>%
            tibble::deframe()
    }
    return(var_order)
}

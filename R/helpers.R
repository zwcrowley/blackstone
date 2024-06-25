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
        var_order <- {{ df }} %>% dplyr::filter(., .data$timing == "Post") %>%
                                  dplyr::select(c("question","response","n_answers")) %>%
                                  tidyr::complete(.data$question, .data$response) %>%
                                  tidyr::pivot_wider(names_from = "response", values_from = "n_answers") %>%
                                  dplyr::group_by(.data$question) %>% rev() %>%
                                  dplyr::arrange(dplyr::across(-c("question"), dplyr::desc)) %>%
                                  dplyr::select("question") %>%
                                  dplyr::distinct() %>%
                                  tibble::deframe() %>%
                                  as.character()
        # var_order <- {{ df }} %>% dplyr::filter(., .data$timing == "Post") %>% tidyr::complete(.data$question, .data$response) %>%
        #     tidyr::pivot_wider(id_cols = -c("timing", "percent_answers", "percent_answers_label"), names_from = "response", values_from = "n_answers") %>%
        #     dplyr::group_by(.data$question) %>% rev() %>%
        #     dplyr::arrange(dplyr::across(-c("question"), dplyr::desc)) %>%
        #     dplyr::select("question") %>%
        #     unique() %>%
        #     tibble::deframe()
    } else if (isFALSE(pre_post)) {
        var_order <- {{ df }} %>% dplyr::select(c("question","response","n_answers")) %>%
                                  tidyr::complete(.data$question, .data$response) %>%
                                  tidyr::pivot_wider(names_from = "response", values_from = "n_answers") %>%
                                  dplyr::group_by(.data$question) %>% rev() %>%
                                  dplyr::arrange(dplyr::across(-c("question"), dplyr::desc)) %>%
                                  dplyr::select("question") %>%
                                  dplyr::distinct() %>%
                                  tibble::deframe() %>%
                                  as.character()
    }
    return(var_order)
}


#' Get path to `bre` example data
#'
#' @description `bre` comes bundled with some example files in its `inst/extdata`
#'      directory. This function make them easy to access.
#'
#' @param path Name of file. If `NULL`, all of the example files will be listed.
#' @importFrom fs path_package
#' @export
#' @examples
#' bre_example()
#' bre_example("fake_data.csv")
bre_example <- function(path = NULL) {
    if (is.null(path)) {
        dir(fs::path_package("extdata", package = "bre"))
    } else {
        fs::path_package("extdata", path, package = "bre")
    }
}

#' Get path to `blackstone` example data
#'
#' @description `blackstone` comes bundled with some example files in its `inst/extdata`
#'      directory. This function make them easy to access.
#'
#' @param path Name of file. If `NULL`, all of the example files will be listed.
#' @importFrom fs path_package
#' @export
#' @examples
#' blackstoneExample()
#' blackstoneExample("fake_data.csv")
blackstoneExample <- function(path = NULL) {
    if (is.null(path)) {
        dir(fs::path_package("extdata", package = "blackstone"))
    } else {
        fs::path_package("extdata", path, package = "blackstone")
    }
}

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

#' Helper function to italicize `n` in header names of flextable objects
#'
#' @description Helper function that can be used to apply italics to 'n' dynamically
#'      using regular expressions
#'
#' @return A flextable as_paragraph chunk that is formatted as a header name for flextable object.
#'
#' @noRd
italicize_n_header <- function(header_name) {
    # Match both "(n =" and "n =" or "\n(n"
    if (stringr::str_detect(header_name, "\\(?n =|\\n\\(n")) {
        # Split the string into parts: before and after 'n ='
        parts <- stringr::str_split(header_name, "(?<=\\()n =|n =", n = 2, simplify = TRUE)

        # Get prefix and suffix
        prefix <- parts[1]  # Text before "n ="
        suffix <- parts[2]  # Text after "n ="

        # Replace \n with a newline in the prefix
        prefix <- stringr::str_replace(prefix, "\\n", "\n")

        # Create the as_paragraph expression for the header
        flextable::as_paragraph(prefix, flextable::as_i("n"), " = ", suffix)
    } else {
        # If no "n" pattern is found, return the name as a paragraph
        flextable::as_paragraph(header_name)
    }
}

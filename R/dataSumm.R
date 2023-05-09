#' Creates a summary table of counts and percentages
#'
#' @param var A column selected from a tibble/data frame that is a categorical/factor variable to that to be summarized into a table.
#'
#' @return a new tibble/data frame with the data in 5 columns: item, response, n_answers, percent_answers and percent_answers_label.
#' Item is the name of the original item, Response is all of the categorical responses possible for the item. n_answers is the count of each response,
#' percent_answers is the percentage of each response and percent_answers_label is a character variable of percentage labelled with percent sign for use as a label.
#'
#' @export
#'
#' @examples
#' data <- dplyr::tibble(
#'   role = c(
#'     "Faculty", "Postdoc", "Undergraduate student", "Graduate student",
#'     "Graduate student", "Postdoc", "Postdoc", "Faculty",
#'     "Faculty", "Graduate student", "Graduate student", "Postdoc",
#'     "Faculty", "Faculty", "Faculty", "Faculty", "Faculty", "Graduate student",
#'     "Undergraduate student","Undergraduate student"
#'   )
#' )
#'
#' data %>%
#'   dplyr::select(role) %>%
#'   dataSumm()
dataSumm <- function(var) {
  clean_df <- {{ var }} %>%
    tidyr::drop_na() %>%
    tidyr::pivot_longer(tidyselect::everything(), names_to = "question", values_to = "response") %>%
    dplyr::group_by(.data$question, .data$response) %>%
    dplyr::summarize(n_answers = dplyr::n(), .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$question) %>%
    dplyr::mutate(percent_answers = .data$n_answers / sum(.data$n_answers)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(percent_answers_label = scales::percent(.data$percent_answers, accuracy = 1)) %>%
    dplyr::arrange(dplyr::desc(.data$n_answers)) %>%
    dplyr::mutate(response = forcats::fct_inorder(.data$response))
  return(clean_df)
}

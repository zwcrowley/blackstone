#' Creates a summary table of counts and percentages from a data frame pre-processed with the
#'  dataSumm() and returns a flextable object.
#'
#' @param df a [tibble][tibble::tibble-package] or data frame pre-processed with the dataSumm() so that it has a summary that includes
#' 5 columns: item, response, n_answers, percent_answers and percent_answers_label.
#' Item is the name of the original item, Response is all of the categorical responses possible
#' for the item. n_answers is the count of each response,
#' percent_answers is the percentage of each response and percent_answers_label is a
#' character variable of percentage labelled with percent sign for use as a label.
#'
#' @param totals If true, returns a summary table with a last row of totals,
#' if false, does not have a final row of totals. Set True by default.
#'
#' @return a [flextable][flextable::flextable-package] object with the 3 columns, response, counts and percentages,
#' Colors are set to The Mark USA branding
#' @export
#'
#' @examples
#' data <- dplyr::tibble(
#'   role = c(
#'     "Faculty", "Postdoc", "Undergraduate student", "Graduate student",
#'     "Graduate student", "Postdoc", "Postdoc", "Faculty",
#'     "Faculty", "Graduate student", "Graduate student", "Postdoc",
#'     "Faculty", "Faculty", "Faculty", "Faculty", "Faculty", "Graduate student",
#'     "Undergraduate student", "Undergraduate student"
#'   )
#' )
#'
#' role_summ <- data %>%
#'   dplyr::select(role) %>%
#'   TheMarkUSA::dataSumm()
#'
#' role_summ %>% tblSumm()
tblSumm <- function(df, totals = TRUE) {
  . <- NULL

  if (isTRUE(totals)) {
    tbl <- {{ df }} %>%
      dplyr::select(.data$response, .data$percent_answers_label, .data$n_answers) %>%
      janitor::adorn_totals("row") %>%
      flextable::flextable() %>%
      flextable::set_header_labels(response = "Response", percent_answers_label = "Percent", n_answers = "Count") %>%
      flextable::align(j = 2:flextable::ncol_keys(.), align = "center", part = "all") %>%
      flextable::fontsize(size = 10, part = "header") %>%
      flextable::fontsize(size = 9, part = "body") %>%
      flextable::hline(part = "all", border = officer::fp_border(color = "gray")) %>%
      flextable::bg(bg = "#2C2C4F", part = "header") %>%
      flextable::color(color = "white", part = "header") %>%
      flextable::bold(part = "header", bold = TRUE) %>%
      flextable::bold(i = flextable::nrow_part(., part = "body"), bold = TRUE) %>%
      flextable::bg(i = flextable::nrow_part(., part = "body"), bg = "#f6f6f6", part = "body") %>%
      flextable::hline(i = flextable::nrow_part(., part = "body") - 1, part = "body", border = officer::fp_border(color = "black")) %>%
      flextable::autofit(part = "all")
  } else {
    tbl <- {{ df }} %>%
      dplyr::select(.data$response, .data$percent_answers_label, .data$n_answers) %>%
      flextable::flextable() %>%
      flextable::set_header_labels(response = "Response", percent_answers_label = "Percent", n_answers = "Count") %>%
      flextable::align(j = 2:flextable::ncol_keys(.), align = "center", part = "all") %>%
      flextable::fontsize(size = 10, part = "header") %>%
      flextable::fontsize(size = 9, part = "body") %>%
      flextable::hline(part = "all", border = officer::fp_border(color = "gray")) %>%
      flextable::bg(bg = "#2C2C4F", part = "header") %>%
      flextable::color(color = "white", part = "header") %>%
      flextable::bold(part = "header", bold = TRUE) %>%
      flextable::autofit(part = "all")
  }

  return(tbl)
}

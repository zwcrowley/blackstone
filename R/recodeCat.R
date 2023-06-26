#' Recode Numeric Variables to Factor Variables
#'
#' @param df Required, a [tibble][tibble::tibble-package]/data frame of survey items that are numeric variables that need to be converted
#' into factor variables, Can be anywhere from 3 to 7 point scales.
#'
#' @param scale_labels Required, a character vector of labels of the desired scale levels. The function will use this vector to convert
#' the numeric variables into factor variables, must be arranged low to high with the exact number of levels as the data contains,
#' or else NA will be returned for variables outside the range of user supplied values.
#'
#' @param number_levels A character vector that of all the numeric values original numeric variables that are to be recoded, in the correct order.
#' Both scale_labels and number_levels should be in the same order that the user wants the variables to be recoded. For example, if
#' a variable from df has 3 numeric values of 1,2,and 3, to be recoded to as "Minimal", "Slight", "Moderate",
#' number_levels should equal: c(1,2,3) and scale_labels should equal: c("Minimal", "Slight", "Moderate"). See more examples below.
#' Defaults to NULL.
#'
#' @return a [tibble][tibble::tibble-package] with the original numeric variables along with
#'   new variables that are now factors with the prefix "cat_{variable_name}", with levels taken from scale_labels character
#'   vector.
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
#' levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")
#' recodeCat(df = items, scale_labels = levels_min_ext)
recodeCat <- function(df, scale_labels , number_levels = NULL) {

  if (is.null(number_levels)) {
    # Changes scale_labels to tibble pulls out index and saves that as a vector, gets number of levels from scale_labels:
    number_levels <- scale_labels %>% tibble::enframe() %>% dplyr::select("name") %>% tibble::deframe()
  }

  # IF/ELSE statement, first if number_levels equals 3:
  if (length(number_levels) == 3) {
    new_df <- df %>%
      dplyr::mutate(dplyr::across(
        tidyselect::where(is.numeric), ~ factor(
          dplyr::case_match(
            ., number_levels[1] ~ scale_labels[1],
            number_levels[2] ~ scale_labels[2],
            number_levels[3] ~ scale_labels[3]), levels = scale_labels), .names = "cat_{.col}"))

    # If number_levels) == 4
  } else if (length(number_levels) == 4) {
    new_df <- df %>%
      dplyr::mutate(dplyr::across(
        tidyselect::where(is.numeric), ~ factor(
          dplyr::case_match(
            ., number_levels[1] ~ scale_labels[1],
            number_levels[2] ~ scale_labels[2],
            number_levels[3] ~ scale_labels[3],
            number_levels[4] ~ scale_labels[4]), levels = scale_labels), .names = "cat_{.col}"))

    # If number_levels) == 5
  } else if (length(number_levels) == 5) {
    new_df <- df %>%
      dplyr::mutate(dplyr::across(
        tidyselect::where(is.numeric), ~ factor(
          dplyr::case_match(
            ., number_levels[1] ~ scale_labels[1],
            number_levels[2] ~ scale_labels[2],
            number_levels[3] ~ scale_labels[3],
            number_levels[4] ~ scale_labels[4],
            number_levels[5] ~ scale_labels[5]), levels = scale_labels), .names = "cat_{.col}"))

    # If number_levels) == 6
  } else if (length(number_levels) == 6) {
    new_df <- df %>%
      dplyr::mutate(dplyr::across(
        tidyselect::where(is.numeric), ~ factor(
          dplyr::case_match(
            ., number_levels[1] ~ scale_labels[1],
            number_levels[2] ~ scale_labels[2],
            number_levels[3] ~ scale_labels[3],
            number_levels[4] ~ scale_labels[4],
            number_levels[5] ~ scale_labels[5],
            number_levels[6] ~ scale_labels[6]), levels = scale_labels), .names = "cat_{.col}"))
    # If number_levels) == 7
  } else if (length(number_levels) == 7) {
    new_df <- df %>%
      dplyr::mutate(dplyr::across(
        tidyselect::where(is.numeric), ~ factor(
          dplyr::case_match(
            ., number_levels[1] ~ scale_labels[1],
            number_levels[2] ~ scale_labels[2],
            number_levels[3] ~ scale_labels[3],
            number_levels[4] ~ scale_labels[4],
            number_levels[5] ~ scale_labels[5],
            number_levels[6] ~ scale_labels[6],
            number_levels[7] ~ scale_labels[7]), levels = scale_labels), .names = "cat_{.col}"))

  }

  return(new_df)
}

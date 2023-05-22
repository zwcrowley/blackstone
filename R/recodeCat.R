#' Recode Numeric Variables to Factors Variables
#'
#' @param df A tibble/data frame of survey items that are numeric
#'   variables that need to be converted into factor variables.
#'
#' @param scale_labels character vector of levels to set the numeric variables
#'   to factor/categorical values
#'
#' @return new tibble/data frame with the original numeric variables along with
#'   new variables mapped to categorical values from scale_labels character
#'   vector.
#' @export
#'
#' @examples
#' items <- dplyr::tibble(
#'   Pre_Organization = c(1, 2, 3, 4, 5, 4, 3, 2, 1),
#'   Post_Organization = dplyr::if_else(Pre_Organization < 5, Pre_Organization + 1, Pre_Organization),
#'   Pre_Source = c(2, 2, 3, 5, 4, 3, 2, 1, 2),
#'   Post_Source = dplyr::if_else(Pre_Source < 4, Pre_Source + 2, Pre_Source),
#'   Pre_Publish = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   Post_Publish = Pre_Publish + 2,
#'   Pre_Write = c(2, 2, 2, 3, 3, 3, 4, 4, 4),
#'   Post_Write = Pre_Write + 1,
#'   Pre_Research = c(1, 1, 2, 2, 3, 3, 4, 4, 4),
#'   Post_Research = Pre_Research + 1
#' )
#' levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")
#' recodeCat(items, levels_min_ext)
recodeCat <- function(df, scale_labels) {

  # Changes scale_labels to tibble pulls out index and saves that as a vector, gets number of levels from scale_labels:
  number_levels <- scale_labels %>% tibble::enframe() %>% dplyr::select("name") %>% tibble::deframe()

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

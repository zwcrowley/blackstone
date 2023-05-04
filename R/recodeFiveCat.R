#' Recode Numeric Variables to 5 Category Factors
#'
#' @param df A tibble/data frame of survey items that are numeric
#'   variables, in 5 point scales and pre-post, that need to be converted into factor variables.
#'
#' @param set_5_levels character vector of 5 levels to set the numeric variables
#'   to factor/categorical values
#'
#' @return new tibble/data frame with the original numeric variables along with
#'   new variables mapped to categorical values from set_5_levels character
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
#' recodeFiveCat(items, levels_min_ext)
recodeFiveCat <- function(df, set_5_levels) {
  new_df <- df %>% dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ factor(dplyr::case_match(
    ., 1 ~ set_5_levels[1],
    2 ~ set_5_levels[2],
    3 ~ set_5_levels[3],
    4 ~ set_5_levels[4],
    5 ~ set_5_levels[5]
  ), levels = set_5_levels), .names = "cat_{.col}"))
  return(new_df)
}

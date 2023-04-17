#' Recode Numeric Variables to 5 Category Factors
#'
#' @param df A tibble/data frame of survey items that are categorical/character variables, in 5 point scales and pre-post, that will be inserted into a stacked bar chart with The Mark USA branding.
#'
#' @param set_5_levels character vector of 5 levels to set the numeric variables to factor/categorical values
#'
#' @return new tibble/data frame with the original numeric variables along with new variables mapped to categorical values from set_5_levels character vector.
#' @export
#'
#' @examples
#' items <- dplyr::tibble(
#'   Pre_Orgs = c(1, 2, 3, 4, 5, 4, 3, 2, 1),
#'   Post_Orgs = c(1, 2, 3, 4, 5, 4, 3, 2, 1)
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

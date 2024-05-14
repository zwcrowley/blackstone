#' Recode Numeric Variables to Factor Variables
#'
#' @param df Required, a [tibble][tibble::tibble-package]/data frame of survey items that are numeric variables that need to be converted
#' into factor variables, Can be anywhere from 3 to 7 point scales.
#'
#' @param scale_labels Required, a named character vector of labels of the desired scale levels for the new factor variables.
#'  The function will use this vector to convert the numeric variables into factor variables, all levels must be supplied in the correct range
#'  otherwise else NA will be returned for variables outside the range of user supplied values. The named character vector should have the new
#'  labels as the "name" and the old labels as the "variable" like this: c("<new label>" = "<original variable value>") which would look like this:
#'   `levels_min_ext <- c("Minimal" = "1", "Slight" = "2", "Moderate" = "3", "Good" = "4", "Extensive" = "5")`
#'
#' @return a [tibble][tibble::tibble-package] with the original numeric variables along with
#'   new variables that are now factors with the prefix `cat_{variable_name}`, with levels taken from scale_labels character
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
#'
#' levels_min_ext <- c("Minimal" = "1", "Slight" = "2", "Moderate" = "3",
#'                      "Good" = "4", "Extensive" = "5")
#'
#' recodeCat(df = items, scale_labels = levels_min_ext)
recodeCat <- function(df, scale_labels) {

  # First, convert the numeric variables to factor and set as levels from the variables in the user supplied named vector: scale_labels
  new_df <- df %>% dplyr::mutate(dplyr::across(
    tidyselect::where(is.numeric), ~ factor(., levels = scale_labels)))

  # Second, recode the variables using names(scale_labels) as the factor levels, create new variables with the prefix "cat_", and select those columns:
  new_df <- new_df %>% dplyr::mutate(
    dplyr::across(
      tidyselect::everything(), ~ factor(
        forcats::fct_recode(., !!!scale_labels), levels = names(scale_labels)), .names = "cat_{.col}")) %>%
    dplyr::select(tidyselect::starts_with("cat_"))
  # Finally, bind the columns of the original df with the columns of new_df:
  new_df <- dplyr::bind_cols(df, new_df)

  return(new_df)
}

#' Clean Up and Format Open-ended Text
#'
#' @param df Required, A [tibble][tibble::tibble-package]/data frame containing the character variable of text.
#'
#' @param var Required, the character variable to clean up from the [tibble][tibble::tibble-package]/data frame, needs to be in quotes.
#'
#' @param remove_values Required, a character vector of additional text to remove from the text, see example.
#'
#' @return A [tibble][tibble::tibble-package] containg one character variable of clean text ready for use or output.
#' @importFrom stringi stri_rand_lipsum
#' @export
#'
#' @examples
#' # Example data:
#' #  Training usefulness composite scale- 5 variables of that make up a scale:
#' # Responsible, Ethics, Standards, Practices, Morals
#' #  these are all on a 5-point likert scale of 1 to 5 needs to be
#' #  recoded to: c("Not at all useful", "Slightly useful", "Somewhat useful",
#' #                "Very useful", "Extremely useful")
#' # levels useful:
#' levels_useful <- c("Not at all useful", "Slightly useful", "Somewhat useful",
#'                    "Very useful", "Extremely useful")
#' # Data:
#' data <- dplyr::tibble(
#'  Responsible = sample(levels_useful, size = 100, replace = TRUE,
#'                        prob = c(0.1, 0.2, 0.3, 0.2, 0.1)),
#'  Ethics = sample(levels_useful, size = 100, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.2, 0.1)),
#'  Standards = sample(levels_useful, size = 100, replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.3)),
#'  Practices = sample(levels_useful, size = 100, replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.3)),
#'  Morals = sample(levels_useful, size = 100, replace = TRUE, prob = c(0.05, 0.05, 0.2, 0.3, 0.4)),
#'  Responsible_oe = ifelse(Responsible == "Not at all useful",
#'                  stringi::stri_rand_lipsum(sample(1:3, replace = TRUE,
#'                  prob = c(0.4, 0.2, 0.1))), NA_character_),
#'  Ethics_oe = ifelse(Ethics == "Not at all useful",
#'                  stringi::stri_rand_lipsum(sample(1:3, replace = TRUE,
#'                  prob = c(0.4, 0.2, 0.1))), NA_character_),
#'  Standards_oe = ifelse(Standards == "Not at all useful",
#'                  stringi::stri_rand_lipsum(sample(1:3, replace = TRUE,
#'                  prob = c(0.4, 0.2, 0.1))), NA_character_),
#'  Practices_oe = ifelse(Practices == "Not at all useful",
#'                  stringi::stri_rand_lipsum(sample(1:3, replace = TRUE,
#'                  prob = c(0.4, 0.2, 0.1))), NA_character_),
#'  Morals_oe = ifelse(Morals == "Not at all useful",
#'                  stringi::stri_rand_lipsum(sample(1:3, replace = TRUE,
#'                  prob = c(0.4, 0.2, 0.1))), NA_character_)
#'  ) %>% dplyr::select(dplyr::ends_with("_oe"))
#'
#' # Set up character vector of text or other things like punctuation to remove from the text data:
#' remove_values <- c("N/A", ".", "A")
#'
#' # Make a nice table with the function:
#' data %>% openendedCleanup(., "Responsible_oe", remove_values)
openendedCleanup <- function(df, var, remove_values) {
    # Set . to NULL to stop message when using dot notation in functions:
    . <- NULL

    clean_data <- {{ df }} %>% dplyr::select(tidyselect::all_of(var)) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::if_else(. %in% remove_values, NA_character_, .)),
                      dplyr::across(dplyr::where(is.character), ~ stringr::str_wrap(stringr::str_to_sentence(.), width = 80))) %>%
        tidyr::drop_na() %>% dplyr::arrange({{ var }})

    return(clean_data)
}

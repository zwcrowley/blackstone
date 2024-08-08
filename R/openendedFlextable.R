#' Create a Formatted Flextable for Open-ended Text
#'
#' @param df Required, A [tibble][tibble::tibble-package] or data frame containing the character variable of text.
#'
#' @param header_label Required, label for the header of the table (can be a description of the prompt or full question text).
#'
#' @return a [flextable][flextable::flextable-package] object that is nicely formatted in BRE branding and in alphabetically order for randomization.
#'
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
#'                  stringi::stri_rand_lipsum(sample(1:100)), NA_character_),
#'  Ethics_oe = ifelse(Ethics == "Not at all useful",
#'                  stringi::stri_rand_lipsum(sample(1:100)), NA_character_),
#'  Standards_oe = ifelse(Standards == "Not at all useful",
#'                  stringi::stri_rand_lipsum(sample(1:100)), NA_character_),
#'  Practices_oe = ifelse(Practices == "Not at all useful",
#'                  stringi::stri_rand_lipsum(sample(1:100)), NA_character_),
#'  Morals_oe = ifelse(Morals == "Not at all useful",
#'                  stringi::stri_rand_lipsum(sample(1:100)), NA_character_)
#'  ) %>% dplyr::select(dplyr::ends_with("_oe"))
#'
#' # Set up character vector of text or other things like punctuation to remove from the text data:
#' remove_values <- c("N/A", ".", "A")
#'
#' # Make a nice table after cleaning up the responses from the variable "Responsible_oe":
#' data %>% blackstone::openendedCleanup(., "Responsible_oe", remove_values) %>%
#'   openendedFlextable(., header_label = "Made up text example in a nicely formatted table")
openendedFlextable <- function(df, header_label) {
    extrafont::loadfonts("all", quiet = TRUE)
    flextable::set_flextable_defaults(font.family = "Arial")

    header_label <- stringr::str_wrap(header_label, width = 80)
    ref_table <- data.frame(key = colnames({{ df }}), label = header_label)
    tbl <- {{ df }} %>% flextable::flextable() %>%
        flextable::set_header_df(mapping = ref_table, key = "key") %>%
        flextable::theme_zebra() %>%
        flextable::fontsize(size = 11, part = "header") %>%
        flextable::fontsize(size = 10, part = "body") %>%
        flextable::bg(bg = blackstoneColors["dark_blue"], part = "header") %>%
        flextable::color(color = "white", part = "header") %>%
        flextable::autofit(part = "all")
    return(tbl)
}

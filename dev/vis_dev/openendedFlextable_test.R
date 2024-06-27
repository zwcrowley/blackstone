#' Helper function to create flextable from a variable of open-ended text reponses
#'
#' @description A function to make the Open-Ended full table: pass a tibble/df to df, and header_label is a character vector of the text of the question to be
#' used as the header for the table:
#'
#' @return a flextable object or open-ended text.
#'
#' @noRd
openendedFlextable <- function(df, header_label) {
    header_label <- stringr::str_wrap(header_label, width = 80)
    ref_table <- data.frame(key = colnames({{ df }}), label = header_label)
    tbl <- {{ df }} %>% flextable::flextable() %>%
        flextable::set_header_df(mapping = ref_table, key = "key") %>%
        flextable::theme_zebra() %>%
        flextable::fontsize(size = 11, part = "header") %>%
        flextable::fontsize(size = 10, part = "body") %>%
        flextable::bg(bg = "#2C2C4F", part = "header") %>%
        flextable::color(color = "white", part = "header") %>%
        flextable::autofit(part = "all")
    return(tbl)
}

library(magrittr)
# levels useful:
levels_useful <- c("Not at all useful", "Slightly useful", "Somewhat useful",
                    "Very useful", "Extremely useful")
 # Data:
data <- dplyr::tibble(
    Responsible = sample(levels_useful, size = 100, replace = TRUE,
                          prob = c(0.1, 0.2, 0.3, 0.2, 0.1)),
    Responsible_oe = ifelse(Responsible == "Not at all useful",
                            stringi::stri_rand_lipsum(sample(1:100)), NA_character_)
    ) %>% dplyr::select(dplyr::ends_with("_oe"))

# Set up character vector of text or other things like punctuation to remove from the text data:
remove_values <- c("N/A", ".", "A")

# Make a nice table with the function:
data %>% blackstone::openendedCleanup(., "Responsible_oe", remove_values) %>% openendedFlextable(., header_label = "Made up text example in a nicely formatted table")

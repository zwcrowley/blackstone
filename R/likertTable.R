#' Summary Table for Likert Items for Blackstone Research and Evaluation
#'
#' [likertTable()] creates a summary table of frequencies and percentages for Likert scale items that can show breakdowns for
#'      the data passed to it. The table that contains frequency and percent in each row and a column for each Likert scale response,
#'      all items must have the same scale labels.
#'
#' @param df Required, a [tibble][tibble::tibble-package] or data frame of categorical/factor data that have the same scale labels.
#'
#' @param scale_labels Required, a character vector of labels for the response scale of all the Likert items, must be in the desired order,
#'    e.g. if you have a 5 item scale of minimal to extensive it should look like this:
#'    `levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")`.
#'
#' @param question_labels Default is NULL. Takes in a named character vector to both supply labels the questions and sort the order of the
#'  questions.The named character vector should have the new labels as the "name" and the old labels as the "variable" sorted in the
#'  desired order of appearing in the table, first item will appear at the top of the table. See examples.
#'
#' @param str_width Default is 20. The character length to wrap the question column. If question_labels are supplied and very long use this to
#'   keep the question column from being too large for the table.
#'
#' @return a [flextable][flextable::flextable-package] object with columns for question, each likert response item, and a total column for each
#'   items with the total n for that item. Colors are set to Blackstone Research and Evaluation branding
#'
#' @export
#'
#' @examples
#'  data <- tibble::tribble(
#'      ~Organization, ~Source, ~Publish, ~Write, ~Research,
#'                 5L,      3L,       3L,     4L,        5L,
#'                 4L,      4L,       3L,     3L,        3L,
#'                 4L,      4L,       3L,     5L,        3L,
#'                 2L,      5L,       4L,     5L,        3L,
#'                 3L,      4L,       5L,     4L,        5L,
#'                 3L,      4L,       2L,     4L,        2L,
#'                 5L,      4L,       5L,     5L,        3L,
#'                 3L,      5L,       5L,     4L,        3L,
#'                 5L,      4L,       4L,     4L,        5L,
#'                 3L,      5L,       5L,     4L,        3L,
#'                 5L,      4L,       2L,     5L,        5L,
#'                 4L,      3L,       5L,     5L,        4L,
#'                 2L,      4L,       5L,     4L,        3L,
#'                 5L,      4L,       4L,     5L,        4L,
#'                 3L,      5L,       2L,     5L,        5L,
#'                 5L,      4L,       4L,     4L,        5L,
#'                 4L,      4L,       5L,     3L,        5L,
#'                 3L,      5L,       5L,     4L,        4L,
#'                 4L,      3L,       5L,     4L,        5L,
#'                 2L,      5L,       5L,     5L,        3L
#'      )
#'
#' # Scale labels for the Likert items:
#' levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")
#' # Question labels as a named vector with the naming structure
#' # like this: c("new label" = "original variable name"):
#' question_labels <- c("Publish a lot of high quality papers" =  "Publish",
#'                   "Write a lot of research papers" = "Write",
#'                   "Research in a lab with faculty" = "Research",
#'                   "Organization of a large research project" = "Organization",
#'                   "Source work for a research paper" = "Source")
#' # Named Vector for recodeCat():
#' named_levels_min_ext <- c("Minimal" = "1", "Slight" = "2", "Moderate" = "3",
#'                     "Good" = "4", "Extensive" = "5")
#' # Recode the numeric to factor variables using the levels from levels_min_ext:
#' cat_items <- blackstone::recodeCat(data, named_levels_min_ext)
#' # Select the factor variables, and remove the prefix that recodeCat() added to the factor variables:
#' cat_items <- cat_items %>% dplyr::select(dplyr::where(is.factor)) %>%
#'                            dplyr::rename_with(., ~ stringr::str_remove(.,"cat_"))
#' # Another way to convert all to factor variables with levels if already character variables :
#' # cat_items <- data %>%
#' #   mutate(across(everything(),~ factor(., levels = levels_min_ext)))
#' # Pass the factor variables and the levels to likertTable :
#' cat_items %>% likertTable(scale_labels = levels_min_ext, question_labels = question_labels)
#' # Call likertTable without a question_labels:
#' cat_items %>% likertTable(scale_labels = levels_min_ext)
likertTable <- function(df, scale_labels, question_labels = NULL, str_width = 20) {

    extrafont::loadfonts("all", quiet = TRUE)

    flextable::set_flextable_defaults(font.family = "Arial")

    . <- NULL

    # Make sure the all vars in df are factors with scale_labels as their levels and pivot everything to question and response:
    table <- {{ df }}  %>% dplyr::mutate(dplyr::across(tidyselect::everything(), ~ factor(., levels = scale_labels))) %>%
        tidyr::pivot_longer(tidyselect::everything(), names_to = "question", values_to = "response")

    # if question_labels is null, set question as factor in order of appearance:
    if (is.null(question_labels)) {
        table <-  table %>%
            dplyr::mutate(question = forcats::fct_inorder(.data$question))
    } else { # else is when question_labels is not null:
        table <- table %>%
            dplyr::mutate(question = forcats::fct_recode(.data$question, !!!question_labels),
                          question = forcats::fct_relevel(.data$question, names(question_labels)))
    }
    table <- table %>%
        dplyr::group_by(.data$question, .data$response) %>%
        dplyr::summarize(n_answers = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(.data$question) %>%
        dplyr::mutate(percent_answers = .data$n_answers / sum(.data$n_answers),
                      total_n = sum(.data$n_answers)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(percent_answers_label = paste0("(",scales::percent(.data$percent_answers, accuracy = 1),")"),
                      percent_answers_label = dplyr::if_else(.data$percent_answers_label == "(0%)",
                                                             "(<1%)", .data$percent_answers_label)) %>% # replace 0% with <1%
        dplyr::select(-"percent_answers") %>%
        tidyr::unite("value", c(.data$n_answers,.data$percent_answers_label), sep = " ") %>%
        tidyr::pivot_wider(names_from = .data$response,   values_from = .data$value,
                           names_vary = "slowest", names_expand = TRUE) %>%
        dplyr::mutate(question = stringr::str_wrap(.data$question, width = str_width)) %>%
        dplyr::relocate(.data$total_n, .after = tidyselect::last_col()) %>%
        dplyr::rename_with(., ~ stringr::str_wrap(., 10), .cols = tidyselect::everything()) %>%
        dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~ dplyr::case_when(is.na(.) ~ "-",
                                                                                        TRUE ~ .))) %>%
        flextable::flextable() %>%
        flextable::align(j = 2:flextable::ncol_keys(.), align = "center", part = "all") %>%
        flextable::colformat_double(digits = 2) %>%
        flextable::set_header_labels(question = "Question", total_n = "n") %>%
        flextable::italic(j = flextable::ncol_keys(.), italic = TRUE, part = "header") %>%
        flextable::fontsize(size = 10, part = "header") %>%
        flextable::fontsize(size = 9, part = "body") %>%
        flextable::hline(part = "all", border = officer::fp_border(color = "gray")) %>%
        flextable::bg(bg = "#2C2C4F", part = "header") %>%
        flextable::color(color = "white", part = "header") %>%
        flextable::autofit(part = "all")

    return(table)
}

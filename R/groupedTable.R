#' Grouped Summary Table for Blackstone Research and Evaluation
#'
#' [groupedTable()] creates a summary table of frequencies and percentages that can show breakdowns by groups and the totals for
#'      the data passed to it. The table shows each item and all the responses by row.
#'
#' @param df Required, a [tibble][tibble::tibble-package] or data frame of categorical/factor data that also can contain a categorical
#'  group variable to split up the data, e.g. role, gender, education level, etc.
#'
#' @param col_group Default is NULL. The name of the categorical group variable to split up the data, e.g. role, gender, education level,
#'  etc. Must be in quotes (e.g. "role").
#'
#' @param question_labels Default is NULL. Takes in a named character vector to both supply labels the questions and sort the order of the
#'  questions.The named character vector should have the new labels as the "name" and the old labels as the "variable" sorted in the
#'  desired order of appearing in the table, first item will appear at the top of the table. See examples.
#'
#' @param str_width Default is 20. The character length to wrap the question column. If question_labels are supplied and very long use this to
#'  keep the question column from being too large for the table.
#'
#' @return a [flextable][flextable::flextable-package] object with columns for question, response, and counts and percentages for each group
#'  and a total column. Colors are set to Blackstone Research and Evaluation branding
#'
#' @export
#'
#' @examples
#' data <- dplyr::tibble(
#'   Cohort = factor(c(1,2,1,2,1,2,1,2,1,2,1,2), levels = c(1, 2)),
#'   gender = factor(c(
#'              "Female", "Female","Female","Male", "Female","Male",
#'              "Male", "Female","Male", "Female", "Male", "Female"
#'              ), levels = c("Female", "Male")),
#'   year = factor(c(
#'       "grad", "undergrad", "grad", "undergrad", "grad","undergrad",
#'       "undergrad", "undergrad", "grad", "undergrad", "grad","undergrad"
#'                 ), levels = c("grad", "undergrad")),
#'  department = factor(c(
#'       "Chemistry", "Biology", "Chemistry", "Biology", "Physics","Biology",
#'       "Biology", "Physics", "Chemistry", "Chemistry", "Physics","Biology"
#'                ), levels = c("Biology", "Chemistry", "Physics")),
#'  ethnicity = factor(c(
#'       "Asian", "Black", "white", "Hispanic or Latino", "white","Asian",
#'       "Black", "Asian", "white", "white", "Hispanic or Latino","Hispanic or Latino"
#'                   ), levels = c( "Asian", "Black", "Hispanic or Latino", "white"))
#' )
#'
#' # Labels for questions column of table, pass to question_labels argument:
#' labels <- c('Gender' = "gender",
#'              'Ethnicity' = "ethnicity",
#'              'Year in school' = "year",
#'              'Department of Affiliation' = "department")
#'
#' # Call groupedTable with a grouping variable:
#' data %>% groupedTable(col_group = "Cohort", question_labels = labels)
#' # Call groupedTable without a grouping variable:
#' data %>% groupedTable(question_labels = labels)
groupedTable <- function(df, col_group = NULL, question_labels = NULL, str_width = 20) {

    extrafont::loadfonts("all", quiet = TRUE)

    flextable::set_flextable_defaults(font.family = "Arial")

    . <- NULL

    # If col_group is not supplied and is NULL:
    if (is.null(col_group)) {
        # Get total n of df/user supplied dataframe:
        total_n <- {{ df }} %>% nrow()
        # Use map to get a list of dfs for each column in the user supplied df and run all data manipulation:
        table <-  purrr::map(names(df), ~ df %>% dplyr::count(.data[[.x]]) %>% dplyr::arrange(.data[[.x]]) %>%
                        dplyr::mutate(question = colnames(df[.x][1]), # set new var as the name of the original current data
                                      response = .data[[.x]], # copy original col var as response
                                      percent_answers = .data$n / sum(.data$n)) %>% dplyr::relocate("question":"response", .after = .data[[.x]]) %>%
                        dplyr::mutate(percent_answers_label = paste0("(",scales::percent(.data$percent_answers, accuracy = 1),")"),
                                      percent_answers_label = dplyr::if_else(.data$percent_answers_label == "(0%)",
                                                                             "(<1%)", .data$percent_answers_label)) %>% # replace 0% with <1%
                        dplyr::select(-c(1,percent_answers)) %>% # drop first original column and percent_answers
                        tidyr::unite(value, c(n,percent_answers_label), sep = " ")
        )  %>% purrr::list_rbind() %>% dplyr::mutate(dplyr::across(-c("question","response"), ~ dplyr::case_when(is.na(.) ~ "-", TRUE ~ .)))
        # Change question to question_labels if supplied by user and not null:
        if (is.null(question_labels)) {
            table <-  table %>%
                dplyr::mutate(question = forcats::fct_inorder(.data$question))
        } else { # else is when question_labels is not null:
            table <- table %>%
                dplyr::mutate(question = forcats::fct_recode(.data$question, !!!question_labels),
                              question = forcats::fct_relevel(.data$question, names(question_labels))) %>%
                dplyr::arrange(.data$question)
        }
        # Set as grouped data with flextable(), with question as grouping, then decorate as a flextable():
        table <- table %>% dplyr::mutate(question = stringr::str_wrap(.data$question, width = str_width)) %>%
            flextable::as_grouped_data(., "question") %>%
            flextable::flextable() %>%
            flextable::align(j = flextable::ncol_keys(.), align = "center", part = "all") %>% # center last column
            flextable::colformat_double(digits = 2) %>%
            flextable::set_header_labels(question = "Question", response = "Response", value = paste0("n = ", total_n)) %>%
            flextable::fontsize(size = 10, part = "header") %>%
            flextable::fontsize(size = 9, part = "body") %>%
            flextable::hline(part = "all", border = officer::fp_border(color = "gray")) %>%
            flextable::bg(bg = blackstoneColors["dark_blue"], part = "header") %>%
            flextable::color(color = "white", part = "header") %>%
            flextable::bg(i = ~ !is.na(question), bg = blackstoneColors["light_grey"], part = "body") %>%
            flextable::footnote(i = 1, j = flextable::ncol_keys(.), "value" = flextable::as_paragraph(c("n (%)")),
                                ref_symbols = c("1"), part = "header") %>%  # add footnote referencing last header col
            flextable::autofit(part = "all")

        return(table)

    } else {# if col_group is supplied and not NULL:

        df_total <- {{ df }} %>% dplyr::select(-c(!!sym(col_group)))

        total_n <- {{ df }} %>% dplyr::summarise(totals = dplyr::n())
        # Create a table of all vars without grouping, table_total
        # Select all cols except groups:
        cols <- {{ df }}  %>% dplyr::select(-c(!!sym(col_group))) %>% names()
        # Use map to get a list of dfs for each column in the user supplied df and run all data manipulation:
        table_total <-  purrr::map(cols, ~df_total %>% dplyr::count(.data[[.x]]) %>% dplyr::arrange(.data[[.x]]) %>%
                                       dplyr::mutate(question = colnames(df_total[.x][1]), # set new var as the name of the original current data
                                                     response = .data[[.x]], # copy original col var as response
                                                     percent_answers = .data$n / sum(.data$n)) %>% dplyr::relocate("question":"response", .after = .data[[.x]]) %>%
                                       dplyr::mutate(percent_answers_label = paste0("(",scales::percent(.data$percent_answers, accuracy = 1),")"),
                                                     percent_answers_label = dplyr::if_else(.data$percent_answers_label == "(0%)",
                                                                                            "(<1%)", .data$percent_answers_label)) %>% # replace 0% with <1%
                                       dplyr::select(-c(percent_answers)) %>% # drop first original column and percent_answers
                                       tidyr::unite("value", c(n,percent_answers_label), sep = " ")) %>% # end of map()
            purrr::list_rbind() %>% dplyr::select(-cols) %>% # bind as rows and drop all vars in "cols"
            dplyr::rename_with(., ~ paste0("Total\n (n = ", total_n$totals,")"), .cols = !("question":"response")) %>%
            dplyr::mutate(dplyr::across(-c("question","response"), ~ dplyr::case_when(is.na(.) ~ "-", TRUE ~ .)))
        # Change question to question_labels if supplied by user and not null:
        if (is.null(question_labels)) {
            table_total <-  table_total %>%
                dplyr::mutate(question = forcats::fct_inorder(.data$question)) %>%
                dplyr::arrange(.data$question)
        } else { # else is when question_labels is not null:
            table_total <- table_total %>%
                dplyr::mutate(question = forcats::fct_recode(.data$question, !!!question_labels),
                              question = forcats::fct_relevel(.data$question, names(question_labels))) %>%
                dplyr::arrange(.data$question)
        }

        ## By groups:
        # Get total n of df/user supplied dataframe, by grouping variable:
        total_group_n <- {{ df }}  %>% dplyr::group_by(!!sym(col_group)) %>% dplyr::summarise(totals = dplyr::n()) %>% dplyr::rename(., groups = 1)

        # Select all cols except groups:
        cols <- {{ df }}  %>% dplyr::select(-c(!!sym(col_group))) %>% names()
        # Create a table with two map functions, one outer that does it for each df, and inner that does all the data manipulation down the columns:
        table_groups <- purrr::map(cols, ~ df %>% dplyr::group_by(!!sym(col_group)) %>% dplyr::count(.data[[.x]]) %>% dplyr::arrange(.data[[.x]]) %>% dplyr::ungroup() %>%
                                       dplyr::mutate(question = colnames(df[.x][1]), # set new var as the name of the original current data
                                                     response = .data[[.x]], # copy original col var as response
                                                     percent_answers = .data$n / sum(.data$n)) %>% dplyr::relocate("question":"response", .after = .data[[.x]]) %>%
                                       dplyr::mutate(percent_answers_label = paste0("(",scales::percent(.data$percent_answers, accuracy = 1),")"),
                                                     percent_answers_label = dplyr::if_else(.data$percent_answers_label == "(0%)",
                                                                                            "(<1%)", .data$percent_answers_label)) %>% # replace 0% with <1%
                                       dplyr::select(-c(percent_answers)) %>% # drop first original column and percent_answers
                                       tidyr::unite("value", c(n,percent_answers_label), sep = " ")) %>% # end of map()
            purrr::list_rbind() %>% dplyr::select(-cols) %>% # bind as rows and drop all vars in "cols"
            tidyr::pivot_wider(names_from = !!sym(col_group), values_from = .data$value) %>%
            dplyr::rename_with(., ~ paste0(total_group_n$groups,"\n (n = ", total_group_n$totals,")"), .cols = !("question":"response")) %>%
            dplyr::mutate(dplyr::across(-c("question","response"), ~ dplyr::case_when(is.na(.) ~ "-", TRUE ~ .)))
        # Change question to question_labels if supplied by user and not null:
        if (is.null(question_labels)) {
            table_groups <-  table_groups %>%
                dplyr::mutate(question = forcats::fct_inorder(.data$question)) %>%
                dplyr::arrange(.data$question)
        } else { # else is when question_labels is not null:
            table_groups <- table_groups %>%
                dplyr::mutate(question = forcats::fct_recode(.data$question, !!!question_labels),
                              question = forcats::fct_relevel(.data$question, names(question_labels))) %>%
                dplyr::arrange(.data$question)
        }

        # Merge the table_groups and table_total by question and response:
        table <- dplyr::left_join(table_groups, table_total, by = dplyr::join_by("question", "response"))

        # Set as grouped data with flextable(), with question as grouping, then decorate as a flextable():
        table <- table %>% dplyr::mutate(question = stringr::str_wrap(.data$question, width = str_width)) %>% flextable::as_grouped_data(., "question") %>%
            flextable::flextable() %>%
            flextable::align(j = flextable::ncol_keys(.), align = "center", part = "all") %>% # center last column
            flextable::colformat_double(digits = 2) %>%
            flextable::set_header_labels(question = "Question", response = "Response") %>%
            flextable::fontsize(size = 10, part = "header") %>%
            flextable::fontsize(size = 9, part = "body") %>%
            flextable::hline(part = "all", border = officer::fp_border(color = "gray")) %>%
            flextable::bg(bg = blackstoneColors["dark_blue"], part = "header") %>%
            flextable::color(color = "white", part = "header") %>%
            flextable::bg(i = ~ !is.na(question), bg = blackstoneColors["light_grey"], part = "body") %>%
            flextable::footnote(i = 1, j = flextable::ncol_keys(.), "value" = flextable::as_paragraph(c("n (%)")),
                                ref_symbols = c("1"),part = "header") %>%  # add footnote referencing last header col
            flextable::autofit(part = "all")

        return(table)
    }

}

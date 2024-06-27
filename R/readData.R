#' Convert two row headers from SurveyMonkey data into long from data
#'
#' `getHeaders()` is a helper function to get headers into a long form tibble.
#'
#' @param file_path Required, A file path with extension of .csv or .xlsx.
#'
#' @return A [tibble][tibble::tibble-package] with 3 columns: `header_1`, `header_2`, and `combined_header`.
#'      `header_1` is the first header row, `header_2` is the second header row, and `combined_header` is the combination of the two headers.
#'
#' @export
getHeaders <- function(file_path) {
    ext <- tools::file_ext(file_path) # get file_path extension
    # Get the headers from either .csv or .xlsx by reading in just the first two rows when col_names is set to false:
    sm_data_headers <- switch(ext,
                              csv = readr::read_csv(file_path, col_names = FALSE, n_max = 2, show_col_types = FALSE), # only first two rows
                              xlsx = readxl::read_xlsx(file_path, col_names = FALSE, n_max = 2, .name_repair = "unique_quiet"), # only first two rows
                              # Return an error message if file is not .csx or .xlsx:
                              warning("Invalid file! Please use a .csv or .xlsx file")
    )
    # Manipulate the data so that headers is a three column tibble: header 1 = `header_1`, header 2 = `header_2`, and `combined_header` is the combination of the two:
    headers <- sm_data_headers %>%
        dplyr::mutate(type = c('header_1', 'header_2')) %>%
        tidyr::pivot_longer(!"type") %>%
        tidyr::pivot_wider(names_from = "type", values_from = "value") %>%
        dplyr::select(!"name") %>%
        dplyr::mutate(header_2 = dplyr::case_when(.data[["header_2"]] %in% c("Response", "Open-Ended Response") ~ NA_character_, TRUE ~ header_2), # turn "Response" and Open-Ended Response" to NA
                      combined_header = dplyr::coalesce(.data[["header_2"]], .data[["header_1"]])) # combine two columns into new col, take first non-missing, header_2, then header_1
    return(headers)
}


#' Create a codebook for SurveyMonkey data
#'
#' `createCodebook()` creates a partial codebook that can be edited to create useful variable names from SurveyMonkey data.
#'      It returns a long form tibble to use as a code book for the SurveyMonkey data, it returns 5 columns described below and `variable_name`
#'      is the column that will be used in the function [readRenameData()] to rename the variable names in the data to be imported into R.
#'
#' @inheritParams getHeaders
#'
#' @return A [tibble][tibble::tibble-package] with 5 columns: `header_1`, `header_2`, `combined_header`, `variable_name` and `position`.
#'      `header_1` is the first header row, `header_2` is the second header row, `combined_header` is the combination of the two headers,
#'      `position` is the column number for each `combined_header`, and `variable_name` is a cleaned up version for `combined_header` and
#'      will be the column to edit to change the column names later on to shorter and more meaningful names.
#'
#' @export
createCodebook <- function(file_path) {
    codebook <- getHeaders(file_path = file_path) %>% # helper function that returns tibble of 3 cols: header_1, header_2, combined_header
        dplyr::mutate(position = seq_along(.data[["combined_header"]]), # make `position` by creating numeric sequence along `combined_header`
                      variable_name = janitor::make_clean_names(.data[["combined_header"]])) # make `variable_name` by cleaning names of new col `combined_header`
    return(codebook)
}


#' Import SurveyMonkey data and create new variable names
#'
#' `readRenameData()` Reads in SurveyMonkey data with new variable names taken from the codebook [tibble][tibble::tibble-package] column named `variable_name`
#'      created by using the function [createCodebook()].
#'
#' @inheritParams getHeaders
#'
#' @param codebook Required, A [tibble][tibble::tibble-package] created by using the function [createCodebook()] that has the column named `variable_name`
#'      that will be the new names of the data imported into R.
#'
#' @return A [tibble][tibble::tibble-package] of SurveyMonkey data with new variable names.
#'
#' @export
readRenameData <- function(file_path, codebook) {
    # Using `getHeaders()`- Condense the two header rows into one row (column named `combined_header`) and set as a character vector:
    sm_names <- getHeaders(file_path = file_path) %>% # helper function that returns tibble of 3 cols: header_1, header_2, combined_header
        dplyr::select("combined_header") %>% # select the combined header
        tibble::deframe() # set as a character vector
    ext <- tools::file_ext(file_path) # get file_path extension
    # Read in the data, using switch with ext from above to determine the function, skipping first 2 lines, and then set the column names using the vector `sm_names`:
    sm_data <- switch(ext,
                      csv = readr::read_csv(file_path, skip = 2, col_names = sm_names, show_col_types = FALSE),
                      xlsx = readxl::read_xlsx(file_path, skip = 2, col_names = sm_names, .name_repair = "unique_quiet"),
                      # Return an error message if file is not .csx or .xlsx:
                      warning("Invalid file! Please use a .csv or .xlsx file")
    )

    # Create a named vector of the columns from `codebook` named `variable_name` and `combined_header` where `variable_name` is the names and
    # will be the new names in data (tibble::deframe() creates a named vector where `names` is the first col and second col will be the `values`):
    sm_new_names <- {{ codebook }} %>% dplyr::select("variable_name", "combined_header") %>% tibble::deframe()

    # Use it to rename the vars in `sm_data` so they match (current names of sm_data were `combined_header`, the use of a named vector ensures the right
    # columns are being renamed):
    sm_data <- sm_data %>% dplyr::rename(!!!sm_new_names)
    sm_data

    return(sm_data)
}

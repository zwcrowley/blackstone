#' Create Flextable in The Mark USA Branding
#'
#' @param data Required, A [tibble][tibble::tibble-package]/data frame of summary/descriptice or
#'  test statistics.
#'
#' @return A nicely formatted table that is a [flextable()] object.
#' @export
#'
#' @examples
#' # Example data:
#' data <- dplyr::tibble(
#'   Organization = c(1, 2, 3, 4, 5, 4, 3, 2, 1),
#'   Source = c(2, 2, 3, 5, 4, 3, 2, 1, 2),
#'   Publish = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   Write = c(2, 2, 2, 3, 3, 3, 4, 4, 4),
#'   Research = c(1, 1, 2, 2, 3, 3, 4, 4, 4),
#'   group = factor(c(
#'       "grad", "undergrad", "grad", "undergrad", "grad",
#'       "undergrad", "undergrad", "grad", "undergrad"
#'                     ), levels = c("grad", "undergrad")
#'     )
#' )
#' # Summarise the data into mean and sd for each numeric column, grouped by "group":
#' data <- data %>% dplyr::group_by(group) %>%   dplyr::summarise(dplyr::across(
#'                                    .cols = dplyr::where(is.numeric),
#'                                    .fns =  list(Mean = mean, SD = sd),
#'                                    .names = "{col}_{fn}"
#'                                                   )) %>%
#'              dplyr::ungroup() %>%
#'              tidyr::pivot_longer(cols = -group,
#'                                  names_pattern = "([^_]+)_(.*)",
#'                                  names_to = c("variable", "stat"),
#'                                  values_to = "value",
#'                                  values_drop_na = TRUE) %>%
#'              tidyr::pivot_wider(names_from = stat, values_from = value)
#'
#' # Make a nice table with the function:
#' data %>% test_flextable()
test_flextable <- function(data) {
    # Set . to NULL to stop message when using dot notation in functions:
    . <- NULL
    # Call to piped flextable calls,
    test_table <- {{ data }} %>% flextable::flextable() %>%
        flextable::align(j = 2:flextable::ncol_keys(.), align = "center", part = "all") %>%
        flextable::fontsize(size = 12, part = "header") %>%
        flextable::fontsize(size = 10, part = "body") %>%
        flextable::hline(part = "all", border = officer::fp_border(color = "gray")) %>%
        flextable::bg(bg = "#2C2C4F", part = "header") %>%
        flextable::color(color = "white", part = "header") %>%
        flextable::autofit(part = "all")
    return(test_table)
}

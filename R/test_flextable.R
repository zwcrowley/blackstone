# Function to prettify test output:
test_flextable <- function(data) {
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

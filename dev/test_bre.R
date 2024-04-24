# Single time point data:
items_single <- dplyr::tibble(
    Organization = c(1, 2, 3, 4, 5, 4, 3, 2, 1),
    Source = c(2, 2, 3, 5, 4, 3, 2, 1, 2),
    Publish = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    Write = c(2, 2, 2, 3, 3, 3, 4, 4, 4),
    Research = c(1, 1, 2, 2, 3, 3, 4, 4, 4)
)
# scale_labels as a named character vector, items in correct order:
levels_min_ext <- c("Minimal" = "1", "Slight" = "2", "Moderate" = "3", "Good" = "4", "Extensive" = "5")

# bar_scale_labels as just the names from levels_min_ext:
bar_scale_labels <- names(levels_min_ext)

# Question labels as a named vector with the naming structure like this: c("{new label}" = "{original variable name}"):
question_labels <- c("Publish a lot of high quality papers" =  "Publish",
                     "Write a lot of research papers" = "Write",
                     "Research in a lab with faculty" = "Research",
                     "Organization of a large research project" = "Organization",
                     "Source work for a research paper" = "Source")
# Recode the numeric to factor variables using the levels from levels_min_ext:
cat_items_single <- bre::recodeCat(items_single, levels_min_ext)
# Select the factor variables:
cat_items_single <- cat_items_single %>% dplyr::select(dplyr::where(is.factor))
# Pass the factor variables and the levels to 'stackedBarChart()':
stacked_chart_single <- bre::stackedBarChart(
    df = cat_items_single, pre_post = FALSE, scale_labels = bar_scale_labels,
    percent_label = TRUE, width = 0.6
)
stacked_chart_single

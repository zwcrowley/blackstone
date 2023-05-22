test_that("plots accept same data types", {
 items <- dplyr::tibble(
   Pre_Organization = c(1, 2, 3, 4, 5, 4, 3, 2, 1),
   Post_Organization = dplyr::if_else(Pre_Organization < 5, Pre_Organization + 1, Pre_Organization),
   Pre_Source = c(2, 2, 3, 5, 4, 3, 2, 1, 2),
   Post_Source = dplyr::if_else(Pre_Source < 4, Pre_Source + 2, Pre_Source),
   Pre_Publish = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
   Post_Publish = Pre_Publish + 2,
   Pre_Write = c(2, 2, 2, 3, 3, 3, 4, 4, 4),
   Post_Write = Pre_Write + 1,
   Pre_Research = c(1, 1, 2, 2, 3, 3, 4, 4, 4),
   Post_Research = Pre_Research + 1
 )

 items_single <- dplyr::tibble(
   Organization = c(1, 2, 3, 4, 5, 4, 3, 2, 1),
   Source = c(2, 2, 3, 5, 4, 3, 2, 1, 2),
   Publish = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
   Write = c(2, 2, 2, 3, 3, 3, 4, 4, 4),
   Research = c(1, 1, 2, 2, 3, 3, 4, 4, 4),
 )

 levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")
 # Recode the numeric to factor variables using the levels from levels_min_ext:
 cat_items <- TheMarkUSA::recodeCat(items, levels_min_ext)
 cat_items_single <- TheMarkUSA::recodeCat(items_single, levels_min_ext)

 # Select the factor variables:
 cat_items <- cat_items %>% dplyr::select(dplyr::where(is.factor))
 cat_items_single <- cat_items_single %>% dplyr::select(dplyr::where(is.factor))

 # Pass the factor variables and the levels to 'stackedBarChart()':
 stacked_chart_1 <- TheMarkUSA::stackedBarChart(
   df = cat_items, pre_post = TRUE, scale_labels = levels_min_ext,
   question_order = NULL, question_labels = NULL, percent_label = TRUE, width = NULL
 )
 stacked_chart_2 <- TheMarkUSA::stackedBarChart(
   df = cat_items_single, pre_post = FALSE, scale_labels = levels_min_ext,
   question_order = NULL, question_labels = NULL, percent_label = TRUE, width = NULL
 )

  expect_type(stacked_chart_1, "list")
  expect_type(stacked_chart_2, "list")
})

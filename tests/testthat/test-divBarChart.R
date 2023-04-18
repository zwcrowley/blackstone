test_that("plots accept same data types", {
  items_1 <- dplyr::tibble(
    cat_Pre_Sources = c("Good", "Moderate", "Minimal", "Slight", "Slight", "Moderate", "Good"),
    cat_Post_Sources = c("Good", "Good", "Minimal", "Moderate", "Moderate", "Good", "Extensive")
  )
  items_2 <- dplyr::tibble(
    cat_Pre_Orgs = c("Good", "Moderate", "Minimal", "Slight", "Slight", "Moderate", "Moderate"),
    cat_Post_Orgs = c("Good", "Moderate", "Minimal", "Moderate", "Moderate", "Good", "Extensive")
  )

  levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")

  items_1 <- items_1 %>% dplyr::mutate(
    cat_Pre_Sources = factor(cat_Pre_Sources, levels = levels_min_ext),
    cat_Post_Sources = factor(cat_Post_Sources, levels = levels_min_ext)
  )

  items_2 <- items_2 %>% dplyr::mutate(
    cat_Pre_Orgs = factor(cat_Pre_Orgs, levels = levels_min_ext),
    cat_Post_Orgs = factor(cat_Post_Orgs, levels = levels_min_ext)
  )

  div_stacked_chart_1 <- divBarChart(items_1, levels_min_ext)
  div_stacked_chart_2 <- divBarChart(items_2, levels_min_ext)

  expect_type(div_stacked_chart_1, "list")
  expect_type(div_stacked_chart_2, "list")
})

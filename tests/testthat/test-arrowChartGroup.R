test_that("plots accept same data types", {
  items_1 <- dplyr::tibble(
    Pre_Sources = c(1, 2, 3, 4, 5, 4, 3),
    Post_Sources = c(1, 2, 3, 4, 5, 4, 3),
    edu_level = factor(c("grad", "undergrad", "grad", "undergrad", "grad", "undergrad", "undergrad"),
                       levels = c("grad", "undergrad"))
  )
  items_2 <- dplyr::tibble(
    Pre_Organization = c(1, 2, 3, 4, 5, 4, 3, 2, 1),
    Post_Organization = dplyr::if_else(Pre_Organization < 5, Pre_Organization + 1, Pre_Organization),
    Pre_Source = c(2, 2, 3, 5, 4, 3, 2, 1, 2),
    Post_Source = dplyr::if_else(Pre_Source < 4, Pre_Source + 2, Pre_Source),
    Pre_Publish = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    Post_Publish = Pre_Publish + 2,
    Pre_Write = c(2, 2, 2, 3, 3, 3, 4, 4, 4),
    Post_Write = Pre_Write + 1,
    Pre_Research = c(1, 1, 2, 2, 3, 3, 4, 4, 4),
    Post_Research = Pre_Research + 1,
    edu_level = factor(c("undergrad", "undergrad", "grad", "grad", "undergrad", "grad", "undergrad", "grad", "undergrad"),
                      levels = c("grad", "undergrad"))
  )

  levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")

  three_colors <- c("#79AB53", "#4B9FA6", "#2C2C4F")

  arrow_chart_1 <- arrowChartGroup(items_1, "edu_level", levels_min_ext, three_colors)
  arrow_chart_2 <- arrowChartGroup(items_2, "edu_level", levels_min_ext, three_colors)

  expect_type(arrow_chart_1, "list")
  expect_type(arrow_chart_2, "list")
})

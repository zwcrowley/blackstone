test_that("plots accept same data types", {
  items_1 <- dplyr::tibble(
    Pre_Sources = c(1, 2, 3, 4, 5, 4, 3),
    Post_Sources = c(1, 2, 3, 4, 5, 4, 3),
    group = c("grad", "undergrad", "grad", "undergrad", "grad", "undergrad", "undergrad")
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
    group = c("undergrad", "undergrad", "grad", "grad", "undergrad", "grad", "undergrad", "grad", "undergrad")
  )

  levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")

  items_1 <- items_1 %>% dplyr::mutate(
    group = factor(group, levels = c("grad", "undergrad"))
  )

  items_2 <- items_2 %>% dplyr::mutate(
    group = factor(group, levels = c("grad", "undergrad"))
  )

  threeScale_theMark_colors <- c("#79AB53", "#4B9FA6", "#2C2C4F")

  arrow_chart_1 <- arrowChart(items_1, levels_min_ext, threeScale_theMark_colors)
  arrow_chart_2 <- arrowChart(items_2, levels_min_ext, threeScale_theMark_colors)

  expect_type(arrow_chart_1, "list")
  expect_type(arrow_chart_2, "list")
})

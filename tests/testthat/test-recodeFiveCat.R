test_that("function outputs expected data types and structure", {
  items_1 <- dplyr::tibble(Pre_Orgs = c(1, 2, 3, 4, 5, 4, 3, 2, 1), Post_Orgs = c(1, 2, 3, 4, 5, 4, 3, 2, 1))

  levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")

  cat_items_1 <- recodeFiveCat(items_1, levels_min_ext)

  df_expected <- dplyr::tibble(
    Pre_Orgs = c(1, 2, 3, 4, 5, 4, 3, 2, 1), Post_Orgs = c(1, 2, 3, 4, 5, 4, 3, 2, 1),
    cat_Pre_Orgs = c("Minimal", "Slight", "Moderate", "Good", "Extensive", "Good", "Moderate", "Slight", "Minimal"),
    cat_Post_Orgs = c("Minimal", "Slight", "Moderate", "Good", "Extensive", "Good", "Moderate", "Slight", "Minimal")
  )

  df_expected <- df_expected %>% dplyr::mutate(
    cat_Pre_Orgs = factor(.data$cat_Pre_Orgs, levels = levels_min_ext),
    cat_Post_Orgs = factor(.data$cat_Post_Orgs, levels = levels_min_ext)
  )
  str_cat_items_1 <- str(cat_items_1)
  str_df_expected <- str(df_expected)

  expect_equal(cat_items_1, df_expected)
  expect_equal(str_cat_items_1, str_df_expected)
})

test_that("plots have known output", {
    disp_hist_base <- function() hist(mtcars$disp)
    vdiffr::expect_doppelganger("disp-histogram-base", disp_hist_base)
    disp_hist_ggplot <- ggplot2::ggplot(mtcars, ggplot2::aes(disp)) + ggplot2::geom_histogram(binwidth = 30)
    vdiffr::expect_doppelganger("disp-histogram-ggplot", disp_hist_ggplot)
})

test_that("plots accept same data types", {
    items_1 <- dplyr::tibble(
        cat_Pre_Sources = c("Good","Moderate","Minimal","Slight","Slight","Moderate","Good"),
        cat_Post_Sources = c("Good","Good","Minimal","Moderate","Moderate","Good","Extensive"))
    items_2 <- dplyr::tibble(
        cat_Pre_Orgs = c("Good","Moderate","Minimal","Slight","Slight","Moderate","Moderate"),
        cat_Post_Orgs = c("Good","Moderate","Minimal","Moderate","Moderate","Good","Extensive"))

    levels_min_ext = c('Minimal', 'Slight', 'Moderate', 'Good', 'Extensive')

    stacked_chart_1 <- stackedBarChart(items_1,levels_min_ext)
    stacked_chart_2 <- stackedBarChart(items_2,levels_min_ext)

    expect_type(stacked_chart_1, "list")
    expect_type(stacked_chart_2, "list")
})

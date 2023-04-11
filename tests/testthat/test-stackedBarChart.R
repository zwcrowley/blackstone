test_that("plots have known output", {
    disp_hist_base <- function() hist(mtcars$disp)
    vdiffr::expect_doppelganger("disp-histogram-base", disp_hist_base)
    disp_hist_ggplot <- ggplot2::ggplot(mtcars, ggplot2::aes(disp)) + ggplot2::geom_histogram()
    vdiffr::expect_doppelganger("disp-histogram-ggplot", disp_hist_ggplot)
})

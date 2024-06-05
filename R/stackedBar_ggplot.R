#' Helper function for creating stacked bar chart
#'
#' [stackedBar_ggplot()] creates a stacked bar chart and returns a ggplot object.
#'
#' @param df_gg Required, A passed [tibble][tibble::tibble-package]/data frame from main function of survey items that are categorical/character
#'   variables, in 3 to 7 point scales, that will be inserted into a stacked bar chart.
#'
#' @param y_gg Required, the variable to plot on the y-axis; either timing if pre-post, question if not.
#'
#' @param pre_post Logical, default is FALSE. -passed from main function- If true, returns a pre-post stacked bar chart.
#'
#' @param label_gg Required, the variable used to label the bars of the plot; either 'percent_answers_label' or 'n_question.'
#'
#' @param label_colors_named Required, passed character vector of label colors from main function for the legend, named for scale_labels and must be in the desired order,
#'
#' @param legend_labels Required, passed character vector of labels from main function for the legend, should be str_wrap text and must be in the desired order
#'
#' @param width_gg Input a value between 0.3 and 0.8 to set the thickness of the bars. -passed from main function- Default is NULL.
#'
#' @param fill_colors_gg Passed character vector of color codes from main function, that corresponds to the colors for each scale label and bar in the chart.
#'
#' @param key_width
#'
#' @param key_height
#'
#' @param overall_n_gg Logical, default is FALSE. -passed from main function- If TRUE, returns an overall *n* for all questions that is in the upper left tag of the plot.
#'    If False, adds *n* to each question/item after the respective labels.
#'
#' @param N_df_gg The value or values to use for the *n*, either the overall total *n* if overall_n_gg is TRUE or individual *n* for each question.
#'
#' @param font_family
#'
#' @param font_size
#'
#' @return A [ggplot2][ggplot2::ggplot2-package] object that plots the items into a stacked bar chart.
#' @noRd
stackedBar_ggplot <- function(df_gg, y_gg, pre_post = FALSE, label_gg, label_colors_named, width_gg, fill_colors_gg, key_width, key_height,
                              overall_n_gg, N_df_gg, font_family = "Arial", font_size = 10) {

    # Load all fonts:
    extrafont::loadfonts("all", quiet = TRUE)

    # Set . to NULL to stop message when using dot notation in mutate:
    . <- NULL

    # Chart creation:
    stacked_bar_chart_gg <- {{ df_gg }} %>%
        ggplot2::ggplot(ggplot2::aes(
            x = .data[["percent_answers"]], y = forcats::fct_rev(.data[[{{y_gg}}]]), group = .data[["question"]]
        )) +
        ggplot2::geom_col(ggplot2::aes(fill = .data[["response"]]),
                          position = "fill", color = "black", # color is outline of fill boxes
                          width = {{ width_gg }}, key_glyph = draw_key_cust #  draw_key_cust is custom key function from `gg_helpers.R`
        ) +
        ggplot2::geom_text(ggplot2::aes(label = {{ label_gg }}, color = .data[["label_color"]]),
                           position = ggplot2::position_fill(vjust = 0.5),
                           stat = "identity", size = font_size, size.unit = "pt", family = font_family
        ) +
        ggplot2::scale_color_identity() +
    if (isTRUE(pre_post)) {
        stacked_bar_chart_gg <-  stacked_bar_chart_gg + ggplot2::facet_wrap(dplyr::vars( .data[["question"]] ), ncol = 1, strip.position = "left")
    }
        stacked_bar_chart_gg <-  stacked_bar_chart_gg +
            ggplot2::scale_fill_manual(values = {{ fill_colors_gg }}, labels = NULL) + # turn off labels in legend
            ggplot2::guides(fill = guide_legend(
                nrow = 1, keywidth = key_width, keyheight = key_height, # keywidth and keyheight need to be supplied as a grid::unit() to change size of keys!!!!!!
                override.aes = list(color = label_colors_named, # manually sets the color of the legend text to white or black
                                    label = legend_labels) # manually sets the legend labels to wrapped scale_labels
            )) +
            addBarChartTheme(font_size = font_size, font_family = font_family) # function from `gg_helpers.R`


    if (isTRUE(pre_post)) {
        stacked_bar_chart_gg <- stacked_bar_chart_gg + addBarChartPrePostTheme(font_size = font_size, font_family = font_family)
    }

    # Set tag to N_df if overall_n_gg == TRUE
    if (isTRUE(overall_n_gg)) {
        stacked_bar_chart_gg <- stacked_bar_chart_gg + addPlotTag(n = N_df_gg, font_size = font_size, font_family = font_family)
    }

    return(stacked_bar_chart_gg)

} # end of stackedBar_ggplot()


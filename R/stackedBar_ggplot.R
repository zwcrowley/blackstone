#' Helper function for creating stacked bar chart
#'
#' [stackedBar_ggplot()] creates a stacked bar chart and returns a ggplot object.
#'
#' @param df_gg Required, A passed [tibble][tibble::tibble-package]/data frame from main function of survey items that are categorical/character
#'   variables, in 3 to 7 point scales, that will be inserted into a stacked bar chart.
#'
#' @param x_gg Required, the variable to plot on the x-axis, usually percent answers to make the right size bars.
#'
#' @param y_gg Required, the variable to plot on the y-axis; either timing if pre-post, question if not.
#'
#' @param group_gg Required, the variable used to group the bars of the plot; usually the question.
#'
#' @param group_gg Required, the variable used to label the bars of the plot; either percent_answers or n_answers.
#'
#' @param label_color_gg Required, the variable used to color label the bars of the plot; usually label_color
#'
#' @param scale_labels_gg Required, passed character vector of labels from main function for the response scale, must be in the desired order,
#'    e.g. if you have a 5 item scale of minimal to extensive it should look like this: `levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")`.
#'    This argument accepts a character vector of 3 to 7 items.
#'
#' @param width_gg Input a value between 0.3 and 0.8 to set the thickness of the bars. -passed from main function- Default is NULL.
#'
#' @param fill_colors_gg Passed character vector of color codes from main function, that corresponds to the colors for each scale label and bar in the chart.
#'
#' @param overall_n_gg Logical, default is FALSE. -passed from main function- If TRUE, returns an overall *n* for all questions that is in the upper left tag of the plot.
#'    If False, adds *n* to each question/item after the respective labels.
#'
#' @param N_df_gg The value or values to use for the *n*, either the overall total *n* if overall_n_gg is TRUE or individual *n* for each question.
#'
#' @param pre_post Logical, default is FALSE. -passed from main function- If true, returns a pre-post stacked bar chart.
#'
#' @return A [ggplot2][ggplot2::ggplot2-package] object that plots the items into a stacked bar chart.
#' @noRd
stackedBar_ggplot <- function(df_gg, x_gg , y_gg, fill_gg, group_gg, label_gg, label_color_gg, scale_labels_gg, width_gg, fill_colors_gg,
                                  overall_n_gg, N_df_gg, pre_post = FALSE) {

    # Load all fonts:
    extrafont::loadfonts("all", quiet = TRUE)

    # Set . to NULL to stop message when using dot notation in mutate:
    . <- NULL

    # Create a font family character var so that it is easy to change, could also be a new arg:
    font_family <- c("Arial")

    stacked_bar_chart_gg <- {{df_gg}} %>%
        ggplot2::ggplot(ggplot2::aes(
            x = {{x_gg}}, y = forcats::fct_rev({{y_gg}}), fill = {{fill_gg}},
            label = {{label_gg}}, group = {{group_gg}})) +
        ggplot2::geom_col(width = {{width_gg}}, position = ggplot2::position_stack(reverse = TRUE), color = "black") +
        ggplot2::geom_text(ggplot2::aes(color = {{label_color_gg}}),
                           family = font_family,
                           fontface = "bold", position = ggplot2::position_stack(vjust = .5, reverse = TRUE), size = 3.5
        ) +
        ggplot2::scale_color_identity()
    if (isTRUE(pre_post)) {
        stacked_bar_chart_gg <-  stacked_bar_chart_gg + ggplot2::facet_wrap(~question, ncol = 1, strip.position = "left")
    }
    stacked_bar_chart_gg <-  stacked_bar_chart_gg +
        ggplot2::scale_fill_manual(breaks = {{scale_labels_gg}}, values = {{fill_colors_gg}}, drop = FALSE,
                                   labels = paste("<span style='color:", {{fill_colors_gg}}, "'>",
                                                  stringr::str_wrap({{scale_labels_gg}}, width = 10) %>% gsub("\n", "<br>", .), "</span>"),
                                   guide = ggplot2::guide_legend(override.aes = ggplot2::aes(color = NA, fill = NA))
        ) + ggplot2::guides(color = "none")

    if (isTRUE(pre_post)) {
        stacked_bar_chart_gg <- stacked_bar_chart_gg +
            ggplot2::theme_void(base_family = font_family, base_size = 11) +
            ggplot2::theme(strip.placement = "outside",
                           strip.text.y.left = ggtext::element_markdown(
                               angle = 0, hjust = 1, color = "black", size = 11, family = font_family,
                               margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 0, unit = "pt")),
                           axis.text.y = ggtext::element_markdown(
                               angle = 0, hjust = 1, color = "black", size = 11, family = font_family,
                               margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
                           ),
                           plot.margin = ggplot2::margin(t = 35, r = 5, b = 35, l = 5, unit = "pt"),
                           legend.text = ggtext::element_markdown(angle = 0, hjust = 0.5, vjust = 0, halig	= 0.5, valign = 0,
                                                                  size = 11, family = font_family, face = "bold",
                                                                  margin = ggplot2::margin(t = 5, r = 10, b = 5, l = 10, unit = "pt")
                           ),
                           legend.justification = c("right", "top"),
                           legend.position = "top",
                           legend.key = ggplot2::element_blank(),
                           legend.title = ggplot2::element_blank()
            )
    } else {
        stacked_bar_chart_gg <- stacked_bar_chart_gg +
            ggplot2::theme_void(base_family = font_family, base_size = 11) +
            ggplot2::theme(
                axis.text.y = ggtext::element_markdown(
                    angle = 0, hjust = 1, color = "black", size = 11, family = font_family,
                    margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
                ),
                plot.margin = ggplot2::margin(t = 35, r = 5, b = 35, l = 5, unit = "pt"),
                legend.text = ggtext::element_markdown(angle = 0, hjust = 0.5, vjust = 0, halig	= 0.5, valign = 0,
                                                       size = 11, family = font_family, face = "bold",
                                                       margin = ggplot2::margin(t = 5, r = 10, b = 5, l = 10, unit = "pt")
                ),
                legend.justification = c("right", "top"),
                legend.position = "top",
                legend.key = ggplot2::element_blank(),
                legend.title = ggplot2::element_blank()
            )
    }

    # Set tag to N_df if overall_n_gg == TRUE
    if (isTRUE(overall_n_gg)) {
        stacked_bar_chart_gg <-  stacked_bar_chart_gg +  ggplot2::labs(title = NULL, y = labels, x = NULL, tag = paste0("(*n* = ",  N_df_gg , ")")) +
            ggplot2::theme(plot.tag = ggtext::element_markdown(color = "black", size = 10, family = font_family),
                           plot.tag.position = "topleft")
    } else {
        stacked_bar_chart_gg <-  stacked_bar_chart_gg +  ggplot2::labs(title = NULL, y = labels, x = NULL, tag = NULL)
    }

    return(stacked_bar_chart_gg)

} # end of stackedBar_ggplot()


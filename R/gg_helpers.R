#' Helper Functions for ggplot2 visuals for the `bre` package
#'
#'
#' Helper function to Add a Plot Tag
#'
#' @description A function for add a plot tag to the upper left corner of a ggplot with the total n of the sample.
#'      Pass as args the total n, font size and family.
#'
#' @param n Required, numeric, total n for the sample in the chart
#'
#' @param font_size Required, numeric, the font size for the chart.
#'
#' @param font_family Required, character, the name of the font family to use for chart.
#'
#' @return A list of ggplot2 labs and theme objects to add to a ggplot2 object.
#'
#' @export
addPlotTag <- function(n, font_size, font_family) {
    list(
        ggplot2::labs(tag = paste0("(*n* = ", n , ")")),
        ggplot2::theme(plot.tag = ggtext::element_markdown(color = "black", size = font_size, family = font_family),
                       plot.tag.location = "panel", # places tag within panel
                       # plot.tag.position = "topleft"
                       plot.tag.position = c(-0.035, 1.04) # manually positions the tag using coordinates
        )
    )
}

#' Helper function to Add Legend and Theme Options to a Bar Chart
#'
#' @description A function for add all legend and plot theme options to a bar chart, pass as args the
#'      font size and family.
#'
#' @param font_size Required, numeric, the font size for the chart.
#'
#' @param font_family Required, character, the name of the font family to use for chart.
#'
#' @return A list of ggplot2 guides and theme objects to add to a ggplot2 object.
#'
#' @export
addBarChartLegendTheme <- function(font_size, font_family) {
    list(
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1,
                                                     override.aes = list(label = "  ", # sets the width of `key_glyph = draw_key_label` in geom_col for the legend.
                                                                         size = 5 # sets the size of legend key along with the two spaces in the previous line
                                                     )
        )
        ),
        ggplot2::theme_void(base_size = font_size, base_family = font_family),
        ggplot2::theme(legend.position = "top",
                       legend.direction = "horizontal",
                       legend.text.position = "right",
                       legend.text = ggplot2::element_text(angle = 0, hjust = 0, vjust = 0.5,
                                                           family = font_family, size = ggplot2::rel(0.8)
                       ),
                       legend.title = ggplot2::element_blank(),
                       axis.text = ggplot2::element_text(angle = 0, hjust = 1, vjust = 0.5,
                                                         family = font_family, color = "black"
                       ),
                       axis.text.x = ggplot2::element_blank(),
                       axis.text.y = ggtext::element_markdown( # Controls the 'question' if pre_post is F, or 'timing' if Pre post labels
                           angle = 0, hjust = 1, halign = 1, color = "black",
                           margin = ggplot2::margin(t = 5, r = -15, b = 5, l = 5, unit = "pt")
                       ),
                       plot.margin = ggplot2::margin(t = 30, r = 5, b = 30, l = 5, unit = "pt"),
                       panel.spacing = grid::unit(0, "cm")
        )
    )

}


#' Helper function to Add Theme Options to a Pre-Post Bar Chart
#'
#' @description A function for add all plot theme options to a Pre-Post bar chart, pass as args the
#'      font size and family.
#'
#' @param font_size Required, numeric, the font size for the chart.
#'
#' @param font_family Required, character, the name of the font family to use for chart.
#'
#' @return A list of ggplot2 guides and theme objects to add to a ggplot2 object.
#'
#' @export
# addBarChartPrePostTheme: sets the fill legend, and all theming for sb chart and div chart: args = font_size, font_family:
addBarChartPrePostTheme <- function(font_size, font_family) {
    ggplot2::theme(strip.placement = "outside",
                   strip.switch.pad.wrap = grid::unit(0, "cm"),
                   strip.text.y.left = ggtext::element_markdown( # Controls the question text on left- facet
                       angle = 0, hjust = 1, color = "black", family = font_family, size = font_size,
                       margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
                   ),
                   axis.text.y = ggtext::element_markdown( size = ggplot2::rel(0.8) # Controls the 'timing' Pre post labels
                   )
    )

}

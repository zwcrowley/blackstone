#' Helper Functions for ggplot2 visuals for the `bre` package
#'
#'
#' Helper function to Add Theme Options to a Bar Chart
#'
#' @description A function for add all plot theme options to a bar chart, pass as args the
#'      font size and family.
#'
#' @param font_size Required, numeric, the font size for the chart.
#'
#' @param font_family Required, character, the name of the font family to use for chart.
#'
#' @return A list of ggplot2 guides and theme objects to add to a ggplot2 object.
#'
#' @export
addBarChartTheme <- function(font_size, font_family) {
    list(
        ggplot2::theme(text = ggplot2::element_text(
            angle = 0, hjust = 1, vjust = 0.5,
            family = font_family, size = font_size
        ),
        legend.position = "top",
        legend.title = ggplot2::element_blank(),
        legend.background = ggplot2::element_blank(),
        legend.box.spacing = grid::unit(0, "cm"), # no space btw legend and plot
        legend.key.spacing.x = grid::unit(0, "cm"), # no space btw legend key boxes
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(
            angle = 0, hjust = 1, vjust = 0.5,
            family = font_family, size = font_size
        ),
        axis.text.x = ggplot2::element_blank(), # turn off x axis labels
        axis.text.y = ggtext::element_markdown( # Controls the '.data[["question"]]' if pre_post is F, or 'timing' if Pre post labels
            color = "black", margin = ggplot2::margin(t = 5, r = -15, b = 5, l = 5, unit = "pt")
        ),
        plot.background = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank()
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
addBarChartPrePostTheme <- function(font_size, font_family) {
    list(
        ggplot2::theme(strip.background = ggplot2::element_blank(),
                       strip.clip = "on",
                       strip.placement = "outside",
                       strip.switch.pad.wrap = grid::unit(0, "cm"),
                       strip.text.y.left = ggtext::element_markdown( # Controls the .data[["question text on left- facet"]]
                           angle = 0, hjust = 1, color = "black", family = font_family, size = font_size,
                           margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
                       ),
                       axis.text.y = ggtext::element_markdown(size = ggplot2::rel(0.8) # Controls the 'timing' Pre post labels
                       )
        )
    )
}


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
                       plot.tag.position = c(-0.01, 1.05)  # c(-0.035, 1.04) # manually positions the tag using coordinates
        )
    )
}


#' Helper function to create a custom draw_key_* for the legend key argument key_glyph
#'
#' @description Helper function to create a custom draw_key_* for the legend key argument key_glyph. Custom key glyph
#' that will draw the legend as filled rect boxes colored by fill and text labels that will  be the labels of the
#' fill aesethic, set in 'guide_legend', argument of 'override.aes'.
#'
#' @return a custom draw_key_* function, returns a grid::grobTree.
#'
#' @noRd
draw_key_cust <- function(data, params, size) {
    grid::grobTree(
        grid::rectGrob( # rect needs to be drawn first since default alpha is 1.
            gp = grid::gpar(
                col = "black", # adds black outline box to each key
                fill = scales::alpha(data$fill %||% data$colour %||% "grey20", data$alpha),
                lty = data$linetype %||% 1
            )
        ),
        grid::textGrob( # text needs to be drawn last so it is not overwritten by rect colors since default alpha is 1.
            data$label, 0.5, 0.5,
            hjust = 0.5, # horz center justified text
            vjust = 0.5, # vert center justified text
            rot = data$angle %||% 0,
            gp = grid::gpar(
                col = scales::alpha(data$colour %||% data$fill %||% "black", data$alpha),
                fontfamily = "Arial", # data$family %||% "", forces font to Arial
                fontface = data$fontface %||% 1,
                fontsize = 10, # (data$size %||% 3.88) * .pt, forces font size to 10 pt
                lineheight = 0.9
            )
        )
    ) # end of grid::grobTree()
}

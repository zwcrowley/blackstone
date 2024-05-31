#' Helper Functions for ggplot2 visuals for the `bre` package
#'
#'
#' Helper function to Add a Plot Tag
#'
#' @description A function for add a plot tag to the upper left corner of a ggplot with the total n of the sample.
#'      Pass as args the total n, font size and family.
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

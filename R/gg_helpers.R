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

#' Helper function to create a custom legend grob
#'
#' @description Helper function to create a custom legend grob, taken from grid::legendGrob, added a new arg `sym_size` that
#'      allows the user to change the size of the pch symbol used in the legend keys.
#'
#' @return A grid::grob that contains a custom legend.
#'
#' @noRd
legendGrobCustom <- function(labels, nrow, ncol, byrow=FALSE, do.lines = has.lty || has.lwd, lines.first=TRUE,
                             hgap=grid::unit(1, "lines"), vgap = grid::unit(1, "lines"), default.units="lines",
                             pch, sym_size = grid::unit(1, "char"),
                             gp = gpar(), vp = NULL) {

        ## Type checking on arguments; labels: character, symbol or expression:
        labels <- grDevices::as.graphicsAnnot(labels)
        labels <- if (is.character(labels)) as.list(labels) else as.expression(labels)
        nkeys <- if (is.call(labels)) 1 else length(labels)
        if (nkeys == 0) return(nullGrob(vp=vp))
        if (!is.unit(hgap))
            hgap <- grid::unit(hgap, default.units)
        if (length(hgap) != 1) stop("'hgap' must be single unit")
        if (!is.unit(vgap))
            vgap <- grid::unit(vgap, default.units)
        if (length(vgap) != 1) stop("'vgap' must be single unit")
        ## nrow, ncol
        miss.nrow <- missing(nrow)
        miss.ncol <- missing(ncol)
        if (miss.nrow && miss.ncol) {ncol <- 1; nrow <- nkeys} # defaults to 1-column legend
        else if ( miss.nrow && !miss.ncol) nrow <- ceiling(nkeys / ncol)
        else if (!miss.nrow &&  miss.ncol) ncol <- ceiling(nkeys / nrow)
        if (nrow < 1) stop("'nrow' must be >= 1")
        if (ncol < 1) stop("'ncol' must be >= 1")
        if (nrow * ncol < nkeys)
            stop("nrow * ncol < #{legend labels}")
        ## pch, gp
        if (has.pch <- !missing(pch) && length(pch) > 0) pch <- rep_len(pch, nkeys)
        if (doGP <- length(nmgp <- names(gp)) > 0) {
            if (has.lty  <-  "lty" %in% nmgp) gp$lty  <- rep_len(gp$lty, nkeys)
            if (has.lwd  <-  "lwd" %in% nmgp) gp$lwd  <- rep_len(gp$lwd, nkeys)
            if (has.col  <-  "col" %in% nmgp) gp$col  <- rep_len(gp$col,  nkeys)
            if (has.fill <- "fill" %in% nmgp) gp$fill <- rep_len(gp$fill, nkeys)
        } else {
            gpi <- gp
            if (missing(do.lines)) do.lines <- FALSE
        }

        ## main
        u0 <- grid::unit(0, "npc")
        u1 <- grid::unit(1, "char")
        ord <- if (lines.first) 1:2 else 2:1
        fg <- frameGrob(vp = vp)	  # set up basic frame grob (for packing)
        for (i in seq_len(nkeys)) {
            if (doGP) {
                gpi <- gp
                if (has.lty)	 gpi$lty <- gp$lty[i]
                if (has.lwd)	 gpi$lwd <- gp$lwd[i]
                if (has.col)	 gpi$col <- gp$col[i]
                if (has.fill)    gpi$fill <- gp$fill[i]
            }
            if (byrow) {
                ci <- 1 + (i - 1) %%  ncol
                ri <- 1 + (i - 1) %/% ncol
            } else {
                ci <- 1 + (i - 1) %/% nrow
                ri <- 1 + (i - 1) %%  nrow
            }
            ## borders; unit.c creates a 4-vector of borders (bottom, left, top, right)
            vg <- if (ri != nrow) vgap else u0
            symbol.border <- grid::unit.c(vg, u0, u0, 0.5 * hgap)
            text.border   <- grid::unit.c(vg, u0, u0, if (ci != ncol) hgap else u0)

            ## points/lines grob:
            plGrob <- if(has.pch && do.lines)
                gTree(children = gList(linesGrob(c(sym_size:1, sym_size:1 + 0.5), 0.5, gp=gpi),
                                       pointsGrob(0.5, 0.5, default.units="npc", pch=pch[i], gp=gpi, size = sym_size))[ord])
            else if (has.pch) pointsGrob(0.5, 0.5, default.units="npc", pch=pch[i], gp=gpi, size = sym_size)
            else if (do.lines) linesGrob(c(sym_size:1, sym_size:1 + 0.5), 0.5, gp=gpi)
            else nullGrob() # should not happen...
            fg <- packGrob(fg, plGrob,
                           col = 2*ci-1, row = ri, border = symbol.border,
                           width = u1, height = u1, force.width = TRUE)
            ## text grob: add the labels
            gpi. <- gpi
            gpi.$col <- "black" # maybe needs its own 'gp' in the long run (?)
            fg <- packGrob(fg, textGrob(labels[[i]], x = 0, y = 0.5,
                                        just = c("left", "centre"), gp=gpi.),
                           col = 2*ci, row = ri, border = text.border)
        }
        fg
}



#' Helper Functions for colors of visuals for the `bre` package
#'
#'
#' BRE Colors as a Named Vector
#'
#' @description A utils function for loading Blackstone Research and Evaluation colors for charts.
#'
#' @return A named vector of hex colors for Blackstone Research and Evaluation.
#'
#' @export
breColors <- c("dark_blue" = "#283251",
                "light_grey" = "#eaeaeb",
                "med_grey" = "#cecece",
                "main_grey" = "#c0bfbf")

#' Helper function that creates a blue-grey palette.
#'
#' @description A function to blue-grey palette that can be used to make many different numbers of sequential colors to charts, a colorRampPalette function
#' that accepts the number for breaks for the palette.
#'
#' @return a colorRampPalette function.
#'
#' @noRd
pal_bre_grey_blue <- colorRampPalette(c(breColors["main_grey"], breColors["dark_blue"]))

#' Helper function that creates text label colors.
#'
#' @description A function to label the text color in charts inside the fill of bar charts, returns either "black" or "white" depending on the luminance of the color scale passed to it.
#'
#' @param colors Required, a character vector of hex color codes, usually for the color palette of a chart.
#'
#' @param names Optional, a character vector the same length of `colors` argument to add names to the returned vector.
#'
#' @return a character vector of colors either "black" or "white" for labeling text in fill colors.
#'
#' @export
labelColorMaker <- function(colors, names = NULL) {

    label_color <- ifelse(farver::decode_colour(colors, "rgb", "hcl")[, "l"] > 52, "black", "white") # convert to hcl: if the l in hcl (luminance) > 50, text is black, white otherwise.

    if (!is.null(names)) {
        names(label_color) <- names # Add names to the vector label_color to make it a named vector if names is not NULL.
    }
    return(label_color)
}

#' Helper to create a sequential color scale using `Blues 3` that is reversed.
#'
#' @description A function to create a sequential color scale using `Blues 3`, that is reversed and slightly darkened.
#'
#' @param n_colors Required, the number of color hex codes to return.
#'
#' @return a character vector hex color codes the length of `n_colors` from the `Blues 3` palette.
#'
#' @export
seqFillColors <- function(n_colors) {
    colorspace::darken(colorspace::sequential_hcl(n_colors, "Blues 3", rev = TRUE), amount = 0.1)
    # colorspace::sequential_hcl(n_colors, "Blues 3", rev = TRUE) # not darkened
}


#' Helper to create a sequential color scale using `Blue-Red 3` that is reversed.
#'
#' @description A function to create a sequential color scale using `Blue-Red 3` that is reversed and slightly darkened..
#'
#' @param n_colors Required, the number of color hex codes to return.
#'
#' @return a character vector hex color codes the length of `n_colors`from the `Blue-Red 3` palette from the package `colorspace`.
#'
#' @export
divFillColors <- function(n_colors) {
    colorspace::darken(colorspace::diverging_hcl(n_colors, "Blue-Red 3", rev = TRUE), amount = 0.05)
    # colorspace::diverging_hcl(n_colors, "Blue-Red 3", rev = TRUE) # not darkened
}


#' Helper function to create qualitative colors: either default to `viridis` or `Okabe-Ito`.
#'
#' @description A function to create qualitative colors: pal argument either default to `viridis` or `Okabe-Ito`, reversed if rev_colors is set to TRUE.
#'
#' @param n_colors Required, the number of color hex codes to return.
#'
#' @param pal Required, the name of the palette to use, defaults to `viridis` or can use `Okabe-Ito`.
#'
#' @param rev_colors Logical, defaults to FALSE, if true returns a reverse color codes where the darkest color comes first.
#'
#' @return a character vector of hex color codes the length of `n_colors`.
#' @importFrom grDevices palette.colors
#'
#' @export
qualFillColors <- function(n_colors, pal = "viridis", rev_colors = FALSE) {
    if (pal == "viridis") {
        viridisLite::viridis(n = n_colors, alpha = 1, begin = 0, end = 1, direction = dplyr::if_else(isTRUE(rev_colors), -1, 1), option = "viridis")
    } else if (pal == "Okabe-Ito") {
        if (isTRUE(rev_colors)) {
            rev(grDevices::palette.colors(n = n_colors, palette = "Okabe-Ito"))
        } else if (isFALSE(rev_colors)) {
            grDevices::palette.colors(n = n_colors, palette = "Okabe-Ito")
        }
    } else {
        stop("Error: for `pal` argument either enter 'viridis' or 'Okabe-Ito'")
    }
}

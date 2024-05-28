#' Helper Functions for `bre` package
#'
#'
#' BRE Colors
#'
#' @description A utils function for loading Blackstone Res and Eval colors for charts.
#'
#' @return named vector of hex colors.
#'
#' @noRd
bre_colors <- c("dark_blue" = "#283251",
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
pal_bre_grey_blue <- colorRampPalette(c(bre_colors["main_grey"], bre_colors["dark_blue"]))

#' Helper function that creates text label colors.
#'
#' @description A function to label the text color in charts inside the fill of bar charts, returns a
#'
#' @return a character vector of colors either black or white.
#'
#' @noRd
labelColorMaker <- function(colors, names = NULL) {

    label_color <- ifelse(farver::decode_colour(colors, "rgb", "hcl")[, "l"] > 52, "black", "white") # convert to hcl: if the l in hcl (luminance) > 50, text is black, white otherwise.

    if (!is.null(names)) {
        names(label_color) <- names # Add names to the vector label_color to make it a named vector if names is not NULL.
    }
    return(label_color)
}

#' Helper to create a sequential color scale using `cividis` that is reversed.
#'
#' @description A function to create a sequential color scale using `cividis` that is reversed.
#'
#' @return a character vector hex color codes the length of `n_colors`.
#'
#' @noRd
seqFillColors <- function(n_colors) {
    viridisLite::cividis(n = n_colors, alpha = 1, begin = 0, end = 1, direction = -1)
}


#' Helper to create a sequential color scale using `Blue-Red 3` that is reversed.
#'
#' @description A function to create a sequential color scale using `Blue-Red 3` that is reversed.
#'
#' @return a character vector hex color codes the length of `n_colors`.
#'
#' @noRd
divFillColors <- function(n_colors) {
    colorspace::darken(colorspace::diverging_hcl(n_colors, "Blue-Red 3", rev = TRUE), amount = 0.25, method = "relative", space = "HCL")
    # colorspace::diverging_hcl(n_colors, "Blue-Red 3", rev = TRUE)
}


#' Helper function to create qualitative colors: either default to `viridis` or `Okabe-Ito`.
#'
#' @description A function to create qualitative colors: pal argument either default to `viridis` or `Okabe-Ito`, reversed if rev_colors is set to TRUE.
#'
#' @return a character vector hex color codes the length of `n_colors`.
#' @importFrom grDevices palette.colors
#'
#' @noRd
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

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


#' BRE Qualitative Colors as a Vector
#'
#' @description A utils function for loading Qualitative color scale Blackstone Research and Evaluation colors for charts.
#'
#' @param add_names Required, logical, if FALSE returns a vector of hex color codes, if TRUE returns a named vector with color names
#'      that can be used to select from.
#'
#' @return A vector or named vector of hex colors for Qualitative color scale for Blackstone Research and Evaluation.
#'
#' @export
qualColors <- function(add_names = FALSE) {
    qual_colors <- c(
        `orange`         = "#E69F00",   # 1
        `sky blue`       = "#56B4E9",   # 2
        `bluish green`   = "#009E73",   # 3
        `magenta`        = "#CC79A7",   # 4
        `vermillion`     = "#D55E00",   # 5
        `blue`           = "#0072B2",   # 6
        `viridis purple` = "#440154FF", # 7
        `dark grey`      = "#999999",   # 8
        `dark green`     = "#117733",   # 9
        `bre blue`       = "#283251",   # 10
        `yellow green`   = "#999933"    # 11
        )
    if (isFALSE(add_names)) {
        return(unname(qual_colors))
    } else if (isTRUE(add_names)) {
        return(qual_colors)
    }

}
#' Helper function that makes selections from a color palette.
#'
#' @description A function return hex color codes from a color palette by name or numbered position.
#'
#' @param pal Required, the named color palette as a named vector of color hex codes, defaults to `qualColors`.
#'
#' @param cols Required, a character vector of names or numbered position to use to select
#'      from the named vector of color hex codes , defaults to `NULL` and returns the full palette.
#'
#' @return a named character vector of color hex codes.
#'
#' @export
customCols <- function(pal = qualColors(), cols = NULL) {
    # Gather all color names or vector positions into a new character vector:
    if (is.null(cols)) { # if nothing passed, return full palette:
        return(pal)
    } else {# Otherwise, return a new palette by selection:
        return(pal[cols])
    }
}

#' Helper function to create qualitative colors from the `Okabe-Ito` palette.
#'
#' @description A function to create a custom qualitative colors palette from the `Okabe-Ito` palette, reversed if rev_colors is set to TRUE.
#'      Drops black and yellow for use with all BRE charts and adds three other colors: "#440154FF", "#283251", and "#999933".
#'
#' @param n_colors Required, supply a non-negative integer that is the desired the number of color hex codes to return.
#'#'
#' @param rev_colors Logical, defaults to FALSE, if true returns a reverse color codes where the darkest color comes first.
#'
#' @return a character vector of hex color codes the length of `n_colors`.
#'
#' @export
qualFillColors <- function(n_colors, rev_colors = FALSE) {
    if (n_colors > length(qualColors())) {
        warning("This palette can handle a maximum of ", length(qualColors()), " values.",
                "You have supplied ", n_colors, ", colors will be recycled to reach this number of values.")
    } else if (n_colors < 0) {
        stop("`n_colors` must be a non-negative integer.")
    }
    # Uses rep() to recycle the color values so the returned color code vector is length of n_colors
    # and has no NA's.
    new_pal <- rep(qualColors(), len = n_colors)
    if (isTRUE(rev_colors)) {
        # new_pal <- rev(customCols(cols = c(1:n_colors))) # reversed qualColors palette
        new_pal <- rev(new_pal) # reversed qualColors palette
    }

    return(new_pal)

}

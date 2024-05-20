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

    label_color <- ifelse(farver::decode_colour(colors, "rgb", "hcl")[, "l"] > 50, "black", "white") # convert to hcl: if the l in hcl (luminance) > 50, text is black, white otherwise.

    if (!is.null(names)) {
        names(label_color) <- names # Add names to the vector label_color to make it a named vector if names is not NULL.
    }
    return(label_color)
}

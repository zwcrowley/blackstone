# Testing out new BRE colors for all charts:
bre_blue <- "#283251"
bre_grey <- "#c0bfbf"
bre_light_grey <- "#eaeaeb"
bre_med_grey <- "#cecece"

bre_colors <- c("dark_blue" = "#283251",
                "light_grey" = "#eaeaeb",
                "med_grey" = "#cecece",
                "main_grey" = "#c0bfbf")

# Creating a grey to blue pallete:
pal_bre_grey_blue <- colorRampPalette(c(bre_colors["main_grey"], bre_colors["dark_blue"]))
palette_bre_grey_blue_five <- palette_bre_grey_blue(5) # set to 5 points
palette_bre_grey_blue(5)
# Trying out a grey to blue palette:
# grey_blue_five <- c("#C0BFBF", "#9A9BA3", "#747888", "#4E556C", "#283251")
scales::show_col(palette_bre_grey_blue(5))
scales::show_col(palette_bre_grey_blue(6))
scales::show_col(palette_bre_grey_blue(7))
scales::show_col(palette_bre_grey_blue(20))

# Creating a light grey to blue pallete:
palette_bre_light_grey_blue <- colorRampPalette(c(bre_light_grey, bre_blue))
palette_bre_light_grey_blue_five <- palette_bre_light_grey_blue(5) # set to 5 points

# Trying out a grey to blue palette:
# grey_blue_five <- c("#C0BFBF", "#9A9BA3", "#747888", "#4E556C", "#283251")
scales::show_col(palette_bre_light_grey_blue(5))
scales::show_col(palette_bre_light_grey_blue(6))
scales::show_col(palette_bre_light_grey_blue(7))
scales::show_col(palette_bre_light_grey_blue(20))


# Trying out a white to blue palette:
palette_bre_blues <- colorRampPalette(colors = c("white", "#283251"))
palette_bre_blues_twelve <- palette_bre_blues(12)
scales::show_col(palette_bre_blues(5))
scales::show_col(palette_bre_blues(6))
scales::show_col(palette_bre_blues(7))
scales::show_col(palette_bre_blues(20))
scales::show_col(palette_bre_blues_twelve[1:12])

palette_bre_light_blue <- colorRampPalette(colors = c("#E8E9EC", "#283251"))
scales::show_col(palette_bre_light_blue(5))

# Code to generate text label colors of white or black depending on the shade of color:
scales::show_col(pal_bre_grey_blue(5))
fill_colors <- pal_bre_grey_blue(5)
# hcl <- farver::decode_colour(fill_colors, "rgb", "hcl") # Taken from the source code from the function scales::show_col()
# label_col <- ifelse(hcl[, "l"] > 50, "black", "white") # Found here on line 110: https://github.com/r-lib/scales/blob/HEAD/R/colour-manip.R
# Transformed into a one-liner: need to namespace this function: farver::decode_colour
label_color <- ifelse(farver::decode_colour(fill_colors, "rgb", "hcl")[, "l"] > 50, "black", "white") # convert to hcl: if the l in hcl (luminance) > 50, text is black, white otherwise.

# Try using colorspace package:
library("colorspace")
hcl_palettes(plot = TRUE)

# diverging_hcl(7, palette = "Tropic", h2 = 0, register = "mytropic")
scales::show_col(diverging_hcl(11, "mytropic"))

# blue:
hcl_palettes(n = 5, palette = "Blues 3", plot = TRUE)
scales::show_col(sequential_hcl(n = length(scale_labels) + 1, palette = "Blues 3", rev = TRUE)[1:length(scale_labels) + 1]) # drops the bottom white color
sequential_hcl(n = length(scale_labels) + 1, palette = "Blues 3", rev = TRUE)[1:length(scale_labels) + 1]
# For ggplot:
scale_fill_discrete_sequential("Blues 3")
sequential_hcl(
    n = 5,
    palette = "Blues 3",
    rev = FALSE,
    register = "blue_bre",
)

# GGPLOT Test:
dsamp <- ggplot2::diamonds[1 + 1:1000 * 50, ]
gg <- ggplot2::ggplot(dsamp, ggplot2::aes(carat, price, color = cut)) + ggplot2::geom_point()
gg + scale_color_discrete_sequential(palette = "Blues 3", nmax = 8, order = 4:8)

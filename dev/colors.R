# Testing out new BRE colors for all charts:
bre_blue <- "#283251"
bre_grey <- "#c0bfbf"
bre_light_grey <- "#eaeaeb"
bre_med_grey <- "#cecece"

bre_colors <- c("dark_blue" = "#283251",
                "light_grey" = "#eaeaeb",
                "med_grey" = "#cecece",
                "main_grey" = "#c0bfbf")

### MATERIAL DESIGN ########
# Used material design app to generate colors, based off of BRE Blue (#283251) as primary: https://m2.material.io/inline-tools/color/
# Generates PRIMARY, COMPLEMENTARY, 2 ANALOGOUS, and 2 TRIADIC Colors for the palette:
# Set names like in md app:
names_nums_pals <- c("900", "800", "700", "600", "500", "400", "300", "200", "100", "50")
# Primary: 900 is main (BRE Blue, "#283251")
md_bre_blue <- c("#283251", "#32436a", "#384c77", "#405683", "#475e8c", "#62759a", "#7e8eab", "#a1aec4", "#c5cedc", "#e9ebf0") %>% purrr::set_names(names_nums_pals)
# COMPLEMENTARY: 800 is main ("#514728")
md_bre_comp <- c("#2e2505", "#514728", "#726646", "#877a59", "#b2a481", "#d1c29e", "#f4e4bf", "#fff0cb", "#fff5cf", "#fffad4") %>% purrr::set_names(names_nums_pals)
# ANALOGOUS: 800 is main for teal ("#284751"), 900 is main for teal ("#322851")
md_bre_teal <- c("#18323a", "#284751", "#355a66", "#436e7b", "#4f7d8c", "#68909e", "#82a4b1", "#a3bfc9", "#c3dae0", "#e2f1f8") %>% purrr::set_names(names_nums_pals)
md_bre_purple <- c("#322851", "#403869", "#484175", "#514b81", "#57528a", "#6d6b9a", "#8586ab", "#a6a8c4", "#c9cadc", "#e9eaf0") %>% purrr::set_names(names_nums_pals)

# Triadic: 900 is main for both ("#514728") and ("#512832")
md_bre_tri_purp <- c("#472851", "#603166", "#6e3670", "#7d3d79", "#88427f", "#9a5791", "#ac72a4", "#c498be", "#dbc0d7", "#f0e6ee") %>% purrr::set_names(names_nums_pals)
md_bre_maroon <- c("#512832", "#63353e", "#734147", "#854c53", "#92555b", "#a56f73", "#b9898b", "#d2a9ac", "#ebcacb", "#ffe7e5") %>% purrr::set_names(names_nums_pals)

#########

# Creating a grey to blue pallete:
palette_bre_grey_blue <- colorRampPalette(c(bre_colors["main_grey"], bre_colors["dark_blue"]))
palette_bre_grey_blue_five <- palette_bre_grey_blue(5) # set to 5 points
pal_bre_grey_blue(5)
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
palette_bre_gold_blue <- colorRampPalette(colors = c("#FFD700", "#283251")) #FFD700
scales::show_col(palette_bre_gold_blue(12)[2,5,7,10,12])

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


divergingx_palettes(n = 5, palette = "cividis", plot = TRUE)

divergingx_palettes(n = 7, palette = "geyser", plot = TRUE)

hcl_palettes(palette = "BluYl", n = 7, plot = TRUE)

hcl_palettes(palette = "ag_GrnYl", n = 7, plot = TRUE)

hcl_palettes(palette = "YlGnBu", n = 7, plot = TRUE)

hcl_palettes(palette = "Blue-Red 3", n = 7, plot = TRUE)

hcl_palettes(palette = "Blues", n = 7, plot = TRUE)
divergingx_hcl(5, "cividis")
demoplot(divergingx_hcl(5, "cividis"), type = "bar")
demoplot(lighten(divergingx_hcl(5, "cividis")), type = "bar")
demoplot(c("#283251", "#3E4C6E", "#7C7C7C", "#BFB170", "#FFE93F"), type = "bar")
demoplot(divergingx_hcl(5, "geyser"), type = "bar")
demoplot(darken(divergingx_hcl(5, "geyser")), type = "bar")
"#00214E"

# Create a single swatchplot to share with Eval Management Team
swatchplot(
    "Lighter Cividis" = lighten(divergingx_hcl(7, "cividis", rev = TRUE), amount = 0.2, method = "relative", space = "HCL"),
    "Cividis" = divergingx_hcl(7, "cividis", rev = TRUE),
    "Blues"  = darken(sequential_hcl(7, "Blues", rev = TRUE)),
    "Darker Geyser" = darken(divergingx_hcl(7, "geyser", rev = TRUE)),
    "YlGnBu"  = sequential_hcl(7, "YlGnBu", rev = TRUE),
    "Viridis" = sequential_hcl(7, "Viridis", rev = TRUE),
    "Diverging Blue-Red 3"  = diverging_hcl(7, "Blue-Red 3", rev = TRUE)
)

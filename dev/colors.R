# Testing out new BRE colors for all charts:
bre_blue <- "#283251"
bre_grey <- "#c0bfbf"
bre_light_grey <- "#eaeaeb"
bre_med_grey <- "#cecece"
# Creating a grey to blue pallete:
palette_bre_grey_blue <- colorRampPalette(c(bre_grey, bre_blue))
palette_bre_grey_blue_five <- palette_bre_grey_blue(5) # set to 5 points

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

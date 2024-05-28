library(magrittr)

# functions:
labelColorMaker <- function(colors, names = NULL) {

    label_color <- ifelse(farver::decode_colour(colors, "rgb", "hcl")[, "l"] > 52, "black", "white") # convert to hcl: if the l in hcl (luminance) > 50, text is black, white otherwise.

    if (!is.null(names)) {
        names(label_color) <- names # Add names to the vector label_color to make it a named vector if names is not NULL.
    }
    return(label_color)
}
# labelColorMaker(fill_colors)

# Function to create a sequential color scale using `cividis`, reversed:
seqFillColors <- function(n_colors) {
    viridisLite::cividis(n = n_colors, alpha = 1, begin = 0, end = 1, direction = -1)
}

# Function to create diverging a color scale using `Blue-Red 3`, reversed:
divFillColors <- function(n_colors) {
    colorspace::darken(colorspace::diverging_hcl(n_colors, "Blue-Red 3", rev = TRUE), amount = 0.25, method = "relative", space = "HCL")
    # colorspace::diverging_hcl(n_colors, "Blue-Red 3", rev = TRUE)
}

# Function to create qualitative colors: either `viridis` (default) or `Okabe-Ito`, both reversed:
qualFillColors <- function(n_colors, pal = "viridis", rev_colors = FALSE) {
    if (pal == "viridis") {
        viridis::viridis(n = n_colors, alpha = 1, begin = 0, end = 1, direction = dplyr::if_else(isTRUE(rev_colors), -1, 1), option = "viridis")
    } else if (pal == "Okabe-Ito") {
        if (isTRUE(rev_colors)) {
            rev(palette.colors(n = n_colors, palette = "Okabe-Ito"))
        } else if (isFALSE(rev_colors)) {
            palette.colors(n = n_colors, palette = "Okabe-Ito")
                }
    } else {
        stop("Error: for `pal` argument either enter 'viridis' or 'Okabe-Ito'")
    }
}

# seqFillColors(length(scale_labels))
# divFillColors(length(scale_labels))
# qualFillColors(length(scale_labels))
# qualFillColors(length(scale_labels), pal = "Okabe-Ito")
#
# fill_colors = "seq"
# fill_colors = "div"
# fill_colors = qualFillColors(length(scale_labels))
#
# if (length(fill_colors) > 1) {
#     if (length(fill_colors) >= length(scale_labels)){
#         new_fill_colors <- fill_colors
#     } else {
#         stop("Error: the length of `fill_colors` needs to be greater than or equal to the length of `scale_labels.`")
#     }
# } else if (fill_colors == "seq") {
#     new_fill_colors <- seqFillColors(length(scale_labels))
# } else if (fill_colors == "div") {
#     new_fill_colors <- divFillColors(length(scale_labels))
# }

#
bre_colors <- c("dark_blue" = "#283251",
                "light_grey" = "#eaeaeb",
                "med_grey" = "#cecece",
                "main_grey" = "#c0bfbf")


pal_bre_grey_blue <- colorRampPalette(c(bre_colors["light_grey"], bre_colors["dark_blue"]))

#########
items <- dplyr::tibble(
  pre_Organization = c(1, 2, 3, 4, 5, 4, 3, 2, 1),
  post_Organization = dplyr::if_else(pre_Organization < 5, pre_Organization + 1, pre_Organization),
  pre_Source = c(2, 2, 3, 5, 4, 3, 2, 1, 2),
  post_Source = dplyr::if_else(pre_Source < 4, pre_Source + 2, pre_Source),
  pre_Publish = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  post_Publish = pre_Publish + 2,
  pre_Write = c(2, 2, 2, 3, 3, 3, 4, 4, 4),
  post_Write = pre_Write + 1,
  pre_Research = c(1, 1, 2, 2, 3, 3, 4, 4, 4),
  post_Research = pre_Research + 1
)
#'
items_single <- dplyr::tibble(
  Organization = c(1, 2, 3, 4, 5, 4, 3, 2, 1),
  Source = c(2, 2, 3, 5, 4, 3, 2, 1, 2),
  Publish = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  Write = c(2, 2, 2, 3, 3, 3, 4, 4, 4),
  Research = c(1, 1, 2, 2, 3, 3, 4, 4, 4),
)
#'
# Set scale_labels for recodeCat function:
# scale_labels as a named character vector, items in correct order:
levels_min_ext <- c(
  "Minimal" = "1", "Slight" = "2", "Moderate" = "3",
  "Good" = "4", "Extensive" = "5"
)
#'
# bar_scale_labels as just the names from levels_min_ext:
bar_scale_labels <- names(levels_min_ext)
#'
# Question labels as a named vector with the naming structure
# like this: c("new label" = "original variable name"):
question_labels <- c(
  "Publish a lot of high quality papers" = "Publish",
  "Write a lot of research papers" = "Write",
  "Research in a lab with faculty" = "Research",
  "Organization of a large research project" = "Organization",
  "Source work for a research paper" = "Source"
)
#'
# Recode the numeric to factor variables using the levels from levels_min_ext:
cat_items <- bre::recodeCat(items, levels_min_ext)
cat_items_single <- bre::recodeCat(items_single, levels_min_ext)
#'
# Select the factor variables:
cat_items <- cat_items %>% dplyr::select(dplyr::where(is.factor))
cat_items_single <- cat_items_single %>% dplyr::select(dplyr::where(is.factor))

stackedBarChart(
   df = cat_items_single, pre_post = FALSE, scale_labels = bar_scale_labels,
   question_labels = NULL, percent_label = TRUE, width = NULL, overall_n = F
)

################
df = cat_items_single
pre_post = FALSE
scale_labels = bar_scale_labels
question_labels = NULL
question_order = FALSE
percent_label = TRUE
width = NULL
overall_n = TRUE
fill_colors = qualFillColors(length(scale_labels))

    # Load all fonts:
    extrafont::loadfonts("all", quiet = TRUE)

    # Set . to NULL to stop message when using dot notation in mutate:
    . <- NULL

    # Start of data manipulation: ----
    # Make sure the all vars in df are factors with scale_labels as their levels:
    new_df <- {{ df }} %>% dplyr::mutate(dplyr::across(tidyselect::everything(), ~ factor(., levels = scale_labels)))

    # Changes scale_labels to tibble pulls out index and saves that as a vector, gets number of levels from scale_labels:
    number_levels <- scale_labels %>%
        tibble::enframe() %>%
        dplyr::select("name") %>%
        tibble::deframe()

    # Error messages if number_levels is less than 3:
    if (length(number_levels) < 3) {
        stop("Error: the response options in scale_labels are less than 3, they must be between 3 and 7 for this function.")
    }
    # Error messages if number_levels is greater than 7:
    if (length(number_levels) > 7) {
        stop("Error: the response options in scale_labels are greater than 7, they must be between 3 and 7 for this function.")
    }

    if (isTRUE(pre_post)) {
        # Sets up new_df if pre_post is TRUE:
        new_df <- new_df %>%
            tidyr::pivot_longer(tidyselect::everything(), names_to = "question", values_to = "response") %>%
            dplyr::mutate(question = stringr::str_remove(.data$question, "cat_")) %>%
            tidyr::separate(.data$question, into = c("timing", "question"), sep = "_", extra = "merge") %>%
            dplyr::group_by(.data$question, .data$timing, .data$response) %>%
            dplyr::summarize(n_answers = dplyr::n(), .groups = "keep") %>%
            dplyr::ungroup() %>%
            tidyr::drop_na() %>%
            dplyr::group_by(.data$question, .data$timing) %>%
            dplyr::mutate(
                percent_answers = .data$n_answers / sum(.data$n_answers),
                percent_answers_label = scales::percent(.data$percent_answers, accuracy = 1),
                response = forcats::fct_relevel(.data$response, scale_labels),
                timing = stringr::str_to_title(.data$timing),
                timing = factor(.data$timing, levels = c("Pre", "Post"))
            ) %>%
            dplyr::ungroup()

        # Set up a new question order if not supplied by the user after finding the most positive valenced items for post
        # (top levels depending on total response levels):
        if (isFALSE(question_order)) {
            question_order <- new_df %>% dplyr::filter(., .data$timing == "Post")  %>% tidyr::complete(.data$question, .data$response) %>%
                tidyr::pivot_wider(id_cols = -c("timing", "percent_answers", "percent_answers_label"), names_from = "response", values_from = "n_answers") %>%
                dplyr::group_by(.data$question) %>% rev() %>%
                dplyr::arrange(dplyr::across(-c("question"), dplyr::desc)) %>%
                dplyr::select("question") %>%
                unique() %>%
                tibble::deframe()
            # change the factor levels of question to be ordered by the question_order:
            new_df <- new_df %>% dplyr::mutate(question = forcats::fct_relevel(.data$question, question_order))
        } else {
            # If question_order == TRUE,set up the levels for question using the user supplied order = question_labels:
            new_df <- new_df %>% dplyr::mutate(question = factor(.data$question, levels = question_labels))
        }
        # If the user supplies a named vector for questions labels:
        if (!is.null(question_labels)) {
            names(question_labels) <- names(question_labels) %>%
                stringr::str_wrap(., width = 30) %>%
                gsub("\n", "<br>", .)
            new_df <- new_df %>%
                dplyr::mutate(question = forcats::fct_recode(.data$question, !!!question_labels))
        }
        # Get total n for each question, grouped by question and timing:
        totals_new_df <- new_df %>%
            dplyr::group_by(.data$question, .data$timing) %>%
            dplyr::summarize(total = sum(.data$n_answers), .groups = "keep") %>%
            dplyr::ungroup() %>%
            dplyr::group_by(.data$question) %>%
            dplyr::distinct(.data$question, .data$total) %>%
            dplyr::ungroup()
        # End of if pre_post == TRUE
    } else {
        # If pre_post is FALSE, set up new_df:
        new_df <- new_df %>%
            tidyr::pivot_longer(tidyselect::everything(), names_to = "question", values_to = "response") %>%
            dplyr::mutate(question = stringr::str_remove(.data$question, "cat_")) %>%
            dplyr::group_by(.data$question, .data$response) %>%
            dplyr::summarize(n_answers = dplyr::n(), .groups = "keep") %>%
            dplyr::ungroup() %>%
            tidyr::drop_na() %>%
            dplyr::group_by(.data$question) %>%
            dplyr::mutate(
                percent_answers = .data$n_answers / sum(.data$n_answers),
                percent_answers_label = scales::percent(.data$percent_answers, accuracy = 1),
                response = forcats::fct_relevel(.data$response, scale_labels)
            ) %>%
            dplyr::ungroup()

        # Set up a new question order if not supplied by the user after finding the most positive valenced items for post
        # (top levels depending on total response levels):
        if (isFALSE(question_order)) {
            question_order <- new_df %>% tidyr::complete(.data$question, .data$response) %>%
                tidyr::pivot_wider(id_cols = -c("percent_answers", "percent_answers_label"), names_from = "response", values_from = "n_answers") %>%
                dplyr::group_by(.data$question) %>% rev() %>%
                dplyr::arrange(dplyr::across(-c("question"), dplyr::desc)) %>%
                dplyr::select("question") %>%
                unique() %>%
                tibble::deframe()
            # change the factor levels of question to be ordered by the question_order:
            new_df <- new_df %>% dplyr::mutate(question = forcats::fct_relevel(.data$question, question_order))
        } else {
            # If question_order == TRUE,set up the levels for question using the user supplied order = question_labels:
            new_df <- new_df %>% dplyr::mutate(question = factor(.data$question, levels = question_labels))
        }
        # If the user supplies a named vector for questions labels:
        if (!is.null(question_labels)) {
            names(question_labels) <- names(question_labels) %>%
                stringr::str_wrap(., width = 30) %>%
                gsub("\n", "<br>", .)
            new_df <- new_df %>%
                dplyr::mutate(question = forcats::fct_recode(.data$question, !!!question_labels))
        }
        # Get total n for each question, grouped by question:
        totals_new_df <- new_df %>%
            dplyr::group_by(.data$question) %>%
            dplyr::summarize(total = sum(.data$n_answers), .groups = "keep") %>%
            dplyr::ungroup()
    } # End of if pre_post == FALSE

    # Create fill_colors vectors and label_color variable -----
    # IF/ELSE statement, first if number_levels equals 3, sets up the label_color and fill color:
    # if (length(number_levels) == 3) {
    #     new_df <- new_df %>% dplyr::mutate(label_color = "black")
    #     # 3 colors for chart:
    #     fill_colors <- c("#79AB53", "#4B9FA6", "#2C2C4F")
    #     # If number_levels) == 4
    # } else if (length(number_levels) == 4) {
    #     new_df <- new_df %>% dplyr::mutate(label_color = dplyr::if_else(.data$response == levels(.data$response)[1], "black", "white"))
    #     # 4 colors for chart:
    #     fill_colors <- c("#FFE699", "#79AB53", "#4B9FA6", "#2C2C4F")
    #     # If number_levels) == 5
    # } else if (length(number_levels) == 5) {
    #     new_df <- new_df %>% dplyr::mutate(label_color = dplyr::if_else(.data$response == levels(.data$response)[1], "black", "white"))
    #     # 5 colors for chart:
    #     fill_colors <- c("#FFE699", "#79AB53","#767171", "#4B9FA6", "#2C2C4F")
    #     # If number_levels) == 6
    # } else if (length(number_levels) == 6) {
    #     new_df <- new_df %>% dplyr::mutate(label_color = dplyr::if_else(.data$response == levels(.data$response)[2], "black", "white"))
    #     # 6 colors for chart:
    #     fill_colors <- c("gray","#FFE699", "#79AB53","#767171", "#4B9FA6", "#2C2C4F")
    #     # If number_levels) == 7
    # } else if (length(number_levels) == 7) {
    #     new_df <- new_df %>% dplyr::mutate(label_color = dplyr::if_else(.data$response == levels(.data$response)[2], "black", "white"))
    #     # 7 colors for chart:
    #     fill_colors <- c("gray","#FFE699", "#79AB53","#767171", "#4B9FA6", "#37546d", "#2C2C4F")
    # }

    ### Set up fill color: -------
    # fill_colors <- pal_bre_grey_blue(length(scale_labels)) # create a seq color palette of grey to blue using the length of scale_labels
    #fill_colors <- colorspace::sequential_hcl(n = length(scale_labels) + 1, palette = "Blues 3", rev = TRUE)[1:length(scale_labels) + 1] #creates blue color scale:
    # fill_colors <- viridisLite::cividis(n = length(scale_labels), alpha = 1, begin = 0, end = 1, direction = -1)

    # fill_colors = "seq"
    # fill_colors = "div"
    # fill_colors = qualFillColors(length(scale_labels))

    if (length(fill_colors) > 1) {
        if (length(fill_colors) >= length(scale_labels)) {
            new_fill_colors <- fill_colors
        } else {
            stop("Error: the length of `fill_colors` needs to be greater than or equal to the length of `scale_labels.`")
        }
    } else if (fill_colors == "seq") {
        new_fill_colors <- seqFillColors(length(scale_labels))
    } else if (fill_colors == "div") {
        new_fill_colors <- divFillColors(length(scale_labels))
    }

    ### Set up label colors for text on bars of percentage or counts: -------
    # Use the internal function labelColorMaker(), to create text color labels of black or white, see `helpers.R`:
    label_colors <- labelColorMaker(new_fill_colors, names = scale_labels)
    # create a new col `label_color` using the named vector `label_colors` to map the text color to the variable response
    new_df <- new_df %>% dplyr::mutate(., label_color = label_colors[response]) # create a new col `label_color` using the named vector `label_colors` to map the text color to the variable response


    # Set default width for geom_col() bars if not supplied by user:
    if (is.null(width)) {
        width <- dplyr::if_else(dplyr::n_distinct(new_df$question) < 4, 0.5,
                                dplyr::if_else(dplyr::n_distinct(new_df$question) < 7, 0.75, 0.95)
        )
    }

    if (isTRUE(overall_n)) {
        # Return N_df that will be an overall n for all the items, only if all totals_new_df$total are equal to each other: ----
        N_df <- NULL
        if (length(unique(totals_new_df$total)) == 1) {
            # Get overall n if it is the same for each item:
            N_df <- totals_new_df %>%
                dplyr::summarize(N = mean(.data$total)) %>%
                tibble::deframe()
        }

        # Error messages if N_df is null, not filled by last if statement above:
        if (is.null(N_df)) {
            stop("Error: Can not use `overall_n` for this function, responses for variables are not of equal length. Use argument: `overall_n = FALSE`.")
        }
    }

    # Otherwise, if overall_n == FALSE, return a stacked_bar_chart with n for each question appended to the question label:
    if (isFALSE(overall_n)) {
        # Change the label of the variable "question" by adding n of each to the end of the character string:
        labels_n_questions <- paste0(totals_new_df$question, " ", "(*n* = ", totals_new_df$total, ")")

        # Set factor labels for question to labels:
        new_df <- new_df %>%
            dplyr::mutate(question = factor(.data$question, levels = levels(.data$question), labels = labels_n_questions))
    }

    # Call stackedBar_ggplot ----
    # Set labels to percent or n_answers:
    if (isTRUE(percent_label)) {
        label_gg <- new_df$percent_answers_label
    } else {
        label_gg <- new_df$n_answers
    }

    # Final call to stackedBar_ggplot() for pre_post == TRUE:
    if (isTRUE(pre_post)) {
        stacked_bar_chart <- stackedBar_ggplot(df_gg = new_df, x_gg = .data$percent_answers , y_gg = .data$timing, fill_gg = .data$response, group_gg = .data$question,
                                               label_gg = label_gg, label_color_gg = .data$label_color, scale_labels_gg = scale_labels,
                                               width_gg = width, new_fill_colors_gg = new_fill_colors, overall_n_gg = overall_n, N_df_gg = N_df, pre_post = TRUE)
        # Final call to stackedBar_ggplot() if pre_post == FALSE:
    } else {
        # Load all fonts:
        extrafont::loadfonts("all", quiet = TRUE)

        # Set . to NULL to stop message when using dot notation in mutate:
        . <- NULL

        # Create a font family character var so that it is easy to change, could also be a new arg:
        font_family <- c("Arial")

        df_gg = new_df
        x_gg = new_df$percent_answers
        y_gg = new_df$question
        fill_gg = new_df$response
        group_gg = new_df$question
        label_gg = label_gg
        label_color_gg = new_df$label_color
        scale_labels_gg = scale_labels
        width_gg = width
        fill_colors_gg = new_fill_colors
        overall_n_gg = overall_n
        N_df_gg = N_df
        pre_post = FALSE

        stacked_bar_chart <- {{df_gg}} %>%
            ggplot2::ggplot(ggplot2::aes(
                x = {{x_gg}}, y = forcats::fct_rev({{y_gg}}), fill = {{fill_gg}},
                label = {{label_gg}}, group = {{group_gg}})) +
            ggplot2::geom_col(width = {{width_gg}}, position = ggplot2::position_stack(reverse = TRUE), color = "black") +
            ggplot2::geom_text(ggplot2::aes(color = {{label_color_gg}}),
                               family = font_family,
                               fontface = "bold",
                               position = ggplot2::position_stack(vjust = .5, reverse = TRUE), size = 3.5
            ) +
            ggplot2::scale_color_identity() +
            ggplot2::scale_fill_manual(breaks = {{scale_labels_gg}}, values = {{fill_colors_gg}}, drop = FALSE,
                                       labels = stringr::str_wrap({{scale_labels_gg}}, width = 10) %>% gsub("\n", "<br>", .)
                                       # guide = ggplot2::guide_legend(override.aes = ggplot2::aes(color = NA, fill = NA))
            ) +
            ggplot2::guides(color = "none") +
            ggplot2::theme_void(base_family = font_family, base_size = 11) +
            ggplot2::theme(
                axis.text.y = ggtext::element_markdown(
                    angle = 0, hjust = 1, color = "black", size = 11, family = font_family,
                    margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
                ),
                plot.margin = ggplot2::margin(t = 35, r = 5, b = 35, l = 5, unit = "pt"),
                legend.text = ggtext::element_markdown(angle = 0, hjust = 0.5, vjust = 0.5, halign = 0.5, valign = 0.5,
                                                       size = 11, family = font_family, face = "bold",
                                                       margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
                ),
                # legend.justification = c("right", "top"),
                legend.position = "top",
                legend.key = ggplot2::element_blank(),
                legend.title = ggplot2::element_blank()
            )

    }
stacked_bar_chart


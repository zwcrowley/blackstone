#' Stacked Bar Chart for The Mark Reporting
#'
#' @param df A tibble/dataframe of survey items that are categorical/character variables, in 5 point scales and pre-post, that will be inserted into a stacked bar chart with The Mark USA branding.
#'
#' @return A ggplot object that plots the items into a stacked bar chart and can be exported.
#' @export
#'
#' @examples
#' items <- tibble(cat_Pre_Sources = c("Good","Moderate","Minimal","Slight","Slight","Moderate","Good"),
#'               cat_Post_Sources = c("Good","Good","Minimal","Moderate","Moderate","Good","Extensive"),
#'               cat_Pre_Orgs = c("Good","Moderate","Minimal","Slight","Slight","Moderate","Moderate"),
#'               cat_Post_Orgs = c("Good","Moderate","Minimal","Moderate","Moderate","Good","Extensive"))
#' stackedBarChart(items)
stackedBarChart <- function(df) {
    # Sets up the color options-
    # The Mark colors for 5 scales:
    fiveScale_theMark_colors <-  c("#767171", "#FFE699", "#79AB53", "#4B9FA6", "#2C2C4F")

    # The Mark colors for 2 scales:
    twoScale_theMark_colors <-  c("#4B9FA6", "#2C2C4F")

    # Sets up the text for the question and timing labels on the left side of the chart, passed to strip in facet_nested(),
    # hjust moves the text left = 0, right = 1, the first in the list is for question, second for timing:
    texts <- list(ggplot2::element_text(size = 14, family = "Gill Sans MT", hjust = 1),
                  ggplot2::element_text(size = 11, family = "Gill Sans MT",  hjust = 0.6))

    # Stacked bar chart:
    # Create a new df that will be passed to ggplot:
    # Double curly brackets allows tidy functions to read in the values in the df:
    new_df <-  {{df}} %>%
        dplyr::pivot_longer(tidyselect::everything(), names_to = "question", values_to = "response") %>%
        dplyr::mutate(question = stringr::str_remove(question, "cat_")) %>%
        dplyr::separate(question, into = c("timing", "question"), sep = "_") %>%
        dplyr::group_by(question, timing, response) %>%
        dplyr::count(name = "n_answers") %>%
        dplyr::ungroup() %>%
        dplyr::group_by(question, timing) %>%
        dplyr::mutate(percent_answers = n_answers / sum(n_answers)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(percent_answers_label = scales::label_percent(percent_answers, accuracy = 1),
               timing = forcats::factor(timing, levels = c("Pre","Post")),
               label_color = dplyr::if_else(response == levels(response)[2], "black", "white")) #This sets the color options so that when the second value in response is set the light yellow the geom_text will be set to black in the scale_color_manual(values = c('black','white')) call.

    # This will calculate the N by taking the max of the sum of all columns once in wide format, use this for the tag N=:
    N_df <- new_df %>% dplyr::select(-c(percent_answers,percent_answers_label)) %>%
        dplyr::pivot_wider(names_from = c(question,timing), values_from = c(n_answers)) %>%
        dplyr::summarise(dplyr::across(tidyselect::where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% max()

    # Pass new_df to ggplot:
    stacked_bar_chart <- new_df  %>%
        ggplot2::ggplot(ggplot2::aes(x = question, y = percent_answers, fill = response, label = n_answers)) +
        # set the col width and position
        ggplot2::geom_col(width = 3, position = ggplot2::position_stack(reverse = TRUE)) +
        # set the labels on the columns
        ggplot2::geom_text(ggplot2::aes(y = percent_answers, label = n_answers, group = response, color = label_color),
                           family = "Gill Sans MT", fontface = "bold",
                  position = ggplot2::position_stack(vjust = .5,reverse = T), size = 3) +
        ggplot2::scale_color_manual(values = c('black','white')) + #Sets up the color of the geom_text from the label_color in the df.
        ggplot2::coord_flip() +
        # set the bars to a facet based on timing and puts the labels to the left side: scales = "free_y", switch = "y", space = "free",
        ggh4x::facet_nested_wrap(vars(question,timing), ncol = 1, scales = "free", strip.position = "left",
                          strip = ggh4x::strip_nested(text_y = texts, by_layer_y = TRUE)) +
        # sets fill color, drop=F shows all values, and labels wraps the text in the legend
        ggplot2::scale_fill_manual(values = fiveScale_theMark_colors, drop = FALSE, labels = function(response) stringr::str_wrap(response, width = 10)) +
        # changes the legend text to color of fill, sets the size and family of text, spaces the labels:
        ggplot2::guides(color = "none",fill = ggh4x::guide_stringlegend(size = 12, family = "Gill Sans MT", face = "bold", hjust = 0, vjust = 0, ncol = 5,
                                                        spacing.x = 14, spacing.y = 0)) +
        ggplot2::labs(title = NULL, fill = NULL, y = NULL, x = NULL, tag = paste("N=",N_df,sep = "")) +
        # Changes the col to relative, does not take up the full 100%.
        ggplot2::scale_y_discrete(expand = c(0,0)) + # Changes the col to relative, does not take up the full 100%
        # Changes the size of the facet panel, for this set up cols are the width and rows are the height
        ggh4x::force_panelsizes(cols = c(10, 10), rows = c(.5, .5), respect = TRUE) +
        ggplot2::labs(title = NULL, fill = NULL, y = NULL, x = NULL, tag = paste("N=",N_df,sep = "")) +
        ggplot2::theme_void(base_family = "Gill Sans MT", base_size = 12) +
        ggplot2::theme(plot.margin = ggplot2::margin(.5, .5, .5, 0, "cm"), # add a margin to the whole plot: top,right,bottom,left
              # strip.text.x.top = element_text(margin = margin(), size = 36, family = "Gill Sans MT", face = "bold"), # flips Pre-Post labels on left
              # strip.text.y.right = element_text(size = 36),
              panel.spacing.y = ggplot2::unit(.1, "inches"),
              legend.position = "top")

    return(stacked_bar_chart)

}

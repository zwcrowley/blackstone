stackedBarChart <- function(df) {
    # Sets up the color options-
    # The Mark colors for 5 scales:
    fiveScale_theMark_colors <-  c("#767171", "#FFE699", "#79AB53", "#4B9FA6", "#2C2C4F")

    # The Mark colors for 2 scales:
    twoScale_theMark_colors <-  c("#4B9FA6", "#2C2C4F")

    # Sets up the text for the question and timing labels on the left side of the chart, passed to strip in facet_nested(),
    # hjust moves the text left = 0, right = 1, the first in the list is for question, second for timing:
    texts <- list(element_text(size = 14, family = "Gill Sans MT", hjust = 1),
                  element_text(size = 11, family = "Gill Sans MT",  hjust = 0.6))

    # Stacked bar chart:
    # Create a new df that will be passed to ggplot:
    # Double curly brackets allows tidy functions to read in the values in the df:
    new_df <-  {{df}} %>%
        pivot_longer(everything(), names_to = "question", values_to = "response") %>%
        mutate(question = str_remove(question, "cat_")) %>%
        separate(question, into = c("timing", "question"), sep = "_") %>%
        group_by(question, timing, response) %>%
        count(name = "n_answers") %>%
        ungroup() %>%
        group_by(question, timing) %>%
        mutate(percent_answers = n_answers / sum(n_answers)) %>%
        ungroup() %>%
        mutate(percent_answers_label = percent(percent_answers, accuracy = 1),
               timing = factor(timing, levels = c("Pre","Post")),
               label_color = if_else(response == levels(response)[2], "black", "white")) #This sets the color options so that when the second value in response is set the light yellow the geom_text will be set to black in the scale_color_manual(values = c('black','white')) call.

    # This will calculate the N by taking the max of the sum of all columns once in wide format, use this for the tag N=:
    N_df <- new_df %>% select(-c(percent_answers,percent_answers_label)) %>%
        pivot_wider(names_from = c(question,timing), values_from = c(n_answers)) %>%
        summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% max()

    # Pass new_df to ggplot:
    stacked_bar_chart <- new_df  %>%
        ggplot(aes(x = question, y = percent_answers, fill = response, label = n_answers)) +
        # set the col width and position
        geom_col(width = 3, position = position_stack(reverse = TRUE)) +
        # set the labels on the columns
        geom_text(aes(y = percent_answers, label = n_answers, group = response, color = label_color), family = "Gill Sans MT", fontface = "bold",
                  position = position_stack(vjust = .5,reverse = T), size = 3) +
        scale_color_manual(values = c('black','white')) + #Sets up the color of the geom_text from the label_color in the df.
        coord_flip() +
        # set the bars to a facet based on timing and puts the labels to the left side: scales = "free_y", switch = "y", space = "free",
        facet_nested_wrap(vars(question,timing), ncol = 1, scales = "free", strip.position = "left",
                          strip = strip_nested(text_y = texts, by_layer_y = TRUE)) +
        # sets fill color, drop=F shows all values, and labels wraps the text in the legend
        scale_fill_manual(values = fiveScale_theMark_colors, drop = FALSE, labels = function(response) str_wrap(response, width = 10)) +
        # changes the legend text to color of fill, sets the size and family of text, spaces the labels:
        guides(color = "none",fill = guide_stringlegend(size = 12, family = "Gill Sans MT", face = "bold", hjust = 0, vjust = 0, ncol = 5,
                                                        spacing.x = 14, spacing.y = 0)) +
        labs(title = NULL, fill = NULL, y = NULL, x = NULL, tag = paste("N=",N_df,sep = "")) +
        # Changes the col to relative, does not take up the full 100%.
        scale_y_discrete(expand = c(0,0)) + # Changes the col to relative, does not take up the full 100%
        # Changes the size of the facet panel, for this set up cols are the width and rows are the height
        force_panelsizes(cols = c(10, 10), rows = c(.5, .5), respect = TRUE) +
        labs(title = NULL, fill = NULL, y = NULL, x = NULL, tag = paste("N=",N_df,sep = "")) +
        theme_void(base_family = "Gill Sans MT", base_size = 12) +
        theme(plot.margin = margin(.5, .5, .5, 0, "cm"), # add a margin to the whole plot: top,right,bottom,left
              # strip.text.x.top = element_text(margin = margin(), size = 36, family = "Gill Sans MT", face = "bold"), # flips Pre-Post labels on left
              # strip.text.y.right = element_text(size = 36),
              panel.spacing.y = unit(.1, "inches"),
              legend.position = "top")

    return(stacked_bar_chart)

}

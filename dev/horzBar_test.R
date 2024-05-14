# Test to fix horzBarChart() y text labels:
library(magrittr)

data <- dplyr::tibble(
    role = c(
        "Faculty", "Postdoc", "Undergraduate student", "Graduate student",
        "Graduate student", "Postdoc", "Postdoc", "Faculty",
        "Faculty", "Graduate student", "Graduate student", "Postdoc",
        "Faculty", "Faculty", "Faculty", "Faculty", "Faculty", "Graduate student",
        "Undergraduate student", "Undergraduate student"
    )
)

role_summ <- data %>%
    dplyr::select(role) %>%
    bre::dataSumm()

role_color <- c("#2C2C4F", "#4B9FA6", "#79AB53", "#767171", "yellow")

bre::horzBarChart(df = role_summ, scale_colors = role_color, width = 0.6)

width = 0.6
role_summ %>% ggplot2::ggplot(ggplot2::aes(x = .data$n_answers, y = forcats::fct_rev(.data$response), label = .data$percent_answers_label,
                                           fill = .data$response)) +
    ggplot2::geom_col(width = width, color = "black") +
    ggplot2::scale_fill_manual(values = role_color) +
    ggrepel::geom_text_repel(nudge_x = .1, fontface = "bold", size = 4,
                             min.segment.length = Inf) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, .2))) +
    ggplot2::theme_void(base_family = "Gill Sans MT", base_size = 12) +
    ggplot2::theme(
        legend.position = "none",
        axis.text.y = ggplot2::element_text(
            angle = 0, hjust = 1, color = "black",
            margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
        ),
        axis.text.x = ggplot2::element_text(
            angle = 0, hjust = 1, color = "#767171",
            margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
        ),
        plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
    )

ggplot2::ggplot(role_summ, ggplot2::aes(x = .data$n_answers, y = forcats::fct_rev(.data$response), label = .data$percent_answers_label)) +
    ggplot2::geom_col(fill = role_color, color = "black", width = 0.6) +
    ggrepel::geom_text_repel(nudge_x = 0.2, fontface = "bold", size = 4, min.segment.length = Inf) +
    ggplot2::theme_void(base_family = "Gill Sans MT", base_size = 12) +
    ggplot2::theme(
        legend.position = "none",
        axis.text.y = ggplot2::element_text(
            angle = 0, hjust = 1, color = "black",
            margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
        ),
        axis.text.x = ggplot2::element_text(
            angle = 0, hjust = 1, color = "#767171",
            margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
        ),
        plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
    )

df <- role_summ
scale_colors <- role_color
bar_chart <- {{ df }} %>%
    ggplot2::ggplot(ggplot2::aes(
        x = .data$n_answers, y = forcats::fct_rev(.data$response),
        label = .data$percent_answers_label, fill = .data$response
    )) +
    ggplot2::geom_col(width = width, color = "black") +
    ggplot2::scale_fill_manual(values = scale_colors) +
    ggrepel::geom_text_repel(
        nudge_x = .1, fontface = "bold", size = 4,
        min.segment.length = Inf
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.025, 0.2))) +
    ggplot2::theme_void(base_family = "Gill Sans MT", base_size = 12) +
    ggplot2::theme(
        legend.position = "none",
        axis.text.y = ggplot2::element_text(
            angle = 0, hjust = 1, color = "black",
            margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
        ),
        axis.text.x = ggplot2::element_text(
            angle = 0, hjust = 1, color = "#767171",
            margin = ggplot2::margin(t = 5, r = 0, b = 5, l = 5, unit = "pt")
        ),
        plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
    )

bar_chart

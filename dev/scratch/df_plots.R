library(tidyverse)
library(TheMarkUSA)
mark_colors_six <- c("#2C2C4F", "#4B9FA6", "#767171", "#79AB53", "#FFE699","#BFBFBF")

# Read in data:
data <- read.csv("inst/extdata/fake_data.csv")

# Create a vector of the variables we want from the data:
selected_horz_bars <- c("gender", "role")

# Create a df of the name of variable and the bar chart to be created with it:
horz_bars_df <- map(selected_horz_bars, \(x) data %>% dplyr::select(all_of(x)) %>% TheMarkUSA::dataSumm(.,sort_n = TRUE) %>%
                        TheMarkUSA::horzBarChart(., scale_colors = mark_colors_six, width = 0.4)) %>% setNames(., selected_horz_bars)


selected_horz_bars %>% list() %>% as.character() %>%
    str_c('"', ., '"') %>%
    str_c(collapse = ",") %>% cat()

cat("Order of horizontal bar charts in the exported report will be: ", selected_horz_bars %>% str_c('"', ., '"') %>% str_c(collapse = ","))

inserted_sb <- c("1", "2","3")
sb_id <- "4"
inserted_sb <<- c(inserted_sb, sb_id)
inserted_sb <- inserted_sb[!inserted_sb %in% sb_id]

#####################
# Setup for stacked bar charts
# set up df for 5 inputs, they will look like this and replicate with lists:
# input[["title_"]]{id}, input[["vars_"]]{id}, input[["levels_"]]{id}, input[["percent_label_"]]{id}, input[["overall_n_"]]{id}
# For example, the lists as how they would be in shiny as individual lists:
title <- list("Research Skills", "Collaboration Skills", "Training Usefulness")
vars <- list(c("Organization", "Source", "Publish", "Write", "Research"),
             c("Discuss", "Share", "Integrate"),
             c("Responsible", "Ethics", "Standards", "Practices", "Morals")
)
levels <- list(c("Minimal", "Slight", "Moderate", "Good", "Extensive"),
               c("Never", "Rarely", "Sometimes", "Frequently", "Always"),
               c("Not at all useful","Slightly useful","Somewhat useful", "Very useful", "Extremely useful")
)
percent_label <- list(TRUE, TRUE, TRUE)
overall_n <- list(FALSE, FALSE, FALSE)

# Try to replicate above using individual lists/vectors with the reactiveValue list- replicate actual shiny case:
# If user creates 3, then 4 UI ids and then removes the 4th:
inserted_sb <- c("1", "2","3")
sb_id <- "4"
inserted_sb <<- c(inserted_sb, sb_id)
inserted_sb <- inserted_sb[!inserted_sb %in% sb_id]

# Individual vectors of how inputs would be inside shiny app:
title_1 <- c("Research Skills")
title_2 <- c("Collaboration Skills")
title_3 <- c("Training Usefulness")

vars_1 <- list("Organization", "Source", "Publish", "Write", "Research")
vars_2 <- list("Discuss", "Share", "Integrate")
vars_3 <- list("Responsible", "Ethics", "Standards", "Practices", "Morals")

levels_1 <- c("Minimal, Slight, Moderate, Good, Extensive")
levels_2 <- c("Never, Rarely, Sometimes, Frequently, Always")
levels_3 <- c("Not at all useful, Slightly useful, Somewhat useful, Very useful, Extremely useful")

percent_label_1 <- c(TRUE)
percent_label_2 <- c(TRUE)
percent_label_3 <- c(TRUE)

overall_n_1 <- c(FALSE)
overall_n_2 <- c(FALSE)
overall_n_3 <- c(FALSE)

# Use map to create a list of each input (get() will pull it out of the global env.):
# title <- map(inserted_sb, \(x) get(paste0("title_", x)))
# vars <- map(inserted_sb, \(x) get(paste0("vars_", x)))
# levels <- map(inserted_sb, \(x) get(paste0("levels_", x)))
# percent_label <- map(inserted_sb, \(x) get(paste0("percent_label_", x)))
# overall_n <- map(inserted_sb, \(x) get(paste0("overall_n_", x)))
#
# # Bind list together as named list of lists:
# input_df <- list("title" = title, "vars" = vars, "levels" = levels, "percent_label" = percent_label, "overall_n" = overall_n)

# Try in one call to get all input lists and bind together as a df using list_cbind()
# vector of input names will be x and inserted_sb will be y:
input_names <- c("title", "vars", "levels", "percent_label", "overall_n")
# Replicates the above repeated calls to map and sets as a tibble:
# input_df <- map(input_names, \(x) map(inserted_sb, \(y) get(paste0(x, "_", y)))) %>% setNames(input_names) %>% as_tibble()
# As a list of lists:
sb_options_df <- map(input_names, \(x) map(inserted_sb, \(y) get(paste0(x, "_", y)))) %>% set_names(input_names) %>% as_tibble()

# As a tibble with correct var types:
sb_options_df <- tibble(title = map_chr(inserted_sb, \(x) get(paste0("title_", x))),
                        variables_names = map(inserted_sb, \(x) get(paste0("vars_", x))),
                        levels = map(inserted_sb, \(x) get(paste0("levels_", x))),
                        percent_label = map_lgl(inserted_sb, \(x) get(paste0("percent_label_", x))),
                        overall_n = map_lgl(inserted_sb, \(x) get(paste0("overall_n_", x)))
                            )
sb_options_df

# A tibble: 1 × 5
# title variables_names levels    percent_label overall_n
# <chr> <list>        <list>       <lgl>         <lgl>
# 1 new  <list [2]>   <chr [1]>    TRUE

create_sb_plot <- function(title, variables_names, levels, percent_label, overall_n) {
    variables_names <- variables_names %>% unlist() %>% as.character()
    levels_names <- levels %>% as.character() %>% str_split(., ", ") %>% unlist()
    stacked_bar <- data %>% dplyr::select(all_of(variables_names)) %>%
        TheMarkUSA::stackedBarChart(., pre_post = FALSE, scale_labels = levels_names, percent_label = percent_label, overall_n = overall_n)
    return(stacked_bar)
}

stacked_bar_list <- purrr::pmap(sb_options_df, create_sb_plot) %>% set_names(., sb_options_df[["title"]])
stacked_bar_list



#### Works in Shiny app ***** Outputs a list of lists (can change to a tibble if that works better)
# Create a df of all inputs after action buttion #create_stacked_bar is clicked:
# sb_options_df <- eventReactive(input$create_stacked_bar, {
#     # vector of input names will be x and inserted_sb[["id"]] will be y:
#     input_names <- c("title", "vars", "levels", "percent_label", "overall_n")
#     # map(input_names, \(x) map(inserted_sb[["id"]], \(y) input[[str_glue("{x}_{y}")]])) %>% setNames(input_names) %>% as_tibble()
#     map(input_names, \(x) map(inserted_sb[["id"]], \(y) input[[str_glue("{x}_{y}")]])) %>% set_names(input_names)
# })

# output[["table"]] <- renderPrint({ sb_options_df() })
####

title_sb <- sb_options_df[["title"]]
# Iterate over the list of lists that holds all the options for new sbs: sb_options_df
# WORKS!!!!!!!!!
stacked_bars_list <- pmap(sb_options_df, \(title, vars, levels, percent_label, overall_n ) data %>% dplyr::select(all_of(vars)) %>%
                          TheMarkUSA::stackedBarChart(., pre_post = FALSE, scale_labels = levels, percent_label = percent_label, overall_n = overall_n)) %>%
                        set_names(., title_sb)

stacked_bars_list[["Research Skills"]]
stacked_bars_list[["Collaboration Skills"]]
stacked_bars_list[["Training Usefulness"]]


unlist(sb_options_df[["levels"]][[1]])


####### WORKS IN APP- when switch to shiny inputs:
# New approach vary a function by the id of the input and pull in each input:
create_sb_plot <- function(df, id) {
    variables_names <- get(paste0("vars_", id)) %>% unlist()
    stacked_bar <- df %>% dplyr::select(all_of(variables_names)) %>%
        TheMarkUSA::stackedBarChart(., pre_post = FALSE, scale_labels = get(paste0("levels_", id)),
                                    percent_label = get(paste0("percent_label_", id)), overall_n = get(paste0("overall_n_", id)))
    return(stacked_bar)
}
# Get titles to name the stacked_bar list:
titles <- map_chr(inserted_sb, \(x) get(paste0("title_", x)))

stacked_bar_list <- purrr::map(inserted_sb, \(x) create_sb_plot(df = data, id = x)) %>% set_names(., titles)
stacked_bar_list

# ######### WORKING!!!!!!!
# # Function to create a stacked bar for each set of inputs- use pmap to iterate over a tibble/df to get full list output of all sbs:
# # New approach vary a function by the id of the input and pull in each input:
# create_sb_plot <- function(df, id) {
#   variables_names <- input[[str_glue("vars_{id}")]] %>% unlist() %>% as.character()
#   levels_names <- input[[str_glue("levels_{id}")]] %>% as.character() %>% str_split(., ", ") %>% unlist()
#   stacked_bar <- df %>% dplyr::select(all_of(variables_names)) %>%
#     TheMarkUSA::stackedBarChart(., pre_post = FALSE, scale_labels = levels_names,
#                                 percent_label = input[[str_glue("percent_label_{id}")]], overall_n = input[[str_glue("overall_n_{id}")]])
#   return(stacked_bar)
# }
# ######### WORKING!!!!!!! Doesn't require any reactives now, should add them at some point-
# ######### doesn't work with shinymeta yet:
# stacked_bars_list <- eventReactive(input[["create_stacked_bar"]], {
#   purrr::map(inserted_sb[["id"]], \(x) create_sb_plot(df = data(), id = x)) %>% set_names(., titles_sb())
# })
inserted_sb <- list(id = c(1,2,4))

title_1 <- c("Research Skills")
title_2 <- c("Collaboration Skills")
title_4 <- c("Training Usefulness")

vars_1 <- c("Organization", "Source", "Publish", "Write", "Research")
vars_2 <- c("Discuss", "Share", "Integrate")
vars_4 <- c("Responsible", "Ethics", "Standards", "Practices", "Morals")

levels_1 <- c("minimaL,  slight,moderate, good,  Extensive")
levels_2 <- c("Never,Rarely, Sometimes,  frequently, always")
levels_4 <- c("Not at all useful,Slightly useful, Somewhat useful,  Very useful, Extremely useful")

percent_label_1 <- c(TRUE)
percent_label_2 <- c(TRUE)
percent_label_4 <- c(TRUE)

overall_n_1 <- c(FALSE)
overall_n_2 <- c(FALSE)
overall_n_4 <- c(FALSE)

# Create a vector of the names of the curent inputs for all 5 inputs,
# Make this a reactive and repeat for all 5 inputs:
titles_names <- map_chr(inserted_sb[["id"]], ~ paste0("title_", .x))
var_names <- map_chr(inserted_sb[["id"]], ~ paste0("vars_", .x))
levels_names <- map_chr(inserted_sb[["id"]], ~ paste0("levels_", .x))
percent_label_names <- map_chr(inserted_sb[["id"]], ~ paste0("percent_label_", .x))
overall_n_names <- map_chr(inserted_sb[["id"]], ~ paste0("overall_n_", .x))

# levels_1 %>% str_split(., ",") %>% list_c() %>% str_trim() %>% str_to_title()

# Bundle together all of the inputs into vectors:
titles <- map_chr(titles_names, ~ get(.x) %||% "")
vars <- map(var_names, ~ get(.x) %||% "")
levels <- map(levels_names, ~ get(.x) %||% "" %>% str_split(., ",") %>% list_c() %>% str_trim() %>% str_to_sentence())
percent_label <- map_lgl(percent_label_names, ~ get(.x) %||% "")
overall_n <- map_lgl(overall_n_names, ~ get(.x) %||% "")

# Create a tibble from all inputs:
sb_options_df <- tibble("title" = titles,
                        "variables_names" = vars,
                        "levels" = levels,
                        "percent_label" = percent_label,
                        "overall_n" = overall_n)

# Function to create a stacked bar chart using TheMarkUSA::stackedBarChart() inside pmap() and a tibble/df of the inputs that
# has the column names same as args, except `df` which will be passed separately:
create_sb_plot <- function(df, title, variables_names, levels, percent_label, overall_n) {
    variables_names <- variables_names %>% unlist()
    levels_names <- levels %>% unlist()
    stacked_bar <- df %>% dplyr::select(all_of(variables_names)) %>%
        TheMarkUSA::stackedBarChart(., pre_post = FALSE, scale_labels = levels_names, percent_label = percent_label, overall_n = overall_n)
    return(stacked_bar)
}

sb_new_list <- purrr::pmap(sb_options_df, create_sb_plot, df = data) %>% set_names(., titles)
sb_new_list

titles_sb <- metaReactive2({
    # req(input[["create_stacked_bar"]])
    isolate(metaExpr({"# Create a vector of the titles to name the stacked bar charts:"
        map_chr(..(inserted_sb[["id"]]), ~ ..(str_glue("titles_{.x}") %||% ""))
    })
    )
})

titles_names <- metaReactive({map_chr(..(inserted_sb[["id"]]), ~ paste0("title_", .x))})
expandChain(titles_names())
titles_sb <- metaReactive({map_chr(..(titles_names()), ~ input[[.x]] %||% "")})
expandChain(titles_sb())

title <- metaReactive2({
    req(input[["create_stacked_bar"]])
    title <- map_chr(title_names(), ~ input[[.x]] %||% "")
    isolate(metaExpr({"# Create a vector of the titles to name each of the stacked bar charts:"
        ..(title)
    })
    )
})
input <- list()
input$title_input <- "New title very long looks nice"
str_replace_all(str_glue("{input$title_input}.zip"), " ", "_")
str_replace_all(paste0(input[["title_input"]],".zip"), " ", "_")
str_replace_all(str_glue("{input[[title_input]]}.zip"), " ", "_")
str_replace_all(input[["title_input"]], " ", "_")

########################
#######################
# Open-Ended Tables work:
# Function to make the OE full table: df, and header_label = is a character vector of the text of the question to be used as the header for the table:
openendedFlextable <- function(df, header_label) {
    # Wrap the text for the label of the header/question prompt called "header_label":
    header_label <- stringr::str_wrap(header_label, width = 80)
    # Create a reference table to set header names for flextable():
    ref_table <- data.frame(key = colnames({{df}}), label = header_label)
    # Create flextable() output:
    tbl <- df %>% flextable::flextable() %>%
        flextable::set_header_df(mapping = ref_table, key = "key") %>%
        flextable::theme_zebra() %>%
        flextable::fontsize(size = 11, part = "header") %>%
        flextable::fontsize(size = 10, part = "body") %>% # sets fontsize for header and body
        flextable::bg(bg = "#2C2C4F", part = "header") %>%  # Header color
        flextable::color(color = "white", part = "header") %>%  # Header font text color
        flextable::autofit(part = "all")

    return(tbl)
} # end of function

# Set up character vector of text or other things like punctuation to remove from the text data when using TheMarkUSA::openendedCleanup():
remove_values <- c("N/A", ".", "A")
var <- "Responsible_oe"
# Make a nice table with the function: EXAMPLE: %>% dplyr::arrange(!!rlang::sym(var))
resp_oe <- data %>% openendedCleanup(., "Responsible_oe", remove_values) %>%
    openendedFlextable(., header_label = str_glue("This is the question text for Responsible_oe"))

# Get all column names for fake data that have "_oe" in them:
selected_columns_oe <- data %>% select(contains("_oe")) %>% names()

oe_tbl_list <- map(selected_columns_oe, \(x) data %>% dplyr::select(all_of(x)) %>% TheMarkUSA::openendedCleanup(., x, remove_values) %>%
                    openendedFlextable(., header_label = str_glue("These are the responses from the question {x}"))) %>%
                        setNames(., selected_columns_oe)
oe_tbl_list # Works in regular R env

# Writing for shinymeta/shiny context:
map(..(selected_columns_oe()), \(x) ..(data()) %>% TheMarkUSA::openendedCleanup(., all_of(x), remove_values) %>%
        openendedFlextable(., header_label = str_glue("These are the responses from the question {x}"))) %>%
            setNames(., ..(selected_columns_oe()))

# Fix OE cleanup function from TheMarkUSA package:
openendedCleanup <- function(df, var, remove_values) {
    # Set . to NULL to stop message when using dot notation in functions:
    . <- NULL

    clean_data <- {{ df }} %>% dplyr::select(tidyselect::all_of({{ var }})) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::if_else(. %in% remove_values, NA_character_, .)),
                      dplyr::across(dplyr::where(is.character), ~ stringr::str_wrap(stringr::str_to_sentence(.), width = 80))) %>%
        tidyr::drop_na() %>% dplyr::arrange({{ var }})

    return(clean_data)
}

map(selected_columns_oe, \(x) data %>% openendedCleanup(., x, remove_values))
data %>% openendedCleanup(., "Responsible_oe", remove_values)
data %>% openendedCleanup(., Responsible_oe, remove_values)

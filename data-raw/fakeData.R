# Creating fake data for Rmd Template use:
# Zack Crowley


# Load libraries:
library(here)
library(scales)
library(readxl)
library(writexl)
library(janitor)
library(charlatan)
library(janeaustenr)
library(tidytext)
library(tidyverse)

# Set seed to reproduce data:
set.seed(04242021)

# Creating a fake dataset to use to build a script to create reports in R:

# Create data (n = 100) for baseline and annual datasets:of the following variables:
#  Character vectors of all char variables:
#  `Unique Identifier` = a unique ID (1 to 100)
#  role = role of respondent takes on a scale of 1 to 4 needs to be recoded to:
#  1= "Undergraduate student", 2 ="Graduate student", 3= "Postdoc", 4 = "Faculty"
# role
role_char <- c("Undergraduate student", "Graduate student", "Postdoc",  "Faculty")
# gender, three options ("Female","Male","Non-binary"), prob = c(0.51, 0.47, 0.02)
gender_char <- c("Female","Male","Non-binary")
#  Research skills composite scale- 5 variables of that make up a scale: Organization, Source, Publish, Write, Research
#  these are all on a 5-point likert scale of 1 to 5 needs to be recoded to: c("Minimal", "Slight", "Moderate", "Good", "Extensive")
# levels min_ext:
levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")

#  Collaboration skills composite scale- 3 variables of that make up a scale: Discuss, Share, Integrate
#  these are all on a 5-point likert scale of 1 to 5 needs to be recoded to: c("Never", "Rarely", "Sometimes", "Frequently", "Always")
# levels never to always:
levels_never_always <- c("Never", "Rarely", "Sometimes", "Frequently", "Always")

#  Training usefulness composite scale- 5 variables of that make up a scale: Responsible, Ethics, Standards, Practices, Morals
#  these are all on a 5-point likert scale of 1 to 5 needs to be recoded to: c("Not at all useful", "Slightly useful", "Somewhat useful", "Very useful", "Extremely useful")
# levels useful:
levels_useful <- c("Not at all useful", "Slightly useful", "Somewhat useful", "Very useful", "Extremely useful")

# Open-ended text variables- follow-up questions to explain when "Not at all useful" selected from the any of the Training usefulness items:
# (made up of non-sense text from jane austin books)
# Responsible_oe, Ethics_oe, Standards_oe, Practices_oe, Morals_oe
words <- austen_books() %>%
    group_by(book) %>%
    mutate(linenumber = row_number(),
           chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                   ignore_case = TRUE)))) %>%
    ungroup() %>% filter(book == "Pride & Prejudice") %>% unnest_tokens(word, text) %>% select(word) %>% deframe()
# Create a tibble of 1000, that is made up of random text with words ranging in length from 12 to 36:
oe_data <- purrr::map(.x = 1:1000, .f = function(x){paste(sample(words, size = sample(12:36,1, replace = TRUE), replace = TRUE), sep = ' ', collapse = ' ')}) %>% list_c() %>% as_tibble_col(column_name = "oe_text")

# baseline dataset:
data <- dplyr::tibble(
    `Unique Identifier` = c(1:100),
    gender = sample(c("Female","Male","Non-binary"), size = 100, replace = TRUE, prob = c(0.51, 0.47, 0.02)),
    role = sample(c("Undergraduate student", "Graduate student", "Postdoc",  "Faculty"), size = 100, replace = TRUE, prob = c(0.4, 0.2, 0.2, 0.1)),
    lgbtqplus = sample(c("Yes", "No", "Prefer not to disclose"), size = 100, replace = TRUE, prob = c(0.1, 0.83, 0.06)),
    ethnicity = sample(c("White (Non-Hispanic/Latino)", "Asian", "Black",  "Hispanic/Latino", "Prefer not to specify"), size = 100, replace = TRUE, prob = c(0.4, 0.24, 0.14, 0.17, 0.05)),
    Organization = sample(levels_min_ext, size = 100, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.2, 0.1)),
    Source = sample(levels_min_ext, size = 100, replace = TRUE, prob = c(0.05, 0.05, 0.2, 0.3, 0.4)),
    Publish = sample(levels_min_ext, size = 100, replace = TRUE, prob = c(0.5, 0.2, 0.1, 0.1, 0.1)),
    Write = sample(levels_min_ext, size = 100, replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.3)),
    Research = sample(levels_min_ext, size = 100, replace = TRUE, prob = c(0.05, 0.05, 0.2, 0.3, 0.4)),
    Discuss = sample(levels_never_always, size = 100, replace = TRUE, prob = c(0.05, 0.05, 0.2, 0.3, 0.4)),
    Share = sample(levels_never_always, size = 100, replace = TRUE, prob = c(0.5, 0.2, 0.1, 0.1, 0.1)),
    Integrate = sample(levels_never_always, size = 100, replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.3)),
    Responsible = sample(levels_useful, size = 100, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.2, 0.1)),
    Ethics = sample(levels_useful, size = 100, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.2, 0.1)),
    Standards = sample(levels_useful, size = 100, replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.3)),
    Practices = sample(levels_useful, size = 100, replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.3)),
    Morals = sample(levels_useful, size = 100, replace = TRUE, prob = c(0.05, 0.05, 0.2, 0.3, 0.4)),
    Responsible_oe = ifelse(Responsible == "Not at all useful", sample(oe_data$oe_text), NA_character_),
    Ethics_oe = ifelse(Ethics == "Not at all useful", sample(oe_data$oe_text), NA_character_),
    Standards_oe = ifelse(Standards == "Not at all useful", sample(oe_data$oe_text), NA_character_),
    Practices_oe = ifelse(Practices == "Not at all useful", sample(oe_data$oe_text), NA_character_),
    Morals_oe = ifelse(Morals == "Not at all useful", sample(oe_data$oe_text), NA_character_)
)

# Write out fake data set "data" to .xlsx excel file in new directory called "extdata", and a .csv file with the same name and location:
write_xlsx(data, "extdata/fake_data.xlsx")
# .csv file:
write_csv(data, "extdata/fake_data.csv")
# .csv file to dashboard_test:
write_csv(data, "dashboard_test/fake_data.csv")
####
# How to make new names that can be used in "R_Reporting_template.Rmd" to create charts and tables:
# Cleaning names
data <- data %>% clean_names()
# Set names- add suffix to variables to use later to build charts and tables:
# For gender and role variables, create horizontal bar charts
names(data)[2:3] <- str_glue("{names(data)[2:3]}_horzbar")
# For composite scales, create stacked bar charts, with scale levels:
# Research items
names(data)[4:8] <- str_glue("{names(data)[4:8]}_stack_levels_min_ext")
# Collab items
names(data)[9:11] <- str_glue("{names(data)[9:11]}_stack_levels_never_always")
# Training usefulness items
names(data)[12:16] <- str_glue("{names(data)[12:16]}_stack_levels_useful")

# Write out data that has the names suffixes added that are used to know how to create visuals/charts/tables:
# .xlsx excel file in new directory called "extdata", and a .csv file with the same name and location:
write_xlsx(data, "extdata/fake_data_new_names.xlsx")
# .csv file:
write_csv(data, "extdata/fake_data_new_names.csv")


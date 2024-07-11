# Fake SurveyMonkey and demographic data for `blackstone` package:
# Zack Crowley
# Creating a fake dataset to use in the 'blackstone' package, this will be all the standard SurveyMonkey data fields
# and three demographic variables: This will be the basis for pre-post data generated in the other two scripts in
# `extdata`- `sm_data_pre.R` and `sm_data_post.R`

# Prevents sci notation and sets the output of decimals to 4 (0.0000):
options(scipen = 999, digits = 4)
# Set seed to reproduce data:
set.seed(04240629)
# Load Libraries:
library(readr)
library(dplyr)
library(charlatan)
# setup the providers for charlatan
lp <- LoremProvider_en_US$new()

# Total N of 100 fake survey respondets:
n <- 100

### Variable description for all likert scale items/composite scales:
### Knowledge single item:
# To what extent are you knowledgeable in conducting research in your field of study?
## 5 point Likert scale for knowledge item:
# 1 = Not knowledgeable at all
# 2 = A little knowledgeable
# 3 = Somewhat knowledgeable
# 4 = Very knowledgeable
# 5 = Extremely knowledgeable
levels_knowledge <- c("Not knowledgeable at all", "A little knowledgeable", "Somewhat knowledgeable", "Very knowledgeable", "Extremely knowledgeable")

## Research skills confidence composite scale- 8 variables of that make up a scale: research_1:research_8
## Research Items- question root (in header 1):
# Please rate your level of confidence in performing the following research tasks on a scale from not at all confident to extremely confident.
# research_1: Research relevant background literature
# research_2: Identify a scientific problem
# research_3: Develop testable and realistic research questions
# research_4: Develop a falsifiable hypothesis
# research_5: Conduct quantitative data analysis
# research_6: Design an experiment/Create a research design
# research_7: Interpret findings and making recommendations
# research_8: Scientific or technical writing
## 5 point Likert scale for research items:
# 1 = Not at all confident
# 2 = Slightly confident
# 3 = Somewhat confident
# 4 = Very confident
# 5 = Extremely confident
# levels_confidence:
levels_confidence <- c("Not at all confident", "Slightly confident", "Somewhat confident", "Very confident", "Extremely confident")

## Ability to perform research skills composite scale- 6 variables of that make up a scale: ability_1:ability_6
## Ability Items- question root (in header 1):
# Please rate your ability to do the following on a scale from minimal to extensive.
# ability_1: Judge the value of new information or evidence presented to me
# ability_2: Approach complex issues in a variety of ways
# ability_3: Weigh both sides of an argument
# ability_4: Identify analogies between theories
# ability_5: Eliminate extraneous variables when designing experiments
# ability_6: Rephrase the arguments of others in my own words
## 5 point Likert scale for ability items:
# 1 = Minimal
# 2 = Slight
# 3 = Moderate
# 4 = Good
# 5 = Extensive
# levels min_ext:
levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")

## Ethical decision-making skills composite scale- 5 variables of that make up a scale: ethics_1:ethics_5
## Ethics Items- question root (in header 1):
# Please rate your agreement with the following statements on a scale from strongly disagree to strongly agree.
## Question items: in header 2
# ethics_1: I understand the different aspects of ethical research
# ethics_2: I am able to adhere to ethics in research
# ethics_3: I know where to find resources on ethical research conduct
# ethics_4: I am knowledgeable in ethical research conduct
# ethics_5: I can conduct ethical research
## 5 point Likert scale for ethics items:
# 1 = Strongly disagree
# 2 = Disagree
# 3 = Neither agree nor disagree
# 4 = Agree
# 5 = Strongly agree
# levels_agree5:
levels_agree5 <- c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")

# Set up probs to sample from for the  pre survey data:
prob_1 = c(0.3, 0.2, 0.2, 0.2, 0.1)
prob_2 = c(0.2, 0.2, 0.25, 0.2, 0.15)
prob_3 = c(0.1, 0.22, 0.35, 0.23, 0.1)
prob_4 = c(0.5, 0.2, 0.1, 0.1, 0.1)
# purrr::map_dbl(list(prob_1,prob_2,prob_3,prob_4), ~ sum(.))
# Likert scale survey items for pre data:
sm_data_pre <- tibble(knowledge = sample(levels_knowledge, size = n, replace = TRUE, prob = prob_4),
                      research_1 = sample(levels_confidence, size = n, replace = TRUE, prob = prob_3),
                      research_2 = sample(levels_confidence, size = n, replace = TRUE, prob = prob_4),
                      research_3 = sample(levels_confidence, size = n, replace = TRUE, prob = prob_1),
                      research_4 = sample(levels_confidence, size = n, replace = TRUE, prob = prob_3),
                      research_5 = sample(levels_confidence, size = n, replace = TRUE, prob = prob_1),
                      research_6 = sample(levels_confidence, size = n, replace = TRUE, prob = prob_2),
                      research_7 = sample(levels_confidence, size = n, replace = TRUE, prob = prob_4),
                      research_8 = sample(levels_confidence, size = n, replace = TRUE, prob = prob_2),
                      ability_1 = sample(levels_min_ext, size = n, replace = TRUE, prob = prob_3),
                      ability_2 = sample(levels_min_ext, size = n, replace = TRUE, prob = prob_2),
                      ability_3 = sample(levels_min_ext, size = n, replace = TRUE, prob = prob_1),
                      ability_4 = sample(levels_min_ext, size = n, replace = TRUE, prob = prob_4),
                      ability_5 = sample(levels_min_ext, size = n, replace = TRUE, prob = prob_1),
                      ability_6 = sample(levels_min_ext, size = n, replace = TRUE, prob = prob_2),
                      ethics_1 = sample(levels_agree5, size = n, replace = TRUE, prob = prob_3),
                      ethics_2 = sample(levels_agree5, size = n, replace = TRUE, prob = prob_2),
                      ethics_3 = sample(levels_agree5, size = n, replace = TRUE, prob = prob_1),
                      ethics_4 = sample(levels_agree5, size = n, replace = TRUE, prob = prob_4),
                      ethics_5 = sample(levels_agree5, size = n, replace = TRUE, prob = prob_3)
                    )

### Post data:
# Set up probs to sample from for the  post survey data:
post_prob_1 = c(0.1, 0.2, 0.2, 0.2, 0.3)
post_prob_2 = c(0.05, 0.1, 0.2, 0.3, 0.35)
post_prob_3 = c(0.03, 0.22, 0.23, 0.26, 0.26)
post_prob_4 = rev(c(0.5, 0.2, 0.1, 0.1, 0.1))
# purrr::map_dbl(list(post_prob_1,post_prob_2,post_prob_3,post_prob_4), ~ sum(.))
# Post data will also include 5 open-ended items which will be random sentences if the fake respondent
# answered "Strongly disagree" or "Disagree" to the corresponding ethics items, will be nameed as ethics_1_oe and so on,
# Set up the pool of open-ended responses to sample from:
open_ended_resp <- sapply(1:n, function(x) lp$paragraph(nb_sentences = 3))

## Ethics Open-Ended Response Items- question prompts (in header 1):
# ethics_1_oe: Please describe how your department could help you improve in this area- I understand the different aspects of ethical research
# ethics_2_oe: Please describe how your department could help you improve in this area- I am able to adhere to ethics in research
# ethics_3_oe: Please describe how your department could help you improve in this area- I know where to find resources on ethical research conduct
# ethics_4_oe: Please describe how your department could help you improve in this area- I am knowledgeable in ethical research conduct
# ethics_5_oe: Please describe how your department could help you improve in this area- I can conduct ethical research
## Question items in header 2 all: "Open-Ended Response"

# Likert scale survey items for post data:
sm_data_post <- tibble(knowledge = sample(levels_knowledge, size = n, replace = TRUE, prob = post_prob_4),
                       research_1 = sample(levels_confidence, size = n, replace = TRUE, prob = post_prob_3),
                       research_2 = sample(levels_confidence, size = n, replace = TRUE, prob = post_prob_4),
                       research_3 = sample(levels_confidence, size = n, replace = TRUE, prob = post_prob_1),
                       research_4 = sample(levels_confidence, size = n, replace = TRUE, prob = post_prob_3),
                       research_5 = sample(levels_confidence, size = n, replace = TRUE, prob = post_prob_1),
                       research_6 = sample(levels_confidence, size = n, replace = TRUE, prob = post_prob_2),
                       research_7 = sample(levels_confidence, size = n, replace = TRUE, prob = post_prob_4),
                       research_8 = sample(levels_confidence, size = n, replace = TRUE, prob = post_prob_2),
                       ability_1 = sample(levels_min_ext, size = n, replace = TRUE, prob = post_prob_3),
                       ability_2 = sample(levels_min_ext, size = n, replace = TRUE, prob = post_prob_2),
                       ability_3 = sample(levels_min_ext, size = n, replace = TRUE, prob = post_prob_1),
                       ability_4 = sample(levels_min_ext, size = n, replace = TRUE, prob = post_prob_4),
                       ability_5 = sample(levels_min_ext, size = n, replace = TRUE, prob = post_prob_1),
                       ability_6 = sample(levels_min_ext, size = n, replace = TRUE, prob = post_prob_2),
                       ethics_1 = sample(levels_agree5, size = n, replace = TRUE, prob = post_prob_3),
                       ethics_2 = sample(levels_agree5, size = n, replace = TRUE, prob = post_prob_2),
                       ethics_3 = sample(levels_agree5, size = n, replace = TRUE, prob = post_prob_1),
                       ethics_4 = sample(levels_agree5, size = n, replace = TRUE, prob = post_prob_4),
                       ethics_5 = sample(levels_agree5, size = n, replace = TRUE, prob = post_prob_3)
                       ) %>%
                mutate(ethics_1_oe = ifelse(ethics_1 %in% c("Strongly disagree", "Disagree"), sample(open_ended_resp), NA_character_),
                       ethics_2_oe = ifelse(ethics_2 %in% c("Strongly disagree", "Disagree"), sample(open_ended_resp), NA_character_),
                       ethics_3_oe = ifelse(ethics_3 %in% c("Strongly disagree", "Disagree"), sample(open_ended_resp), NA_character_),
                       ethics_4_oe = ifelse(ethics_4 %in% c("Strongly disagree", "Disagree"), sample(open_ended_resp), NA_character_),
                       ethics_5_oe = ifelse(ethics_5 %in% c("Strongly disagree", "Disagree"), sample(open_ended_resp), NA_character_))

# Get counts for each variable in `sm_data_pre` that is a character or factor:
# pre_counts <- sm_data_pre %>% dplyr::select(dplyr::where(is.character), dplyr::where(is.factor)) %>% names() %>%
#                 purrr::map(., \(x) sm_data_pre %>% count(.data[[x]]))
#
#
# # Get counts for each variable in `sm_data_post` that is a character or factor:
# post_counts <- sm_data_post %>% dplyr::select(dplyr::where(is.character), dplyr::where(is.factor)) %>% names() %>%
#                     purrr::map(., \(x) sm_data_post %>% count(.data[[x]]))

### Merging `sm_data_demo` with pre and post data:
# Read in sm_data_demo`:
sm_data_demo <- readr::read_csv("inst/extdata/sm_data_demo.csv", show_col_types = FALSE)

# Merge sm_data_pre with sm_data_demo and move demographic vars to the end of the tibble:
sm_data_pre_cleanNames <- dplyr::bind_cols(sm_data_demo, sm_data_pre) %>% relocate(gender, ethnicity, first_gen, .after = last_col())

# Merge sm_data_post with sm_data_demo and move demographic vars to the end of the tibble:
sm_data_post_cleanNames <- dplyr::bind_cols(sm_data_demo, sm_data_post) %>% relocate(gender, ethnicity, first_gen, .after = last_col())


### Add two header rows for the names of each tibble for the pre and post data in order to emulate SurveyMonkey data export:
### Pre data header replacement:
# Vector of header 1 for pre data:
question_name_header_1 <- c("Respondent ID", "Collector ID", "Start Date", "End Date", "IP Address", "Email Address", "First Name", "Last Name", "Custom Data 1", # unique_id
                            "To what extent are you knowledgeable in conducting research in your field of study?", # knowledge
                            "Please rate your level of confidence in performing the following research tasks on a scale from not at all confident to extremely confident.",
                            NA, NA, NA, NA, NA, NA, NA,  # research items
                            "Please rate your ability to do the following on a scale from minimal to extensive.", NA, NA, NA, NA, NA, # ability items
                            "Please rate your agreement with the following statements on a scale from strongly disagree to strongly agree.", NA, NA, NA, NA, # ethics items
                            "With which gender do you most closely identify?",  # gender
                            "Which race/ethnicity best describes you? (Please choose only one.)",  # ethnicity
                            "Are you a first-generation college student?") # first_gen

# Vector of header 2 for pre data:
full_text_header_2 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "Response", # sm data fields and knowledge
                        "Research relevant background literature", "Identify a scientific problem", "Develop testable and realistic research questions", "Develop a falsifiable hypothesis",
                        "Conduct quantitative data analysis", "Design an experiment/Create a research design", "Interpret findings and making recommendations", "Scientific or technical writing", # research items
                        "Judge the value of new information or evidence presented to me", "Approach complex issues in a variety of ways", "Weigh both sides of an argument",
                        "Identify analogies between theories", "Eliminate extraneous variables when designing experiments", "Rephrase the arguments of others in my own words", # ability items
                        "I understand the different aspects of ethical research", "I am able to adhere to ethics in research", "I know where to find resources on ethical research conduct",
                        "I am knowledgeable in ethical research conduct", "I can conduct ethical research", # ethics items
                        "Response", "Response", "Response") # gender # ethnicity # first_gen

# Renaming pre data:
# First add names to the vector `full_text_header_2` using `question_name_header_1`
full_text_header_2_named <- full_text_header_2 %>% purrr::set_names(names(sm_data_pre_cleanNames))
# Convert whole tibble to character variables, Add the `full_text_header_2` vector as row 1 in the tibble and
# set names to question_name_header_1: ready to write out to .csv
sm_data_pre <- sm_data_pre_cleanNames %>% mutate(across(everything(), as.character))  %>%
                add_row(!!!full_text_header_2_named, .before = 1) %>%
                purrr::set_names(question_name_header_1)

### Post data header replacement:
# Vector of header 1 for post data:
question_name_header_1_post <- c("Respondent ID", "Collector ID", "Start Date", "End Date", "IP Address", "Email Address", "First Name", "Last Name", "Custom Data 1", # unique_id
                                 "To what extent are you knowledgeable in conducting research in your field of study?", # knowledge
                                 "Please rate your level of confidence in performing the following research tasks on a scale from not at all confident to extremely confident.",
                                 NA, NA, NA, NA, NA, NA, NA,  # research items
                                 "Please rate your ability to do the following on a scale from minimal to extensive.", NA, NA, NA, NA, NA, # ability items
                                 "Please rate your agreement with the following statements on a scale from strongly disagree to strongly agree.", NA, NA, NA, NA, # ethics items
                                 "Please describe how your department could help you improve in this area- I understand the different aspects of ethical research",
                                 "Please describe how your department could help you improve in this area- I am able to adhere to ethics in research",
                                 "Please describe how your department could help you improve in this area- I know where to find resources on ethical research conduct",
                                 "Please describe how your department could help you improve in this area- I am knowledgeable in ethical research conduct",
                                 "Please describe how your department could help you improve in this area- I can conduct ethical research", # ethics open-ended
                                 "With which gender do you most closely identify?",  # gender
                                 "Which race/ethnicity best describes you? (Please choose only one.)",  # ethnicity
                                 "Are you a first-generation college student?") # first_gen

# Vector of header 2 for pre data:
full_text_header_2_post <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "Response", # sm data fields and knowledge
                             "Research relevant background literature", "Identify a scientific problem", "Develop testable and realistic research questions", "Develop a falsifiable hypothesis",
                             "Conduct quantitative data analysis", "Design an experiment/Create a research design", "Interpret findings and making recommendations", "Scientific or technical writing", # research items
                             "Judge the value of new information or evidence presented to me", "Approach complex issues in a variety of ways", "Weigh both sides of an argument",
                             "Identify analogies between theories", "Eliminate extraneous variables when designing experiments", "Rephrase the arguments of others in my own words", # ability items
                             "I understand the different aspects of ethical research", "I am able to adhere to ethics in research", "I know where to find resources on ethical research conduct",
                             "I am knowledgeable in ethical research conduct", "I can conduct ethical research", # ethics items
                             "Open-Ended Response", "Open-Ended Response","Open-Ended Response","Open-Ended Response","Open-Ended Response", # ethics open-ended
                             "Response", "Response", "Response") # gender # ethnicity # first_gen

# Renaming post data:
# First add names to the vector `full_text_header_2` using `question_name_header_1_post`
full_text_header_2_post_named <- full_text_header_2_post %>% purrr::set_names(names(sm_data_post_cleanNames))
# Convert whole tibble to character variables, Add the `full_text_header_2` vector as row 1 in the tibble and
# set names to question_name_header_1_post: ready to write out to .csv
sm_data_post <- sm_data_post_cleanNames %>% mutate(across(everything(), as.character))  %>%
    add_row(!!!full_text_header_2_post_named, .before = 1) %>% # add as first row the named full_text_header_2_post
    purrr::set_names(question_name_header_1_post)


# # Write out pre data tibble `sm_data_pre` to the `extdata` folder:
# readr::write_csv(sm_data_pre, "inst/extdata/sm_data_pre.csv")
# # Write out pre data tibble `sm_data_post` to the `extdata` folder:
# readr::write_csv(sm_data_post, "inst/extdata/sm_data_post.csv")

### Merge and write out data with clean names:
# Add pre and post prefixes to all variables that will be merged, (i.e. the survey items that differ pre-post the SM items and demos are identical):
# Pre data:
sm_data_pre_cleanNames <- sm_data_pre_cleanNames %>% rename_with(~ paste0("pre_", .), .cols = c(knowledge:ethics_5))
# Pre data:
sm_data_post_cleanNames <- sm_data_post_cleanNames %>% rename_with(~ paste0("post_", .), .cols = c(knowledge:ethics_5_oe))
## Merge pre-post data:
sm_data_clean <- sm_data_pre_cleanNames %>% dplyr::left_join(sm_data_post_cleanNames, by = join_by(respondent_id, collector_id, start_date, end_date, ip_address, email_address,
                                                                                                   first_name, last_name, unique_id, gender, ethnicity, first_gen))

# # Write out clean and merged pre-post sm data tibble `sm_data_clean` to the `extdata` folder:
# readr::write_csv(sm_data_clean, "inst/extdata/sm_data_clean.csv")



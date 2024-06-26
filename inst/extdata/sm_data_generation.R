# Fake SurveyMonkey and demographic data for `bre` package:
# Zack Crowley
# Creating a fake dataset to use in the 'bre' package, this will be all the standard SurveyMonkey data fields
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
###
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

## Ability to perform research skills composite scale- 8 variables of that make up a scale: research_1:research_8
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

## Ethical decision-making skills composite scale- 8 variables of that make up a scale: research_1:research_8
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
sm_data_pre <- tibble(research_1 = sample(levels_confidence, size = n, replace = TRUE, prob = prob_3),
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
purrr::map_dbl(list(post_prob_1,post_prob_2,post_prob_3,post_prob_4), ~ sum(.))
# Post data will also include 5 open-ended items which will be random sentences if the fake respondent
# answered "Strongly disagree" or "Disagree" to the corresponding ethics items, will be nameed as ethics_1_oe and so on,
# Set up the pool of open-ended responses to sample from:
open_ended_resp <- sapply(1:n, function(x) lp$paragraph(nb_sentences = 3))

# Likert scale survey items for post data:
sm_data_post <- tibble(research_1 = sample(levels_confidence, size = n, replace = TRUE, prob = post_prob_3),
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

# Merge sm_data_pre with sm_data_demo
sm_data_demo %>% merge(sm_data_pre)

### Add two header rows for the names to emulate SurveyMonkey data for both the pre and post data:

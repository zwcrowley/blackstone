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
ip <- InternetProvider_en_US$new()
lp <- LoremProvider_en_US$new()
np <- NumericsProvider$new()
person <- PersonProvider_en_US$new()

# Total N of 100 fake survey respondets:
n <- 100

# SurveyMonkey respondent_id, numeric vector
respondent_id <- as.numeric(114628000001:114628000100)

# IP address and emails:
email_address <- sapply(1:n, function(x) ip$email())
ip_address  <- sapply(1:n, function(x) ip$ipv4())

# Generate Unique ID, generate 100 with starting number of 24290001:
unique_id <- as.numeric(24290001:24290100)

# Names from the `charlatan` package, create 300 male and 300 female names and last names as vectors:
female_names <- sapply(1:n , function(x) person$first_name_female())
last_names_female <- sapply(1:n , function(x) person$last_name_female())
male_names <- sapply(1:n , function(x) person$first_name_male())
last_names_male <- sapply(1:n , function(x) person$last_name_male())

# Add respondent_id, email_address,ip_address, unique_id  to tibble and generate gender, first and last names, ethnicity, and first_gen using sample of size 100:
sm_data_demo <- tibble(
    respondent_id = respondent_id, # from above
    collector_id = as.numeric(431822954), # same number- assuming that is how SM does it
    start_date = sample(seq(as.Date("2024-06-01"), as.Date("2024-06-25"), by = "day"), size = n, replace = TRUE),
    end_date = start_date + 1,
    ip_address = ip_address, # from above
    email_address = email_address, # from above
    gender = sample(c("Female","Male","Non-binary", "Do not wish to specify"), size = n, replace = TRUE, prob = c(0.52, 0.44, 0.02, 0.02)),
    first_name = if_else(gender == "Female", sample(female_names), sample(male_names)), # sample from female names vector if gender is female, otherwise use male name vector.
    last_name = if_else(gender == "Female", sample(last_names_female), sample(last_names_male)), # sample from female last names vector if gender is female, otherwise use  male last name vector.
    unique_id = unique_id, # from above
    role = sample(c("Undergraduate student", "Graduate student", "Postdoc",  "Faculty"), size = n, replace = TRUE, prob = c(0.4, 0.2, 0.2, 0.1)),
    ethnicity = sample(c("White (Non-Hispanic/Latino)", "Asian", "Black",  "Hispanic or Latino", "American Indian or Alaskan Native",
                         "Native Hawaiian or other Pacific Islander", "Do not wish to specify"), size = n, replace = TRUE, prob = c(0.30, 0.20, 0.15, 0.20, 0.05, 0.06, 0.03)),
    first_gen = sample(c("Yes", "No", "I'm not sure"), size = n, replace = TRUE, prob = c(0.52, 0.44, 0.03))
) %>% dplyr::relocate(gender, .after = unique_id) # move gender to beginning of demos vars
sm_data_demo

# Write out data tibble with sm data and demos to `extdata` folder:
readr::write_csv(sm_data_demo, "inst/extdata/sm_data_demo.csv")


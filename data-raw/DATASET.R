## code to prepare `DATASET` dataset goes here
library(charlatan)
# setup the providers for charlatan
ip <- InternetProvider_en_US$new()
lp <- LoremProvider_en_US$new()
np <- NumericsProvider$new()
person <- PersonProvider_en_US$new()

# Total N of 100 clients:
n <- 100

# SurveyMonkey respondent_id
respondent_id <- sapply(1:n, function(x) np$integer(min = 10000, max = 99999))

# IP address and emails:
email_address <- sapply(1:n, function(x) ip$email())
ip_address  <- sapply(1:n, function(x) ip$ipv4())

# Generate Unique ID, generate 100 with starting number of 2429:
unique_id <- as.numeric(paste0("242900", 1:9)) %>% append(as.numeric(paste0("24290", 10:99))) %>% append(as.numeric(paste0("2429", 100)))

# Names from the `charlatan` package, create 300 male and 300 female names and last names as vectors:
female_names <- sapply(1:n , function(x) person$first_name_female())
last_names_female <- sapply(1:n , function(x) person$last_name_female())
male_names <- sapply(1:n , function(x) person$first_name_male())
last_names_male <- sapply(1:n , function(x) person$last_name_male())

# Add unique_id to tibble and generate gender, first and last names, age, ethnicity, zip code, session_location using sample of size 100:
data_unique <- tibble(
    respondent_id = respondent_id, # from above
    collector_id = as.numeric(431822954),
    start_date = sample(seq(as.Date("2024-06-1"), as.Date("2024-06-30"), by = "day"), size = n, replace = TRUE),
    end_date = start_date + 1,
    ip_address = ip_address, # from above
    email_address = email_address, # from above
    gender = sample(c("Female","Male","Non-binary", "Do not wish to specify"), size = n, replace = TRUE, prob = c(0.52, 0.44, 0.02, 0.02)),
    first_name = if_else(gender == "Female", sample(female_names), sample(male_names)), # sample from female names vector if gender is female, otherwise use male name vector.
    last_name = if_else(gender == "Female", sample(last_names_female), sample(last_names_male)), # sample from female last names vector if gender is female, otherwise use  male last name vector.
    unique_id = unique_id, # from above
    ethnicity = sample(c("White (Non-Hispanic/Latino)", "Asian", "Black",  "Hispanic or Latino", "American Indian or Alaskan Native",
                         "Native Hawaiian or other Pacific Islander", "Do not wish to specify"), size = n, replace = TRUE, prob = c(0.32, 0.21, 0.15, 0.20, 0.05, 0.06, 0.01)),
    first_gen = sample(c("Yes", "No", "I'm not sure"), size = n, replace = TRUE, prob = c(0.55, 0.44, 0.01))
)
data_unique
# readr::read_csv("sm_data/fake_sm_pre.csv", show_col_types = FALSE)

# Write out data tibble with sm data and demos to `data-raw` folder:
# readr::write_csv(data_unique, "data_sm_demo.csv")
usethis::use_data(DATASET, overwrite = TRUE)

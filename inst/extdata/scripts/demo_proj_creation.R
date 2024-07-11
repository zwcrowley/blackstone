# Fake Demo Project Creation for `blackstone` package:
# Zack Crowley
# Creating a fake project directory structure to train how to use the `here` package

# Prevents sci notation and sets the output of decimals to 4 (0.0000):
options(scipen = 999, digits = 4)
# Set seed to reproduce data:
set.seed(4240629)
# Load Libraries:
library(readr)
library(dplyr)
library(fs)

# Set up file path to inst/extdata:
extdata_fp <- fs::path("inst", "extdata")

# Create the top-level directory for the fake demo project and name it: "2429_Demo-Project"
dir_create(extdata_fp, "2429_Demo-Project")

# Set up file path to "2429_Demo-Project":
demo_proj_fp <- fs::path(extdata_fp, "2429_Demo-Project")

# Create the 4 sub-folders for each project at Blackstone:
demo_subfolder_names <- c("Archive", "Phase 1 - Sales and Finance", "Phase 2 - Evaluation","Phase 3 - Reporting")
dir_create(path(demo_proj_fp, demo_subfolder_names))

# Set up file path to "Phase 2 - Evaluation":
eval_fp <- fs::path(demo_proj_fp, "Phase 2 - Evaluation")

# Create subfolders for the evaluation folder:
eval_subfolder_names <- c( "Instruments", "Client Meeting Notes", "Concept and Data Analysis Guide", "Data", "Data Analysis", "Logic Model")
dir_create(path(eval_fp, eval_subfolder_names))

# Set up file path to "Data" and "Data Analysis":
data_fp <- fs::path(eval_fp, "Data")
data_analysis_fp <- fs::path(eval_fp, "Data Analysis")

# Create subfolders for the "Data" and "Data Analysis" folders:
data_subfolder_names <- c("Year 1 (2021-2022)", "Year 2 (2022-2023)","Year 3 (2023-2024)")
dir_create(path(data_fp, data_subfolder_names)) # Data
dir_create(path(data_analysis_fp, data_subfolder_names)) # Data Analysis

# Create subfolders for Data/Year 3 (2023-2024) for pre and post survey data
data_year3_subfolder_names <- c("Pre_Survey_Data", "Post_Survey_Data", "clean_data")
dir_create(path(data_fp, "Year 3 (2023-2024)", data_year3_subfolder_names)) # Data Year 3

# Copy example data files to respective folders
# file_exists(path(extdata_fp, "sm_data_pre.csv"))
file_copy(path = path(extdata_fp, "sm_data_pre.csv"), new_path = path(data_fp, "Year 3 (2023-2024)", "Pre_Survey_Data"), overwrite = TRUE)
# file_exists(path(extdata_fp, "sm_data_post.csv"))
file_copy(path = path(extdata_fp, "sm_data_post.csv"), new_path = path(data_fp, "Year 3 (2023-2024)", "Post_Survey_Data"), overwrite = TRUE)
# file_exists(path(extdata_fp, "sm_data_clean.csv"))
file_copy(path = path(extdata_fp, "sm_data_clean.csv"), new_path = path(data_fp, "Year 3 (2023-2024)", "clean_data"), overwrite = TRUE)

# Create subfolders for "Data Analysis"/Year 3 (2023-2024) for analysis:
dir_create(path(data_analysis_fp, "Year 3 (2023-2024)", "analysis")) # Data Analysis Year 3
# This is where I will create an R project

# Create an .Rmd file in the analysis folder
file_create(path(data_analysis_fp, "Year 3 (2023-2024)", "analysis", "report.Rmd"))

# Create an R project in the Year 3 (2023-2024)/analysis folder:
usethis::create_project(path = path(data_analysis_fp, "Year 3 (2023-2024)", "analysis"), open = FALSE) # do not open R project

# Return directory tree for demo project folder
fs::dir_tree(demo_proj_fp)

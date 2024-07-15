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

# Set up file path to inst/:
inst_fp <- fs::path("inst")

# Create the top-level directory for the fake demo project and name it: "2429_DEMO-PROJ"
dir_create(inst_fp, "2429_DEMO-PROJ")

# Set up file path to "2429_DEMO-PROJ":
demo_proj_fp <- fs::path(inst_fp, "2429_DEMO-PROJ")

# Create the 4 sub-folders for each project at Blackstone:
demo_subfolder_names <- c("Archive", "Phase 1 - Sales and Finance", "Phase 2 - Evaluation","Phase 3 - Reporting")
dir_create(path(demo_proj_fp, demo_subfolder_names))

# Add a text file placeholder for all directories so that they are included in .git:
placeholder_text <- c("This is placeholder text", "so that all folders", "are saved to the git repo","Thank you!")
# Creating "placeholder.txt" files to all folders in `demo_subfolder_names` expect "Phase 2 - Evaluation":
demo_subfolder_names_noEval <- c("Archive", "Phase 1 - Sales and Finance","Phase 3 - Reporting")
# Creating "placeholder.txt" files to all folders in `demo_subfolder_names`:
demo_subfolder_placeholder_files <- path(demo_proj_fp, demo_subfolder_names_noEval, paste0("demo_placeholder_", seq_along(demo_subfolder_names_noEval)), ext = "txt")
file_create(demo_subfolder_placeholder_files)
# Adding `placeholder_text` to all files using `purrr::walk()`:
purrr::walk(demo_subfolder_placeholder_files, \(paths) readr::write_lines(x = placeholder_text, paths))

# Set up file path to "Phase 2 - Evaluation":
eval_fp <- fs::path(demo_proj_fp, "Phase 2 - Evaluation")

# Create subfolders for the evaluation folder:
eval_subfolder_names <- c( "Instruments", "Client Meeting Notes", "Concept and Data Analysis Guide", "Data", "Data Analysis", "Logic Model")
dir_create(path(eval_fp, eval_subfolder_names))

# Creating "placeholder.txt" files to all folders in `eval_subfolder_names` expect "Data" and "Data Analysis":
eval_subfolder_names_noData <- c( "Instruments", "Client Meeting Notes", "Concept and Data Analysis Guide", "Logic Model")
eval_subfolder_placeholder_files <- path(eval_fp, eval_subfolder_names_noData, paste0("eval_placeholder_", seq_along(eval_subfolder_names_noData)), ext = "txt")
file_create(eval_subfolder_placeholder_files)
# Adding `placeholder_text` to all files
purrr::walk(eval_subfolder_placeholder_files, \(paths) readr::write_lines(x = placeholder_text, paths))

# Set up file path to "Data" and "Data Analysis":
data_fp <- fs::path(eval_fp, "Data")
data_analysis_fp <- fs::path(eval_fp, "Data Analysis")

# Create subfolders for the "Data" and "Data Analysis" folders:
data_subfolder_names <- c("Year 1 (2021-2022)", "Year 2 (2022-2023)","Year 3 (2023-2024)")
dir_create(path(data_fp, data_subfolder_names)) # Data
dir_create(path(data_analysis_fp, data_subfolder_names)) # Data Analysis

# Creating "placeholder.txt" files to all folders in `eval_subfolder_names` expect "Year 3 (2023-2024)":
data_subfolder_names_1_2 <- c("Year 1 (2021-2022)", "Year 2 (2022-2023)")
data_subfolder_placeholder_files <- path(data_fp, data_subfolder_names_1_2, paste0("data_placeholder_", seq_along(data_subfolder_names_1_2)), ext = "txt")
file_create(data_subfolder_placeholder_files)
# Adding `placeholder_text` to all files
purrr::walk(data_subfolder_placeholder_files, \(paths) readr::write_lines(x = placeholder_text, paths))


# Create subfolders for Data/Year 3 (2023-2024) for pre and post survey data
data_year3_subfolder_names <- c("Pre_Survey_Data", "Post_Survey_Data", "clean_data")
dir_create(path(data_fp, "Year 3 (2023-2024)", data_year3_subfolder_names)) # Data Year 3

# File path to "inst/extdata":
extdata_fp <- fs::path("inst/extdata")

# Copy example data files to respective folders
file_exists(path(extdata_fp, "sm_data_pre.csv"))
file_copy(path = path(extdata_fp, "sm_data_pre.csv"), new_path = path(data_fp, "Year 3 (2023-2024)", "Pre_Survey_Data"), overwrite = TRUE)
# file_exists(path(extdata_fp, "sm_data_post.csv"))
file_copy(path = path(extdata_fp, "sm_data_post.csv"), new_path = path(data_fp, "Year 3 (2023-2024)", "Post_Survey_Data"), overwrite = TRUE)
# file_exists(path(extdata_fp, "sm_data_clean.csv"))
file_copy(path = path(extdata_fp, "sm_data_clean.csv"), new_path = path(data_fp, "Year 3 (2023-2024)", "clean_data"), overwrite = TRUE)

# Create subfolders for "Data Analysis"/Year 3 (2023-2024) for analysis:
dir_create(path(data_analysis_fp, "Year 3 (2023-2024)", "analysis")) # Data Analysis Year 3
# This is where I will create an R project

## Creating analysis.Rproj file:
file_create(path(data_analysis_fp, "Year 3 (2023-2024)", "analysis", "analysis.Rproj"))

# Writing out all options for "analysis.Rproj"
# Set up text to add to "analysis.Rproj":
r_proj_text <- c("Version: 1.0",
                 "",
                 "RestoreWorkspace: Default",
                 "SaveWorkspace: Default",
                 "AlwaysSaveHistory: Default",
                 "",
                 "EnableCodeIndexing: Yes",
                 "UseSpacesForTab: Yes",
                 "NumSpacesForTab: 4",
                 "Encoding: UTF-8",
                 "",
                 "RnwWeave: Sweave",
                 "LaTeX: pdfLaTeX",
                 "",
                 "AutoAppendNewline: Yes",
                 "StripTrailingWhitespace: Yes")

# Write out `r_proj_text` into "analysis.Rproj":
readr::write_lines(x = r_proj_text, path(data_analysis_fp, "Year 3 (2023-2024)", "analysis", "analysis.Rproj"))

# Create an .Rmd file in the analysis folder
file_create(path(data_analysis_fp, "Year 3 (2023-2024)", "analysis", "report.Rmd"))

# Return directory tree for demo project folder
fs::dir_tree(demo_proj_fp)

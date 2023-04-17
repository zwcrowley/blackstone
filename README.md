
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TheMarkUSA

<!-- badges: start -->
<!-- badges: end -->

The goal of TheMarkUSA is to make data cleaning and visualizations
easier and faster for The Mark USA, Inc. The functions in TheMarkUSA
create visuals with The Mark USA, Inc. branding and helper functions for
common data cleaning and manipulation tasks for use for everyone at The
Mark, USA Inc.

## Installation

You can install the development version of TheMarkUSA from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("zcrowleyTheMark/TheMarkUSA")
```

## Example

A common problem with the current workflow at The Mark is converting
numeric data to character/factor variables, especially 5 item likert
scales: recodeFiveCat is a helper function to recode numeric data with 5
levels into categorical/factor variables.

``` r
library(TheMarkUSA)
items <- dplyr::tibble(
  Pre_Orgs = c(1, 2, 3, 4, 5, 4, 3, 2, 1),
   Post_Orgs = c(1, 2, 3, 4, 5, 4, 3, 2, 1)
)
levels_min_ext <- c("Minimal", "Slight", "Moderate", "Good", "Extensive")
cat_items_1 <- recodeFiveCat(items, levels_min_ext)
```

The user simply passes a data frame of all the items to be recoded with
a character vector of the 5 scale likert labels.

## Data Visualizitons Examples

TheMarkUSA currently contains two helper functions for generating
visualizations: stackedBarChart and divBarChart.

Both take in two arguments: **df** A tibble/data frame of survey items
that are categorical/character variables, in 5 point scales and
pre-post, that will be inserted into a stacked bar chart with The Mark
USA branding. **set_5\_levels** character vector of 5 levels to set the
scale for the plot

stackedBarChart creates a fully stacked bar chart that has the
branding/style of The Mark USA, INC.

``` r
# Select only the categorical/factor vars from the df in the last chunk: cat_items_1
cat_items_plot <- cat_items_1 %>% dplyr::select(cat_Pre_Orgs,cat_Post_Orgs)
# Run the function with the categorical items and the character vector of the factor levels
stacked_chart_1 <- stackedBarChart(cat_items_plot,levels_min_ext)

stacked_chart_1
```

<img src="man/figures/README-stackedBarChart-1.png" width="100%" />

divBarChart creates a diverging and fully stacked bar chart that has the
branding/style of The Mark USA, INC.

``` r
# Select only the categorical/factor vars from the df in the last chunk: cat_items_1
cat_items_plot <- cat_items_1 %>% dplyr::select(cat_Pre_Orgs,cat_Post_Orgs)
# Run the function with the categorical items and the character vector of the factor levels
div_chart_1 <- divBarChart(cat_items_plot,levels_min_ext)

div_chart_1
```

<img src="man/figures/README-divBarChart-1.png" width="100%" />

More functions and visuals will be added to TheMarkUSA as needed, be
sure to reach out with any ideas for the package or issues!

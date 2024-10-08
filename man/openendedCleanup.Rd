% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/openendedCleanup.R
\name{openendedCleanup}
\alias{openendedCleanup}
\title{Clean Up and Format Open-ended Text}
\usage{
openendedCleanup(df, var, remove_values)
}
\arguments{
\item{df}{Required, A \link[tibble:tibble-package]{tibble}/data frame containing the character variable of text.}

\item{var}{Required, the character variable to clean up from the \link[tibble:tibble-package]{tibble}/data frame, needs to be in quotes.}

\item{remove_values}{Required, a character vector of additional text to remove from the text, see example.}
}
\value{
A \link[tibble:tibble-package]{tibble} which contains one character variable of clean text ready for use or output.
}
\description{
Clean Up and Format Open-ended Text
}
\examples{
# Example data:
#  Training usefulness composite scale- 5 variables of that make up a scale:
# Responsible, Ethics, Standards, Practices, Morals
#  these are all on a 5-point likert scale of 1 to 5 needs to be
#  recoded to: c("Not at all useful", "Slightly useful", "Somewhat useful",
#                "Very useful", "Extremely useful")
# levels useful:
levels_useful <- c("Not at all useful", "Slightly useful", "Somewhat useful",
                   "Very useful", "Extremely useful")
# Data:
data <- dplyr::tibble(
 Responsible = sample(levels_useful, size = 100, replace = TRUE,
                       prob = c(0.1, 0.2, 0.3, 0.2, 0.1)),
 Ethics = sample(levels_useful, size = 100, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.2, 0.1)),
 Standards = sample(levels_useful, size = 100, replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.3)),
 Practices = sample(levels_useful, size = 100, replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.3)),
 Morals = sample(levels_useful, size = 100, replace = TRUE, prob = c(0.05, 0.05, 0.2, 0.3, 0.4)),
 Responsible_oe = ifelse(Responsible == "Not at all useful",
                 stringi::stri_rand_lipsum(sample(1:100)), NA_character_),
 Ethics_oe = ifelse(Ethics == "Not at all useful",
                 stringi::stri_rand_lipsum(sample(1:100)), NA_character_),
 Standards_oe = ifelse(Standards == "Not at all useful",
                 stringi::stri_rand_lipsum(sample(1:100)), NA_character_),
 Practices_oe = ifelse(Practices == "Not at all useful",
                 stringi::stri_rand_lipsum(sample(1:100)), NA_character_),
 Morals_oe = ifelse(Morals == "Not at all useful",
                 stringi::stri_rand_lipsum(sample(1:100)), NA_character_)
 ) \%>\% dplyr::select(dplyr::ends_with("_oe"))

# Set up character vector of text or other things like punctuation to remove from the text data:
remove_values <- c("N/A", ".", "A")

# Cleanup the open-ended response in the variable "Responsible_oe" with the function:
data \%>\% openendedCleanup(., "Responsible_oe", remove_values)
}

# Load raw data from .csv file
exampleData <- read.csv("data-raw/simulated-data.csv")
nhanes <- read.csv("data-raw/NHANES.csv")
ve <- read.csv("data-raw/sample_300.csv")
# Apply preprocessing...
# Save the cleaned data in the required R package location
usethis::use_data(exampleData, overwrite = TRUE)
usethis::use_data(nhanes, overwrite = TRUE)
usethis::use_data(ve, overwrite = TRUE)

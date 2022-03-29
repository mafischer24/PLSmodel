## Script to generate
library(tidyverse)
library(janitor)

# Generate Greenland dataframe by importing interpolation function
source('interpolation.R')

# Greenland dataframe ready for model
greenland_df <- interpolate_greenland()

# Generate Alaska dataframe

# First need to load list of sample names
fname <- list.files("Samples/alaska_csv", full.names = T)

# Read all samples into a list
filelist <- map(fname, read_csv)

# Add names to samples
names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)

# select the columns we want
filelist <- map(filelist, function(x) {
  x %>%
    select(wavenumber, absorbance)
})

# Make the columns names the wavenumbers and the values the absorbance values
reformattedData <- lapply(filelist, function(x) {
  pivot_wider(x, names_from = wavenumber, values_from = absorbance)

})

# create a matrix of the wavenumbers from each sample
wavenumber_matrix <- map(reformattedData, names)

# Convert matrix into data frame where each sample is its own row of wavenumber values
wavenumber_df <- as.data.frame(do.call("rbind", wavenumber_matrix))
# After this Vivienne didn't "trust" it to store the names so she added them as a column..
# Something we should worry about? It's because detaching to create absorbance matrix...

  # This is Vivienne's function dropNames
  # Don't know if we need the following line
  #wavenumber_df$dataset <- names(filelist) ## make this a specific column, don't trust it to store
  # Rename column header from "wavenumbers" to "Vi" (FUNCTION #3)
  dropNames <- function(data) {
    names(data) <- paste("V", 1:ncol(data), sep = "")
    return(data)
  }

# creating new list of df where there aren't any wavenumbers...only absorbance values [1:3697]
absorbance_matrix <- map(reformattedData, dropNames)
absorbance_df <- as.data.frame(do.call("rbind", absorbance_matrix))

# Read in
AK_wav <- read_csv("Samples/alaska_csv/AS-01\ (8_24_16).0.csv")
ak_wavenumbers <- AK_wav$wavenumber

colnames(absorbance_df) <- ak_wavenumbers

alaska_wet_chem <- read_csv("Maxwell-Alaska Samples  - Final Top 100.csv") %>%
  janitor::clean_names() %>%
  select(-notes, -toc_percent)

names(alaska_wet_chem)[2] <- "BSi"

absorbance_df <- absorbance_df %>%
  rownames_to_column(var = "sample")

# Alaska dataframe ready for model
alaska_df <- inner_join(absorbance_df, alaska_wet_chem, by = "sample") %>%
  select(BSi, everything()) %>%
  column_to_rownames(var = "sample")

# Dataframe with both Greenland and Alaska samples ready for model
gl_ak_combined_df <- rbind(greenland_df, alaska_df) %>%
  # Deleted last column because of NA and 0 values
  select(-1883)

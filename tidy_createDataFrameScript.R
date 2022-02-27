# Load relevant libraries----
library(tidyverse)
library(data.table)

## read in txt files automatically----

# How much to include in these functions
# These functions are mostly for organizational purpose right now
# Functionally, we will need to assess what we need to break up

# Change function names
read_txt_files <- function(x) {
  fname <- list.files(x, full.names = T)
  # read.csv or read.table: which is more universal?
  # Change these lines once we change script to convert to .csv, not sure if we have any business
  # naming the columns here
  # Need to separate columns
  # Need to get rid of index
  filelist <- map(fname, read.csv, sep = "")

  #  filelist <- map(filelist, separate(col = 1, into = c("wavenumber", "absorbance"), sep = ","))

  # This naming convention hack might need to be in a separate function
  # Or we ensure that people use the same naming convention for samples
  # Maybe deal with making that uniform in the .py .csv convert script
  names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)

  # Separate index, wavenumber, and absorbance into three columns (from one, separated by comma)
  # Need to do this via mapping onto each sample file in list
  filelist <- map(filelist, function(x) {
    separate(x, col = 1, into = c("index", "wavenumber", "absorbance"), sep = ",") %>%
      # Delete the index column
      select(-index)
      })
  # This returns list of lists of characters, need to convert to numeric
  # Is this is the place to do it?
  # Not sure where Vivienne's file converts to numeric, because it is also character at this point
  return(filelist)
}


create_df <- function(x) {
  reformattedData <- map(x, function(x) {
    pivot_wider(x, names_from = wavenumber, values_from = absorbance)
  })
  wavenumber_matrix <- map(reformattedData, names)

  wavenumber_df <- as.data.frame(do.call("rbind", wavenumber_matrix))
  # After this Vivienne didn't "trust" it to store the names so she added them as a column..
  # Something we should worry about?
}

# Load relevant libraries----
library(tidyverse)
library(data.table)

## read in txt files automatically----

# include V1 and V2 to absorbance and wavenumber?


# How much to include in these functions

read_txt_files <- function(x) {
  fname <- list.files(x, full.names = T)
  filelist <- map(fname, read.table, sep = "")
  names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)


  return(filelist)
}


clean <-read_txt_files("Samples/greenlandSamples")

create_df <- function(x) {
  reformattedData <- map(x, function(x){pivot_wider(x, names_from = V1, values_from = V2)})
  wavenumber_matrix <- map(reformattedData, names)

  wavenumber_df <- as.data.frame(do.call("rbind", wavenumber_matrix))
  # After this Vivienne didn't "trust" it to store the names so she added them as a column..
  # Something we should worry about?


}


new <-create_df(clean)

# Rename column header from "wavenumbers" to "Vi" (FUNCTION #3)
# Because the wavenumbers aren't exactly the same, we should rename to generic "v1, v2, etc"
# Can we just truncate the wavenumbers?
# Perhaps later we can have a warning if the values aren't the same after truncating, or changing a lot?

dropNames <- function(data) {
  names(data) <- paste("V", 1:ncol(data), sep = "")
  return(data)
}


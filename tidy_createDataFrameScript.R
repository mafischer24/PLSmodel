# Load relevant libraries----
library(tidyverse)

# How much to include in these functions
# These functions are mostly for organizational purpose right now
# Functionally, we will need to assess what we need to break up

read_files <- function(x) {
  # Read in each sample file
  fname <- list.files(x, full.names = T)
  filelist <- map(fname, read.csv, sep = "")

  # Attach names to samples

  # This naming convention hack might need to be in a separate function
  # Or we ensure that people use the same naming convention for samples
  # Maybe deal with making that uniform in the .py .csv convert script
  # This is custom to the greenland samples
  names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)

  # Separate index, wavenumber, and absorbance into three columns (from one, separated by comma)
  # Need to do this via mapping onto each sample file in list
  filelist <- map(filelist, function(x) {
    separate(x, col = 1, into = c("index", "wavenumber", "absorbance"), sep = ",") %>%
      # Delete the index column
      select(-index)

    # Possible if statement to make any format (that doesn't have index column) readable:
    # This if statement might not be necessary if we don't need to convert to .csv and can just feed any format in
    # Essentially, we are stripping the extra column that the .py convert to python script creates if said index column exists, which it doesn't for the raw files
    # We should add headers if there are none, and eliminate any additional columns
    # A problem for another day

    #   if(substring(x, 1, 1) == ","){
    #   separate(x, col = 1, into = c("index", "wavenumber", "absorbance"), sep = ",") %>%
    #     # Delete the index column
    #     select(-index)
    #     print("we had indices")}
    #   else{
    #     separate(x, col = 1, into = c("wavenumber", "absorbance"), sep = ",")
    #     print("we didn't have indicices")
    #   }
  })

  # This returns list of lists of characters, need to convert to numeric
  # Is this is the place to do it?
  # Not sure where Vivienne's file converts to numeric, because it is also character at this point
  return(filelist)
}


create_wavenumber_df <- function(x) {
  # Make the columns names the wavenumbers and the values the absorbance values
  reformattedData <- map(x, function(x) {
    pivot_wider(x, names_from = wavenumber, values_from = absorbance)
  })

  # create a matrix of the wavenumbers from each sample
  wavenumber_matrix <- map(reformattedData, names)

  # Convert matrix into data frame where each sample is its own row of wavenumber values
  wavenumber_df <- as.data.frame(do.call("rbind", wavenumber_matrix))
  # After this Vivienne didn't "trust" it to store the names so she added them as a column..
  # Something we should worry about? It's because detaching to create absorbance matrix...


}

create_absorbance_df <- function(x){
  # I think there's a better way to do this part but cannot right now

  reformattedData <- map(x, function(x) {
    pivot_wider(x, names_from = wavenumber, values_from = absorbance)
  })

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

}

# Write to .csv in example file, should it write to the same .csv every time?
# Not sure if we need to have a column with sample names like in Vivienne's code? That might be missing if we have bugs later

# Should this be in script or in example file?

# Below here is unchanged from Vivienne's code. Can be streamlined I think

# Read in calibration csv with same number of samples as our transformedData
wet_chem_data <- read_csv("csvFiles/wet-chem-data.csv") ### 28

# Read in absorbance values for each sample
absorbance <- read_csv("csvFiles/absorbance.csv") ### 28:3698  #Missing column names filled in: 'X1'

# Rename wet_chem_data columns
names(wet_chem_data)[1] <- "dataset"
names(wet_chem_data)[2] <- "BSiPercent"

# bind calibration data to transformed data
wetChemAbsorbance <- full_join(wet_chem_data, absorbance, by = "dataset")

## this replaces .0 with a space, the backslashes escape the special character . in regular expressions
wetChemAbsorbance$dataset <- gsub("\\.0", "", wetChemAbsorbance$dataset)

## this replaces cm with a space, the backslashes escape the special character . in regular expressions
wetChemAbsorbance$dataset <- gsub("cm", "", wetChemAbsorbance$dataset)

# Write csv file
write.csv(wetChemAbsorbance, "csvFiles/wetChemAbsorbance.csv", row.names = F)



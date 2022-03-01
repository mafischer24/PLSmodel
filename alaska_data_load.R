library(tidyverse)
library(janitor)

#upload alaska files
#afname <- list.files("Samples/alaska_csv", full.names = T)
#alaska_filelist <- lapply(afname, read.table, sep=",", header=TRUE)

#grab names
#grab file name
#anames <- list.files("Samples/alaska_csv", full.names = F)
#split by space: makes a list of list of names
#anames <-  str_split(anames,pattern = " " )
#create a variable to hold names
#alaska_sample_names <- numeric(length = length(anames))
#populate the variables
#for(i in 1:length(anames)){
#  alaska_sample_names[i]<- anames[[i]][1]
#}

#attach names to column
#names(alaska_filelist) <- alaska_sample_names

##need to reformat be removing the first column of numbers + change absorbance

#make a wave number matrix + absorbance matrix ##not complete so disregard for now
#for(i in 1:100){

  alaska_filelist[[i]] %>%
    select(-1)

  #pivot wider
  # add each row to respective matrix
}

## add calibration data for alaska
alaska_wet_chem <- read_csv("Maxwell-Alaska Samples  - Final Top 100.csv") %>%
  clean_names() %>%
  select(-notes, -toc_percent)

names(alaska_wet_chem)[1] <- "dataset"
names(alaska_wet_chem)[2] <- "BSiPercent"

# Read in absorbance values for each sample
library(data.table)
setDT(alaska_absorbance_df, keep.rownames = "dataset")[]

# bind calibration data to transformed data
AlaskaWetChemAbsorbance <- full_join(alaska_wet_chem, alaska_absorbance_df, by = "dataset")

## this replaces .0 with a space, the backslashes escape the special character . in regular expressions
AlaskaWetChemAbsorbance$dataset <- gsub("\\.0", "", AlaskaWetChemAbsorbance$dataset)

## this replaces cm with a space, the backslashes escape the special character . in regular expressions
AlaskaWetChemAbsorbance$dataset <- gsub("cm", "", AlaskaWetChemAbsorbance$dataset)

# Write csv file
write.csv(AlaskaWetChemAbsorbance, "csvFiles/AlaskaWetChemAbsorbance.csv", row.names = F)


## streamline the process

## assuming that both dataframe has been read have the same sample column name
create_model_df_1 <- function(wetChemData, absorbData, sampleColName){
  # bind calibration data to transformed data
  wetChemAbsorb <- full_join(wetChemData, absorbData, by = sampleColName)
  ## this replaces .0 with a space, the backslashes escape the special character . in regular expressions
  wetChemAbsorb$dataset <- gsub("\\.0", "", wetChemAbsorb$dataset)
  ## this replaces cm with a space, the backslashes escape the special character . in regular expressions
  wetChemAbsorb$dataset <- gsub("cm", "", wetChemAbsorb$dataset)

  return(wetChemAbsorb)
}



# the same function but directly  from path
create_model_df_2 <- function(wetChemDataPath, absorbDataPath){
  #read FTIRS  files
  ftirs <- read_files(absorbDataPath)
  #create absorbance df
  absorbData <- create_absorbance_df(ftirs)
  #change data to numeric (best to do create_absorbance_df function)

  #check if the the absorbance data has name attribute
  if(Negate(is.null)(names(absorbData))) {
    require(data.table)
    setDT(absorbData, keep.rownames = "dataset")[]
  }

  #read bsi wet chem percentages
  wetChemData <- read_csv(wetChemDataPath) %>%
    clean_names()
  #rename columns assuming the first col is sample names and second col is BSi percents
  names(wetChemData)[1] <- "dataset"
  names(wetChemData)[2] <- "BSiPercent"
  #select only those two cols
  wetChemData <- wetChemData %>%
    select(dataset, BSiPercent)

  # bind calibration data to transformed data
  wetChemAbsorb <- full_join(wetChemData, absorbData, by = "dataset")

  #clean sample names
  ## this replaces .0 with a space, the backslashes escape the special character . in regular expressions
  wetChemAbsorb$dataset <- gsub("\\.0", "", wetChemAbsorb$dataset)
  ## this replaces cm with a space, the backslashes escape the special character . in regular expressions
  wetChemAbsorb$dataset <- gsub("cm", "", wetChemAbsorb$dataset)

  return(wetChemAbsorb)
}

#test
aAbsC <- lapply(alaska_absorbance_df, as.numeric)
apply(is.na(aAbsC), 2, which) #chr w/ scientific notation was tuned to NA so need to fix before
testfun_path <- create_model_df_2("Maxwell-Alaska Samples  - Final Top 100.csv",
                                  "Samples/alaska_csv" )
testfun_df <- create_model_df_1(alaska_wet_chem, alaska_absorbance_df, "dataset")



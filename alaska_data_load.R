library(tidyverse)
library(janitor)

#alternative to read_files() not fully developed yet
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

#  alaska_filelist[[i]] %>%
<<<<<<< HEAD
#    select(-1)
=======
 #   select(-1)
>>>>>>> ae39f920a29b2a4be853672bbd2f304fd6ff4b49

  #pivot wider
  # add each row to respective matrix
#}

## add calibration data for alaska
alaska_wet_chem <- read_csv("Maxwell-Alaska Samples  - Final Top 100.csv") %>%
  clean_names() %>%
  select(-notes, -toc_percent)

names(alaska_wet_chem)[1] <- "dataset"
names(alaska_wet_chem)[2] <- "BSiPercent"

write.csv(alaska_wet_chem, "csvFiles/AlaskaWetChem.csv", row.names = F)

alaska_absorbance_df <- create_absorbance_df(alaska)

#not sure why but when rthe next line is run alone it will produce an error but if you run the prevouis with it runs ok
AlaskaWetChemAbsorbance <- create_model_df_1(alaska_wet_chem, alaska_absorbance_df, "dataset")

# Write csv file
#write.csv(AlaskaWetChemAbsorbance, "csvFiles/AlaskaWetChemAbsorbance.csv", row.names = F)




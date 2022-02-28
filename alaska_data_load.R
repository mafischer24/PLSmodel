library(tidyverse)
#upload alaska files
afname <- list.files("Samples/alaska_csv", full.names = T)
alaska_filelist <- lapply(afname, read.table, sep=",", header=TRUE)

#grab names
#grab file name
anames <- list.files("Samples/alaska_csv", full.names = F)
#split by space: makes a list of list of names
anames <-  str_split(anames,pattern = " " )
#create a variable to hold names
alaska_sample_names <- numeric(length = length(anames))
#populate the variables
for(i in 1:length(anames)){
  alaska_sample_names[i]<- anames[[i]][1]
}

#attach names to column
names(alaska_filelist) <- alaska_sample_names

##need to reformat be removing the first column of numbers + change absorbance

#make a wave number matrix + absorbance matrix
for(i in 1:100){

  alaska_filelist[[i]] %>%
    select(-1)

  #pivot wider
  # add each row to respective matrix
}



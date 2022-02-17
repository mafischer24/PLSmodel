library(tidyverse)

afname <- list.files("Samples/alaskaSamples", full.names = T)
alaska_filelist <- lapply(afname, read.table, sep="")

#how to make a regular exp to extract names of samples?
names(alaska_filelist) <- grep(".*/(.*)\\..*", afname) ## not sure what that mean but it numbers samples

#attach names to column

##the data has one column, I am assuming it is wavenumber and absorbance values
## why values are a factor? need to reformat


#make a wave number matrix + absorbance matrix
for(i in 1:100){
  alaska_samples <- alaska_filelist[[i]] %>%
    ## seperating into two columns
    separate(col = 1, into = c("wavenumber", "absorbance"), sep = ",")
  #pivot wider
  # add each row to respective matrix
}



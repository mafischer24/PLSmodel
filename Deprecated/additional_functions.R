## assuming that both dataframe has been read have the same sample column name
create_model_df_1 <- function(wetChemData, absorbData, sampleColName){
  if(Negate(is.null)(names(absorbData))) {
    require(data.table)
    setDT(absorbData, keep.rownames = "dataset")[]
  }
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

#edited just to have numeric values for the final matrix

read_files <- function(x) {
  # Read in each sample file
  fname <- list.files(x, full.names = T)
  filelist <- map(fname, read_csv)

  names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)

  # select the columns we want
  filelist <- map(filelist, function(x) {
    x %>%
      select(wavenumber, absorbance)
  })

  return(filelist)
}

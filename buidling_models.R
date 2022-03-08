library(tidyverse)
library(janitor)
library(pls)

##building differnt models to test out:
# model 1
##greenland data only: full spectrum
wetChemAbsorbance <- read_csv("csvFiles/wetChemAbsorbance.csv") %>%
  select(-1, -X1)

greenlandPLS <- plsr(BSiPercent~., ncomp =10, data=wetChemAbsorbance, validation = "CV", segments = 10)

summary(greenlandPLS)

##intopolated greenland data only: (data set found in interpolation.R, maybe we should write to csv in file )

##having errors with this one if i change ncomp=10 or segements =10 ?
greenlandPLS_interp <- plsr(BSi~., ncomp =6, data=gl_interp, validation = "CV", segments = 5)

summary(greenlandPLS_interp) #RMSE is larger than w/o interpolation

# model 2
#alaska only

akWetChemAbsorbance <- read_csv("csvFiles/AlaskaWetChemAbsorbance.csv") %>%
  column_to_rownames(var="dataset")

#why is there NA rows?
akWetChemAbsorbance <- na.omit(akWetChemAbsorbance)
alaskaPLS <- plsr(BSiPercent~., ncomp = 10, data=akWetChemAbsorbance, validation = "CV", segments = 10)

summary(alaskaPLS)

#model 3
# joining alaska +  interpolated greenland
#extract col names of one df
colName <- colnames(gl_interp)

ak_wetChemAbs_transposed <- akWetChemAbsorbance
#change column names to be able to rbind
colnames(ak_wetChemAbs_transposed) <- colName
#stack the two the dataframes
akGL_wetChemAbs <- rbind(gl_interp, ak_wetChemAbs_transposed) %>%
  #I also deleted the last column becuase it has NA values for most at wavenumber 368.38622
  select(-1883)

akGLpls <- plsr(BSi~., ncomp = 10, data=akGL_wetChemAbs, validation = "CV", segments = 10)

summary(akGLpls) # best results so far

#model 4
# only considering one interval
#names(akGL_wetChemAbs %>% select(contains(`1282.60124`)))
akGL_wetChemAbs_1050_1280 <- akGL_wetChemAbs %>%
  select(BSi, `1049.22567`:`1282.60124`)

akGLpls_1050 <- plsr(BSi~., ncomp = 10, data=akGL_wetChemAbs_1050_1280, validation = "CV", segments = 10)
summary(akGLpls_1050)

#model 5
# considering  multiple intervals
#368 - 3750cm-1  # using most spectrum perhaps cut in model 3
#435 - 480cm-1
#790 - 830cm-1
#1050 - 1280cm-1

akGL_wetChemAbs_intervals <- akGL_wetChemAbs %>%
  select(BSi, `1049.22567`:`1282.60124`,`433.96282`:`480.25219`,`788.84798`:`831.2799`  )

akGLpls_intervals <- plsr(BSi~., ncomp = 10, data=akGL_wetChemAbs_intervals, validation = "CV", segments = 10)
summary(akGLpls_intervals)


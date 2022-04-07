library(tidyverse)
library(janitor)
library(pls)

##building differnt models to test out:
# model 1
##greenland data only: full spectrum
#Greenland un-intropolated + full spectrum
wetChemAbsorbance <- read_csv("csvFiles/wetChemAbsorbance.csv") %>%
  select(-1, -X1)
greenlandPLS <- plsr(BSiPercent~., ncomp =10, data=wetChemAbsorbance,
                     validation = "CV", segments = 10)
summary(greenlandPLS)

##intopolated greenland only data :
##having errors with this one if i change ncomp=10 or segements =10 ?
greenlandPLS_interp <- plsr(BSi~., ncomp =10, data= greenland_df, validation = "CV", segments = 10)

summary(greenlandPLS_interp) #RMSE is smaller here with interpolation

#testing the greenland model with alaska data
predict(greenlandPLS_interp, alaska_df%>%select(-1))
predplot(greenlandPLS_interp, ncomp = 3, newdata =  alaska_df, asp = 1, line = TRUE)


# model 2
#alaska only

alaska_df <- na.omit(generate_alaska())
alaskaPLS <- plsr(BSi~., ncomp = 10, data=alaska_df, validation = "CV", segments = 10)
summary(alaskaPLS)

#predicting using geenland data
predict(alaskaPLS, greenland_df%>%select(-1))
predplot(alaskaPLS, ncomp = 4, newdata =  greenland_df, asp = 1, line = TRUE)

#model 3
# using alaska +  interpolated greenland data
#extract col names of one df

akGLpls <- plsr(BSi~., ncomp = 10, data=gl_ak_combined_df, validation = "CV", segments = 10)

summary(akGLpls) # best results so far # why did it drop from 3.99 to 3.97??

#model 4
# only considering one interval
#names(akGL_wetChemAbs %>% select(contains(`1282.60124`)))
akGL_wetChemAbs_1050_1280 <- gl_ak_combined_df %>%
  select(BSi, `1049.22567`:`1282.60124`)

akGLpls_1050 <- plsr(BSi~., ncomp = 10, data=akGL_wetChemAbs_1050_1280, validation = "CV", segments = 10)
summary(akGLpls_1050)

#model 5
# considering  multiple intervals
#368 - 3750cm-1  # using most spectrum perhaps cut in model 3
#435 - 480cm-1
#790 - 830cm-1
#1050 - 1280cm-1

akGL_wetChemAbs_intervals <- gl_ak_combined_df %>%
  select(BSi, `1049.22567`:`1282.60124`,`433.96282`:`480.25219`,`788.84798`:`831.2799`  )

akGLpls_intervals <- plsr(BSi~., ncomp = 10, data=akGL_wetChemAbs_intervals, validation = "CV", segments = 10)
summary(akGLpls_intervals)


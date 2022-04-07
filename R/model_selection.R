########################################
# Building models for MSE calculations #
########################################

# Using the compiled data load, hopefully will avoid some of the sync problems
# that building_models.R ran into
# (it won't even run for me at the moment, hence this file)

# Loading in all the data cleanly, it auto runs the file
# Also happens to load Tidyverse and Janitor, so no need to redo that import
source('R/compiled_data_load.R')
library(pls)

# For MSE function not tied to a model
library(Metrics)

# Removing all the extraneous files we created in that file. Maybe want to do this
# in that file instead of this one, but we can talk about that
rm(absorbance_df, absorbance_matrix, AK_wav, akWetChemAbsorbance, alaska_wet_chem,
   filelist, reformattedData, wavenumber_df, wavenumber_matrix, fname)


# Removing the glitched 368 wavenumber col, it's not in the combined df
alaska_df <- alaska_df %>%
  select(-last_col())
greenland_df <- greenland_df %>%
  select(-last_col())

# This name's character salad, just a rename here
combined_df <- gl_ak_combined_df[1:128,]
rm(gl_ak_combined_df)

# Setting the target outputs
alaska_actual = alaska_df$BSi
greenland_actual = greenland_df$BSi
both_actual <- combined_df$BSi


######################################
#### First Model - Greenland Only ####
######################################

# Model Creation
greenlandPLS <- plsr(BSi~., ncomp =10, data=greenland_df, validation = "CV", segments = 10)
summary(greenlandPLS)

# Getting the vector with only the predictions from the 10-component model (can modify if we want)
predictions_gl <- as.data.frame(predict(greenlandPLS, combined_df%>%select(-1)))$`BSi.10 comps`
predictions_gl_ak <- as.data.frame(predict(greenlandPLS, alaska_df%>%select(-1)))$`BSi.10 comps`

### Testing on Both ###
# Mean absolute deviation (For Greg)
mae_gl_both <- mae(both_actual, predictions_gl)
# Root Mean Squared Error (for model selection)
mse_gl_both <- mse(both_actual, predictions_gl)

### Testing on Alaska Only ###
# Mean absolute deviation (For Greg)
mae_gl_ak <- mae(alaska_actual, predictions_gl_ak)
# Root Mean Squared Error (for model selection)
mse_gl_ak <- mse(alaska_actual, predictions_gl_ak)

#Saving the scores for later graphing, maybe
scores_df <- as.data.frame(rbind(c("modeled_on", "tested_on","mse","mad"),
                        c("Greenland","Alaska", mse_gl_ak, mae_gl_ak),
                        c("Greenland","Combined", mse_gl_both, mae_gl_both))) %>%
  row_to_names(row_number = 1)

# Cleaning the environment for my sanity
rm(mse_gl_ak, mae_gl_ak, mse_gl_both,mae_gl_both, predictions_gl_ak, predictions_gl)

# For Greenland Only, testing on both
# MAD/MAE:  5.33
# MSE:      42.50

# For Greenland Only, testing on Alaska Only
# MAD/MAE:  6.45
# MSE:      53.77

# Comment: Not bad for a model trained on 28 samples, but rather bad by most other metric.
#   Gotta give it props for getting as far as it did with that sample size.

####################################
#### Second Model - Alaska Only ####
####################################

# Model Creation
alaskaPLS <- plsr(BSi~., ncomp = 10, data = alaska_df, validation = "CV", segments = 10)
summary(alaskaPLS) # Model accuracy on just itself seems lower than the greenland ones, interestingly.

# Prediction Vectors
predictions_ak <- as.data.frame(predict(alaskaPLS, combined_df%>%select(-1)))$`BSi.10 comps`
predictions_ak_gl <- as.data.frame(predict(alaskaPLS, greenland_df%>%select(-1)))$`BSi.10 comps`

### Testing on Both ###
# Mean absolute deviation/error
mae_ak_both <- mae(both_actual, predictions_ak)
# Root Mean Squared Error (for model selection)
mse_ak_both <- mse(both_actual, predictions_ak)

### Testing on Alaska Only ###
# Mean absolute deviation (For Greg)
mae_ak_gl<- mae(greenland_actual, predictions_ak_gl)
# Root Mean Squared Error (for model selection)
mse_ak_gl <- mse(greenland_actual, predictions_ak_gl)
# Adding them to the data frame
scores_df <- rbind(scores_df,
                   c("Alaska","Greenland",mse_ak_gl, mae_ak_gl),
                   c("Alaska","Combined",mse_ak_both, mae_ak_both))

# Cleanup time!
rm(mae_ak_gl, mae_ak_both, mse_ak_gl, mse_ak_both, predictions_ak, predictions_ak_gl)

# For Alaska only, testing on both
# MAD/MAE:  5.34
# MSE:      61.63

# For Alaska only, testing on Greenland
# MAD/MAE:  13.60
# MSE:      261.25

# Comment: Oof. Alaska, you have too many samples to be losing like this...
#   Alaska's predictions vector seems to be half negative for the Greenland samples too,
#   which is weird. Greenland consistently overpredicted the Alaska BSi percentage
#   and it's looking like alaska is consistently underpredicting Greenland, badly.
#   I wonder if it's something related to the soil compositions, the spect setting, or both

###########################################
#### Third Model - Both, Full Spectrum ####
###########################################

# Model
combined_PLS <- plsr(BSi~., ncomp=10, data = combined_df, validation = "CV", segments = 10)
summary(combined_PLS)

# Prediction Vector
predictions_comb <- as.data.frame(predict(combined_PLS, combined_df%>%select(-1)))$`BSi.10 comps`

# Testing on both (testing on each part seems a little silly and I'm tired of typing)
mae_comb <- mae(both_actual, predictions_comb)
mse_comb <- mse(both_actual, predictions_comb)

# Adding to the df
scores_df <- rbind(scores_df,
                   c("Combined","Combined",mse_comb,mae_comb))

# Cleanup
rm(mse_comb, mae_comb, predictions_comb)

# For Combined, testing on itself
# MAD/MAE:  2.04
# MSE:      6.87

# Comment: Much better looking, honestly. Mean error is almost within 2% BSi,
#   which may not be as good as Greg hopes for but is still pretty good
#   and is probably better than we're getting otherwise

#################################################
#### Fourth Model - Both, Limited Spectrum 1 ####
#################################################

# Selection (Thanks for the code, Rana)
limited_wavenumbers <- combined_df %>%
  select(BSi, `1049.22567`:`1282.60124`)

# Model
combined_PLS <- plsr(BSi~., ncomp=10, data = limited_wavenumbers, validation = "CV", segments = 10)
summary(combined_PLS)

# Prediction Vector
predictions_comb <- as.data.frame(predict(combined_PLS, combined_df%>%select(-1)))$`BSi.10 comps`

# Testing on both (testing on each part seems a little silly and I'm tired of typing)
mae_comb <- mae(both_actual, predictions_comb)
mse_comb <- mse(both_actual, predictions_comb)

# Adding to the df
scores_df <- rbind(scores_df,
                   c("Limited - One Segment","Combined",mse_comb,mae_comb))

# Cleanup
rm(mse_comb, mae_comb, predictions_comb, limited_wavenumbers)

# For Limited, one segment, testing on the combined data
# MAD/MAE:  2.85
# MSE:      12.55

# Comment: Better than either of the isolated models by far but can't beat the
#   full spectrum.

#################################################
#### Fifth Model - Both, Limited Spectrum 2 ####
#################################################

# Selection (Thanks for the code, Rana)
limited_wavenumbers <- combined_df %>%
  select(BSi, `1049.22567`:`1282.60124`,`433.96282`:`480.25219`,`788.84798`:`831.2799`  )

# Model
combined_PLS <- plsr(BSi~., ncomp=10, data = limited_wavenumbers, validation = "CV", segments = 10)
summary(combined_PLS)

# Prediction Vector
predictions_comb <- as.data.frame(predict(combined_PLS, combined_df%>%select(-1)))$`BSi.10 comps`

# Testing on both (testing on each part seems a little silly and I'm tired of typing)
mae_comb <- mae(both_actual, predictions_comb)
mse_comb <- mse(both_actual, predictions_comb)

# Adding to the df
scores_df <- rbind(scores_df,
                   c("Limited - Multi-Segment","Combined",mse_comb,mae_comb))

# Cleanup
rm(mse_comb, mae_comb, predictions_comb, limited_wavenumbers)

# For Limited, multi segment, testing on the combined data
# MAD/MAE:  2.79
# MSE:      13.37

# Comment: About equally bad as the other limited one. Slightly worse in the MSE,
#   slightly better in the MAD. Either way, can't beat the full spectrum.

######################################
######################################

# Final Comments:

# By both MSE and MAD, we can be confident that the full spectrum combined
# model is the best one for predicting the full dataset. I am curious, though,
# about whether it'd be better with a location boolean variable (since the
# Greenland model consistently overpredicted Alaska and the Alaskan model
# consistently underpredicts the Greenland data), but who knows. Also TOC
# was not used here because it's not yet fully integrated into the dataset builder.

# The Scores Data frame is available if we want to graph the data. The code for the
# single prediction vectors is easily copyable if we wanted to test all five
# on all three possible outputs, but that was a lot of typing and not a lot
# of actual usefulness so I chose not to at this time.

library(tidyverse)
library(Metrics)
library(pls)
#read in greenland samples
fname <- list.files("Samples/greenlandSamples", full.names = T)
filelist <- lapply(fname, read.table, sep="")
names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)

# plotting all of the data sprectrum
par(mfrow = c(6, 5))
par(mar=c(1,1,1,1))

for (i in 1:length(filelist))
{
  plot(filelist[[i]])
} ## the last two seem off

#reformat data

#intopolation

#create model (assuming vivienne;s code is all run)
plsModel <- plsr(BSiPercent~., ncomp = 10, data=wetChemAbsorbance, validation = "CV", segments = 5)
summary(plsModel)
RMSEP <- RMSEP(plsModel)
par(mfrow = c(1, 1))
plot(RMSEP)

plot(plsModel, ncomp = 3:8, asp = 1, line = TRUE) # there are some outliers,  best # 3,4,5
#score plots: are often used to look for patterns, groups or outliers in the data.
plot(plsModel, plottype = "scores", comps = 3:5)
#explained vaiance
explvar(plsModel)
#loading plot: interpretation purposes, for instance to look for known spectral peaks or profiles:
plot(plsModel, "loadings", comps = 2:8, legendpos = "topleft", xlab = "nm")
abline(h = 0)


## a loop to generate residual graphs + means for each component in the model
  for (i in 2:10){ #only after running all of viviine code!
    plsModel <- plsr(BSiPercent~., ncomp = i, data=wetChemAbsorbance, validation = "CV", segments = 10)

    predicted_bsi <- as.data.frame(plsModel$fitted.values)
    #select model with 3 components
    predicted_bsi <- as.data.frame(predicted_bsi[,c(i)])
    #Rename wet_chem_data columns
    names(predicted_bsi )[1] <- "BSiPercent_Predicted"
    #Combine actual and predicted BSi wetchem data
    BSi <- cbind(actual_bsi_wetchem, predicted_bsi)
    #reformat into long so we can graph
    BSi_Long <- BSi %>%
      select(dataset, BSiPercent, BSiPercent_Predicted)%>%
      gather(key = "variable", value = "value", -dataset)
    #Create dataframe with residual error ----
    #Calculating Residuals: Difference between actual and predicted
    BSi$Difference <- (BSi$BSiPercent_Predicted - BSi$BSiPercent)
    #Table of differences for each sample
    Difference <- BSi %>%
      select(dataset, Difference)
    Difference <- round_df(Difference, 2)
    #Calculate absolute values, mean and median of residual errors ----
    Abs <- (abs(Difference$Difference))
    print(paste("for comp ", i, " the average residual is ", mean(Abs)))

    print(Difference %>%
      mutate(highlight_flag = ifelse(Difference >= '0', T, F)) %>%
      ggplot (aes(x = dataset, y = Difference)) +
      geom_col(aes(fill = highlight_flag)) +
      scale_fill_manual(values = c('red', 'green'), name = "Overfitting") +
      geom_text( data = Difference, aes(label = Difference), fontface ="bold", size = 2.5, vjust = 0) +
      labs(
        y = "Difference in Percentage",
        x = "Sample ID",
        title=expression("Residuals: Full Spectrum" ~ cm^{-1}),
        subtitle= paste0("For 28 Samples with ", i, " comp and ", 5, "segments"),
        colour = "variabl") +
      theme(legend.position = c(0.1, 0.85),
            axis.text.x  = element_text(angle = 90)) )
  }


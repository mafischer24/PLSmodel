library(tidyverse)
library(Metrics)
library(pls)

#quartz vs diatoms vs lake sample spectra ( what kind of data are we looking at? )
# what is ss?  sea sand
SS <- reformattedData$`SS.0` %>%
  pivot_longer(everything(), names_to = "wavenumber", values_to = "absorbance")

WQ <- reformattedData$`WQ.0` %>%
  pivot_longer(everything(), names_to = "wavenumber", values_to = "absorbance")

# where is the diatom data
#random lake sample

ggplot() +
  geom_line(data = WQ, aes(x=as.numeric(wavenumber), y= absorbance, color = "blue"))+
  geom_line(data = SS, aes(x=as.numeric(wavenumber), y= absorbance))



# fitting of acutal data with pls prediction vs linear prediction (why pls)
## lineir regression model:
# where is the area under the cureve results?

pls_bsi <- data.frame( wet_chem = wetChemAbsorbance$BSiPercent, pls = unlist(predicted_bsi_3))

pls_lm <- lm(pls ~ wet_chem, pls_bsi)
pls_bsi_r <- summary(pls_lm)$r.squared

ggplot(pls_bsi, aes(x=wet_chem, y = pls)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "darkGreen") +
  geom_text(x = 5, y = 25, label = paste("R^2: ",round_df(pls_bsi_r, 3)))+
  xlab("Wet Chemical BSi Percent (%)") +
  ylab("PLS Predicted BSi Percent (%)")+
  ggtitle("Predeicted Fitness Using PLS Model")+
  theme_bw()




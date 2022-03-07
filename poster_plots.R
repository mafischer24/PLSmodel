library(tidyverse)
library(Metrics)
library(pls)
library(tidyr)
library(ggpubr)

#quartz vs diatoms vs lake sample spectra ( what kind of data are we looking at? )
# what is ss?  sea sand
SS <- reformattedData$`SS.0` %>%
  pivot_longer(everything(), names_to = "wavenumber", values_to = "absorbance")

WQ <- reformattedData$`WQ.0` %>%
  pivot_longer(everything(), names_to = "wavenumber", values_to = "absorbance")

DB <- read.table("~/Downloads/DB 7 [2].0 (1).dpt", sep = " ") %>%
  separate(col = "V1", into = c("wavenumber", "absorbance"),
           convert = TRUE, sep = "[[:space:]]")

DE <- read.table("~/Downloads/DE 2.0.dpt", sep = " ") %>%
  separate(col = "V1", into = c("wavenumber", "absorbance"),
           convert = TRUE, sep = "[[:space:]]")
# where is the diatom data
#random lake sample
#435 - 480cm-1
#790 - 830cm-1
#1050 - 1280cm-1

start <- c(435, 790, 1050)
end <- c(480, 830, 1280)


ggplot() +
  geom_line(data = WQ, aes(x=as.numeric(wavenumber), y= absorbance, color = "Washed quartz"), color = "black")+
  geom_line(data = DB, aes(x=as.numeric(wavenumber), y= absorbance, color = "High BSi Sample"), color = "blue") +
  geom_line(data = DE, aes(x=as.numeric(wavenumber), y= absorbance), color = "darkred") +
  theme_bw() +
  xlim(4000, 300)+
  xlab("Absorbance") +
  ylab(expression(paste("Wavenumber ", cm^{-1}))) +
  geom_rect(inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin= - Inf, ymax=Inf),
            color="transparent", fill="orange", alpha=0.3) +
  ggtitle("Example FTIRS Spectra")




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




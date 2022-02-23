library(tidyverse)
library(Metrics)
library(pls)

#quartz vs diatoms vs lake sample spectra ( what kind of data are we looking at? )
# what is ss?
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
lm <- lm()


library(tidyverse)
library(Metrics)
library(pls)

#quartz vs diatoms vs lake sample spectra ( what kind of data are we looking at? )
SS <- reformattedData$`SS.0` %>%
  pivot_longer(everything(), names_to = "wavenumber", values_to = "absorbance")

WQ <- reformattedData$`WQ.0` %>%
  pivot_longer(everything(), names_to = "wavenumber", values_to = "absorbance")
ggplot(data = WQ, aes(x= ))


# fitting of acutal data with pl s prediction vs linera prediction (why pls)


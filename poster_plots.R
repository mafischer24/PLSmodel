library(tidyverse)
library(Metrics)
library(pls)

#quartz vs diatoms vs lake sample spectra ( what kind of data are we looking at? )
# what is ss?  sea sand
SS <- reformattedData$`SS.0` %>%
  pivot_longer(everything(), names_to = "wavenumber", values_to = "absorbance") %>%
  mutate(wavenumber = as.numeric(wavenumber))

WQ <- reformattedData$`WQ.0` %>%
  pivot_longer(everything(), names_to = "wavenumber", values_to = "absorbance")%>%
  mutate(wavenumber = as.numeric(wavenumber))

DB <- read.table("~/Downloads/DB 7 [2].0.dpt")
names(DB)[1] <- "wavenumber"
names(DB)[2] <- "absorbance"
DB <- DB %>%
  mutate(wavenumber = as.numeric(wavenumber))

# where is the diatom data
#random lake sample

#find areas for BSI to highlight
intervals <- c(368, 3750, 435, 480, 790, 830,1050, 1280)
WQ %>% filter_all(any_vars(. %in% intervals))
which(WQ == contains(intervals))

ggplot() +
  geom_line(data = WQ, aes(x=as.numeric(wavenumber), y= absorbance, color = "blue"))+
  geom_line(data = DB, aes(x=as.numeric(wavenumber), y= absorbance)) +
  scale_x_reverse() +
  theme_bw() +
  geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=min(dat$value),
                                               ymax=max(dat$value), group=group), color="transparent", fill="orange", alpha=0.3)



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






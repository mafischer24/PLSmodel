library(tidyverse)
library(ggplot2)
library(Metrics)
library(pls)
library(tidyr)
library(ggpubr)
library(janitor)
library(gbm)

set.seed(1)
#fig 1
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


#find areas for BSI to highlight
intervals <- c(368, 3750, 435, 480, 790, 830,1050, 1280)
WQ %>% filter_all(any_vars(. %in% intervals))
which(WQ == contains(intervals))

fig1 <- ggplot() +
  geom_line(data = WQ, aes(x=as.numeric(wavenumber), y= absorbance, color =  "Washed Qaurtz (No BSi)"))+
  geom_line(data = DB, aes(x=as.numeric(wavenumber), y= absorbance, color = "High BSi and TOC Sample")) +
  xlim(4000, 300)+
  xlab("Absorbance") +
  ylab(expression(paste("Wavenumber ", cm^{-1}))) +
  geom_rect(inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin= - Inf, ymax=Inf, fill= "BSi Range"),
            color="transparent", alpha=0.3) +
  geom_rect(inherit.aes=FALSE, aes(xmin=2800, xmax=3000, ymin= - Inf, ymax=Inf, fill= "TOC Range"),
            color="transparent", alpha=0.3) +
  ggtitle("Example FTIRS Spectra") +
  labs(colour="Sample",
       fill = "Correlated Peaks") +
  theme_bw()+
  scale_color_manual( values = c( "Washed Qaurtz (No BSi)" ="black", "High BSi and TOC Sample"="darkgreen")) +
  scale_fill_manual(values = c( "BSi Range"="orange","TOC Range"="yellow")) +
  theme(legend.position = "bottom",
        legend.text=element_text(size=8), legend.box="vertical", legend.margin=margin())
#ggsave(filename = "fig1.tiff" , plot = fig1,  device='tiff', dpi=700)



# fitting of acutal data with pls prediction vs linear prediction (why pls)

#prepare data
predicted_bsi <- as.data.frame(akGLpls$fitted.values)
#select model with 3 components
predicted_bsi <- as.data.frame(predicted_bsi[,c(2)])
#Rename wet_chem_data columns
names(predicted_bsi)[1] <- "BSiPercent_Predicted"
pls_bsi <- data.frame(wet_chem =akGL_wetChemAbs$BSi, pls = unlist(predicted_bsi))
#selected 15 random samples to be have the same number of observations as the area one
pls_bsi <- pls_bsi[sample(nrow(pls_bsi), 15), ]
## linear regression model:
pls_lm <- lm(pls ~ wet_chem, pls_bsi)
pls_bsi_r <- summary(pls_lm)$r.squared

lm_pls <- ggplot(pls_bsi, aes(x=wet_chem, y = pls)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "darkGreen") +
  geom_text(x = 8, y = 24, label = paste("R^2: ",round_df(pls_bsi_r, 3)))+
  xlab("Wet Chemical BSi Percent (%)") +
  ylab("PLS Predicted BSi Percent (%)")+
  ggtitle("Predeicted Fitness Using PLS Model")+
  labs(subtitle = "For 15 Random Samples")+
  theme_bw()

# area under peak ( most reported is only the largest peak)
ftirs_peak_areas <- read_csv("~/Downloads/FTIRS Peak Areas for Greenland Calibration Samples - Sheet1.csv") %>%
  clean_names()

calibration_wetchem <- read_csv("~/Downloads/Wet Chemistry Calibration Data - Sheet1.csv") %>%
  clean_names()

area_bsi <- ftirs_peak_areas %>%
  inner_join(calibration_wetchem,  "sample_id")

## linear regression model:
area_lm <- lm(peak_area_1000_1150_cm_1 ~ percent_b_si_based_on_chemical_digestion_method , area_bsi)
area_bsi_r <- summary(area_lm)$r.squared

lm_area <- ggplot(area_bsi, aes(x=percent_b_si_based_on_chemical_digestion_method, y = peak_area_1000_1150_cm_1)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  geom_text(x = 8, y = 14, label = paste("R^2: ",round_df(area_bsi_r, 3)))+
  xlab("Wet Chemical BSi Percent (%)") +
  ylab(expression(paste("1050 - 1150 ", cm^{-1}, " Peak Area Estimated")))+
  ggtitle("Predeicted Fitness Using Peak Areas") +
  labs(subtitle = "For 15 Samples")+
  theme_bw()

fig2 <- grid.arrange(lm_area, lm_pls, nrow = 1,
                     top = "Comparison Between traditional methods and PLS Model")
#ggsave(filename = "fig2.tiff" , plot = fig2,  device='tiff', dpi=700, width = 10)


#figure 3 : differnt models mean residuals:
#function that builds a mean residual dataframe for provided pls model
mean_res_df <- function(test_model){
  name <- as.character(deparse(substitute(test_model)))
  print(name)
  model_res <- residuals(test_model)
  mean_residuals_df <- data.frame(ncomp = 1:test_model$ncomp, mean_residuals = 1:test_model$ncomp)
  print(mean_residuals_df)
  for(i in 1:test_model$ncomp){
    mean_residuals_df$mean_residuals[i] <- mean(abs(model_res[,1,i]))
  }
  mean_residuals_df <- round_df(mean_residuals_df, 2)
  mean_residuals_df$model <- rep(name,test_model$ncomp )
  return(mean_residuals_df)
}

mean_residuals_all <- rbind(mean_res_df(akGLpls), mean_res_df(greenlandPLS),
                            mean_res_df(alaskaPLS),  mean_res_df(akGLpls_1050),
                            mean_res_df(akGLpls_intervals))

fig3 <- ggplot(mean_residuals_all, aes(x= ncomp, y = mean_residuals, color = model)) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = scales::breaks_extended(Q = 1:10)) +
  theme_bw() +
  geom_line(size = 0.5, alpha = 0.2)+
  xlab("Number of Components") +
  ylab("Mean Residuals (%)") +
  ggtitle("Mean Residuals") +
  scale_color_brewer(palette = "Greens") +
  scale_color_manual(values=c("#00bfff","#8DD181", "#B8E2B1","#A3C562", "#5A9E4E"),
                     name="Model",
                     labels=c("AK & GL (n= 127)", "AK & GL at 1050-1280 cm^-1 (n=127)",
                              "AK & GL at correlated intervals (n=127)", "AK only (n=100)", "GL only (n=28)"))

#figure 4 : different models rmsep:
#function that builds a mean residual dataframe for provided pls model
  create_rmsep_df <- function(test_model){
    name <- as.character(deparse(substitute(test_model)))
    model_rmsep <- RMSEP(test_model)
    rmsep_df <- data.frame(ncomp = 0:test_model$ncomp, RMSEP = model_rmsep$val[1,1,])
    rmsep_df <- round_df(rmsep_df, 2)
    rmsep_df$model <- rep(name,test_model$ncomp+1 )
    return(rmsep_df)
  }

rmsep_df_all <- rbind(create_rmsep_df(akGLpls), create_rmsep_df(greenlandPLS),
                           create_rmsep_df(alaskaPLS),  create_rmsep_df(akGLpls_1050),
                           create_rmsep_df(akGLpls_intervals))

fig4 <- ggplot(rmsep_df_all, aes(x= ncomp, y = RMSEP, color = model)) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = scales::breaks_extended(Q = 1:10)) +
  theme_bw() +
  geom_line(size = 0.5, alpha = 0.2)+
  xlab("Number of Components") +
  ylab("RMSEP (%)") +
  ggtitle("RMSEP") +
  labs(subtitle = "Root Mean Square Error of Prediction")+
  scale_color_brewer(palette = "Greens") +
  scale_color_manual(values=c("#00bfff","#8DD181", "#B8E2B1","#A3C562", "#5A9E4E"),
                     name="Model",
                     labels=c("AK & GL (n= 127)", "AK & GL at 1050-1280 cm^-1 (n=127)",
                              "AK & GL at correlated intervals (n=127)", "AK only (n=100)", "GL only (n=28)"))

fig_3_4 <- ggarrange(fig3, fig4,  ncol=2, nrow=1, common.legend = TRUE, legend="bottom", widths = c(5,5) )
#ggsave(plot = fig_3_4, filename = "fig_3_4.tiff" ,  device='tiff', dpi=700, width = 10)


#fig 5
#x variance  explianed
sum_var <- cumsum(explvar(akGLpls))
ind_var <- explvar(akGLpls)
x_var <- data.frame(sum = sum_var, indiv = ind_var, ncomp = 1:10)

ggplot(x_var, aes(x=ncomp, y= indiv ))+
  geom_col()




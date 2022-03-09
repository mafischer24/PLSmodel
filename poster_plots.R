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
  geom_line(data = WQ, aes(x=as.numeric(wavenumber),
                           y= absorbance, color =  "Washed Qaurtz (No BSi)"))+
  geom_line(data = DB, aes(x=as.numeric(wavenumber),
                           y= absorbance, color = "High BSi and TOC Sample")) +
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
  scale_color_manual( values = c( "Washed Qaurtz (No BSi)" ="black",
                                  "High BSi and TOC Sample"="darkgreen")) +
  scale_fill_manual(values = c( "BSi Range"="orange","TOC Range"="yellow")) +
  theme(legend.position = "bottom",
        legend.text=element_text(size=8), legend.box="vertical", legend.margin=margin())
#ggsave(filename = "~/Desktop/dewet/fig1.tiff" , plot = fig1,  device='tiff', dpi=700)



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

lm_area <- ggplot(area_bsi, aes(x=percent_b_si_based_on_chemical_digestion_method,
                                y = peak_area_1000_1150_cm_1)) +
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
#ggsave(filename = "~/Desktop/dewet/fig2.tiff" , plot = fig2,  device='tiff', dpi=700, width = 10)


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
                              "AK & GL at correlated intervals (n=127)", "AK only (n=100)",
                              "GL only (n=28)"))

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
                              "AK & GL at correlated intervals (n=127)", "AK only (n=100)",
                              "GL only (n=28)"))

fig_3_4 <- ggarrange(fig3, fig4,  ncol=2, nrow=1, common.legend = TRUE,
                     legend="bottom", widths = c(5,5) )
#ggsave(plot = fig_3_4, filename = "~/Desktop/dewet/fig_3_4.tiff" ,  device='tiff', dpi=700, width = 10)


#fig 5
#x variance  explianed
sum_var <- cumsum(explvar(akGLpls))
ind_var <- explvar(akGLpls)
x_var <- data.frame(sum = sum_var, indiv = ind_var, ncomp = 1:10)

fig5 <- ggplot(x_var, aes(x=ncomp, y= indiv, fill = "#00bfff" ))+
  geom_col() +
  geom_step(aes(x=ncomp, y=sum_var, color = "#00bfff"), size =0.8, fill = "#00bfff")+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  theme_bw() +
  xlab("Number of Components") +
  ylab("Explained Predictor Variables Variance (%)") +
  ggtitle("Alaska and Greenland Model Explained Variance") +
  scale_color_manual(values= "#00bfff", name="Line",
                     labels=c("Cumulative Explained Variance")) +
  scale_fill_manual(values= "#00bfff", name="Column",
                     labels=c("Individual Explained Variance")) +
  theme(legend.position = "bottom",legend.text=element_text(size=8))

ggsave(plot = fig5, filename = "~/Desktop/dewet/fig5.tiff" ,  device='tiff', dpi=700, width = 7)

#fig 6
#loading plots for akglpls model
fig_6<-  plot(akGLpls, "loadings", comps = 2:6, ylim = c(-0.25, 0.15),
             legendpos = "bottomleft", y = "Loading Value",
     xlab = expression(paste("Wavenumber ", cm^{-1})),
     sub= "How much does each variable contribute?" )
grid(nx = NULL, ny = NULL,
     lty = 1,      # Grid line type
     col = "lightgray", # Grid line color
     lwd = 1)      # Grid line width
abline(h = 0)
title("Loading Plot for Alaska and Greenland Model")


#or
#re-creating the dataframe to be able to use ggplot
df_loading<- data.frame( `ncomp2`= akGLpls$loading.weights[,2],
                 `ncomp3` = akGLpls$loading.weights[,3],
                 `ncomp4` = akGLpls$loading.weights[,4],
                 `ncomp5` = akGLpls$loading.weights[,5],
                 `ncomp6` = akGLpls$loading.weights[,6])
require(data.table)
setDT(df_loading, keep.rownames = "wavenumber")[]
df_loading$wavenumber <- as.numeric(gsub("`", "", df_loading$wavenumber ))

df_loading_longer<- df_loading %>%
  pivot_longer(cols = contains("ncomp"), names_to = "ncomp", values_to = "loading_weight")

fig6 <- ggplot(df_loading_longer, aes(x= wavenumber, y=loading_weight, color = ncomp))+
  geom_line(aes(size = ncomp)) +
  theme_bw()+
  geom_hline(yintercept = 0, color = "darkred") +
  xlim(4000, 300)+
  #facet_wrap(~ncomp)+
  annotate("rect", xmin=1050, xmax=1280, ymin=-Inf, ymax=Inf, alpha=0.2, fill="orange") +
  annotate("rect", xmin=435, xmax=480, ymin=-Inf, ymax=Inf, alpha=0.2, fill="orange") +
  annotate("rect", xmin=790, xmax=830, ymin=-Inf, ymax=Inf, alpha=0.2, fill="orange") +
  scale_color_manual(values=c("#8DD181", "#00bfff", "#B8E2B1","#A3C562", "#5A9E4E"),
                     name="Number of Components",
                     labels=c("2", "3", "4", "5","6")) +
  scale_size_manual(values = c(0.5,1.5, 0.5, 0.5, 0,5), guide = 'none') +
  ylab("Loading Value") +
  xlab(expression(paste("Wavenumber ", cm^{-1}))) +
  ggtitle("Loading Plot for Alaska and Greenland Model") +
  labs(subtitle = "How much does each variable contribute?")

ggsave(plot = fig6, filename = "~/Desktop/dewet/fig6.tiff" ,  device='tiff', dpi=700, width = 7)

# fig 7 residuals
test_model <- akGLpls
i <- 3
df_used <-  akGL_wetChemAbs
actual_bsi_only <- akGL_wetChemAbs$BSi

predicted_bsi <- as.data.frame(test_model$fitted.values)
#select model with 3 components
predicted_bsi <- as.data.frame(predicted_bsi[,c(i)])
#Rename wet_chem_data columns
names(predicted_bsi)[1] <- "BSiPercent_Predicted"
#Combine actual and predicted BSi wetchem data
BSi <- cbind(row.names(df_used),actual_bsi_only, predicted_bsi)
names(BSi)[1] <- "dataset"
names(BSi)[2] <- "BSiPercent_Actual"
BSi$dataset <- as.character(BSi$dataset)
#reformat into long so we can graph
BSi_Long <- BSi %>%
  dplyr::select(dataset, BSiPercent_Actual, BSiPercent_Predicted)%>%
  gather(key = "variable", value = "value", -dataset)
#Create dataframe with residual error ----
#Calculating Residuals: Difference between actual and predicted
BSi$Difference <- (BSi$BSiPercent_Predicted - BSi$BSiPercent_Actual)
#Table of differences for each sample
Difference <- BSi %>%
  dplyr::select(dataset, Difference)
Difference <- round_df(Difference, 2)

fig7 <-  ggbarplot(Difference %>%
            mutate(highlight_flag = ifelse(Difference >= '0', T, F)),
          x = "dataset", y = "Difference",
          fill = "highlight_flag",           # change fill color by mpg_level
          color = "white",            # Set bar border colors to white
          palette = c( "red", "darkgreen"),            # jco journal color palett. see ?ggpar
          sort.val = "desc",          # Sort the value in descending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          x.text.angle = 90,          # Rotate vertically x axis texts
          ylab = "Differences in Precentage",
          xlab = "Samples",
          legend.title = "Overfitting",
          rotate = FALSE,
          ggtheme = theme_minimal(),
          title = "Residuals for Alaska and Greenland PLS Model",
          subtitle= paste0("For each sample with ", i, " components and mean residual of  ",
                           mean_residuals_df$mean_residuals[i], " cm^1") )+
  rremove("x.text")

ggsave(plot = fig7, filename = "~/Desktop/dewet/fig7.tiff" ,  device='tiff', dpi=700, width = 7)


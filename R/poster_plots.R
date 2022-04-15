library(magrittr)
library(ggplot2)
library(Metrics)
library(pls)
library(tidyr)
library(ggpubr)
library(janitor)
library(gbm)
library(purrr)
set.seed(1)

source('R/compiled_data_load.R')
#fig 1
#quartz vs diatoms vs lake sample spectra ( what kind of data are we looking at? )
# what is ss?  sea sand

WQ <- greenland_df[c("WQ.0"),] %>%
  select(-BSi) %>%
  pivot_longer(everything(), names_to = "wavenumber", values_to = "absorbance") %>%
  mutate(wavenumber = as.numeric(wavenumber))


DB <- read.table("DB 7 [2].0 (1).dpt", sep = " ") %>%
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
# intervals <- c(368, 3750, 435, 480, 790, 830,1050, 1280)
# WQ %>% filter_all(any_vars(. %in% intervals))
# which(WQ == contains(intervals))

fig1 <- ggplot() +
  geom_line(data = WQ, aes(x=as.numeric(wavenumber),
                           y= absorbance, color =  "Washed Qaurtz (No BSi)"))+
  geom_line(data = DB, aes(x=as.numeric(wavenumber),
                           y= absorbance, color = "High BSi and TOC Sample")) +
  xlim(4000, 300)+
  ylab("Absorbance") +
  xlab(expression(paste("Wavenumber ", cm^{-1}))) +
  geom_rect(inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin= - Inf, ymax=Inf, fill= "BSi Range"),
            color="transparent", alpha=0.3) +
  geom_rect(inherit.aes=FALSE, aes(xmin=2800, xmax=3000, ymin= - Inf, ymax=Inf, fill= "TOC Range"),
            color="transparent", alpha=0.3) +
  ggtitle("Example FTIRS Spectra") +
  labs(colour="Sample",
       fill = "Correlated Peaks") +
  theme_bw()+
  scale_color_manual( values = c( "Washed Qaurtz With No BSi" ="black",
                                  "Diatoms With High BSi and TOC"="darkgreen")) +
  scale_fill_manual(values = c( "BSi Range"="orange","TOC Range"="yellow")) +
  theme(legend.position = "bottom",
        legend.text=element_text(size=8), legend.box="vertical", legend.margin=margin())
#ggsave(filename = "~/Desktop/dewet/fig1.tiff" , plot = fig1,  device='tiff', dpi=700)


# #fig 2
# # fitting of acutal data with pls prediction vs linear prediction (why pls)
#
# #prepare data
# predicted_bsi <- as.data.frame(akGLpls$fitted.values)
# #select model with 3 components
# predicted_bsi <- as.data.frame(predicted_bsi[,c(4)])
# #Rename wet_chem_data columns
# names(predicted_bsi)[1] <- "BSiPercent_Predicted"
# pls_bsi <- data.frame(wet_chem =akGL_wetChemAbs$BSi, pls = unlist(predicted_bsi))
# #selected 15 random samples to be have the same number of observations as the area one
# #pls_bsi <- pls_bsi[sample(nrow(pls_bsi), 15), ]
# ## linear regression model:
# pls_lm <- lm(pls ~ wet_chem, pls_bsi)
# pls_bsi_r <- summary(pls_lm)$r.squared
#
# lm_pls <- ggplot(pls_bsi, aes(x=wet_chem, y = pls)) +
#   geom_point(size = 4, color = "lightBlue", alpha = 0.6)+
#   geom_smooth(method = "lm", se = FALSE, color = "darkBlue") +
#   geom_text(x = 8, y = 24, label = paste("R^2: ", round_df(pls_bsi_r, 3)))+
#   xlab("Wet Chemical BSi Percent (%)") +
#   ylab("PLS Predicted BSi Percent (%)")+
#   ggtitle("Predeicted Fitness Using PLS Model")+
#   theme_bw()
#
# # area under peak ( most reported is only the largest peak)
# ftirs_peak_areas <- read_csv("~/Downloads/FTIRS Peak Areas for Greenland Calibration Samples - Sheet1.csv") %>%
#   clean_names()
#
# calibration_wetchem <- read_csv("~/Downloads/Wet Chemistry Calibration Data - Sheet1.csv") %>%
#   clean_names()
#
# area_bsi <- ftirs_peak_areas %>%
#   inner_join(calibration_wetchem,  "sample_id")
#
# ## linear regression model:
# area_lm <- lm(peak_area_1000_1150_cm_1 ~ percent_b_si_based_on_chemical_digestion_method , area_bsi)
# area_bsi_r <- summary(area_lm)$r.squared
#
# lm_area <- ggplot(area_bsi, aes(x=percent_b_si_based_on_chemical_digestion_method,
#                                 y = peak_area_1000_1150_cm_1)) +
#   geom_point(size = 4, color= "#FFD580", alpha = 0.7)+
#   geom_smooth(method = "lm", se = FALSE, color = "darkRed") +
#   geom_text(x = 8, y = 14, label = paste("R^2: ",round_df(area_bsi_r, 3)))+
#   xlab("Wet Chemical BSi Percent (%)") +
#   ylab(expression(paste("1050 - 1150 ", cm^{-1}, " Peak Area Estimated")))+
#   ggtitle("Predeicted Fitness Using Peak Areas") +
#   theme_bw()
#
# fig2 <- grid.arrange(lm_area, lm_pls, nrow = 1,
#                      top = "Comparison Between traditional and PLS Model Methods in Estimating BSi Content")
# ggsave(filename = "~/Desktop/dewet/fig2.tiff" , plot = fig2,  device='tiff', dpi=700, width = 10)
#
#
# #figure 3 : differnt models mean residuals:
# #function that builds a mean residual dataframe for provided pls model
# mean_res_df <- function(test_model){
#   name <- as.character(deparse(substitute(test_model)))
#   print(name)
#   model_res <- residuals(test_model)
#   mean_residuals_df <- data.frame(ncomp = 1:test_model$ncomp, mean_residuals = 1:test_model$ncomp)
#   print(mean_residuals_df)
#   for(i in 1:test_model$ncomp){
#     mean_residuals_df$mean_residuals[i] <- mean(abs(model_res[,1,i]))
#   }
#   mean_residuals_df <- round_df(mean_residuals_df, 2)
#   mean_residuals_df$model <- rep(name,test_model$ncomp )
#   return(mean_residuals_df)
# }
#
# mean_residuals_all <- rbind(mean_res_df(akGLpls), mean_res_df(greenlandPLS),
#                             mean_res_df(alaskaPLS),  mean_res_df(akGLpls_1050),
#                             mean_res_df(akGLpls_intervals))
#
# fig3 <- ggplot(mean_residuals_all, aes(x= ncomp, y = mean_residuals, color = model)) +
#   geom_point(size = 2.5) +
#   scale_x_continuous(breaks = scales::breaks_extended(Q = 1:10)) +
#   theme_bw() +
#   geom_line(size = 0.5, alpha = 0.2)+
#   xlab("Number of Components") +
#   ylab("Mean Residuals (%)") +
#   ggtitle("Mean Residuals") +
#   scale_color_brewer(palette = "Greens") +
#   scale_color_manual(values=c("#00bfff","#8DD181", "#B8E2B1","#A3C562", "#5A9E4E"),
#                      name="Model",
#                      labels=c("AK & GL (n= 127)", "AK & GL at 1050-1280 cm^-1 (n=127)",
#                               "AK & GL at correlated intervals (n=127)", "AK only (n=100)",
#                               "GL only (n=28)"))
#
# #figure 4 : different models rmsep:
# #function that builds a mean residual dataframe for provided pls model
#   create_rmsep_df <- function(test_model){
#     name <- as.character(deparse(substitute(test_model)))
#     model_rmsep <- RMSEP(test_model)
#     rmsep_df <- data.frame(ncomp = 0:test_model$ncomp, RMSEP = model_rmsep$val[1,1,])
#     rmsep_df <- round_df(rmsep_df, 2)
#     rmsep_df$model <- rep(name,test_model$ncomp+1 )
#     return(rmsep_df)
#   }
#
# rmsep_df_all <- rbind(create_rmsep_df(akGLpls), create_rmsep_df(greenlandPLS),
#                            create_rmsep_df(alaskaPLS),  create_rmsep_df(akGLpls_1050),
#                            create_rmsep_df(akGLpls_intervals))
# rmsep_df_all<-rmsep_df_all%>%mutate(best = case_when(
#   model=="akGLpls" & ncomp==4 ~ TRUE,
#   model=="greenlandPLS" & ncomp==1 ~ TRUE,
#   model=="akGLpls_1050" & ncomp==4 ~ TRUE,
#   model=="akGLpls_intervals" & ncomp==3 ~ TRUE,
#   model=="alaskaPLS" & ncomp==4 ~ TRUE,
#   TRUE ~ FALSE
# ))
#
# fig4 <- ggplot(rmsep_df_all, aes(x= ncomp, y = RMSEP, color = model, shape = best)) +
#   geom_point(size = 2.5 ) +
#   scale_x_continuous(breaks = scales::breaks_extended(Q = 1:10)) +
#   theme_bw() +
#   geom_line(size = 0.5, alpha = 0.2)+
#   xlab("Number of Components") +
#   ylab("RMSEP (%)") +
#   ggtitle("Root Mean Square Error of Prediction (RMSEP)") +
#   labs(subtitle = "The better model will have a low RMSEP",
#        caption = "AK: Alaska, GL: GreenLand")+
#   scale_shape_discrete(breaks = c(T), name =  "Best number of Components", labels = " ")+
#   scale_color_manual(values=c("#00bfff","#6ADA62", "#B8E2B1","#7A9678", "#5A9E4E"),
#                      name="Model",
#                      labels=c("AK & GL (n= 127)", "AK & GL at 1050-1280 cm^-1 (n=127)",
#                               "AK & GL at correlated intervals (n=127)", "AK only (n=100)",
#                               "GL only (n=28)")) +
#   theme(legend.position = "bottom", legend.title = element_text(size=12),
#         legend.text=element_text(size=12), legend.box="vertical", legend.margin=margin())+
#   guides(color = guide_legend(order = 1,nrow=2, byrow=TRUE), shape = guide_legend(order = 2))
#
# ggsave(plot = fig4, filename = "~/Desktop/dewet/fig4.tiff" ,  device='tiff', dpi=700, width = 10)
#
#
# fig_3_4 <- ggarrange(fig3, fig4,  ncol=2, nrow=1, common.legend = TRUE,
#                      legend="bottom", widths = c(5,5) )
#
#
# #fig 5
# #x variance  explianed
# x_sum_var <- cumsum(explvar(akGLpls))
# sum_var <- 100 * drop(R2(akGLpls, estimate = "train", intercept = FALSE)$val)
# ind_var <- explvar(akGLpls)
# x_var <- data.frame(sum = sum_var, indiv = ind_var, ncomp = 1:10)
#
# fig5 <- ggplot(x_var, aes(x=ncomp, y= indiv,  fill = "#00bfff" ))+
#   geom_col() +
#   geom_step(aes(x=ncomp, y=sum_var, color = "#DC143C"), size =0.8)+
#   scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10)) +
#   theme_bw() +
#   xlab("Number of Components") +
#   ylab("Explained Variance (%)") +
#   ggtitle("Alaska and Greenland Model Explained Variance") +
#   scale_color_manual(values= "#DC143C", name="y Variable",
#                      labels=c("Cumulative explained variance for BSi")) +
#   scale_fill_manual(values= "#00bfff", name="x Variables",
#                      labels=c("Individual contribution for wavenumber variance")) +
#   theme(legend.position = "bottom",legend.text=element_text(size=8),
#         legend.title = element_text(size=8), legend.box="vertical")
#
# ggsave(plot = fig5, filename = "~/Desktop/dewet/fig5.tiff" ,  device='tiff', dpi=700, width = 7)
#
# #fig 6
# #loading plots for akglpls model
# fig_6<-  plot(akGLpls, "loadings", comps = 2:6, ylim = c(-0.25, 0.15),
#              legendpos = "bottomleft", y = "Loading Value",
#      xlab = expression(paste("Wavenumber ", cm^{-1})),
#      sub= "How much does each variable contribute?" )
# grid(nx = NULL, ny = NULL,
#      lty = 1,      # Grid line type
#      col = "lightgray", # Grid line color
#      lwd = 1)      # Grid line width
# abline(h = 0)
# title("Loading Plot for Alaska and Greenland Model")
#
#
# #or
# #re-creating the dataframe to be able to use ggplot
# df_loading<- data.frame(`ncomp_1`= akGLpls$loading.weights[,1],
#                         `ncomp_2`= akGLpls$loading.weights[,2],
#                         `ncomp_3` = akGLpls$loading.weights[,3],
#                         `ncomp_4` = akGLpls$loading.weights[,4],
#                         `ncomp_5` = akGLpls$loading.weights[,5],
#                         `ncomp_6` = akGLpls$loading.weights[,6])
# require(data.table)
# setDT(df_loading, keep.rownames = "wavenumber")[]
# df_loading$wavenumber <- as.numeric(gsub("`", "", df_loading$wavenumber ))
#
# df_loading_longer<- df_loading %>%
#   pivot_longer(cols = contains("ncomp"), names_to = "ncomp", values_to = "loading_weight")%>%
#   separate(col = ncomp, into = c("comp", "ncomp"), sep= "_") %>%
#   select(-comp) %>% mutate(ncomp = as.factor(ncomp))
#
# fig6 <- ggplot(df_loading_longer, aes(x= wavenumber, y=loading_weight, color = ncomp))+
#   geom_line( alpha = 0.8, size= 0.8) +
#   theme_bw()+
#   geom_hline(yintercept = 0, color = "darkred") +
#   xlim(4000, 300)+
#   #facet_wrap(~ncomp)+
#   annotate("rect", xmin=1050, xmax=1280, ymin=-Inf, ymax=Inf, alpha=0.2, fill="orange") +
#   annotate("rect", xmin=435, xmax=480, ymin=-Inf, ymax=Inf, alpha=0.2, fill="orange") +
#   annotate("rect", xmin=790, xmax=830, ymin=-Inf, ymax=Inf, alpha=0.2, fill="orange") +
#   scale_color_brewer(palette = "Spectral",
#   name="Number of Components",
#                      labels=c("1", "2", "3", "4", "5", "6")) +
#   ylab("Loading Value") +
#   xlab(expression(paste("Wavenumber ", cm^{-1}))) +
#   ggtitle("Loading Plot for Alaska and Greenland Model") +
#   labs(subtitle = "How much does each variable contribute?")+
#   theme(legend.position = "bottom",legend.text=element_text(size=8))
#
# ggsave(plot = fig6, filename = "~/Desktop/dewet/fig6.tiff" ,  device='tiff', dpi=700, width = 7)
#
# # fig 7 residuals
# test_model <- akGLpls
# i <- 4
# df_used <-  akGL_wetChemAbs
# actual_bsi_only <- akGL_wetChemAbs$BSi
#
# predicted_bsi <- as.data.frame(test_model$fitted.values)
# #select model with 3 components
# predicted_bsi <- as.data.frame(predicted_bsi[,c(i)])
# #Rename wet_chem_data columns
# names(predicted_bsi)[1] <- "BSiPercent_Predicted"
# #Combine actual and predicted BSi wetchem data
# BSi <- cbind(row.names(df_used),actual_bsi_only, predicted_bsi)
# names(BSi)[1] <- "dataset"
# names(BSi)[2] <- "BSiPercent_Actual"
# BSi$dataset <- as.character(BSi$dataset)
# #reformat into long so we can graph
# BSi_Long <- BSi %>%
#   dplyr::select(dataset, BSiPercent_Actual, BSiPercent_Predicted)%>%
#   gather(key = "variable", value = "value", -dataset)
# #Create dataframe with residual error ----
# #Calculating Residuals: Difference between actual and predicted
# BSi$Difference <- (BSi$BSiPercent_Predicted - BSi$BSiPercent_Actual)
# #Table of differences for each sample
# Difference <- BSi %>%
#   dplyr::select(dataset, Difference)
# Difference <- round_df(Difference, 2)
#
# fig7 <-  ggbarplot(Difference %>%
#             mutate(highlight_flag = ifelse(Difference >= '0', T, F)),
#           x = "dataset", y = "Difference",
#           fill = "highlight_flag",           # change fill color by mpg_level
#           color = "white",            # Set bar border colors to white
#           palette = c( "red", "darkgreen"),            # jco journal color palett. see ?ggpar
#           sort.val = "desc",          # Sort the value in descending order
#           sort.by.groups = FALSE,     # Don't sort inside each group
#           x.text.angle = 90,          # Rotate vertically x axis texts
#           ylab = "Differences in Precentage",
#           xlab = "Samples",
#           legend.title = "Overfitting",
#           rotate = FALSE,
#           ggtheme = theme_minimal(),
#           title = "Residuals for Alaska and Greenland PLS Model",
#           subtitle= paste0("For each sample with ", i, " components and mean residual of ",
#                            mean_residuals_df$mean_residuals[i], "%") ) + rremove("x.text") +
#   geom_hline(yintercept =mean_residuals_df$mean_residuals[i], color = "#00bfff")+
#   geom_hline(yintercept = -mean_residuals_df$mean_residuals[i], color = "#00bfff")
#
# ggsave(plot = fig7, filename = "~/Desktop/dewet/fig7.tiff" ,  device='tiff', dpi=700, width = 7)
#
# #fig 8
# tiff(file="~/Desktop/dewet/fig8.tiff")
# plot(test_model, ncomp =3, asp = 1, line = TRUE, col = "#00bfff")
# dev.off()
#
# akGLpls$call
#
# # Figure drafts
#
# f1 <- ggplot(rmsep_df_all, aes(x= ncomp, y = RMSEP)) +
#   geom_col()+
#   facet_wrap(~model, nrow  = 1) +
#   theme_bw() +
#   scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10))
#
# f2 <- ggplot(mean_residuals_all, aes(x= ncomp, y = mean_residuals)) +
#   geom_col()+
#   facet_wrap(~model, nrow  = 1) +
#   theme_bw() +
#   scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10))
#
# ggarrange(f1, f2,  ncol=1, nrow=2, common.legend = TRUE,
#                      legend="bottom", widths = c(5,5) )
# 100 * drop(R2(akGLpls, estimate = "train", intercept = FALSE)$val)
#
# 100 * drop(R2(akGLpls, estimate = "all", intercept = FALSE)$val)
# akGLpls$Xvar
# summary(akGLpls)
#
# # figue 9
# # pull out the best ncomp for each model:
# models<- list(greenlandPLS, alaskaPLS, akGLpls, akGLpls_1050, akGLpls_intervals)
#
# model_names <- c("greenlandPLS", "alaskaPLS", "akGLpls", "akGLpls_1050", "akGLpls_intervals")
#
# best_ncomp <- map(models, selectNcomp, method = "onesigma")%>%  # output a list
#   map_dbl(mean)
#
# models_r2 <- map(models, R2,estimate = "train", intercept = FALSE)
#
# models_best <- data.frame(model = model_names, best_ncomp = best_ncomp)
#
# for(i in 1:5){
#   best_n <- as.numeric(models_best[i,2])
#   model_res <- residuals(models[[i]])
#   models_best$mean_residuals[i] <- mean(abs(model_res[,1,best_n]))
#
#   model_rmsep <- RMSEP(models[[i]])
#   models_best$RMSEP[i] <- model_rmsep$val[1,1,best_n+1]
#
#   models_best$R2[i] <- models_r2[[i]][1]$val[1,1,best_n]
# }
#
# models_best_long <- models_best %>%
#   pivot_longer(cols = c(3,4,5), names_to = "measure", values_to = "value")
# models_best_long$measure <- factor(models_best_long$measure, levels = c("RMSEP", "R2", "mean_residuals"))
#
# fig9 <- ggplot(models_best_long, aes(x= model, y = value, fill = model)) +
#   geom_col(position='dodge',aes(size = model, colour="red") ) +
#   facet_wrap(~measure, ncol = 1, scales = "free",
#              labeller = labeller(measure =c(mean_residuals ="Mean Absulote Residuals (BSi %)",
#                                             R2 ="R^2",RMSEP = "RMSEP (BSi %)"))) +
#   theme_bw() +
#   scale_fill_manual(values=c("#00bfff","#6ADA62", "#B8E2B1","#7A9678", "#5A9E4E"),
#                      name="Model",
#                      labels=c("AK & GL", bquote("AK & GL at 1050-1280"~cm^-1),
#                               "AK & GL at correlated intervals", "AK only ",
#                               "GL only")) +
#   theme( axis.text.x=element_blank())+
#   geom_text(aes(label = round_df(value,2)), vjust =1.2, size =3.5)+
#   ggtitle("Each Model Performance") +
#   xlab("Model")+ ylab("Value") + labs(subtitle = "Using the `best` number of components")+
#   theme(legend.position = "right") +
#   scale_size_manual(values = c(1, 0, 0, 0, 0), guide = "none")+
#   scale_color_manual(values = c("red", "black", "black", "red", "black"),guide = "none")
#
# ggsave(plot = fig9, filename = "~/Desktop/dewet/fig9.tiff" ,  device='tiff', dpi=700, width = 7, height = 7 )
#
#
# #figure 10
# #predicting Greenland samples with AK model
# gl_predections <- predict(alaskaPLS, wetChemAbsorbance%>%select(-1), ncomp = 4)
# gl_predections_df <- data.frame(predicted = gl_predections, measured = wetChemAbsorbance$BSiPercent)
#
# gl_lm <- lm(BSiPercent.4.comps ~ measured, gl_predections_df)
# gl_bsi_r <- summary(gl_lm)$r.squared
#
# fig10 <- ggplot(gl_predections_df, aes(x=measured, y = BSiPercent.4.comps)) +
#   geom_point(size = 4, color = "#7A9678", alpha = 0.7)+
#   geom_abline(slope = coef(pls_lm)[["wet_chem"]],
#               intercept = coef(pls_lm)[["(Intercept)"]], color = "darkGreen")+
#   geom_text(x = 3, y = 28, label = paste("R^2: ", round_df(gl_bsi_r, 3)))+
#   ylim(0,30)+
#   xlab("Measured Wet Chemical BSi Percent (%)") +
#   ylab("Predicted BSi Percent (%)")+
#   ggtitle("Predeicted Fitness Using AK Only Model")+
#   theme_bw()+
#   labs(subtitle = "Feeding GreenLand samples to Alaska only model restrict Greenland predections to one band")
#
# ggsave(plot = fig10, filename = "~/Desktop/dewet/fig10.tiff" ,  device='tiff', dpi=700, width = 7)
#
# ## double checking the values
# for(i in 1:28){
#   print(paste("BSii from wetChemAbsorbance df: ", wetChemAbsorbance[i,1]))
#   print(paste("prediction using AK only: ",
#               predict(alaskaPLS, wetChemAbsorbance[i,]%>%select(-1), ncomp=4)[1]))
#   print(paste("calibration df output", wet_chem_data[i,] ))
#   print("___________________________________")
# }

#figure 11
region <- c(rep("GL", 28), rep("AK", 103))

all_data <- gl_ak_combined_df %>%
  mutate(region = region) %>%
  rownames_to_column(var = "sample") %>%
  pivot_longer(cols = 3:1883, names_to = "Wavenumber", values_to = "Absorbance") %>%
  mutate(Wavenumber = as.numeric(Wavenumber))
all_data <- as.data.frame(all_data)

fig11 <- ggplot(all_data, aes(x = Wavenumber, y= Absorbance, color = BSi, group = sample), alpha = 0.8) +
  geom_line()+
  facet_wrap(~region) +
  scale_color_continuous(type = "viridis")+
  theme_bw()

#fig 12
pre_interp <- read_csv("Samples/greenland_csv/FISK-10.0.csv") %>%
  select(-1) %>%
  mutate(status = "Before")

post_interp <- greenland_df[c("FISK-10.0"),] %>%
  select(-BSi) %>%
  pivot_longer(everything(), names_to = "wavenumber", values_to = "absorbance") %>%
  mutate(wavenumber = as.numeric(wavenumber), status = "After")

fisk <- rbind(pre_interp, post_interp)

fig12 <- ggplot(fisk, aes(x = wavenumber, y = absorbance))+
  geom_point() +
  facet_wrap(~status)+
  ylab("Absorbance") +
  xlab(expression(paste("Wavenumber ", cm^{-1}))) +
  annotate(geom = "rect", xmin=4000, xmax=Inf, ymin= - Inf, ymax=Inf,
           fill = "red", alpha = 0.1)+
   ggtitle("Example Sample Spectrum")




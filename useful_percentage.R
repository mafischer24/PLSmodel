library(dplyr)
library(Metrics)
library(pls)
library(ggplot2)
#testing how many samples are suffecient for good model prediction

#function to create data frame giving the model to output, percentage used, RMSEP and number of components
create_rmsep_df <- function(test_model, n){
  #extracting RMSEP
  model_rmsep <- RMSEP(test_model)
  #create data frame with 10 componenets
  rmsep_df <- data.frame(ncomp = 1:test_model$ncomp, RMSEP = model_rmsep$val[1,1,2:11])
  rmsep_df <- round_df(rmsep_df, 2)
  rmsep_df$percent <- rep(n)
  return(rmsep_df)
}

gl_ak_df <- na.omit(gl_ak_combined_df)
#building base df for rmsep
df = data.frame()

#testing deffiernt fraction of the data from 10% to 100%

for(j in 1:10){
  percent  <- j*10/100
  #splitting the data into training and testing
  training <- gl_ak_df %>%
    sample_frac(percent)

  testing <- gl_ak_df %>%
    setdiff(training)

  model <- plsr(BSi~., ncomp = 10, data=training, validation = "CV", segments = 10)

  rmsep_df <- create_rmsep_df(model,j*10)

  df <- rbind(df, rmsep_df)
}

#calculating mse and mae (Mean absolute deviation)
######
# need to add another loop but not sure how??
df = data.frame()
k= 10
mse_list <- c()
mae_list <- c()
percent_list <- c()
for(j in 1:k ){
  for(i in 1:10) { # rpeat the the calculation randomly 10 times
    # if you want to repeat more than 10 time then spereate this line in a differnt loop
      percent  <- i*10/100
      training <- gl_ak_df %>%
        sample_frac(percent)

      testing <- gl_ak_df %>%
        setdiff(training)

      model <- plsr(BSi~., ncomp = 10, data=training, validation = "CV", segments = 10)

      #predict using the testing data unless we are using 100% of the data
      if(percent == 1){ testing <- training }

      predictions <- as.data.frame(predict(model, testing%>%select(-1)))

      for(n in 1:length(predictions)){
          mse_list <- append(mse_list,mse(training$BSi,predictions[,n]))
          mse_list <- append(mse_list,mse(testing$BSi,predictions[,n]))
          }
      percent_list <- append(percent_list, percent)
  }
  cbind(df, mse_list,mae_list)
  #Rename column name
  #colnames(df)[ncol(df)-1] <- paste0("mse", j)
  #colnames(df)[ncol(df)] <- paste0("mae", j)
  }


    #   #create vector of mse based on predictions
    #   mse_list <- c()
    #   #popu;ate the vecotor
    #   for(n in 1:length(predictions)){
    #     if(percent == 1){
    #       mse_list <- append(mse_list,mse(training$BSi,predictions[,n]))
    #       } else {
    #         mse_list <- append(mse_list,mse(testing$BSi,predictions[,n]))
    #         }
    #   }
    #   #same thing for mae
    #   mae_list <- c()
    #
    #   for(n in 1:length(predictions)){
    #     if(percent == 1){
    #       mae_list <- append(mae_list,mae(training$BSi,predictions[,n]))
    #     } else {
    #       mae_list <- append(mae_list,mae(testing$BSi,predictions[,n]))
    #     }
    #   }
    # #Mean absolute deviation
    # df[ , ncol(df) + 1] <- mse_list
    # df[ , ncol(df) + 1] <- mae_list
    # #Rename column name
    # colnames(df)[ncol(df)-1] <- paste0("mse", i)
    # colnames(df)[ncol(df)] <- paste0("mae", i)
 # }

#calculate the mean of the mse and mae columns
df_frac<- df %>%
  rowwise() %>%
  mutate(mean_mse= sum(across(contains("mse"))/10, na.rm = T),
         mean_mae= sum(across(contains("mae"))/10, na.rm = T)) %>%
  select(ncomp, percent,RMSEP, mean_mse, mean_mae) %>%
  pivot_longer(cols = 3:5, names_to = "measure", values_to = "value")

ggplot(df_frac , aes(x = ncomp, y = value, color = as.factor(percent))) +
  geom_line() +
  facet_wrap(~measure,scales = "free")
## values seem to be too similar whicj is odd so I think that that the each
# instance for calculation is done on th esame data rather another random data subset

ggplot(df_frac%>% filter(measure=="mean_mae"), aes(x = ncomp, y = value, color = as.factor(percent))) +
  geom_line() +
  geom_jitter()
#basically same line


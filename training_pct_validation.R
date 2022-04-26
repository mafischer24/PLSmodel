library(tidyverse)
library(Metrics)
library(pls)
suppressWarnings({source("R/compiled_data_load.R")})

gl_ak_df <- gl_ak_combined_df %>%
  filter(!is.na(BSi))
rm(gl_ak_combined_df)

# Model Blank
combined_PLS <- plsr(BSi~., ncomp=10, data = combined_df, validation = "CV", segments = 10)
predictions_comb <- as.data.frame(predict(combined_PLS, combined_df%>%select(-1)))$`BSi.10 comps`

### Storage initialization

# Three dimensional array: Iteration, Percent, and Components mse
full_list <- array(dim = c(10,10,10))
full_list_t <- array(dim = c(10,10,10))

# For each iteration
for (i in 1:10) {
  # For each percentage
  for(perc in 1:10) {
    percent <- .10*perc
    if(percent < 1) {
      training <- gl_ak_df %>%
        sample_frac(percent)

      testing <- gl_ak_df %>%
        setdiff(training)
    } else {
      training <- gl_ak_df
      testing <- gl_ak_df
    }
    model <- plsr(BSi~., ncomp = 10, data = training, validation = "CV", segments = 10)
    predictions <- as.data.frame(predict(model, testing%>%select(-1)))
    predictions_t <- as.data.frame(predict(model, training %>%select(-1)))
    # For each n component in the model
    for(n in 1:10) {
      # Calculate MSE against the test set
      rmse <- sqrt(mse(as.numeric(predictions[,n]), testing$BSi))
      test_rmse <- sqrt(mse(as.numeric(predictions_t[,n]), training$BSi))

      #add it to its spot
      full_list[i,perc,n] <- rmse
      full_list_t[i,perc,n] <- test_rmse
    }
  }
  # Progress indicator so I don't go insane
  cat(paste("..", i*10, "% .." , sep = ''))
}

# Now, for calculations! We want to end up with a 2d data frame, with column being a percentage and each row being a number of comps
vecs_list <- array(dim = c(10,10))
vecs_list_t <- array(dim = c(10,10))
# For each percentage
for(i in 1:10){
  vecs_list[i,] <- colMeans(full_list[,i,])
  vecs_list_t[i,] <- colMeans(full_list_t[,i,])
}

mses_by_perc <- as.data.frame(vecs_list) %>%
  cbind.data.frame(1:10) %>%
  mutate(subset = "Testing")

mses_by_perc_training <- as.data.frame(vecs_list_t) %>%
  cbind.data.frame(1:10) %>%
  mutate(subset = "Training")

mses_by_perc_all <- rbind(mses_by_perc, mses_by_perc_training)

colnames(mses_by_perc_all) <- c("1","2",'3','4','5','6',
                            '7','8','9','10','percent', 'subset')

mses_by_perc_all <- mses_by_perc_all %>%
  select(percent, everything()) %>%
  filter(percent != 10)

plot_df <- mses_by_perc_all %>%
  pivot_longer(cols = 2:11, names_to = "ncomps") %>%
  mutate(percent = 10 * percent)

rmsemp_comp <- ggplot(plot_df, aes(x = as.numeric(ncomps), y = value, color = as.factor(percent))) +
  geom_line(aes(linetype=subset)) +
  scale_color_brewer(palette = "RdYlBu") + theme_bw() +
  xlab("Number of Components") +
  ylab("RMSE Value") +
  ggtitle("Root Mean Squared Error by Number of Components") +
  labs(color = "Training Data %") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10))+
  scale_color_viridis_d()

rmsemp_comp

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


# For each iteration
for (i in 1:10) {
  # For each percentage
  for(perc in 1:10) {
    percent <- .10*perc
    if(percent != 1) {
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
    # For each n component in the model
    for(n in 1:10) {
      # Calculate MSE against the test set
      rmse <- sqrt(mse(as.numeric(predictions[,n]), testing$BSi))

      #add it to its spot
      full_list[i,perc,n] <- rmse
    }
  }
  # Progress indicator so I don't go insane
  cat(paste("..", i*10, "% .." , sep = ''))
}

# Now, for calculations! We want to end up with a 2d data frame, with column being a percentage and each row being a number of comps
vecs_list <- array(dim = c(10,10))
# For each percentage
for(i in 1:10){
  vecs_list[i,] <- colMeans(full_list[,i,])
}

mses_by_perc <- as.data.frame(vecs_list) %>%
  cbind.data.frame(1:10)
colnames(mses_by_perc) <- c("p_010","p_020",'p_030','p_040','p_050','p_060','p_070','p_080','p_090','p_100','ncomps')

plot_df <- mses_by_perc %>%
  pivot_longer(cols = p_010:p_100, names_to = "percent") %>%
  mutate(percent = case_when(percent == "p_010" ~ "10%",
                             percent == "p_020" ~ "20%",
                             percent == "p_030" ~ "30%",
                             percent == "p_040" ~ "40%",
                             percent == "p_050" ~ "50%",
                             percent == "p_060" ~ "60%",
                             percent == "p_070" ~ "70%",
                             percent == "p_080" ~ "80%",
                             percent == "p_090" ~ "90%",
                             percent == "p_100" ~ "Full",))

ggplot(plot_df, aes(x = ncomps, y = value, color = percent)) + geom_line() +
  scale_color_brewer(palette = "RdYlBu") + theme_bw() +
  xlab("Number of Components") +
  ylab("RMSE Value") +
  ggtitle("Root Mean Squared Error by Number of Components") +
  labs(color = "Training Data %")


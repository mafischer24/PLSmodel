library(dplyr)

#testing how many samples are suffecient for good model prediction
create_rmsep_df <- function(test_model, n){
  model_rmsep <- RMSEP(test_model)
  rmsep_df <- data.frame(ncomp = 0:test_model$ncomp, RMSEP = model_rmsep$val[1,1,])
  rmsep_df <- round_df(rmsep_df, 2)
  rmsep_df$percent <- rep(n)
  return(rmsep_df)
}

set.seed(1)
df = data.frame()

for(i in 1:10){
  percent  <- i*10/100
  training <- gl_ak_combined_df %>%
    sample_frac(percent)

  testing <- gl_ak_combined_df %>%
    setdiff(training)

  model <- plsr(BSi~., ncomp = 10, data=training, validation = "CV", segments = 10)

  rmsep_df <- create_rmsep_df(model,i*10)

  df = rbind(df, rmsep_df)
}


ggplot(df, aes(x = ncomp, y = RMSEP, color = as.factor(percent))) +
  geom_line()

## Making model to predict TOC with Alaska data
## To be integrated with other model script eventually

library(pls)
library(tidyverse)

# Generate dataframe by importing data load script
source('R/compiled_data_load.R')

alaska_df_toc <- na.omit(generate_alaska()) %>%
  rownames_to_column(var = "sample")

# Think this could be just in the compiled_data_load.R, all we
# have to do is not delete the toc column
# That will be a lot cleaner, but this is good for the sake of being
# separate for now
alaska_wet_chem_toc <- read_csv("Maxwell-Alaska Samples  - Final Top 100.csv") %>%
  janitor::clean_names() %>%
  select(-notes, -b_si_percent)

names(alaska_wet_chem_toc)[2] <- "toc"



alaska_df_toc <- inner_join(alaska_df_toc, alaska_wet_chem_toc )


alaska_df <- alaska_df %>%
  select(toc, everything())


# Build pls model to predict TOC instead of BSi
alaska_toc_mod <- plsr(toc~., ncomp = 10, data = alaska_df, validation = "CV", segments = 10)
summary(alaska_toc_mod)

#### Greenland TOC template

# Need the proper greenland samples

# source("R/interpolation.R")
# greenland_df <- interpolate_greenland()

# Integrate the wet chem greenland TOC files
wet_chem_toc <- read_csv("csvFiles/wet_chem_toc.csv")

# Add TOC values to dataset
greenland_df$toc <- wet_chem_toc$toc

# put TOC first
greenland_df <- greenland_df %>%
  select(toc, everything())

# Build pls model to predict TOC instead of BSi
greenland_toc_mod <- plsr(toc~., ncomp = 10, data = greenland_df, validation = "CV", segments = 10)
summary(greenland_toc_mod)


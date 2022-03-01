library(tidyverse)


# Wavenumber: the wavenumber vector alone
# Absorbance: the absorbance vector alone
# Out_vec: the wavenumber vector to be interpolated onto - in this case using the Alaska Wavenumbers

interpolate <- function(wavenumber, absorbance, out_vec) {

  # The meat of the function: returns both the interpolated absorbance vector and the wavenumber vec.
  tuple <- approx(as.numeric(wavenumber), as.numeric(absorbance), xout = out_vec)

  # Binds the two vectors back into a data frame
  df <- as.data.frame(tuple)

  # Giving the data frame useful names before passing it back
  df <- df %>%
    rename(wavenumber = x, absorbance = y)
}


interpolate_greenland <- function(x) {
  #reading in the greenland data, with one change for the csv conversion
  fname <- list.files("Samples/greenland_csv", full.names = T)

  # Read in each file
  filelist <- lapply(fname, read_csv)

  # Remove whitespace and weird characters (Vivianne's regex)
  names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)

  # Removing the redundant index variable
  filelist <- lapply(filelist, function(x){select(x, wavenumber, absorbance)})

  # Getting the Alaska wavenumbers: (They're all the same across all AK samples. Yes, I checked.)
  AK_wav <- read_csv("Samples/alaska_csv/AS-01\ (8_24_16).0.csv")
  ak_wavenumbers <- AK_wav$wavenumber

  # Running the interpolation function on each individual vector in the file frame.
  # Note: Interpolation does not care if it's a char vec or not, I used as.numeric() everywhere that matters.
  interpolated_greenland <- lapply(filelist, function(x){interpolate(x$wavenumber, x$absorbance, ak_wavenumbers)})

  # Selecting only the absorbance vector from each list; interpolation returns a df of both abs and wavenumber
  interp_gl <- lapply(interpolated_greenland, function(x){select(x,absorbance)})

  # Binding it all into a data frame columnwise (each column one sample, rows being absorbance values)
  interp_df <- as.data.frame(do.call(cbind, interp_gl))

  # Adding the column names back in, as they got lost. Did check, this doesn't misalign any of the names.
  names(interp_df) <- names(filelist)

  # Adding the wavenumbers back in as their own column
  interp_df$wavenumber <- ak_wavenumbers

  # This one's a bit messy, so I'll be better about explaining.
  interp_df_wider <- interp_df %>%
    #Putting the wavenumber column at the very front of the data frame for indexing
    select(wavenumber, everything()) %>%
    # t transposes the frame, turning it on its side. More elegantly than I could get with pivot
    t %>% as.data.frame() %>%
    # Then this takes the wavenumber column, now row, and sets it as the column names instead.
    janitor::row_to_names(1)

  #Adding the BSi numbers:
  wet_chem_BSi <- read_csv("csvFiles/wet-chem-data.csv")

  #Binding the BSi numbers to the dataset
  interp_df_wider$BSi <- wet_chem_BSi$Bsi

  # Reordering to put BSi first
  interp_df_wider <- interp_df_wider %>%
    select(BSi, everything())

  # The end result is a data frame with row names being samples, column names being wavenumbers, and all observations being absorb values
  # With an additional BSi column at the very front.
  # Main question outstanding is whether or not we want to add the names in as their own column, or maintain them as row names?
  return(interp_df_wider)
}

# Proof of concept:
gl_interp <- interpolate_greenland()

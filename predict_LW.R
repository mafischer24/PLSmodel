
interpolate_ftirs = function (wavenumber, absorbance, out_vec = rounded_wavenumbers$wavenumber,
          ...)
{
  if (length(wavenumber) > 3762) {
    warning("Samples provided have significantly larger wavenumber spectrum than wavenumbers interpolated on to. Consider not interpolating (interpolate = FALSE in read_ftirs()) samples to preserve entire spectrum.")
  }
  if (length(wavenumber) < 1881) {
    warning("Returned NA absorbance values.")
  }
  tuple <- approx(as.numeric(LW_A_21_D1_5_6cm_1$wavenumber), as.numeric(LW_A_21_D1_5_6cm_1$absorbance),
                  xout = lw_wavenumbers)
  df <- as.data.frame(tuple)
  df <- df %>% rename(wavenumber = x, absorbance = y)
  return(df)
}

lw_wavenumbers <- LW_A_21_D1_5_6cm_1$wavenumber

interpolated_lw <- interpolate(LW_A_21_D1_5_6cm_1$wavenumber, LW_A_21_D1_5_6cm_1$absorbance, lw_wavenumbers)

read_ftirs = function (dir_path, wet_chem_path = NULL, format = "long", ...)
{
  files <- list.files(dir_path, full.names = TRUE)
  x <- map_dfr(.x = files, .f = read_ftirs_file, interpolate = ...) %>%
    select(sample_id, everything())
  if (!is.null(wet_chem_path)) {
    x <- read_wet_chem(wet_chem_path, x)
  }
  if (format == "wide") {
    x <- pivot_wider(x)
  }
  return(x)
}

# THIS IS WHERE THE ISSUE IS
read_ftirs_file = function (single_filepath, interpolate = TRUE, ...)
{
  x <- read_csv(single_filepath, ...)
  x <- x %>% as_tibble()
  if (ncol(x) > 2) {
    x <- x %>% select(-1)
    warning("Deleted presumed index column.")
  }
  col_names <- names(x)
  if (FALSE %in% ifelse(col_names == c("wavenumber", "absorbance"),
                        TRUE, FALSE)) {
    x <- x %>% rename(wavenumber = col_names[1], absorbance = col_names[2])
    warning("Columns renamed to `wavenumber`, `absorbance`. Please make sure these\n          labels match the contents of the columns.")
  }
  if (interpolate) {
    x <- interpolate_ftirs(x$wavenumber, x$absorbance)
  }
  x <- x %>% mutate(sample_id = tools::file_path_sans_ext(fs::path_file(single_filepath)))
  x <- as.ftirs(x)
  return(x)
}

LW_data <- read_ftirs("/Samples/LW_csv")

predict(akGLpls, interpolated_lw)

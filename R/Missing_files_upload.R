# intopolating alaska
# only to be run once but if you have the files you do not need to run!
source('R/interpolation.R')

fname <- c("Samples/alaska_csv/AW-73 (8_31_16).0.csv",
           "Samples/alaska_csv/AW-7.5 (8_31_16).0.csv",
           "Samples/alaska_csv/AW-34.5 (8_31_16).0.csv")

filelist <- lapply(fname, read_csv)
# Remove whitespace and weird characters (Vivianne's regex)
names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)
# Removing the redundant index variable

filelist <- lapply(filelist, function(x){dplyr::select(x, wavenumber, absorbance)})

# Running the interpolation function on each individual vector in the file frame.
# Note: Interpolation does not care if it's a char vec or not, I used as.numeric() everywhere that matters.

AK_wav <- read_csv("Samples/alaska_csv/AS-01\ (8_24_16).0.csv")
ak_wavenumbers <- AK_wav$wavenumber

interpolated_ak <- lapply(filelist, function(x){interpolate(x$wavenumber, x$absorbance, ak_wavenumbers)})

X1 <- AK_wav[1]

interpolated_ak <- lapply(interpolated_ak, function(x){cbind(X1 , x)})

# not working but would be ideal
# lapply(names(interpolated_ak),
#        function (x) {write.csv(interpolated_ak[[x]],
#                                file = paste0("~/Desktop/PLSmodel", x, ".csv"),
#                                row.names = FALSE)})


write.csv(interpolated_ak[["AW-73 (8_31_16).0"]],
          file = "~/Desktop/PLSmodel/Samples/alaska_csv/AW-73 (8_31_16).0.csv", row.names = FALSE)

write.csv(interpolated_ak[["AW-7.5 (8_31_16).0"]],
          file = "~/Desktop/PLSmodel/Samples/alaska_csv/AW-7.5 (8_31_16).0.csv", row.names = FALSE)

write.csv(interpolated_ak[["AW-34.5 (8_31_16).0"]],
          file = "~/Desktop/PLSmodel/Samples/alaska_csv/AW-34.5 (8_31_16).0.csv", row.names = FALSE)








# new introplated file
gl_dummy_data <- read_ftirs("~/Desktop/PLSmodel/Samples/greenland_csv",
                         "~/Desktop/PLSmodel/csvFiles/wet-chem-data.csv", format = "wide")
ak_dummy_data <-  read_ftirs("~/Desktop/PLSmodel/Samples/alaska_csv",
                             "~/Desktop/PLSmodel/csvFiles/AlaskaWetChem.csv", format = "wide")

combined_data <- rbind(gl_dummy_data, ak_dummy_data)

dummy_model <- plsr(bsi~., ncomp = 5, data=combined_data, validation = "CV", segments = 10)





test1_wave <- read_csv("Samples/alaska_csv/AS-01 (8_24_16).0.csv")

introp <- interpolate_ftirs(as.numeric(test1_wave$wavenumber), as.numeric(test1_wave$absorbance))


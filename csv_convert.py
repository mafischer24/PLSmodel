import numpy as np
import pandas as pd
from os import listdir

# Get the lists of files in both directories
alaska_files = listdir("Samples/alaskaSamples")
greenland_files = listdir("Samples/greenlandSamples")

for file in alaska_files:
    # Read in the .dpt file with the correct column names
    file_pd = pd.read_csv("Samples/alaskaSamples/" + file, sep = ',', names = ['wavenumber','absorbance'])
    file = file.replace(".dpt",".csv")
    # Write to csv format
    file_pd.to_csv("Samples/alaska_csv/" + file)

for file in greenland_files:
    # Readin the .dpt file with the correct column names
    # Note: the sep character is specifically a tab character here
    # I do not know if it would work with just spaces
    file_pd = pd.read_csv("Samples/greenlandSamples/"+ file, sep = "	",names = ['wavenumber','absorbance'])
    file = file.replace(".txt",".csv")
    # Write to csv format
    file_pd.to_csv("Samples/greenland_csv/"+file)

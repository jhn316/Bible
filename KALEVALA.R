library(readr)
library(stringr)

#skip all the comments at the top and bottom
rawKALEVALA <- read_lines("./pg7000.txt", skip = 87, n_max = 23482-375) 

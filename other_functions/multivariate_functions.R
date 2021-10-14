# # Reading in the necessary packages 
list.of.packages <- c("tidyverse","ggplot2", "purrr", "stringr", "sp", "rgdal", "raster",
                      "lubridate", "sf", "labelled","scales",  "raster", "rlist", 'rgeos', 'INLA', 
                      'cowplot', 'gridExtra', 'lme4', 'ggsci', 'patchwork', 'ggcorrplot', 'pscl', 'visreg', 
                      'viridis', 'splines', 'shades', 'glmmTMB', 'ape', 'DHARMa', 'MuMIn')


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


lapply(list.of.packages, library, character.only = TRUE) #applying the library function to packages

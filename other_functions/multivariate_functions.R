# # Reading in the necessary packages 
list.of.packages <- c("tidyverse","ggplot2", "purrr", "stringr", "sp", "rgdal", "raster",
                      "lubridate", "sf", "labelled","scales",  "raster", "rlist", 'rgeos', 
                      'cowplot', 'gridExtra', 'lme4', 'ggsci', 'patchwork', 'ggcorrplot', 'pscl', 'visreg', 
                      'viridis', 'splines', 'shades', 'glmmTMB', 'ape', 'DHARMa', 'AICcmodavg', 'plyr',
                      'effects', 'jtools', 'patchwork', 'ggpubr')


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


lapply(list.of.packages, library, character.only = TRUE) #applying the library function to packages


theme_manuscript <- function(){
  theme_bw() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 12, color = "black"), 
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size =12),
          legend.title=element_text(size=12, colour = 'black'),
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}
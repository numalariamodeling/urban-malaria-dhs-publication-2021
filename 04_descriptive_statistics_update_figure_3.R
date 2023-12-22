rm(list=ls())
library(gsubfn)

#############################################
#Directories 
#############################################
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", gsub("/OneDrive - Northwestern University", "", Sys.getenv("HOME")))))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria") 
projectpath <- file.path(DriveDir, "projects", "Manuscripts", "ongoing", "intervention_prioritization_manuscript", "IJERPH_Submission", "231206_reviews and revision")
result_plots <- file.path(projectpath, "plots") 


#############################################
#libraries and functions
#############################################
list_of_packages <- c("readr","ggplot2", "tidyverse", "patchwork", "RColorBrewer")
read_install_pacakges <- function(packages = list_of_packages
){
  new_packages <- packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new_packages)
  return(sapply(list_of_packages, require, character.only = TRUE))
}
read_install_pacakges()

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



#############################################
#Data analysis 
#############################################

pos_df <- read.csv(file.path(projectpath, 'test_pos_prop_region_year_month.csv')) %>% 
  mutate(year = str_split(yr_reg_imon, "_", simplify = TRUE)[,1],
  region = str_split(yr_reg_imon, "_", simplify = TRUE)[,2],
  month = str_split(yr_reg_imon, "_", simplify = TRUE)[,3]) %>% 
  dplyr::select(test_pos, year,  region, month)
  
tested_df <- read.csv(file.path(projectpath, 'tested_sum_region_year_month.csv')) %>% 
  mutate(year = str_split(yr_reg_imon, "_", simplify = TRUE)[,1],
  region = str_split(yr_reg_imon, "_", simplify = TRUE)[,2],
  month = str_split(yr_reg_imon, "_", simplify = TRUE)[,3]) %>% 
  dplyr::select(num_tested = tested, year, region, month)

df = left_join(pos_df, tested_df, by =c('year', 'region', 'month')) %>%  filter(month!=8, month!=9)

table(df$month)
#############################################
#plots 
#############################################

p <- ggplot(data = df, aes(x= year,  y = test_pos)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(color = region, size = num_tested), width = 0.08)+
  scale_color_brewer(name= "", palette = "RdPu") +
  facet_wrap(vars(month), ncol = 1)+ 
  labs (x = "Year of survey", y = "Test positivity rate per urban cluster", title = "") +
  theme_manuscript()

ggsave(paste0(result_plots, "/" , 'region_month_test_positivity.pdf'), p, width = 8.5, height = 7.5) 
write.csv(df, file.path(projectpath, "positives_tested_by_geopolitical_zone_month_year.csv"))

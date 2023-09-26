rm(list=ls())


## -----------------------------------------
### Paths
## -----------------------------------------

Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Documents","OneDrive", "urban_malaria")
ExDir <- file.path(NuDir, "extracted_data") # chang this directory to reflect where data will be placed
NGDir <-file.path(NuDir, "data", 'nigeria', "nigeria_dhs",  "data_analysis")
DataDir <-file.path(NGDir, "data")
ResultDir <-file.path(NGDir, "results")
DHSData <- file.path(DataDir, 'DHS')
DataIn <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates')
ifelse(!dir.exists(file.path(DataIn, "cleaned_cluster_covariates_all")), 
       dir.create(file.path(DataIn, "cleaned_cluster_covariates_all")), FALSE)
cleandatDir <- file.path(DataIn, 'cleaned_cluster_covariates_all')

# ------------------------------------------
### Data cleaning  
## -----------------------------------------

library(tidyr)
library(purrr)
library(dplyr)
library(stringr)

#DHS data 

files <- list.files(path = file.path(ExDir, 'DHS_survey_extract') , pattern = '.csv', full.names = TRUE, recursive = TRUE)
files<- files[-grep('_0m_|_1000m_|_3000m_|_4000m_|Temp_covereates|DHS_18.csv|pop_density_|p_test_lagos_|pop_density_2000m_buffer_DHS_10_15_18.csv|pop_density_2km_buffer_DHS_10_15_18_30sec|building_density|elevation_|interview_month|p_test_PfPR_urban_state_DHS_10_15_18|edu_a_all_state_DHS_PR_10_15_18', files)]
df <-sapply(files, read.csv, simplify = F)
df <- df %>% map_if(~ all(c('X') %in% colnames(.x)),~dplyr::select(., -X)) %>% 
  map_if(~ all(c('se') %in% colnames(.x)),~dplyr::select(., -se)) %>% 
  map_if(~ all(c('ci_l') %in% colnames(.x)),~dplyr::select(., -ci_l)) %>% 
  map_if(~ all(c('ci_u') %in% colnames(.x)),~dplyr::select(., -ci_u)) %>% 
  map_if(~ all(c('ID') %in% colnames(.x)), ~dplyr::rename(., v001 = ID)) %>%  
  map_if(~ all(c('hv001') %in% colnames(.x)), ~dplyr::rename(., v001 = hv001))%>%  
  map_if(~ all(c('X.hv001.') %in% colnames(.x)), ~dplyr::rename(., v001 = X.hv001.))

   


df <- df %>%  map(~mutate(., dhs_year = str_extract(.id, "\\d+(?=_[a-zA-Z]+.+$)"))) %>%  map(~dplyr::select(., -.id))

df <- df[order(sapply(df,nrow),decreasing = T)] %>% purrr::map(~mutate(., dhs_year = as.character(dhs_year))) %>%
  purrr::map(~mutate(., v001 = as.character(v001)))
  

df <- df %>%  purrr::reduce(left_join, by = c('dhs_year', 'v001'))


#interview month file, we want to select the first value for each month ( we can also check the impact by selecting the last value also)
files <- list.files(path = file.path(ExDir, "DHS_survey_extract") , pattern = '.csv', full.names = TRUE, recursive = TRUE)
files<- files[grep('interview_month', files)]
df_interview_month <-sapply(files, read.csv, simplify = F)
df_interview_month <- df_interview_month %>% map_if(~ all(c('X') %in% colnames(.x)),~dplyr::select(., -X)) %>% 
  map_if(~ all(c('se') %in% colnames(.x)),~dplyr::select(., -se)) %>% 
  map_if(~ all(c('ci_l') %in% colnames(.x)),~dplyr::select(., -ci_l)) %>% 
  map_if(~ all(c('ci_u') %in% colnames(.x)),~dplyr::select(., -ci_u)) %>% 
  map_if(~ all(c('ID') %in% colnames(.x)), ~dplyr::rename(., v001 = ID)) %>%  
  map_if(~ all(c('hv001') %in% colnames(.x)), ~dplyr::rename(., v001 = hv001))

df_interview_month <- df_interview_month %>%  map(~mutate(., dhs_year = str_extract(.id, "\\d+(?=_[a-zA-Z]+.+$)"))) %>%  map(~dplyr::select(., -.id)) 
df_im <- df_interview_month[[1]] %>%  group_by(v001, dhs_year) %>%  mutate(first_interview_month= dplyr::first(interview_month)) %>%  dplyr::select(-c(interview_month)) %>%  
  distinct() %>% mutate(v001 = as.character(v001))

df <- df %>%  left_join(df_im, by = c('dhs_year', 'v001'))
write.csv(df, paste0(ExDir, '/cleaned_datasets/all_DHS_variables_urban_malaria.csv'))


#geospatial covariates 

buffer <- c('_1000m_|_2000m_|_3000m_|_4000m_',  '_0m_|_2000m_|_3000m_|_4000m_', '_0m_|_1000m_|_3000m_|_4000m_','_0m_|_1000m_|_2000m_|_4000m_', '_0m_|_1000m_|_2000m_|_3000m_')

buffer_label <- c('0m', '1000m', '2000m', '3000m', '4000m')

df_geo<- list()

for (i in 1:length(buffer)){
  files <- list.files(path = file.path(ExDir, 'geospatial_covariates') , pattern = '.csv', full.names = TRUE, recursive = FALSE)
  files<- files[-grep('pop_density_FB|secondary_vector', files)]
  files <- files[-grep(buffer[[i]], files)]
  df<-sapply(files, read.csv, simplify = F)
  df <- df %>% map_if(~ all(c('hv001') %in% colnames(.x)), ~dplyr::rename(., v001 = hv001))
  df<- df[order(sapply(df,nrow),decreasing = T)]
  df<- df %>% map_if(~ all(c('.id') %in% colnames(.x)),~dplyr::select(., -.id))
  df<- df %>% map_if(~ all(c('month') %in% colnames(.x)),~dplyr::select(., -month))
  df <- df %>%  purrr::reduce(left_join, by = c('dhs_year', 'v001')) %>%  mutate(dhs_year = as.character(dhs_year))
  df_geo <- append(df_geo, list(df))
}



filenames <- c('0m', '1000m', '2000m', '3000m', '4000m')

for (i in 1:length(df_geo)){
write.csv(df_geo[[i]], paste0(ExDir, '/cleaned_datasets/all_geospatial_variables_urban_malaria_', filenames[[i]], '.csv'))
}


#2018 data alone 
files <- list.files(path = file.path(DataIn, 'DHS_survey_extract') , pattern = '.csv', full.names = TRUE, recursive = TRUE)
files<- files[grep('DHS_18', files)]
files<- files[-grep('housing', files)]
df <-sapply(files, read.csv, simplify = F)
df <- df %>% map_if(~ all(c('X') %in% colnames(.x)),~dplyr::select(., -X)) %>% 
  map_if(~ all(c('se') %in% colnames(.x)),~dplyr::select(., -se)) %>% 
  map_if(~ all(c('ci_l') %in% colnames(.x)),~dplyr::select(., -ci_l)) %>% 
  map_if(~ all(c('ci_u') %in% colnames(.x)),~dplyr::select(., -ci_u)) %>%
  map_if(~ all(c('mv001') %in% colnames(.x)), ~rename(., v001 = mv001))

df <- df %>%  map(~mutate(., dhs_year = str_split(.id, "_", simplify = T)[, 4]) ) %>%  map(~dplyr::select(., -.id))
df<- df[order(sapply(df,nrow),decreasing = T)]
df <- df %>%  purrr::reduce(left_join, by = c('dhs_year', 'v001'))
write.csv(df, paste0(cleandatDir, '/New_082321/2018_mobility_DHS_variables_urban_malaria.csv'))

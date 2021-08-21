rm(list=ls())
#memory.limit(size = 50000)

## -----------------------------------------
### Paths
## -----------------------------------------

Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
DataDir <-file.path(NGDir, "data")
SrcDir <- file.path(NGDir, 'src', 'Research', 'urban_rural_transmission_analysis')
ResultDir <-file.path(NGDir, "results")
BinDir <- file.path(NGDir, "bin")
DHSData <- file.path(DataDir, 'DHS')
DataIn <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates')
ProjectDir <- file.path(NuDir, "projects", "urban_malaria")
manuscript_dir <- file.path(ProjectDir,'manuscript')
illustrations_dir <- file.path(manuscript_dir,'illustrations')

ifelse(!dir.exists(file.path(DataIn, "cleaned_cluster_covariates_all")), 
       dir.create(file.path(DataIn, "cleaned_cluster_covariates_all")), FALSE)

cleandatDir <- file.path(DataIn, 'cleaned_cluster_covariates_all')

# ------------------------------------------
### Required functions and settings
## -----------------------------------------
source(file.path(SrcDir, "functions", "model functions.R"))
library(corrplot)
library(Hmisc)
library(ggcorrplot)
library(BAS)

# ------------------------------------------
### Data cleaning  
## -----------------------------------------

#DHS data 

files <- list.files(path = file.path(DataIn, 'DHS_survey_extract') , pattern = '.csv', full.names = TRUE, recursive = TRUE)
files<- files[-grep('_0m_|_1000m_|_3000m_|_4000m_|Temp_covereates|DHS_18.csv|pop_density_|p_test_lagos_|pop_density_2000m_buffer_DHS_10_15_18.csv|pop_density_2km_buffer_DHS_10_15_18_30sec|building_density|elevation_|interview_month', files)]
df <-sapply(files, read.csv, simplify = F)

df <- df %>% map_if(~ all(c('X') %in% colnames(.x)),~dplyr::select(., -X)) %>% 
  map_if(~ all(c('se') %in% colnames(.x)),~dplyr::select(., -se)) %>% 
  map_if(~ all(c('ci_l') %in% colnames(.x)),~dplyr::select(., -ci_l)) %>% 
  map_if(~ all(c('ci_u') %in% colnames(.x)),~dplyr::select(., -ci_u)) %>% 
  map_if(~ all(c('ID') %in% colnames(.x)), ~rename(., v001 = ID)) %>%  
  map_if(~ all(c('hv001') %in% colnames(.x)), ~rename(., v001 = hv001))


df <- df %>%  map(~mutate(., dhs_year = str_split(.id, "_", simplify = T)[, 4]) ) %>%  map(~dplyr::select(., -.id))

df<- df[order(sapply(df,nrow),decreasing = T)]

df <- df %>%  purrr::reduce(left_join, by = c('dhs_year', 'v001'))

#interview month file, we want to select the first value for each month ( we can also check the impact by selecting the last value also)
files <- list.files(path = file.path(DataIn, 'DHS_survey_extract') , pattern = '.csv', full.names = TRUE, recursive = TRUE)
files<- files[grep('interview_month', files)]
df_interview_month <-sapply(files, read.csv, simplify = F)
df_interview_month <- df_interview_month %>% map_if(~ all(c('X') %in% colnames(.x)),~dplyr::select(., -X)) %>% 
  map_if(~ all(c('se') %in% colnames(.x)),~dplyr::select(., -se)) %>% 
  map_if(~ all(c('ci_l') %in% colnames(.x)),~dplyr::select(., -ci_l)) %>% 
  map_if(~ all(c('ci_u') %in% colnames(.x)),~dplyr::select(., -ci_u)) %>% 
  map_if(~ all(c('ID') %in% colnames(.x)), ~rename(., v001 = ID)) %>%  
  map_if(~ all(c('hv001') %in% colnames(.x)), ~rename(., v001 = hv001))

df_interview_month <- df_interview_month %>%  map(~mutate(., dhs_year = str_split(.id, "_", simplify = T)[, 4]) ) %>%  map(~dplyr::select(., -.id)) 
df_im <- df_interview_month[[1]] %>%  group_by(v001) %>%  mutate(first_interview_month= dplyr::first(interview_month)) %>%  dplyr::select(-c(interview_month)) %>%  distinct()

df <- df %>%  left_join(df_im, by = c('dhs_year', 'v001'))


#geospatial covariates 
files <- list.files(path = file.path(DataIn, 'geospatial_covariates') , pattern = '.csv', full.names = TRUE, recursive = FALSE)
files<- files[-grep('_2000m_|_1000m_|_3000m_|_4000m_|pop_density_FB', files)]
df_geo <-sapply(files, read.csv, simplify = F) #%>% map(~dplyr::select(., -.id)) 

df_geo <- df_geo %>% map_if(~ all(c('hv001') %in% colnames(.x)), ~rename(., v001 = hv001)) 


df_geo<- df_geo[order(sapply(df_geo,nrow),decreasing = T)]

df_geo <- df_geo %>%  purrr::reduce(left_join, by = c('dhs_year', 'v001')) %>%  mutate(dhs_year = as.character(dhs_year)) %>% 
  dplyr::select(-c(contains('.id')))


#joining all datasets together 

df_ <- df %>% left_join(df_geo, by = c('dhs_year', 'v001')) 

write.csv(df, paste0(cleandatDir, '/all_cluster_variables_urban_malaria_0m_geospatial.csv'))

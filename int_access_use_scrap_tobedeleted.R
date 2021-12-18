## These scripts are used to extract cluster level data and variables for urban settings in Nigeria 
rm(list=ls())
memory.limit(size = 50000)

## -----------------------------------------
### Paths
## -----------------------------------------

user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
ProjectDir <- file.path(NuDir, "data", 'nigeria','nigeria_dhs' , 'data_analysis')
DataDir <- file.path(ProjectDir, 'data')
ResultDir =file.path(ProjectDir, "results", "research_plots")
GlobDir <- file.path(DataDir, 'africa_health_district_climate', 'climate', 'global')
DHSData <- file.path(DataDir, 'DHS')
RastDir <- file.path(DataDir, "Raster_files")
DataIn <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'DHS_survey_extract')
GeoDir <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'geospatial_covariates')
shapes <- file.path(NuDir, 'data', 'nigeria_shapefiles')


# -----------------------------------------
### Required functions and settings
## -----------------------------------------
source("00_data_extraction/data_extractor_functions/data_extractor_functions.R")
options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed


## ----------------------------------------------------
### Read in PR  data (DHS 2010, 2015, 2018)  
## ----------------------------------------------------
dhs <- read.files(DataDir, "*NGPR.*\\.DTA", 'NGPR7AFL|NGPR71FL|NGPR61FL', read_dta)  #reads in the PR files
dhs <- dhs %>% map(~filter(., hv025 ==1,hv103 == 1))%>%
  map(~mutate(.,clust_pop = hv013,
              net_ownership = hml1,
              slept_in_pop = hv103,
              net_use = ifelse(hml12 %in% c(1,2), 1,0),
              wt=hv005/1000000,strat=hv022,
              id=hv021, num_p=1))

#total hh members in a cluster 
vars <- c('clust_pop')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv001')
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <-  pmap(list(df,col,by), estim_sum)
  df <- plyr::ldply(df)
  #write.csv(df, file =file.path(DataIn, paste0(vars[i], "_all_DHS_PR_10_15_18.csv")))
  
}

clust_pop <- df

#total itns ownwned
vars <- c('net_ownership')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv001')
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <-  pmap(list(df,col,by), estim_sum)
  df <- plyr::ldply(df)
  #write.csv(df, file =file.path(DataIn, paste0(vars[i], "_all_DHS_PR_10_15_18.csv")))
  
}

net_ownership <- df

#total slept_in population
vars <- c('slept_in_pop')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv001')
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <-  pmap(list(df,col,by), estim_sum)
  df <- plyr::ldply(df)
  #write.csv(df, file =file.path(DataIn, paste0(vars[i], "_all_DHS_PR_10_15_18.csv")))
  
}

slept_in_pop <- df

#NET use
vars <- c('net_use')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv001')
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <-  pmap(list(df,col,by), estim_prop)
  df <- plyr::ldply(df)
  #write.csv(df, file =file.path(DataIn, paste0(vars[i], "_all_DHS_PR_10_15_18.csv")))
  
}

net_use <- df


df_all <- left_join(clust_pop, net_ownership, by= c('hv001','.id'))%>% left_join(net_use, by = c('hv001','.id')) %>%
  left_join(slept_in_pop, by = c('hv001','.id')) %>%  mutate(itnby2 = net_ownership*2) %>% 
  mutate(itn_acces =  itnby2/slept_in_pop)%>%  mutate(net_use_access =  (net_use/itn_acces)) 


#variable distribution and cumulative distribution 
df_behave = data.frame(`Net use access` = df_all$net_use_acces, `Net use` = df_all$net_use)

df_behave_long = df_behave %>%  pivot_longer(everything(),names_to='x_label', values_to='values')

df_list =split(df_behave_long, df_behave_long$x_label)
df_list_ordered = list(df_list$Net.use.access, df_list$Net.use)


xlab=list('% of individuals using bednets given access', '% of individuals using bednets')


p = pmap(list(df_list_ordered,'salmon', 'salmon', 'values', xlab, 25), cdf_hist)
p=p[[1]] + p[[2]]
p

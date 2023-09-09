## These scripts are used to extract cluster level data and variables for urban settings in Nigeria 
rm(list=ls())
memory.limit(size = 50000)

## -----------------------------------------
### Paths
## -----------------------------------------


user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Documents","OneDrive", "urban_malaria")
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



cdf_hist = function(df, fill,color, x, xlab, bins){
  hist=ggplot(df, aes(x =.data[[x]]))+geom_histogram(alpha = 0.4, position="identity", bins=bins)
  max_y=max(ggplot_build(hist)$data[[1]]$count)
  ggplot(df, aes(.data[[x]]))+
    geom_histogram(fill=fill, color= color, alpha = 0.4, position="identity", bins = bins) +
    stat_ecdf(aes_(y =bquote(..y..* .(max_y)), color =color))+
    scale_y_continuous(name= 'Count', sec.axis=sec_axis(trans = ~./max_y, name = 'Cumulative percent', labels = function(x) format(x *100, digits=2, nsmall=0)))+
    theme_manuscript()+theme(legend.position = 'none')+
    xlab(xlab)
}


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
## -------------------------------------------------------------------------
### Read in HR and PR data (DHS 2010, 2015, 2018) and compute ITN variables   
## -------------------------------------------------------------------------
#data 
dhs <- read.files(DHSData, "*NGHR.*\\.DTA", 'NGHR81FL|NGHR7AFL|NGHR71FL|NGHR61FL', read_dta)  #reads in the HR files


#computes household-level access 
dhs <- dhs %>% map(~filter(., hv025 ==1)) %>% 
  map(~dplyr::select(., hhid, hv001, starts_with('hml10'), hv005, 
                                  starts_with('hv103'), hv013, hv021, hv022)) %>% 
  map(~mutate(., potuse = 2 * rowSums(dplyr::select(., contains('hml10')),na.rm=T),
              slept_night = rowSums(dplyr::select(., contains('hv103')), na.rm=T), 
        potuse2 = ifelse(potuse/slept_night > 1, slept_night, potuse),
      access = potuse2/slept_night,
      wt=hv005/1000000,strat=hv022,
      id=hv021)) %>% 
  map(~dplyr::select(., hv001, access, wt, strat, id))
  
names(dhs) <- c("dhs_2010_ng", "dhs_2015_ng", "dhs_2018_ng", "dhs_2021_ng")

#generate mean access proportion by cluster  
vars <- c('access')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv001')
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <-  pmap(list(df,col,by), estim_mean)
  df_access <- plyr::ldply(df)
  #write.csv(df, file =file.path(DataIn, paste0(vars[i], "_all_DHS_PR_10_15_18.csv")))
}

write.csv(df, file =file.path(DataIn, "access_all_DHS_PR_10_15_18.csv"))
#data 
dhs <- read.files(DataDir, "*NGPR.*\\.DTA", 'NGPR81FL|NGPR7AFL|NGPR71FL|NGPR61FL', read_dta)  #reads in the PR files

#create dataset for computing ITN use 
dhs <- dhs %>% map(~filter(., hv025 ==1, hv103 == 1)) %>% 
  map(~dplyr::select(., hhid, hv001, hml12, hv005, 
                     hv103, hv013, hv021, hv022)) %>% 
  map(~mutate(., net_use =ifelse(hml12 %in% c(1,2),1, 0),
              wt=hv005/1000000,strat=hv022,
              id=hv021))

names(dhs) <- c("dhs_2010_ng", "dhs_2015_ng", "dhs_2018_ng", "dhs_2021_ng")

#compute net use
vars <- c('net_use')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv001')
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <-  pmap(list(df,col,by), estim_prop)
  df_use <- plyr::ldply(df)
  #write.csv(df, file =file.path(DataIn, paste0(vars[i], "_all_DHS_PR_10_15_18.csv")))
}



#compute ITN use given access 

df_netU_access <- left_join(df_use,df_access, by=c('.id', 'hv001'))

df_netU_access$netU_access <- df_netU_access$net_use/df_netU_access$access * 100
df_netU_access$netU_access2 <- ifelse(df_netU_access$netU_access > 100, 100,df_netU_access$netU_access)

net_use_access <- df_netU_access %>% dplyr::select(.id, hv001, netU_access2) %>% 
  rename(c(v001 = hv001, net_use_access = netU_access2)) %>% 
  mutate(.id = str_replace(.id, "dhs_2010_ng", "C:/Users/CHZCHI003/OneDrive - University of Cape Town/Documents/OneDrive/urban_malaria/data/nigeria/nigeria_dhs/data_analysis/data/DHS/Downloads/NG_2010_MIS_06192019/NGHR61DT/NGHR61FL.DTA")) %>% 
  mutate(.id = str_replace(.id, "dhs_2015_ng", "C:/Users/CHZCHI003/OneDrive - University of Cape Town/Documents/OneDrive/urban_malaria/data/nigeria/nigeria_dhs/data_analysis/data/DHS/Downloads/NG_2015_MIS_06192019/NGHR71DT/NGHR71FL.DTA")) %>%
  mutate(.id = str_replace(.id, "dhs_2018_ng", "C:/Users/CHZCHI003/OneDrive - University of Cape Town/Documents/OneDrive/urban_malaria/data/nigeria/nigeria_dhs/data_analysis/data/DHS/Downloads/NG_2018_DHS_11072019_1720_86355/NGHR7ADT/NGHR7AFL.DTA")) %>%
  mutate(.id = str_replace(.id, "dhs_2021_ng", "C:/Users/CHZCHI003/OneDrive - University of Cape Town/Documents/OneDrive/urban_malaria/data/nigeria/nigeria_dhs/data_analysis/data/DHS/Downloads/NG_2021_MIS_12052022_1735_141460/NGHR81FL/NGHR81FL.DTA"))

write.csv(net_use_access, file =file.path(DataIn, "net_use_access_all_DHS_PR_10_15_18_21.csv"))


#variable distribution and cumulative distribution 
df_behave = data.frame(`Net use access` = df_netU_access$netU_access2, `Net use` = df_netU_access$net_use)
df_behave_long = df_behave %>%  pivot_longer(everything(),names_to='x_label', values_to='values')
df_list =split(df_behave_long, df_behave_long$x_label)
df_list_ordered = list(df_list$Net.use.access, df_list$Net.use)


xlab=list('% of individuals using bednets given access', '% of individuals using bednets')


p = pmap(list(df_list_ordered,'salmon', 'salmon', 'values', xlab, 25), cdf_hist)
p=p[[1]] + p[[2]]
p

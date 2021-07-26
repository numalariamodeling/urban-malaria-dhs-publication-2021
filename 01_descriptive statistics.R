
x <- c("tidyverse","INLA", "ggplot2", "ggpubr", "inlabru", "rgdal", "sp", "sf", "tmap", 
       'paletteer', 'cowplot', 'gridExtra', 'lme4', 'reshape2')




lapply(x, library, character.only = TRUE) #applying the library function to packages

options(repr.plot.width = 14, repr.plot.height = 8)


#_________________________________Directories

user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
ProjectDir <- file.path(NuDir, 'data', 'nigeria_dhs' , 'data_analysis')
DataDir <- file.path(ProjectDir, "data")
DHSData <- file.path(DataDir, 'DHS')
DataIn <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'DHS_survey_extract')
Rdata <- file.path(DataDir, 'DHS', 'Subset_data', "urban_malaria_rdata")
ResultDir <-file.path(ProjectDir, "results", "research_plots")
HisDir <-file.path(ResultDir, "histograms")
BinDir <- file.path(ProjectDir, "bin")
SrcDir <- file.path(ProjectDir, 'src', 'Research', 'urban_rural_transmission_analysis')
RastDir <- file.path(DataDir, "Raster_files")
CsvDir <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'cleaned_cluster_covariates_all')


#_______________________________ Load pre-clustered data:
clu_df_10_18 <- read.csv(file.path(CsvDir, "all_cluster_variables_urban_malaria.csv"), 
                         header = T, sep = ',') 

# Binarize response:
clu_df_10_18$y <- ifelse(clu_df_10_18$p_test < 0.1, 0,1)

#na count
missing_values <- sapply(clu_df_10_18, function(x) sum(is.na(x)))


#__________________________________maps

# urban cluster points

dhs18_sf <- st_read(file.path(DHSData, "Downloads", "NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp"),) 
mis15_sf <- st_read(file.path(DHSData, "Downloads", "NG_2015_MIS_06192019/NGGE71FL/NGGE71FL.shp"),)
mis10_sf <- st_read(file.path(DHSData, "Downloads", "NG_2010_MIS_06192019/NGGE61FL/NGGE61FL.shp"),)
sf_10_18 <- rbind(dhs18_sf, mis15_sf, mis10_sf)


# join dhs variables to cluster points by year 
df_10_18_fin <- left_join(sf_10_18, clu_df_10_18, by = 
                            c("DHSCLUST" = "v001", "DHSYEAR" = "dhs_year"))%>% filter(URBAN_RURA == "U")



#make an urban map of all cluster values 

u_df_18_fin <- df_10_18_fin %>% filter(DHSYEAR == 2018) 
u_df_15_fin <- df_10_18_fin %>% filter(DHSYEAR == 2015) 
u_df_10_fin <- df_10_18_fin %>% filter(DHSYEAR == 2010)

#read in state shape file 
stateshp <- readOGR(file.path(DataDir, "shapefiles","gadm36_NGA_shp"), layer ="gadm36_NGA_1", use_iconv=TRUE, encoding= "UTF-8")
state_sf <- st_as_sf(stateshp)

#make cluster maps 

clustermap<-function(cluster_shp, title){
  tm_shape(state_sf) + #this is the health district shapfile with DS estimates info
    tm_polygons()+
    tm_shape(cluster_shp)+ #this is the points shape file with LLIN and number of kids info by cluster 
    tm_bubbles(size =0.2, col = "p_test", 
               border.col= "black", palette="seq",textNA = "Missing",
               breaks=c(0, 20, 30, 40, 50, 60, 70, 80, 90, 100), legend.col.show=T)+
    tm_layout(aes.palette = list(seq ="-RdYlBu"), title = title)+
    tm_legend(legend.title.size = 0.8, legend.just="top")
}

map_18<-clustermap(u_df_18_fin, "2018 malaria prevalence by cluster (DHS)")
map_15 <-clustermap(u_df_15_fin, "2015 malaria prevalence by cluster (DHS)")
map_10 <-clustermap(u_df_10_fin, "2010 malaria prevalence by cluster (DHS)")


urban_map<-tmap_arrange(map_18, map_15, map_10)


tmap_save(tm =urban_map, filename = file.path(ResultDir, "maps", "urban_malaria_maps.pdf"), 
          width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


###################################################################################
##_______________________________________urban barplots ___________________________
###################################################################################

paletteer::paletteer_d("awtools::a_palette")

# Binarize response:
clu_df_10_18$y <- ifelse(clu_df_10_18$p_test < 0.1, "less than 10%", "greater than 10%") 


u_bar <- ggplot(clu_df_10_18, aes(x=as.factor(y), fill=as.factor(y))) + 
  geom_bar()+ 
  scale_fill_paletteer_d("awtools::spalette")+
  #geom_text(stat='count', aes(label=..count..), vjust=-1)+
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", vjust =-1)+
  theme_minimal()+
  theme(legend.position = "none")+
  xlab("Malaria prevalence")+
  ylab("Number of clusters")

u_bar

ggsave(paste0(HisDir, '/', Sys.Date(),  'combined_urban_malaria_clusters_percent.pdf'), u_bar, width=13, height=13)



#________________________________all coveriates histogram plots_________________________________


clu_df_cont <- clu_df_10_18[ , -which(names(clu_df_10_18) %in% c("shstate", "region", "X"))]
clu_df_cont$y <- ifelse(clu_df_cont$p_test < 0.1, 0,1) %>% (as.numeric)

labels_data <- list("Education", "Floor type", "Household size", "Housing q", "Mean age", 
                    "Median age", "Net Use", "Roof type", "U5 population" ,"Wall type",
                    "Wealth", "Fever", "Pregnant women", "% of female", "U5 Female", 
                    "U5 net Use", "Malaria pevalence", "Fever treatment", "U5 ACT use","Time to city", 
                    "Building density", "Dominant vector", "Elevation", "1m travel 2015","Housing q 2000", 
                    "Housing q 2015", "1m travel 2019","Travel to healthcare", "U5 pop den (FB)", "Pop density",
                    "Sec vector", "Temperature", "1m walk time")



var_list <- c(4:37)
label_list <- c(1:33)
plot_list = list()

for (i in 1:33) { 
  p<- ggplot(clu_df_cont, aes_string(x=names(clu_df_cont)[var_list[[i]]])) + 
    geom_histogram(fill = "hotpink4")+
    theme_minimal()+
    theme(plot.background = element_rect(colour = "black", fill=NA, size=0.3), 
          axis.title.x = element_text(size=12))+
    labs (title = labels_data[label_list[[i]]], x = "values") +
    xlab("")
  plot_list[[i]]<-p
  variables <- ggarrange(plotlist=plot_list, nrow =7, ncol=6)
  ggsave(paste0(HisDir, '/', Sys.Date(),  'histograms.pdf'), variables, width=13, height=13)
}


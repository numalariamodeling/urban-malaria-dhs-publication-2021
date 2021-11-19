rm(list=ls())
memory.limit(size = 50000)

## -----------------------------------------
### Paths
## -----------------------------------------

user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
ProjectDir <- file.path(NuDir, "data", 'nigeria_dhs' , 'data_analysis')
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

dhs <- read.files(DHSData, "*FL.shp$", 'NGGE61FL|NGGE71FL|NGGE7BFL', shapefile) #read in DHS clusters 
dhs <- map(dhs, st_as_sf) %>%  map(~filter(.x, URBAN_RURA == "U")) %>% map(sf:::as_Spatial)



# buffers of interest

vars <- c(0, 1000, 2000, 3000, 4000)


raster_3 <- raster(file.path(RastDir, "NGA_population_v1_2_gridded.tif"))
raster <- list(raster_3)



for (i in 1:length(vars)) {
  var_name <- paste0('pop_den_gridded_', as.character(vars[i]), 'm')
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- df %>%  map(~rename_with(., .fn=~paste0(var_name), .cols = starts_with('nga')))
  df <- plyr::ldply(df) %>% select(-c(ID))
  write.csv(df, file =file.path(GeoDir, paste0('pop_density_gridded_', as.character(vars[i]), 
                                               'm_buffer', "_DHS_10_15_18.csv")),row.names = FALSE)
}


ddd <- read.csv(file.path(GeoDir, "pop_density_gridded_2000m_buffer_DHS_10_15_18.csv"))

#For buffer of 2000m
area <- pi*((1000)^2)
ddd$popSqM <- ddd$pop_den_gridded_2000m/area  

#histograms for gridded population 

hist(ddd$pop_den_gridded_2000m)

#hist for population per square metre

hist(ddd$popSqM)

#The original population density from columbia
pop_den <- read.csv(file.path(GeoDir, "pop_density_2000m_DHS_10_15_18.csv"))

#histograms for population density

hist(pop_den$pop_density_2000m)





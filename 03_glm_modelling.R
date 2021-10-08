rm(list=ls())
#memory.limit(size = 50000)

## -----------------------------------------
### Paths
## -----------------------------------------

Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
DataDir <-file.path(NGDir, "data")
ResultDir <-file.path(NGDir, "results")
DHSData <- file.path(DataDir, 'DHS')
DataIn <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'cleaned_cluster_covariates_all', 'New_082321')
MultivarData <- file.path(DataIn, 'final_dataset_multivariate_analysis')

# ------------------------------------------
### Required functions and settings
## -----------------------------------------
source("./other_functions/multivariate_functions.R")





# ------------------------------------------
### Read in analysis data 
## -----------------------------------------
dat = read.csv(file.path(MultivarData, 'multivariate_analysis_dataset.csv'))

#load spatial points to do Moran's I test 
sf18 = st_read(file.path(DHSData, "Downloads", "NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp"),) 
sf15 = st_read(file.path(DHSData, "Downloads", "NG_2015_MIS_06192019/NGGE71FL/NGGE71FL.shp"),) 
sf10 = st_read(file.path(DHSData, "Downloads", "NG_2010_MIS_06192019/NGGE61FL/NGGE61FL.shp"),) 
sf_all = rbind(sf18, sf15, sf10) %>%filter(URBAN_RURA == "U") %>%  rename(v001 = DHSCLUST, dhs_year=DHSYEAR) %>% 
  dplyr::mutate(lat = sf::st_coordinates(.)[,1],
                lon = sf::st_coordinates(.)[,2]) 

map = sf_all %>% left_join(dat, by=c('v001', 'dhs_year')) %>%  filter(LATNUM != 0) 
response.dists <- as.matrix(dist(cbind(map$lon, map$lat)))
response.dists.inv <- 1/response.dists
diag(response.dists.inv) <- 0

response.dists.inv[1:5, 1:5]

Moran.I(map$positives, response.dists.inv, na.rm = TRUE)# p=7.34987e-07, spatial autocorrelation exists 

map$wall_type

# ------------------------------------------
### Model fitting 
## -----------------------------------------

#SES models with zero-inflation
map2 = map %>% dplyr::select(positives, edu_a, wealth, housing_2015_4000m, roof_type,child_6_59_tested_malaria, lat, lon) %>%  na.omit()
m1 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3) + ns(housing_2015_4000m)+ ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3])+
                offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
summary(m1)# AIC - 2049.4


m2 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wall_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3]) + ns(housing_2015_4000m)+ ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3])+
                offset(log(child_6_59_tested_malaria)), data=map,  ziformula=~1,family=poisson)
summary(m2)#AIC - 2075.7



#formal test of spatial autocorrelation
sims <- simulateResiduals(m1)
testSpatialAutocorrelation(sims, map2$lat, map2$lon, plot = FALSE)



#account for spatial dependence
map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term

m3 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3) + ns(housing_2015_4000m)+ ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3])+
                offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID), data=map2,  ziformula=~1,family=poisson)
summary(m3)# 1940.3

m4 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+
                offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID), data=map2,  ziformula=~1,family=poisson)
summary(m4)# 1940.3
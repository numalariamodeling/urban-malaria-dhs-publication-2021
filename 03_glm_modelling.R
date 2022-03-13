rm(list=ls())
memory.limit(size = 50000)

## -----------------------------------------
### Paths
## -----------------------------------------

Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
NGDir <-file.path(NuDir, "data", "nigeria","nigeria_dhs",  "data_analysis")
DataDir <-file.path(NGDir, "data")
ResultDir <-file.path(NGDir, "results")
DHSData <- file.path(DataDir, 'DHS')
DataIn <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 
                    'cleaned_cluster_covariates_all', 'New_082321')
MultivarData <- file.path(DataIn, 'final_dataset_multivariate_analysis')

library(sf)
library(dplyr)


# ------------------------------------------
### Required functions and settings
## -----------------------------------------
source("./other_functions/multivariate_functions.R")




# ------------------------------------------
### Read in analysis data 
## -----------------------------------------
dat = read.csv(file.path(MultivarData, 'multivariate_analysis_dataset.csv'))
#seq(min(dat$med_treat_fever, na.rm=T),max(dat$med_treat_fever, na.rm = T), length = 4)[2:3]

#load spatial points from the DHS/MIS to do Moran's I test 
sf18 = st_read(file.path(DHSData, "Downloads", "NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp"),) 
sf15 = st_read(file.path(DHSData, "Downloads", "NG_2015_MIS_06192019/NGGE71FL/NGGE71FL.shp"),) 
sf10 = st_read(file.path(DHSData, "Downloads", "NG_2010_MIS_06192019/NGGE61FL/NGGE61FL.shp"),) 
sf_all = rbind(sf18, sf15, sf10) %>%filter(URBAN_RURA == "U") %>%  dplyr::rename(v001 = DHSCLUST, dhs_year=DHSYEAR) %>% 
  dplyr::mutate(lat = sf::st_coordinates(.)[,1],
                lon = sf::st_coordinates(.)[,2]) 

map = sf_all %>% left_join(dat, by=c('v001', 'dhs_year')) %>%  filter(LATNUM != 0) 
response.dists <- as.matrix(dist(cbind(map$lon, map$lat)))
response.dists.inv <- 1/response.dists
diag(response.dists.inv) <- 0
# 
response.dists.inv[1:5, 1:5]
# 
Moran.I(map$positives, response.dists.inv, na.rm = TRUE)# p=7.34987e-07, spatial autocorrelation exists 


# ------------------------------------------
### Model fitting 
## -----------------------------------------

#SES models with zero-inflation
map2 = map %>% dplyr::select(positives, edu_a, wealth, housing_2015_4000m, roof_type, wall_type, child_6_59_tested_malaria,
                              lat, lon, first_interview_month, dhs_year, region) %>%  na.omit()
map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
map2$month_year = factor(paste(map2$first_interview_month, '_', map2$dhs_year))
levels(map2$month_year)
map2$ID2 <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
map2$region <- factor(map2$region)

#models 

ses_mods <- list(m1 <- glmmTMB(positives~ ns(edu_a, 3)+ ns(wealth, 3) + ns(housing_2015_4000m)+ 
                                 ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3])+ 
                                 offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                 m2 <- glmmTMB(positives~ ns(edu_a, 3)+ ns(wall_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3]) + 
                                 ns(housing_2015_4000m)+ ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3])+
                                 offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson))
summary(m2)
#formal test of spatial autocorrelation
sims <- simulateResiduals(m1)
testSpatialAutocorrelation(sims, map2$lat, map2$lon, plot = FALSE)


#account for spatial dependence

ses_s_mods <- list(m3 <- glmmTMB(positives~ ns(edu_a, 3)+ ns(wealth, 3) + ns(housing_2015_4000m)+ 
                                   ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3])+
                                   offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID), data=map2,  ziformula=~1,family=poisson),
                   m4 <- glmmTMB(positives~region + ns(edu_a, 3)+ ns(wealth, 3)+offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID), 
                                 data=map2,  ziformula=~1,family=poisson),
                   m5 <- glmmTMB(positives~ ns(edu_a, 3)+ ns(wealth, 3)+ offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID) + 
                                   ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson),
                   m6 <- glmmTMB(positives~ ns(edu_a, 3)+ ns(wealth, 3)+ ns(housing_2015_4000m)+ 
                                   ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3]) +offset(log(child_6_59_tested_malaria)) +
                                   mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson))

ses_models <- list(m1, m2, m3, m4, m4, m5, m6)
lapply(ses_models , summary)

#m1 AIC - 2049.5, m2 AIC - 2075.7, m3 AIC - 1940.3, m4 AIC - 1946.4, m5 AIC - 1923.7, m6 AIC -1924.2

#m1: no region - 2018.3, region RE - 1960.5, region factor - 1945.1
#m2: no region - 2048.1, region RE - 2001.2, region factor - 1986.0 
#m3: no region - 1910.5, region RE - 1911.1, region factor - 1907.7
#m4: no region - 1918.9,  region RE - 1920.3, region factor - 1917.2
#m5: no region - 1902.0, region RE - 1903.3, region factor -1899.7 
#m6: no region - 1901.8, region RE -1902.1,  region factor - 1897.9 

summary(m6)

# comparing model AICs

aics_ses <- data.frame(cbind(ldply(models, function(x) cbind(AIC = AIC(x))),
                               model = sapply(1:length(models), function(x) deparse(formula(models[[x]])))))

#demographic factors 
map2 = map %>% dplyr::select(positives, child_6_59_tested_malaria,
                              pop_density_0m,pop_den_U5_FB_4000m,preg_women,all_female_sex,median_age,household_size,
                              lat, lon, first_interview_month, dhs_year, region) %>%  na.omit()
map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
map2$month_year = factor(paste(map2$first_interview_month, '_', map2$dhs_year))
levels(map2$month_year)
map2$ID2 <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
map2$region <- factor(map2$region)

demo_mods <- list(m1 <- glmmTMB(positives~ ns(pop_density_0m, 3)+ ns(pop_den_U5_FB_4000m, 3) + ns(preg_women, 3)+ ns(all_female_sex, 3)
                              + ns(median_age, 3) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                m2 <- glmmTMB(positives~ ns(pop_density_0m, 3)+ ns(pop_den_U5_FB_4000m, 3) + ns(all_female_sex, 3)
                              + ns(median_age, 3) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                m3 <- glmmTMB(positives~ ns(pop_density_0m, 3) + ns(all_female_sex, 3)
                              + ns(median_age, 3) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                m4 <- glmmTMB(positives~  ns(pop_density_0m, 3)+ ns(pop_den_U5_FB_4000m, 3) + ns(all_female_sex, 3) +
                                offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                m5 <- glmmTMB(positives~ region + ns(pop_density_0m, 3)+ ns(pop_den_U5_FB_4000m, 3) + 
                                ns(median_age, 2) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                m6 <- glmmTMB(positives~  ns(pop_density_0m, 3) +
                                ns(median_age, 2) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                m7 <- glmmTMB(positives~ ns(pop_density_0m, 3) 
                              + ns(median_age, 3) + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2),
                              data=map2,  ziformula=~1,family=poisson),
                m8 <- glmmTMB(positives~  ns(pop_density_0m, 2) +
                                ns(median_age, 3) + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), 
                              data=map2,  ziformula=~1,family=poisson)) 
lapply(demo_mods , summary)
# m1 AIC =  2164.1, M2 AIC =  2161.7, M3 AIC = 2162.7, M4 AIC = 2163.7, M5  AIC = 2159.9, M6 AIC = 2159.4, M7 AIC = 2072.7, M8 AIC = 2015.1

summary(m7)

#m1: no region - 2116.6, region RE - 2072.7, region factor - 2058.2
#m2: no region - 2115.5, region RE - 2074.9, region factor -  2061.0 
#m3: no region - 2117.4, region RE - 2073.7, region factor - 2059.5
#m4: no region - 2120.9,  region RE - 2100.2, region factor - 2089.4
#m5: no region - 2112.8, region RE - 2070.5, region factor - 2056.4
#m6: no region - 2115.2, region RE - 2069.5,  region factor - 2055.2 
#m7: no region - 1981.7, region RE - 1978.6,  region factor - 1981.7 # will check this again later
#m8: no region -1979.8, region RE - 1977.9,  region factor - 1973.1

# comparing model AICs
aics_demo <- data.frame(cbind(ldply(demo_mods , function(x) cbind(AIC = AIC(x))),
                               model = sapply(1:length(demo_mods), function(x) deparse(formula(demo_mods[[x]])))))


#behavioral factors 
map2 = map %>% dplyr::select(positives, child_6_59_tested_malaria,net_use, net_use_child, med_treat_fever, ACT_use_U5,
                             lat, lon, first_interview_month, dhs_year, region ) %>%  na.omit()
map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
map2$month_year = factor(paste(map2$first_interview_month, '_', map2$dhs_year))
levels(map2$month_year)
map2$ID2 <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
# 
beh_mods <- list(m1 <- glmmTMB(positives~  ns(net_use, 3) + ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])+
                                 ns(ACT_use_U5)+ offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson), 
                 m2 <- glmmTMB(positives~ region + ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3]) + 
                                 ns(ACT_use_U5)+ offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                 m3 <- glmmTMB(positives~ ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])
                               + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                 m4 <- glmmTMB(positives~ ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])
                               + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  
                               ziformula=~1,family=poisson),
                 m5 <- glmmTMB(positives~ ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])+
                                 ns(ACT_use_U5)+ + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), 
                               data=map2,  ziformula=~1,family=poisson))
lapply(beh_mods, summary)
#m1 AIC = 1873.4, m2 AIC = 1874.8, m3 AIC = 1875.2, m4 AIC = 1844.7 , m5 AIC = 1845.2

#m1: no region - 1985.2, region RE - 1950.2, region factor - 1937.9
#m2: no region -  1988.7, region RE - 1952.5, region factor - 1940.0
#m3: no region - 1986.8 , region RE - 1950.7  , region factor - 1938.3
#m4: no region - 1844.9 , region RE - 1844.1 , region factor - 1840.6
#m5: no region - 1846.9, region RE - 1846.1 , region factor - 1842.5

summary(m1) 
# comparing model AICs

aics_behav <- data.frame(cbind(ldply(beh_mods, function(x) cbind(AIC = AIC(x))),
                 model = sapply(1:length(beh_mods), function(x) deparse(formula(beh_mods[[x]])))))


##accessibility 
map2 = map %>% dplyr::select(positives, child_6_59_tested_malaria,motorized_travel_healthcare_2019_2000m,
                              lat, lon, first_interview_month, dhs_year) %>%  na.omit()
map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
map2$month_year = factor(paste(map2$first_interview_month, '_', map2$dhs_year))
levels(map2$month_year)
map2$ID2 <- factor(rep(1, nrow(map2)))# 
# 
# 
m1 <- glmmTMB(positives~ ns(motorized_travel_healthcare_2019_2000m, 3)+
                 + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
summary(m1)# AIC - 2194.0 
# comparing model AICs
acc_models <- list(m1)

aics_acc <-data.frame(cbind(ldply(acc_models, function(x) cbind(AIC = AIC(x))),
                 model = sapply(1:length(acc_models), function(x) deparse(formula(acc_models[[x]])))))


#environmental variables 
map2 = map %>% dplyr::select(positives, child_6_59_tested_malaria,precipitation_monthly_0m,temperature_monthly_0m,soil_wetness_0m, 
                             dist_water_bodies_0m, elevation_1000m, EVI_0m,lat, lon, first_interview_month, dhs_year,  region) %>%  na.omit()
map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
map2$month_year = factor(paste(map2$first_interview_month, '_', map2$dhs_year))
levels(map2$month_year)
map2$ID2 <- factor(rep(1, nrow(map2)))# 
# 
# 
envi_mods <- list(m1 <- glmmTMB(positives~ region + ns(precipitation_monthly_0m, 3) + ns(temperature_monthly_0m, 3)+ ns(soil_wetness_0m, 3) +
                                  ns(dist_water_bodies_0m, 3) + ns(elevation_1000m, 3) + ns(EVI_0m, 3)
                                + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                  m2 <- glmmTMB(positives~ region + ns(precipitation_monthly_0m, 3) + ns(temperature_monthly_0m, 3)+ ns(soil_wetness_0m, 3) +
                                  + ns(elevation_1000m, 3) + ns(EVI_0m, 3)
                                + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                  m3 <- glmmTMB(positives~ ns(precipitation_monthly_0m, 3) + ns(soil_wetness_0m, 3) + ns(elevation_1000m, 3) + 
                                  ns(EVI_0m, 3) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                  m4 <- glmmTMB(positives~ region + ns(precipitation_monthly_0m, 3) + ns(temperature_monthly_0m, 3)+ ns(soil_wetness_0m, 3) +
                                  + ns(elevation_1000m, 3) + ns(EVI_0m, 3)+ offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) +
                                  ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson),
                  m5 <- glmmTMB(positives~  ns(precipitation_monthly_0m, 3) + ns(temperature_monthly_0m, 3) +
                                  + ns(elevation_1000m, 3) + ns(EVI_0m, 3)+ offset(log(child_6_59_tested_malaria)) +  
                                  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson),
                  m6 <- glmmTMB(positives~ns(precipitation_monthly_0m, 3) + ns(temperature_monthly_0m, 3)  + ns(EVI_0m, 3)
                                + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), 
                                data=map2,  ziformula=~1,family=poisson),
                  m7 <- glmmTMB(positives~ ns(precipitation_monthly_0m, 3)   + ns(EVI_0m, 3)+ offset(log(child_6_59_tested_malaria))
                                +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2, ziformula=~1,family=poisson),
                  )
lapply(beh_mods, summary)
#m1 AIC =  2155.2, M2 AIC = 2152.3, M3 AIC = 2153.8, M4 AIC = 2063.3, M5  AIC = 2059.5, M6 AIC = 2057.7, M7 AIC = 2054.2

#m1: no region - 2149.3, region RE - 2140.5, region factor - 2132.6
#m2: no region - 2146.3, region RE - 2136.1 , region factor - 2127.9
#m3: no region - 2151.3, region RE -2137.2, region factor - 2128.1
#m4: no region - 2052.5, region RE - 2050.9, region factor - 2048.3 
#m5: no region - 2048.8, region RE - 2047.3, region factor - 2044.4
#m6: no region - 2048.9, region RE - 2047.7 , region factor - 2045.0
#m7: no region - 2045.3, region RE - 2044.2, region factor - 2041.8

summary(m1)

aics_env <-data.frame(cbind(ldply(beh_mods, function(x) cbind(AIC = AIC(x))),
                 model = sapply(1:length(beh_mods), function(x) deparse(formula(beh_mods[[x]])))))

# models with the highest aic  

m6 <- glmmTMB(positives~ region + ns(edu_a, 3)+ ns(wealth, 3)+ ns(housing_2015_4000m)+ 
                ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3]) +offset(log(child_6_59_tested_malaria)) +
                mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)

m8 <- glmmTMB(positives~ region+  ns(pop_density_0m, 2) +
                ns(median_age, 2) + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), 
              data=map2,  ziformula=~1,family=poisson)


m4 <- glmmTMB(positives~ region + ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])
              + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  
              ziformula=~1,family=poisson)

m1 <- glmmTMB(positives~ region + ns(motorized_travel_healthcare_2019_2000m, 3)+
                + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)

m7 <- glmmTMB(positives~ region + ns(precipitation_monthly_0m, 3) + ns(EVI_0m, 3)+ offset(log(child_6_59_tested_malaria))
              +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2, ziformula=~1,family=poisson)




#############################

map2 = map %>% dplyr::select(positives, child_6_59_tested_malaria,edu_a, wealth, housing_2015_4000m, roof_type, 
                             pop_density_0m, median_age, med_treat_fever,motorized_travel_healthcare_2019_2000m, 
                             precipitation_monthly_0m,EVI_0m, 
                             lat, lon, first_interview_month, dhs_year, region) %>%  na.omit() %>%  st_drop_geometry()
map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
map2$month_year = factor(paste(map2$first_interview_month, '_', map2$dhs_year))
levels(map2$month_year)
map2$ID2 <- factor(rep(1, nrow(map2)))# 
write.csv(map2, 'data_final_model.csv')

m_SES_demo<- glmmTMB(positives~ region + ns(edu_a, 3)+ ns(wealth, 3)+ ns(housing_2015_4000m)+ 
                       ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3])+ 
                       ns(pop_density_0m, 2) +
                ns(median_age, 2) +
                 +offset(log(child_6_59_tested_malaria)) +
                mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)


m_SES_demo_behave<- glmmTMB(positives~ region + ns(edu_a, 3)+ ns(wealth, 3)+ ns(housing_2015_4000m)
                       + ns(pop_density_0m, 2) +
                         ns(median_age, 3) + ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])
                       + offset(log(child_6_59_tested_malaria)) +
                       mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)


m_SES_demo_behave_access<- glmmTMB(positives~ region + ns(edu_a, 3)+ ns(wealth, 3)+ ns(housing_2015_4000m)+
                            ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3])+ 
                              ns(motorized_travel_healthcare_2019_2000m, 3)+
                            + ns(pop_density_0m, 2) +
                              ns(median_age, 3) + ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])
                            + offset(log(child_6_59_tested_malaria)) +

# this model does not converge 
m_SES_demo_behave_access_env<- glmmTMB(positives~ region + ns(edu_a, 3)+ ns(wealth, 3)+ ns(housing_2015_4000m)+
                       ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3])+ 
                       ns(motorized_travel_healthcare_2019_2000m, 3)+
                       + ns(pop_density_0m, 2) +
                       ns(median_age, 3) + ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])+
                         + ns(precipitation_monthly_0m, 3) + ns(EVI_0m, 3)+
                     offset(log(child_6_59_tested_malaria)) +
                       mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)                              mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)

#final model
m_SES_demo_behave_access_env_reduced<- glmmTMB(positives~ region + ns(edu_a, 3)+ ns(wealth, 3)+ ns(housing_2015_4000m)+
                                     + ns(pop_density_0m, 2) +
                                     ns(median_age, 3) + ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])+
                                     + ns(precipitation_monthly_0m, 3) + ns(EVI_0m, 3)+
                                     offset(log(child_6_59_tested_malaria)) +
                                   mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
summary(m_SES_demo_behave_access_env_reduced)

saveRDS(m_SES_demo_behave_access_env_reduced, file=file.path(MultivarData, 'multivariate_model_poisson.rds'))

library(broom.mixed)

#save model summary results 
result_df <- as.data.frame(tidy(m_SES_demo_behave_access_env_reduced)) %>% filter(effect == "fixed")
write.csv(result_df, 'multivariate_slope_estimates.csv')

library(buildmer)

#all terms are significant 
m <- buildglmmTMB(positives~ region + ns(edu_a, 3)+ ns(wealth, 3)+ ns(housing_2015_4000m)+
               + ns(pop_density_0m, 2) +
               ns(median_age, 3) + ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])+
               + ns(precipitation_monthly_0m, 3) + ns(EVI_0m, 3)+
               offset(log(child_6_59_tested_malaria)),
             data=map2,ziformula=~1,family=poisson, crit = "AIC", correlation = 'ar1(month_year + 0 | ID2)') 
summary(m)
saveRDS(m_SES_demo_behave_access_env_reduced, file=file.path(MultivarData, 'multivariate_model_poisson.rds'))

# Diagnostics 
fit_poisson <- readRDS(file=file.path(MultivarData, 'multivariate_model_poisson.rds'))
summary(fit_poisson)
simulationOutput <- simulateResiduals(fittedModel =fit_poisson, plot = F)
pdf(paste0(ResultDir, '/research_plots/updated_figures/', Sys.Date(), '_residual_diagnostic.pdf'))
plot(simulationOutput)
dev.off()

#effect plots 
library(ggeffects)
pr <- ggpredict(fit_poisson, "edu_a[all]")
col <- c("predicted", "conf.low", "conf.high")
pr[col] <- pr[col]/7 
pdf('test.pdf')
plot(pr)
dev.off()



#multivariate effect plots. 

vars <- list('edu_a', 'wealth', 'pop_density_0m', 'median_age', 'med_treat_fever',
             'precipitation_monthly_0m', 'EVI_0m')
lables <- list('% with post-primary education', '% in the rich wealth quintiles',
               'All age population density', 'Median age', '% of U5 children that sought
      medical treatment for fever', 'Total precipitation', 'Total precipitation',
               'Enhanced Vegetation Index')
y_lim <- list(4.53, 4.6, 40, 6.1, 3.5, 3, 7.5)

p <- list()
for (i in 1:7) { 
  eff <- Effect(vars[[i]], fit_zinbinom)
  eff_dt <- data.frame(eff)
  pt = ggplot(eff_dt,aes_string(vars[[i]], 'fit'))+ 
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha =0.2, fill = "springgreen1")+
    geom_line(color = "maroon", size = 1)+ theme_manuscript()+ 
    labs(x = paste0(as.character(lables[[i]]), ' ', as.character('(adjusted)')), y ='malaria positives')
  p[[i]]<- pt
  
}

eff <- Effect('edu_a', fit_poisson)
eff_dt <- data.frame(eff)
col <- c("fit", "lower", "upper")
eff_dt[col] <- eff_dt[col]/9

m=ggplot(eff_dt, aes(edu_a, fit))+ 
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha =0.2, fill = "springgreen1")+
  geom_line(color = "maroon", size = 1)+ theme_manuscript()

eff <- Effect('edu_a', bm1)
eff_dt <- data.frame(eff)
col <- c("fit", "lower", "upper")
eff_dt[col] <- eff_dt[col]/9

bm1 <- glmmTMB(positives~ns(edu_a, 3), offset=log(child_6_59_tested_malaria), data =map2, ziformula=~1, family=poisson)
summary(bm1)

eff <- Effect('edu_a', bm1)
eff_dt <- data.frame(eff)
col <- c("fit", "lower", "upper")
eff_dt[col] <- eff_dt[col]/9

b=ggplot(eff_dt, aes(edu_a, fit))+ 
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha =0.2, fill = "springgreen1")+
  geom_line(color = "maroon", size = 1) +
  theme_manuscript()

ggsave('bivariate_plots.pdf', b, width = 8, height = 6)

y=p[[1]]+ p[[2]] + p[[3]] + p[[4]] + p[[5]] + p[[6]] + p[[7]]
ggsave('multivariable_plots.pdf', y, width = 8, height = 6)

#bivariate analysis
val = map %>%  drop_na(med_treat_fever)
bi_models <- list(bm1 <- glmmTMB(positives~ns(edu_a, 3)+ offset(log(child_6_59_tested_malaria)), data =map2, ziformula=~1, family=poisson),
              bm2 <- glmmTMB(positives~ns(wealth, 3), data =map, ziformula=~1, family=nbinom2),
              bm3 <- glmmTMB(positives~ns(pop_density_0m,2), data =map, ziformula=~1, family=nbinom2),
              bm4 <- glmmTMB(positives~ns(median_age, 2), data =map, ziformula=~1, family=nbinom2),
              bm5 <- glmmTMB(positives~ ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3]), data =val, ziformula=~1, family=nbinom2),
              bm6 <- glmmTMB(positives~ns(precipitation_monthly_0m, 3), data =map, ziformula=~1, family=nbinom2),
              bm7 <- glmmTMB(positives~ns(EVI_0m,3), data =map, ziformula=~1, family=nbinom2))

#Bivariate effect plots
b <- list()
for (i in 1:7) {
  eff <- Effect(vars[[i]], bi_models[[i]])
  eff_dt <- data.frame(eff)
  pt = ggplot(eff_dt,aes_string(vars[[i]], 'fit'))+ 
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha =0.2, fill = "springgreen1")+
    geom_line(color = "maroon", size = 1)+ theme_manuscript()+ 
    labs(x = paste0(as.character(lables[[i]]), ' ', as.character('(unadjusted)')), y ='malaria positives')
  b[[i]]<- pt
  
}

b_plots=b[[1]]+ b[[2]] + b[[3]] + b[[4]] + b[[5]] + b[[6]] + b[[7]]
b_plots

ggsave('bivariable_plots.pdf', y, width = 8, height = 6)


#merging and arranging multivariate and bivariate plots side by side

p = (edu_b + edu_a) / (wealth_b + wealth) + plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'), legend.position = 'bottom')
ggsave(paste0(ResultDir, '/research_plots/updated_figures/', Sys.Date(), '_bivariate_multivariable_SES.pdf'), p, width = 7, height =5)

p=(pop_density_b + pop_density) / (median_age_b + median_age) + plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'), legend.position = 'bottom')
ggsave(paste0(ResultDir, '/research_plots/updated_figures/', Sys.Date(), '_bivariate_multivariable_demo.pdf'), p, width = 7, height =5)



p=fever_treat_b + fever_treat + plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'), legend.position = 'bottom')
ggsave(paste0(ResultDir, '/research_plots/updated_figures/', Sys.Date(), '_bivariate_multivariable_fever.pdf'), p, width = 7, height =2.8)


p=(precip_b +precip)/ (EVI_b+EVI) + plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'), legend.position = 'bottom')
ggsave(paste0(ResultDir, '/research_plots/updated_figures/', Sys.Date(), '_bivariate_multivariable_enironmental.pdf'), p, width = 7, height=5)

##END

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

#load spatial points to do Moran's I test 
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
map2 = map %>% dplyr::select(positives, edu_a, wealth, housing_2015_4000m, roof_type,child_6_59_tested_malaria,
                              lat, lon, first_interview_month, dhs_year) %>%  na.omit()
map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
map2$month_year = factor(paste(map2$first_interview_month, '_', map2$dhs_year))
levels(map2$month_year)
map2$ID2 <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term

#models 

ses_mods <- list(m1 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3) + ns(housing_2015_4000m)+ 
                                 ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3])+
                                 offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                 m2 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wall_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3]) + 
                                 ns(housing_2015_4000m)+ ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3])+
                                 offset(log(child_6_59_tested_malaria)), data=map,  ziformula=~1,family=poisson))

#formal test of spatial autocorrelation
sims <- simulateResiduals(m1)
testSpatialAutocorrelation(sims, map2$lat, map2$lon, plot = FALSE)


#account for spatial dependence

ses_s_mods <- list(m3 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3) + ns(housing_2015_4000m)+ 
                                   ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3])+
                                   offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID), data=map2,  ziformula=~1,family=poisson),
                   m4 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID), 
                                 data=map2,  ziformula=~1,family=poisson),
                   m5 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+ offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID) + 
                                   ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson),
                   m6 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+ + ns(housing_2015_4000m)+ 
                                   ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3]) +offset(log(child_6_59_tested_malaria)) +
                                   mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson))

ses_models <- list(m1, m2, m3, m4, m4, m5, m6)
lapply(ses_models , summary)

#m1 AIC - 2049.5, m2 AIC - 2075.7, m3 AIC - 1940.3, m4 AIC - 1946.4, m5 AIC - 1923.7, m6 AIC -1924.2


# comparing model AICs

aics_ses <- data.frame(cbind(ldply(models, function(x) cbind(AIC = AIC(x))),
                               model = sapply(1:length(models), function(x) deparse(formula(models[[x]])))))

#demographic factors 
map2 = map %>% dplyr::select(positives, child_6_59_tested_malaria,
                              pop_density_0m,pop_den_U5_FB_4000m,preg_women,all_female_sex,median_age,household_size,
                              lat, lon, first_interview_month, dhs_year) %>%  na.omit()
map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
map2$month_year = factor(paste(map2$first_interview_month, '_', map2$dhs_year))
levels(map2$month_year)
map2$ID2 <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term

demo_mods <- list(m1 <- glmmTMB(positives~ns(pop_density_0m, 3)+ ns(pop_den_U5_FB_4000m, 3) + ns(preg_women, 3)+ ns(all_female_sex, 3)
                              + ns(median_age, 3) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                m2 <- glmmTMB(positives~ns(pop_density_0m, 3)+ ns(pop_den_U5_FB_4000m, 3) + ns(all_female_sex, 3)
                              + ns(median_age, 3) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                m3 <- glmmTMB(positives~ns(pop_density_0m, 3) + ns(all_female_sex, 3)
                              + ns(median_age, 3) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                m4 <- glmmTMB(positives~ns(pop_density_0m, 3)+ ns(pop_den_U5_FB_4000m, 3) + ns(all_female_sex, 3) +
                                offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                m5 <- glmmTMB(positives~ns(pop_density_0m, 3)+ ns(pop_den_U5_FB_4000m, 3) + 
                                ns(median_age, 2) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                m6 <- glmmTMB(positives~ns(pop_density_0m, 3) +
                                ns(median_age, 2) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                m7 <- glmmTMB(positives~ns(pop_density_0m, 3) 
                              + ns(median_age, 2) + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2),
                              data=map2,  ziformula=~1,family=poisson),
                m8 <- glmmTMB(positives~ns(pop_density_0m, 2) +
                                ns(median_age, 2) + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), 
                              data=map2,  ziformula=~1,family=poisson)) 
lapply(demo_mods , summary)
# m1 AIC =  2164.1, M2 AIC =  2161.7, M3 AIC = 2162.7, M4 AIC = 2163.7, M5  AIC = 2159.9, M6 AIC = 2159.4, M7 AIC = 2072.7, M8 AIC = 2015.1

# comparing model AICs
aics_demo <- data.frame(cbind(ldply(demo_mods , function(x) cbind(AIC = AIC(x))),
                               model = sapply(1:length(demo_mods), function(x) deparse(formula(demo_mods[[x]])))))


#behavioral factors 
map2 = map %>% dplyr::select(positives, child_6_59_tested_malaria,net_use, net_use_child, med_treat_fever, ACT_use_U5,
                             lat, lon, first_interview_month, dhs_year, ) %>%  na.omit()
map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
map2$month_year = factor(paste(map2$first_interview_month, '_', map2$dhs_year))
levels(map2$month_year)
map2$ID2 <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
# 
beh_mods <- list(m1 <- glmmTMB(positives~ns(net_use, 3) + ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])+
                                 ns(ACT_use_U5)+ offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson), 
                 m2 <- glmmTMB(positives~ ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3]) + 
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
summary(m1)# AIC - 2215.5
# comparing model AICs
acc_models <- list(m1)

aics_acc <-data.frame(cbind(ldply(acc_models, function(x) cbind(AIC = AIC(x))),
                 model = sapply(1:length(acc_models), function(x) deparse(formula(acc_models[[x]])))))


#environmental variables 
map2 = map %>% dplyr::select(positives, child_6_59_tested_malaria,precipitation_monthly_0m,temperature_monthly_0m,soil_wetness_0m, 
                             dist_water_bodies_0m, elevation_1000m, EVI_0m,lat, lon, first_interview_month, dhs_year) %>%  na.omit()
map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
map2$month_year = factor(paste(map2$first_interview_month, '_', map2$dhs_year))
levels(map2$month_year)
map2$ID2 <- factor(rep(1, nrow(map2)))# 
# 
# 
envi_mods <- list(m1 <- glmmTMB(positives~ns(precipitation_monthly_0m, 3) + ns(temperature_monthly_0m, 3)+ ns(soil_wetness_0m, 3) +
                                  ns(dist_water_bodies_0m, 3) + ns(elevation_1000m, 3) + ns(EVI_0m, 3)
                                + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                  m2 <- glmmTMB(positives~ns(precipitation_monthly_0m, 3) + ns(temperature_monthly_0m, 3)+ ns(soil_wetness_0m, 3) +
                                  + ns(elevation_1000m, 3) + ns(EVI_0m, 3)
                                + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                  m3 <- glmmTMB(positives~ns(precipitation_monthly_0m, 3) + ns(soil_wetness_0m, 3) + ns(elevation_1000m, 3) + 
                                  ns(EVI_0m, 3) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                  m4 <- glmmTMB(positives~ns(precipitation_monthly_0m, 3) + ns(temperature_monthly_0m, 3)+ ns(soil_wetness_0m, 3) +
                                  + ns(elevation_1000m, 3) + ns(EVI_0m, 3)+ offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) +
                                  ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson),
                  m5 <- glmmTMB(positives~ns(precipitation_monthly_0m, 3) + ns(temperature_monthly_0m, 3) +
                                  + ns(elevation_1000m, 3) + ns(EVI_0m, 3)+ offset(log(child_6_59_tested_malaria)) +  
                                  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson),
                  m6 <- glmmTMB(positives~ns(precipitation_monthly_0m, 3) + ns(temperature_monthly_0m, 3)  + ns(EVI_0m, 3)
                                + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), 
                                data=map2,  ziformula=~1,family=poisson),
                  m7 <- glmmTMB(positives~ns(precipitation_monthly_0m, 3)   + ns(EVI_0m, 3)+ offset(log(child_6_59_tested_malaria))
                                +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2, ziformula=~1,family=poisson),
                  )
lapply(beh_mods, summary)
#m1 AIC =  2155.2, M2 AIC = 2152.3, M3 AIC = 2153.8, M4 AIC = 2063.3, M5  AIC = 2059.5, M6 AIC = 2057.7, M7 AIC = 2054.2

aics_env <-data.frame(cbind(ldply(beh_mods, function(x) cbind(AIC = AIC(x))),
                 model = sapply(1:length(beh_mods), function(x) deparse(formula(beh_mods[[x]])))))

# all models 
#SES
all_mods <- list(m5 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+offset(log(child_6_59_tested_malaria)) + 
                                 mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson),
                 m8 <- glmmTMB(positives~ns(pop_density_0m, 2) + ns(median_age, 2) + offset(log(child_6_59_tested_malaria)) +  
                                 mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson),
                 m4 <- glmmTMB(positives~ ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])
                               + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), 
                               data=map2,  ziformula=~1,family=poisson),
                 m7 <- glmmTMB(positives~ns(precipitation_monthly_0m, 3)   + ns(EVI_0m, 3) + offset(log(child_6_59_tested_malaria)) +  
                                 mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson))

lapply(all_mods, summary)
#m5 AIC = 1923.7, m8 AIC - 2015.1 ,m4 AIC = 1844.7  m7 AIC = 2054.2

# comparing model AICs
aics_all_models <- data.frame(cbind(ldply(all_mods, function(x) cbind(AIC = AIC(x))),
                                    model = sapply(1:length(all_mods), function(x) deparse(formula(all_mods[[x]])))))

# 
#all 
map2 = map %>% dplyr::select(positives, child_6_59_tested_malaria,edu_a, wealth,pop_density_0m,median_age, med_treat_fever,
                             precipitation_monthly_0m,
                             EVI_0m,
                             lat, lon, first_interview_month, dhs_year) %>%  na.omit() %>%  st_drop_geometry()
map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
map2$month_year = factor(paste(map2$first_interview_month, '_', map2$dhs_year))
levels(map2$month_year)
map2$ID2 <- factor(rep(1, nrow(map2)))# 

#
#all models 2

all_mods2 <- list(m1 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+ns(pop_density_0m, 2) + ns(median_age, 2)+ 
                                  ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])+
                                  ns(precipitation_monthly_0m, 3) + ns(EVI_0m, 3)+
                                  + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson),
                  m2 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+ns(pop_density_0m, 2) + ns(median_age, 2)+
                                  ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])+
                                  ns(precipitation_monthly_0m, 3) + ns(EVI_0m, 3) + mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), 
                                offset=log(child_6_59_tested_malaria), data=map2,  ziformula=~1,family=poisson),
                  m3 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+ns(pop_density_0m, 2) + ns(median_age, 2)+
                                  ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])+
                                  ns(EVI_0m, 3) + offset(log(child_6_59_tested_malaria)) +
                                  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson),
                  m4 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+ns(pop_density_0m, 2) + ns(median_age, 2)+
                                  ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])+
                                  ns(precipitation_monthly_0m, 3) + ns(EVI_0m, 3) + mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), 
                                offset=log(child_6_59_tested_malaria), data=map2,  ziformula=~1,family=nbinom2),
                  m5 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+ns(pop_density_0m, 2) + ns(median_age, 2)+
                                  ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])+
                                  ns(precipitation_monthly_0m, 3) + ns(EVI_0m, 3) +mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), 
                                offset=log(child_6_59_tested_malaria), data=map2,  ziformula=~1,family=nbinom1))

lapply(all_mods2, summary)
#M1 AIC = 1822.4 , m2 AIC = 1737.2, m3 AIC = 1734.9, m4 AIC = 1738.2113, m5 AIC = 1728.6


aics_all <- data.frame(cbind(ldply(all_mods2, function(x) cbind(AIC = AIC(x))),
                 model = sapply(1:length(all_mods2), function(x) deparse(formula(all_mods2[[x]])))))


#Final Model selection

#Selecting final model based on AIC comparison 
model_aics <- dplyr::bind_rows(aics_all, aics_all_models, aics_env, aics_acc, aics_behav, aics_demo, aics_ses)
model_aics <- model_aics[order(model_aics$AIC),] 
write.csv(model_aics, "AIC_df.csv")

#m5 has the best aic of 1728.6, hence is selected as the final model

#save model summary results 
result_df <- as.data.frame(tidy(m5)) %>% filter(effect == "fixed")
write.csv(result_df, "result_df.csv")

#Saving final model
saveRDS(m5, file=file.path(MultivarData, 'multivariate_model_nbinom2.rds'))

# Diagnostics 


fit_zinbinom <- readRDS(file=file.path(MultivarData, 'multivariate_model_nbinom2.rds'))
summary(fit_zinbinom)


simulationOutput <- simulateResiduals(fittedModel =m5, plot = F)
pdf('diagnostic_plots_2.pdf')
plot(simulationOutput)
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

y=p[[1]]+ p[[2]] + p[[3]] + p[[4]] + p[[5]] + p[[6]] + p[[7]]
ggsave('multivariable_plots.pdf', y, width = 8, height = 6)

#bivariate analysis
val = map %>%  drop_na(med_treat_fever)
bi_models <- list(bm1 <- glmmTMB(positives~ns(edu_a, 3), data =map, family=nbinom2),
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

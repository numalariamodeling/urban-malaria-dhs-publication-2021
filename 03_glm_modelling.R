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
# response.dists <- as.matrix(dist(cbind(map$lon, map$lat)))
# response.dists.inv <- 1/response.dists
# diag(response.dists.inv) <- 0
# 
# response.dists.inv[1:5, 1:5]
# 
# Moran.I(map$positives, response.dists.inv, na.rm = TRUE)# p=7.34987e-07, spatial autocorrelation exists 



# ------------------------------------------
### Model fitting 
## -----------------------------------------

#SES models with zero-inflation
# map2 = map %>% dplyr::select(positives, edu_a, wealth, housing_2015_4000m, roof_type,child_6_59_tested_malaria,
#                              lat, lon, first_interview_month, dhs_year) %>%  na.omit()
# map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
# map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
# map2$month_year = factor(paste(map2$first_interview_month, '_', map2$dhs_year))
# levels(map2$month_year)
# map2$ID2 <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term


#models 
# m1 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3) + ns(housing_2015_4000m)+ ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3])+
#                 offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
# summary(m1)# AIC - 2049.5
# 
# 
# m2 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wall_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3]) + ns(housing_2015_4000m)+ ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3])+
#                 offset(log(child_6_59_tested_malaria)), data=map,  ziformula=~1,family=poisson)
# summary(m2)#AIC - 2075.7



#formal test of spatial autocorrelation
# sims <- simulateResiduals(m1)
# testSpatialAutocorrelation(sims, map2$lat, map2$lon, plot = FALSE)



#account for spatial dependence

# m3 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3) + ns(housing_2015_4000m)+ ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3])+
#                 offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID), data=map2,  ziformula=~1,family=poisson)
# summary(m3)# AIC - 1940.3
# 
# m4 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+
#                 offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID), data=map2,  ziformula=~1,family=poisson)
# summary(m4)# AIC - 1946.4

#account for temporal effect
# m5 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+
#                 offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
# summary(m5)#AIC - 1923.7
# 
# 
# m6 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+ + ns(housing_2015_4000m)+ ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3]) +
#                 offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
# summary(m6)# AIC -1924.2
# 
# 
# m7 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+
#                 offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~mat(pos + 0 | ID),family=poisson)
# summary(m7)# did not run 



#demographic factors 
# map2 = map %>% dplyr::select(positives, child_6_59_tested_malaria,
#                              pop_density_0m,pop_den_U5_FB_4000m,preg_women,all_female_sex,median_age,household_size,
#                              lat, lon, first_interview_month, dhs_year) %>%  na.omit()
# map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
# map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
# map2$month_year = factor(paste(map2$first_interview_month, '_', map2$dhs_year))
# levels(map2$month_year)
# map2$ID2 <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
# 
# m1 <- glmmTMB(positives~ns(pop_density_0m, 3)+ ns(pop_den_U5_FB_4000m, 3) + ns(preg_women, 3)+ ns(all_female_sex, 3)+ 
#                 + ns(median_age, 3) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
# summary(m1)# AIC -  2164.1
# 
# 
# m2 <- glmmTMB(positives~ns(pop_density_0m, 3)+ ns(pop_den_U5_FB_4000m, 3) + ns(all_female_sex, 3)+ 
#                 + ns(median_age, 3) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
# summary(m2)# AIC -  2161.7 
# 
# 
# m3 <- glmmTMB(positives~ns(pop_density_0m, 3) + ns(all_female_sex, 3)+ 
#                 + ns(median_age, 3) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
# summary(m3)# AIC - 2162.7
# 
# 
# m4 <- glmmTMB(positives~ns(pop_density_0m, 3)+ ns(pop_den_U5_FB_4000m, 3) + ns(all_female_sex, 3)+ 
#                + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
# summary(m4)# AIC -   2163.7
# 
# 
# m5 <- glmmTMB(positives~ns(pop_density_0m, 3)+ ns(pop_den_U5_FB_4000m, 3) + 
#                 + ns(median_age, 2) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
# summary(m5)# AIC - 2159.9
# 
# m6 <- glmmTMB(positives~ns(pop_density_0m, 3) + 
#                 + ns(median_age, 2) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
# summary(m6)# AIC -   2159.4
# 
# m7 <- glmmTMB(positives~ns(pop_density_0m, 3) + 
#                 + ns(median_age, 2) + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
# summary(m7)# AIC - 2072.7
# 
# m8 <- glmmTMB(positives~ns(pop_density_0m, 2) + 
#                 + ns(median_age, 2) + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
# summary(m8)# AIC - 2015.1 



#behavioral factors 
m2 <- glmmTMB(positives~ns(ACT_use_U5), data =map, family=poisson)
m3 <- glmmTMB(positives~ns(edu_a, 3), data =map, ziformula=~1, family=poisson)
m4 <- glmmTMB(positives~ns(wealth, 3), data =map, ziformula=~1, family=poisson)
m4 <- glmmTMB(positives~ns(housing_2015_4000m,3), data =map, ziformula=~1, family=poisson)
ggeffects::ggpredict(m4,terms="housing_2015_4000m[all]")%>% plot(rawdata = TRUE, jitter = .01)
summary(m4)
map2 = map %>% dplyr::select(positives, child_6_59_tested_malaria,
                             net_use, net_use_child, med_treat_fever, ACT_use_U5,
                             lat, lon, first_interview_month, dhs_year) %>%  na.omit()
map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
map2$month_year = factor(paste(map2$first_interview_month, '_', map2$dhs_year))
levels(map2$month_year)
map2$ID2 <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
# 
# m1 <- glmmTMB(positives~ns(net_use, 3) + ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])+
#                 ns(ACT_use_U5)+
#                  + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
# summary(m1)# AIC - 1873.4
# 
# 
# m2 <- glmmTMB(positives~ ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3]) + 
#               ns(ACT_use_U5)+
#                 + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
# summary(m2)# AIC - 1874.8 
# 
# 
# m3 <- glmmTMB(positives~ ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])
#                 + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
# summary(m3)# AIC - 1875.2 
# 
# 
# m4 <- glmmTMB(positives~ ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])
#               + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
# summary(m4)# AIC - 1844.7 
# 
# 
# m5 <- glmmTMB(positives~ ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])+
#                 ns(ACT_use_U5)+
#               + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
# summary(m5)# AIC - 1845.2
# 



#accessibility 
# map2 = map %>% dplyr::select(positives, child_6_59_tested_malaria,motorized_travel_healthcare_2019_2000m,
#                              lat, lon, first_interview_month, dhs_year) %>%  na.omit()
# map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
# map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
# map2$month_year = factor(paste(map2$first_interview_month, '_', map2$dhs_year))
# levels(map2$month_year)
# map2$ID2 <- factor(rep(1, nrow(map2)))# 
# 
# 
# m1 <- glmmTMB(positives~ ns(motorized_travel_healthcare_2019_2000m, 3)+
#                 + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
# summary(m1)# AIC - 2215.5




#environmental variables 
# map2 = map %>% dplyr::select(positives, child_6_59_tested_malaria,precipitation_monthly_0m,
#                              temperature_monthly_0m, soil_wetness_0m, dist_water_bodies_0m, 
#                              elevation_1000m, EVI_0m,
#                              lat, lon, first_interview_month, dhs_year) %>%  na.omit()
# map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
# map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
# map2$month_year = factor(paste(map2$first_interview_month, '_', map2$dhs_year))
# levels(map2$month_year)
# map2$ID2 <- factor(rep(1, nrow(map2)))# 
# 
# 
# m1 <- glmmTMB(positives~ns(precipitation_monthly_0m, 3) + ns(temperature_monthly_0m, 3)+ ns(soil_wetness_0m, 3) +
#                 ns(dist_water_bodies_0m, 3) + ns(elevation_1000m, 3) + ns(EVI_0m, 3)
#                 + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
# summary(m1) #2155.2
# 
# 
# m2 <- glmmTMB(positives~ns(precipitation_monthly_0m, 3) + ns(temperature_monthly_0m, 3)+ ns(soil_wetness_0m, 3) +
#                 + ns(elevation_1000m, 3) + ns(EVI_0m, 3)
#               + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
# summary(m2) #2152.3
# 
# 
# 
# m3 <- glmmTMB(positives~ns(precipitation_monthly_0m, 3) + ns(soil_wetness_0m, 3) +
#                 + ns(elevation_1000m, 3) + ns(EVI_0m, 3)
#               + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
# summary(m3) #2153.8
# 
# 
# m4 <- glmmTMB(positives~ns(precipitation_monthly_0m, 3) + ns(temperature_monthly_0m, 3)+ ns(soil_wetness_0m, 3) +
#                 + ns(elevation_1000m, 3) + ns(EVI_0m, 3)
#               + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
# summary(m4) #2063.3
# 
# 
# m5 <- glmmTMB(positives~ns(precipitation_monthly_0m, 3) + ns(temperature_monthly_0m, 3) +
#                 + ns(elevation_1000m, 3) + ns(EVI_0m, 3)
#               + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
# summary(m5) #2059.5
# 
# 
# 
# m6 <- glmmTMB(positives~ns(precipitation_monthly_0m, 3) + ns(temperature_monthly_0m, 3)  + ns(EVI_0m, 3)
#               + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
# summary(m6) #2057.7
# 
# 
# m7 <- glmmTMB(positives~ns(precipitation_monthly_0m, 3)   + ns(EVI_0m, 3)
#               + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
# summary(m7) # 2054.2
# 


# all models 
#SES
# m5 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+
#                 offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
# summary(m5)#AIC - 1923.7

#demo
# m8 <- glmmTMB(positives~ns(pop_density_0m, 2) + 
#                 + ns(median_age, 2) + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
# summary(m8)# AIC - 2015.1 


#behavioral
# m4 <- glmmTMB(positives~ ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])
#               + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
# summary(m4)# AIC - 1844.7 
# 


#environmental 
# m7 <- glmmTMB(positives~ns(precipitation_monthly_0m, 3)   + ns(EVI_0m, 3)
#               + offset(log(child_6_59_tested_malaria)) +  mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
# summary(m7) # 2054.2


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

# m1 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+ns(pop_density_0m, 2) + ns(median_age, 2)+ 
#                 ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])+
#                 ns(precipitation_monthly_0m, 3) + ns(EVI_0m, 3)+
#               + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
# summary(m1) #1822.4 


m2 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+ns(pop_density_0m, 2) + ns(median_age, 2)+
                ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])+
                ns(precipitation_monthly_0m, 3) + ns(EVI_0m, 3)+
                + offset(log(child_6_59_tested_malaria)) +
                mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
summary(m2) #1737.2


#1737.2

#generate marginal predictions
# saveRDS(m2, file=file.path(MultivarData, 'multivariate_model.rds'))


fin_mod <- readRDS(file=file.path(MultivarData, 'multivariate_model.rds'))
summary(fin_mod)
ggeffects::ggpredict(m2,terms="edu_a[all]", type ='zero_inflated')%>% plot(rawdata = TRUE, jitter = .01)

y = predict(fin_mod, map2, type = c('response'))

map2$y = y 

#plot predicted and actual values
plot(map2$positives, map2$y)

library("ggpubr")
ggscatter(map2, x = "positives", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Actual values", ylab = "Predicted values")

ggplot(map2, aes(wealth, y)) +
  geom_point(data = map2, aes(x =wealth, y = positives)) +
  geom_smooth(data=map2, aes(x =wealth, y = y), method = 'glm', method.args = list(family = poisson(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))


# library(glmmTMB)
# library(splines)
# 
# 
# data_e <- data.frame(
#   positives = c(3L, 2L, 0L, 7L, 0L, 5L, 0L, 3L, 6L, 0L, 2L, 0L, 2L, 3L, 2L, 14L, 1L, 3L, 3L, 0L), 
#   values = c(86.1702127659574, 100, 81.0344827586207, 98.7341772151899, 97.5903614457831, 
#              98.1308411214953, 96.551724137931, 100, 100, 96.045197740113, 99.1150442477876, 
#              98.8888888888889, 100, 91.6279069767442, 33.3333333333333, 0, 
#              92.3076923076923, 24, 0, 93.5483870967742))
# m1 <- glmmTMB(
#   positives ~ ns(values, 3, knots = seq(min(values), max(values), length = 4)[2:3]),
#   data = data_e,
#   ziformula =  ~ 1,
#   family = poisson
# )
# 
# head(insight::get_data(m1))
# 
# ggeffects::ggpredict(m1, terms="values [all]", type ='zero_inflated')
# 
# sessionInfo(package = NULL)

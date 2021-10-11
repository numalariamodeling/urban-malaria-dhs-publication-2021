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



# ------------------------------------------
### Model fitting 
## -----------------------------------------

#SES models with zero-inflation
map2 = map %>% dplyr::select(positives, edu_a, wealth, housing_2015_4000m, roof_type,child_6_59_tested_malaria,
                             pop_density_0m,pop_den_U5_FB_4000m,preg_women,all_female_sex,median_age,household_size,
                             net_use, net_use_child, med_treat_fever, ACT_use_U5,
                             lat, lon, first_interview_month, dhs_year) %>%  na.omit()
map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
map2$month_year = factor(paste(map2$first_interview_month, '_', map2$dhs_year))
levels(map2$month_year)
map2$ID2 <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term


#models 
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

m3 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3) + ns(housing_2015_4000m)+ ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3])+
                offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID), data=map2,  ziformula=~1,family=poisson)
summary(m3)# AIC - 1940.3

m4 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+
                offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID), data=map2,  ziformula=~1,family=poisson)
summary(m4)# AIC - 1946.4

#account for temporal effect
m5 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+
                offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
summary(m5)#AIC - 1923.7


m6 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+ + ns(housing_2015_4000m)+ ns(roof_type, knots = seq(min(roof_type),max(roof_type),length =4)[2:3]) +
                offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
summary(m6)# AIC -1924.2


m7 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+
                offset(log(child_6_59_tested_malaria)) + mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~mat(pos + 0 | ID),family=poisson)
summary(m7)# did not run 



#demographic factors 
m1 <- glmmTMB(positives~ns(pop_density_0m, 3)+ ns(pop_den_U5_FB_4000m, 3) + ns(preg_women, 3)+ ns(all_female_sex, 3)+ 
                + ns(median_age, 3) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
summary(m1)# AIC - 2124.5 


m2 <- glmmTMB(positives~ns(pop_density_0m, 3)+ ns(pop_den_U5_FB_4000m, 3) + ns(all_female_sex, 3)+ 
                + ns(median_age, 3) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
summary(m2)# AIC - 2122.9


m3 <- glmmTMB(positives~ns(pop_density_0m, 3) + ns(all_female_sex, 3)+ 
                + ns(median_age, 3) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
summary(m3)# AIC - 2124.0


m4 <- glmmTMB(positives~ns(pop_density_0m, 3)+ ns(pop_den_U5_FB_4000m, 3) + ns(all_female_sex, 3)+ 
               + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
summary(m4)# AIC -  2123.7 


m5 <- glmmTMB(positives~ns(pop_density_0m, 3)+ ns(pop_den_U5_FB_4000m, 3) + 
                + ns(median_age, 2) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
summary(m5)# AIC - 2120.4

m6 <- glmmTMB(positives~ns(pop_density_0m, 3) + 
                + ns(median_age, 2) + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
summary(m6)# AIC - 2119.9

m7 <- glmmTMB(positives~ns(pop_density_0m, 3) + 
                + ns(median_age, 2) + offset(log(child_6_59_tested_malaria)) + + mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
summary(m7)# AIC - 1982.1 

m8 <- glmmTMB(positives~ns(pop_density_0m, 2) + 
                + ns(median_age, 2) + offset(log(child_6_59_tested_malaria)) + + mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
summary(m8)# AIC - 1980.1



#behavioral factors 
m1 <- glmmTMB(positives~ns(net_use, 3) + 
                 + offset(log(child_6_59_tested_malaria)), data=map2,  ziformula=~1,family=poisson)
summary(m1)# AIC - 

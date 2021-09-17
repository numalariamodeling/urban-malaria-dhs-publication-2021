rm(list=ls())
#memory.limit(size = 50000)

# ----------------------------------------------------
### Directories
# ----------------------------------------------------

user = Sys.getenv("USERNAME")
Drive = file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir = file.path(Drive, "Box", "NU-malaria-team")
ProjectDir = file.path(NuDir, 'data', 'nigeria_dhs' , 'data_analysis')
DataDir = file.path(ProjectDir, "data")
DHSData = file.path(DataDir, 'DHS')
DataIn = file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'DHS_survey_extract')
ResultDir =file.path(ProjectDir, "results", "research_plots")
HisDir =file.path(ResultDir, "histograms")
MapsDir = file.path(ResultDir, "maps")
CsvDir = file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'cleaned_cluster_covariates_all', 'New_082321')

# ----------------------------------------------------
### Required functions and settings
## ----------------------------------------------------
source("./other_functions/descriptive_analysis_functions.R")


## ----------------------------------------------------------------
### Creating analysis data  
## ----------------------------------------------------------------
#read in dhs file 
dhs = read.csv(file.path(CsvDir, "all_DHS_variables_urban_malaria.csv"), header = T, sep = ',') %>% dplyr::select(-X)

#read in geospatial dataset and create final data 
files = list.files(path = CsvDir, pattern = '.csv', full.names = TRUE, recursive = FALSE)
files = files[-grep('all_DHS_variables_urban', files)]
df_geo = sapply(files, read.csv, simplify = F)


#find the number of NAs per column in geospatial data 
df_nas = df_geo %>%  map(~summarise_all(., funs(sum(is.na(.)))))
names(df_nas[[1]])<- gsub(pattern = '\\_0m$', replacement = '_nas', x = names(df_nas[[1]]))#remove _0m from the 0m buffer dataset
for(i in seq_along(df_nas)){
  names(df_nas[[i]])<- names(df_nas[[1]])
}
df_nas = bind_rows(df_nas, .id ='column label')


#count the number of negatives per column in geospatial data 
df_neg = df_geo %>%  map(~summarise_all(., funs(sum((.) < 0, na.rm=TRUE))))
names(df_neg[[1]])<- gsub(pattern = '\\_0m$', replacement = '_negs', x = names(df_neg[[1]]))#remove _0m from the 0m buffer dataset
for(i in seq_along(df_neg)){
  names(df_neg[[i]])<- names(df_neg[[1]])
}
df_neg = bind_rows(df_neg, .id ='column label')

df_na_neg = cbind(df_nas, df_neg)
df_na_neg = df_na_neg[,order(colnames(df_na_neg))]
df_na_neg

#select geospatial dataset values with fewest 
df_sp = data.frame(v001 = df_geo[[1]]$v001, dhs_year = df_geo[[1]]$dhs_year, elevation_1000m = df_geo[[2]]$elev_merit_1000m,
                   housing_2000_4000m = df_geo[[5]]$housing_2000_4000m,  housing_2015_4000m = df_geo[[5]]$housing_2015_4000m,
                   minutes_nearest_city_1000m = df_geo[[2]]$minutes_to_city_1000m, minutes_travel_metre_2015_1000m = df_geo[[2]]$minutes_travel_metre_2015_1000m,
                   minutes_travel_metre_2019_2000m = df_geo[[3]]$minutes_travel_metre_2019_2000m, minutes_walking_healthcare_2000m = df_geo[[3]]$minutes_walking_healthcare_2000m,
                   minutes_walking_metre_2000m = df_geo[[3]]$minutes_walking_metre_2000m, motorized_travel_healthcare_2019_2000m = df_geo[[3]]$motorized_travel_healthcare_2019_2000m,
                   pop_den_U5_FB_4000m = df_geo[[5]]$pop_den_U5_FB_4000m, pop_density_0m = df_geo[[1]]$pop_density_0m,
                   precipitation_monthly_0m = df_geo[[1]]$preci_monthly_0m,soil_wetness_0m = df_geo[[1]]$soil_wetness_0m, 
                   temperature_monthly_0m = df_geo[[1]]$temp_survey_month_0m, dist_water_bodies_0m = df_geo[[1]]$dist_water_bodies_0m, EVI_0m = df_geo[[1]]$EVI_0m) 


df_all <- left_join(dhs, df_sp, by =c('v001', 'dhs_year'))

#define variables to be used throughout analysis
positives = df_all$positives 
dhs_year=df_all$dhs_year
num_tested =df_all$child_6_59_tested_malaria
region = df_all$region
interview_month = df_all$first_interview_month
edu_cat = cut(df_all$edu_a, breaks = c(0, 5, 10, 15, 100), include.lowest =TRUE)



## -----------------------------------------------------------------------------------------------------------------------
### Socio-economic variable distribution, cumulative distribution, correlation and relationship with malaria prevalence 
## -----------------------------------------------------------------------------------------------------------------------

#variable distribition and cumulative distribution 
dhs_social = data.frame(`Educational attainment` = df_all$edu_a, Wealth = df_all$wealth, `Improved flooring` =df_all$floor_type,
                        `Improved roofing materials` = df_all$roof_type, `Improved wall` = df_all$wall_type, `improved housing in 2000` =df_all$housing_2000_4000m,
                        `improved housing in 2015` = df_all$housing_2015_4000m) %>%  mutate(improved.housing.in.2000 = improved.housing.in.2000*100,
                                                                                             improved.housing.in.2015= improved.housing.in.2015*100) 

dhs_social_long = dhs_social %>%  pivot_longer(everything(),names_to='x_label', values_to='values')

df_list =split(dhs_social_long, dhs_social_long$x_label)
df_list_ordered = list(df_list$Educational.attainment,df_list$Wealth,
                       df_list$Improved.flooring, df_list$Improved.roofing.materials, df_list$Improved.wall, df_list$improved.housing.in.2000,
                       df_list$improved.housing.in.2015)


x=list('values')
fill = list('#8971B3')
color = list('#8971B3')
xlab=list(expression(atop('% with post-primary', paste('education'))),
          expression(atop('% in the rich wealth', paste('quintiles'))),expression(atop('% in homes with improved', paste(' flooring'))),
          expression(atop('% in homes with a metal', paste('or zinc roof'))), expression(atop('% in homes with an', paste('improved wall type'))),
          expression(atop('% living in improved', paste('housing (2000)'))),
          expression(atop('% living in improved', paste('housing (2015)'))))

bins = list(30)


p = pmap(list(df_list_ordered,fill, color, x, xlab, bins), cdf_hist)
p=p[[1]]+ p[[2]]+ p[[3]]+p[[4]]+ p[[5]]+p[[6]]+p[[7]]+ plot_annotation(tag_levels = 'A')& theme(plot.tag = element_text(size = 12, face = 'bold'))
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), 'social_variable_distribution.pdf'), p, width =8, height =7)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), 'social_variable_distribution.png'), p, width =8, height =7)

#correlation 

dhs_social_ordered = dhs_social[, order(ncol(dhs_social):1)] %>%  mutate(improved.housing.in.2000 = improved.housing.in.2000*100,
                                                                                            improved.housing.in.2015= improved.housing.in.2015*100)
#replace nas with their means 
for(i in 1:ncol(dhs_social_ordered)){
  dhs_social_ordered[is.na(dhs_social_ordered[,i]), i] = mean(dhs_social_ordered[,i], na.rm = TRUE)
}

#correlation matrix 
corr = round(cor(dhs_social_ordered), 1)

# Compute a matrix of correlation p-values
p.mat = cor_pmat(dhs_social_ordered)


corr= ggcorrplot(corr, lab = TRUE, legend.title = "Correlation coefficient")+ 
  theme_corr()
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), 'correlation_coefficients_social.pdf'), corr, width = 13, height = 9)




#relationship with malaria positives
dhs_social_plot = cbind(dhs_social, dhs_year, positives, num_tested)
dhs_social_plot$rate = (dhs_social_plot$positives/dhs_social_plot$num_tested) 
dhs_social_plot = dhs_social_plot  %>%  pivot_longer(!c(dhs_year, positives, num_tested, rate),names_to='x_label', values_to='values')
df_list = split(dhs_social_plot, dhs_social_plot$x_label)
df_list_ordered = list(df_list$Educational.attainment,df_list$Wealth,
                       df_list$Improved.flooring, df_list$Improved.roofing.materials, df_list$Improved.wall, df_list$improved.housing.in.2000,
                       df_list$improved.housing.in.2015)

plots = df_list_ordered %>%  {purrr::map2(., xlab, ~ggplot(.x,aes(x=values, y=positives))+
                                            geom_point(shape=42, size= 3, color = "#f64b77", alpha = 0.5) +
                                            geom_smooth(aes(fill = "Trend"), se = FALSE, color = "#644128", method = 'glm', method.args = list(family = poisson(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))+
                                            geom_smooth(aes(color = "Confidence Interval"), fill = "#a56c56", linetype = 0, method = 'glm', method.args = list(family = poisson(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x), length =4)[2:3]))+
                                            theme_manuscript()+
                                            labs(x = .y, y ='malaria positives')+
                                            guides(fill =FALSE, color =FALSE))}



p<- plots[[1]]+plots[[2]]+ plots[[3]]+ plots[[4]]+ plots[[5]]+ plots[[6]] + plots[[7]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p

ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_bivariate_social.pdf'), p, width = 8, height =7)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_bivariate_social.png'), p, width = 8, height =7)

plots = df_list_ordered %>%  {map2(., xlab, ~ggplot(.x,aes(x=values, y=rate))+
                                          geom_point(shape=42, size= 3, color = "#f64b77", alpha = 0.5) +
                                          geom_smooth(aes(fill = "Trend"), se = FALSE, color = "#644128", method = 'glm', method.args = list(family = quasipoisson(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))+
                                          geom_smooth(aes(color = "Confidence Interval"), fill = "#a56c56", linetype = 0, method = 'glm', method.args = list(family = quasipoisson(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x), length =4)[2:3]))+
                                          theme_manuscript()+
                                          labs(x = .y, y ='malaria test positive rate')+
                                          guides(fill =FALSE, color =FALSE))}

p<- plots[[1]]+plots[[2]]+ plots[[3]]+ plots[[4]]+ plots[[5]]+ plots[[6]]+ plots[[7]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p

ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_bivariate_social_rate.png'), p, width = 8, height =7)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_bivariate_social_rate.png'), p, width = 8, height =7)








## -----------------------------------------------------------------------------------------------------------------------
### Demographic variable distribution, cumulative distribution, correlation and relationship with malaria prevalence 
## -----------------------------------------------------------------------------------------------------------------------
#variable distribution and cumulative distribution 
demo_numeric = data.frame(`Population density` = df_all$pop_density_0m, `U5 population density` = df_all$pop_den_U5_FB_4000m, `Pregnant women` =df_all$preg_women,
                        `Female population` = df_all$all_female_sex, `Household size` = df_all$household_size, `Median age` =df_all$median_age)
                      
demo_numeric_long = demo_numeric %>%  pivot_longer(everything(),names_to='x_label', values_to='values')

df_list =split(demo_numeric_long, demo_numeric_long$x_label)
df_list_ordered = list(df_list$Population.density,df_list$U5.population.density, df_list$Pregnant.women,
                       df_list$Female.population, df_list$Household.size, df_list$Median.age)


x=list('values')
fill = list('#00A08A')
color = list('#00A08A')
xlab=list('Population density',
          expression(atop('Population density,', paste('children 5 years and under'))),'% of pregnant women',
          '% of females', 'Median household size',
          'Median age')
bins = list(25)



p = pmap(list(df_list_ordered,fill, color, x, xlab, bins), cdf_hist)
p=p[[1]]+ p[[2]]+ p[[3]]+p[[4]]+ p[[5]]+p[[6]] + p[[6]]
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_demo_variable_distribution.pdf'), p, width = 14, height =9)

#maps for categorical variable 
clu_num_state = df_all %>%  group_by(shstate) %>%  summarise(n = n()) %>%
  mutate(NAME_1 = str_to_title(shstate), NAME_1 = ifelse(NAME_1 == 'Fct-Abuja', 'Federal Capital Territory',
                                                         ifelse(NAME_1 == 'Nasarawa', 'Nassarawa', NAME_1)))

#read in state shape file 
stateshp = readOGR(file.path(DataDir, "shapefiles","gadm36_NGA_shp"), layer ="gadm36_NGA_1",use_iconv=TRUE, encoding= "UTF-8")
state_sf = st_as_sf(stateshp)

state_df_2 = state_df %>% mutate(NAME_1 = str_to_title(state), NAME_1 = ifelse(NAME_1 == 'Fct Abuja', 'Federal Capital Territory',
                                                       ifelse(NAME_1 == 'Nasarawa', 'Nassarawa', NAME_1))) %>% dplyr::select(-state)
state_map = left_join(state_sf, clu_name_state, by =c('NAME_1'))
state_map$nun_cut = cut(state_map$n, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80), include.lowest = TRUE)
map=ggplot(state_map) +
  geom_sf(aes(fill = nun_cut))+ 
  brightness(viridis::scale_fill_viridis(option="E", discrete =TRUE, labels = c('0 - 10', '11 - 20', '21 - 30', '31 - 40', '41 - 50', '51 - 60', '61 - 70', '71 - 80' )), 0.8)+
  map_theme()+
  guides(fill = guide_legend(title= 'Number of clusters sampled per state', override.aes = list(size = 5)))
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_distribution_clusters_by_state.pdf'), map, width = 14, height =9)


#map by geopolitical region
clu_num_geo = df_all %>%  group_by(region) %>%  summarise(n = n())
clu_num_geo_ = left_join(df_all, clu_num_geo, by=c('region')) %>%  dplyr::select(region, shstate, n) %>%  
  mutate(NAME_1 = str_to_title(shstate), NAME_1 = ifelse(NAME_1 == 'Fct-Abuja', 'Federal Capital Territory',
                                                         ifelse(NAME_1 == 'Nasarawa', 'Nassarawa', NAME_1)))
geo_map = left_join(state_sf, clu_num_geo_, by =c('NAME_1'))

map=ggplot(geo_map) +
  geom_sf(aes(fill =as.factor(n)))+ 
  brightness(viridis::scale_fill_viridis(option="E", discrete = TRUE), 0.8)+
  map_theme()+
  guides(fill = guide_legend(title= 'Number of clusters sampled per geopolitical region', override.aes = list(size = 5)))
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_distribution_clusters_by_GPZ.pdf'), map, width = 14, height =9)


#correlation
colnames(demo_numeric)= c('Population density',
                                  'Population density,\n children 5 years and under','% of pregnant women',
                                  '% of females', 'Median household size',
                                  'Median age')
demo_numeric_reverse=demo_numeric[,order(ncol(demo_numeric):1)]

  
#replace nas with their means 
for(i in 1:ncol(demo_numeric_reverse)){
  demo_numeric_reverse[is.na(demo_numeric_reverse[,i]), i] = mean(demo_numeric_reverse[,i], na.rm = TRUE)
}

#correlation matrix 
corr = round(cor(demo_numeric_reverse), 1)

# Compute a matrix of correlation p-values
p.mat = cor_pmat(demo_numeric_reverse)


corr= ggcorrplot(corr, lab = TRUE, legend.title = "Correlation coefficient")+ 
  theme_corr()
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), 'correlation_coefficients_demo.pdf'), corr, width = 8, height = 5)



#relationship with malaria positives
dhs_demo_plot = cbind(demo_numeric, dhs_year, positives, num_tested) 
is.na(dhs_demo_plot['Population.density'])<- dhs_demo_plot['Population.density'] > 3000
is.na(dhs_demo_plot['U5.population.density'])<- dhs_demo_plot['U5.population.density'] > 10
is.na(dhs_demo_plot['Median.age'])<- dhs_demo_plot['Median.age'] > 30
is.na(dhs_demo_plot['Pregnant.women'])<- dhs_demo_plot['Pregnant.women'] > 30
dhs_demo_plot$rate = (dhs_demo_plot$positives/dhs_demo_plot$num_tested) 
dhs_demo_plot = dhs_demo_plot  %>%  pivot_longer(!c(dhs_year, positives, num_tested, rate),names_to='x_label', values_to='values')
df_list = split(dhs_demo_plot, dhs_demo_plot$x_label)
df_list_ordered = list(df_list$Population.density,df_list$U5.population.density,
                       df_list$Pregnant.women, df_list$Female.population, df_list$Household.size, df_list$Median.age)



#unadjusted positives chopped boundary 
plots = df_list_ordered %>%  {purrr::map2(., xlab, ~ggplot(.x,aes(x=values, y=positives))+
                                          geom_point(shape=42, size= 3, color = "dodgerblue3", alpha = 0.5) +
                                          geom_smooth(aes(fill = "Trend"), se = FALSE, color = "darkred", method = 'glm', method.args = list(family = poisson(link = "log")), formula = y ~ ns(x, 3))+
                                          geom_smooth(aes(color = "Confidence Interval"), fill = "darksalmon", linetype = 0, method = 'glm', method.args = list(family = poisson(link = "log")), formula = y ~ ns(x, 3))+
                                          theme_manuscript()+
                                          labs(x = .y, y ='malaria test positive')+
                                          guides(fill =FALSE, color =FALSE))}



p<- plots[[1]]+plots[[2]]+ plots[[3]]+ plots[[4]]+ plots[[5]]+ plots[[6]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_demo_distribution_age_chopped_bondary.png'), p, width = 8.5, height =4.5)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_demo_distribution_age_chopped_bondary.pdf'), p, width = 8.5, height =4.5)


dhs_demo_plot = cbind(demo_numeric, dhs_year, positives, num_tested) 
dhs_demo_plot$rate = (dhs_demo_plot$positives/dhs_demo_plot$num_tested) 
dhs_demo_plot = dhs_demo_plot  %>%  pivot_longer(!c(dhs_year, positives, num_tested, rate),names_to='x_label', values_to='values')
df_list = split(dhs_demo_plot, dhs_demo_plot$x_label)
df_list_ordered = list(df_list$Population.density,df_list$U5.population.density,
                       df_list$Pregnant.women, df_list$Female.population, df_list$Household.size, df_list$Median.age)



#unadjusted positives expanded boundary 
plots = df_list_ordered %>%  {purrr::map2(., xlab, ~ggplot(.x,aes(x=values, y=positives))+
                                            geom_point(shape=42, size= 3, color = "dodgerblue3", alpha = 0.5) +
                                            geom_smooth(aes(fill = "Trend"), se = FALSE, color = "darkred", method = 'glm', method.args = list(family = poisson(link = "log")), formula = y ~ ns(x, 3))+
                                            geom_smooth(aes(color = "Confidence Interval"), fill = "darksalmon", linetype = 0, method = 'glm', method.args = list(family = poisson(link = "log")), formula = y ~ ns(x, 3))+
                                            theme_manuscript()+
                                            labs(x = .y, y ='malaria test positive')+
                                            guides(fill =FALSE, color =FALSE))}



p<- plots[[1]]+plots[[2]]+ plots[[3]]+ plots[[4]]+ plots[[5]]+ plots[[6]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_demo_distribution_age_expanded_boundary.pdf'), p, width = 8.5, height =4.5)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_demo_distribution_age_expanded_boundary.png'), p, width = 8.5, height =4.5)



dhs_demo_plot = cbind(demo_numeric, dhs_year, positives, num_tested) 
is.na(dhs_demo_plot['Population.density'])<- dhs_demo_plot['Population.density'] > 3000
is.na(dhs_demo_plot['U5.population.density'])<- dhs_demo_plot['U5.population.density'] > 10
is.na(dhs_demo_plot['Median.age'])<- dhs_demo_plot['Median.age'] > 30
is.na(dhs_demo_plot['Pregnant.women'])<- dhs_demo_plot['Pregnant.women'] > 30
dhs_demo_plot$rate = (dhs_demo_plot$positives/dhs_demo_plot$num_tested) 
dhs_demo_plot = dhs_demo_plot  %>%  pivot_longer(!c(dhs_year, positives, num_tested, rate),names_to='x_label', values_to='values')
df_list = split(dhs_demo_plot, dhs_demo_plot$x_label)
df_list_ordered = list(df_list$Population.density,df_list$U5.population.density,
                       df_list$Pregnant.women, df_list$Female.population, df_list$Household.size, df_list$Median.age)


#rate chopped boundary 
plots = df_list_ordered %>%  {purrr::map2(., xlab, ~ggplot(.x,aes(x=values, y=rate))+
                                            geom_point(shape=42, size= 3, color = "dodgerblue3", alpha = 0.5) +
                                            geom_smooth(aes(fill = "Trend"), se = FALSE, color = "darkred", method = 'glm', method.args = list(family = quasipoisson(link = "log")), formula = y ~ ns(x, 3))+
                                            geom_smooth(aes(color = "Confidence Interval"), fill = "darksalmon", linetype = 0, method = 'glm', method.args = list(family = quasipoisson(link = "log")), formula = y ~ ns(x, 3))+
                                            theme_manuscript()+
                                            labs(x = .y, y ='malaria test positive rate')+
                                            guides(fill =FALSE, color =FALSE))}



p<- plots[[1]]+plots[[2]]+ plots[[3]]+ plots[[4]]+ plots[[5]]+ plots[[6]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p

ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_demo_rate_distribution_chopped_boundary.pdf'), p, width = 8.5, height =4.5)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_demo_rate_distribution_chopped_boundary.png'), p, width = 8.5, height =4.5)



#relationship with demo categorical variables 
#state data 
state_rship = glm(positives ~ as.factor(shstate)+ offset(log(num_tested)), data = df_all, family ='poisson')
state_dat =data.frame(coefficient=state_rship$coefficients, std_error = summary(state_rship)$coefficients[, 2])
state_dat$names = rownames(state_dat)
state_dat = state_dat %>%  mutate(lci = coefficient - std_error, uci = coefficient + std_error) %>% 
  filter(names !='(Intercept)') %>%  mutate(index = 1:36)

#effect plot - state  
xname <- expression(paste("Slope estimate"))
p = forest_fun(state_dat, '#644128', "#a56c56", xname, 1:36, state_dat$names)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_slope_estimate_state_comparison_demo.pdf'), p, width = 8.5, height =7)

#GPZ data 
GPZ_rship = glm(positives ~ as.factor(region)+ offset(log(num_tested)), data = df_all, family ='poisson')
GPZ_dat =data.frame(coefficient=GPZ_rship$coefficients, std_error = summary(GPZ_rship)$coefficients[, 2])
GPZ_dat$names = rownames(GPZ_dat)
GPZ_dat = GPZ_dat %>%  mutate(lci = coefficient - std_error, uci = coefficient + std_error) %>% 
  filter(names !='(Intercept)') %>%  mutate(index = 1:5)

#effect plot - GPZ 
p = forest_fun(GPZ_dat, "forestgreen", "darkseagreen", xname,1:5, GPZ_dat$names)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_slope_estimate_GPZ_comparison.pdf'), p, width = 8.5, height =4.5)




## --------------------------------------------------------------------------------------------------------------------------
### Behavioral factors variable distribution, cumulative distribution, correlation and relationship with malaria prevalence 
## --------------------------------------------------------------------------------------------------------------------------

#variable distribution and cumulative distribution 
df_behave = data.frame(`Net use` = df_all$net_use, `Child net use` = df_all$net_use_child, `Medical treatment for fever` =df_all$med_treat_fever,
                          `Effective fever treatment` = df_all$fever)

df_behave_long = df_behave %>%  pivot_longer(everything(),names_to='x_label', values_to='values')

df_list =split(df_behave_long, df_behave_long$x_label)
df_list_ordered = list(df_list$Net.use,df_list$Child.net.use, df_list$Medical.treatment.for.fever,
                       df_list$Effective.fever.treatment)


x=list('values')
fill = list('salmon')
color = list('salmon')
xlab=list('Net use',
          'Child net use','Medical treatment for fever',
          'Effective fever treatment')
bins = list(25)



p = pmap(list(df_list_ordered,fill, color, x, xlab, bins), cdf_hist)
p=p[[1]]+ p[[2]]+ p[[3]]+p[[4]] + plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p

ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_behavioral_variable_distribution.pdf'), p, width = 8, height =4.5)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_behavioral_variable_distribution.png'), p, width = 8, height =4.5)




#correlation 
colnames(df_behave)= c('Net use',
                       'Child net use','Medical treatment for fever',
                       'Effective fever treatment')
df_behave_reverse=df_behave[,order(ncol(df_behave):1)]


#replace nas with their means 
for(i in 1:ncol(df_behave_reverse)){
  df_behave_reverse[is.na(df_behave_reverse[,i]), i] = mean(df_behave_reverse[,i], na.rm = TRUE)
}

#correlation matrix 
corr = round(cor(df_behave_reverse), 1)

# Compute a matrix of correlation p-values
p.mat = cor_pmat(df_behave_reverse)


corr= ggcorrplot(corr, lab = TRUE, legend.title = "Correlation coefficient")+ 
  theme_corr()
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), 'correlation_coefficients_behave.pdf'), corr, width = 8.5, height = 4.5)



dhs_behave_plot = cbind(df_behave, region, positives, num_tested) 
dhs_behave_plot$rate = (dhs_behave_plot$positives/dhs_behave_plot$num_tested) 
dhs_behave_plot = dhs_behave_plot  %>%  pivot_longer(!c(region, positives, num_tested, rate),names_to='x_label', values_to='values')
df_list = split(dhs_behave_plot, dhs_behave_plot$x_label)
df_list_ordered = list(df_list$`Net use`,df_list$`Child net use`, df_list$`Medical treatment for fever`, df_list$`Effective fever treatment`)

#unadjusted positives
plots = df_list_ordered %>%  {purrr::map2(., xlab, ~ggplot(.x,aes(x=values, y=positives))+
                                            geom_point(shape=42, size= 3, color = "purple2", alpha = 0.5) +
                                            geom_smooth(aes(fill = "Trend"), se = FALSE, color = "deeppink4", method = 'glm', method.args = list(family = poisson(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))+
                                            geom_smooth(aes(color = "Confidence Interval"), fill = "deeppink", linetype = 0, method = 'glm', method.args = list(family = poisson(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))+
                                            theme_manuscript()+
                                            labs(x = .y, y ='malaria test positive')+
                                            guides(fill =FALSE, color =FALSE))}

p<- plots[[1]]+plots[[2]]+ plots[[3]]+ plots[[4]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_behavioral_variable_bivariate_positives.pdf'), p, width = 8.5, height =4.5)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_behavioral_variable_bivariate_positives.png'), p, width = 8.5, height =4.5)


#rate
plots= df_list_ordered %>%  {purrr::map2(., xlab, ~ggplot(.x,aes(x=values, y=rate))+
                                                 geom_point(shape=42, size= 3, color = "purple2", alpha = 0.5) +
                                                 geom_smooth(aes(fill = "Trend"), se = FALSE, color = "deeppink4", method = 'glm', method.args = list(family = quasipoisson(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))+
                                                 geom_smooth(aes(color = "Confidence Interval"), fill = "deeppink", linetype = 0, method = 'glm', method.args = list(family = quasipoisson(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))+
                                                 theme_manuscript()+
                                                 labs(x = .y, y ='malaria test positive rate')+
                                                 guides(fill =FALSE, color =FALSE))}

p= plots[[1]]+plots[[2]]+ plots[[3]]+ plots[[4]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_behavioral_variable_rate_bivariate_positives.pdf'), p, width = 8.5, height =4.5)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_behavioral_variable_rate_bivariate_positives.png'), p, width = 8.5, height =4.5)


#region adjustment 
plots = df_list_ordered %>%  {purrr::map2(., xlab, ~ggplot(.x,aes(x=values, y=positives, color=region, group = region, fill=region))+
                                            geom_point(shape=42, size= 5, alpha = 0.7) +
                                            geom_smooth(method = 'glm', method.args = list(family = poisson(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))+
                                            theme_manuscript()+
                                            labs(x = .y, y ='malaria test positive')+
                                            guides(fill =FALSE))}

p= plots[[1]]+plots[[2]]+ plots[[3]]+ plots[[4]]
p = p+ plot_layout(guides = "collect")+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'), legend.position = 'bottom')
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_behavioral_variable_bivariate_positives_region.pdf'), p, width = 8.5, height =7)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_behavioral_variable_bivariate_positives_region.png'), p, width = 8.5, height =7)





## --------------------------------------------------------------------------------------------------------------------------
### Accessibility factors variable distribution, cumulative distribution, correlation and relationship with malaria prevalence 
## --------------------------------------------------------------------------------------------------------------------------

#variable distribution and cumulative distribution 
df_access = data.frame(`motor_travel_healthcare` = df_all$motorized_travel_healthcare_2019_2000m)
hist=ggplot(df_access, aes(x =motor_travel_healthcare))+geom_histogram(alpha = 0.4, position="identity", bins=30)
max_y=max(ggplot_build(hist)$data[[1]]$count)
p=ggplot(df_access, aes(x=motor_travel_healthcare))+
    geom_histogram(fill='darkturquoise', color='darkturquoise', alpha = 0.4, position="identity", bins = 30) +
    stat_ecdf(aes_(y =bquote(..y..* .(max_y)), color ='darkturquoise'))+
    scale_y_continuous(name= 'Count', sec.axis=sec_axis(trans = ~./max_y, name = 'Cumulative percent', labels = function(x) format(x *100, digits=2, nsmall=0)))+
    theme_manuscript()+theme(legend.position = 'none')+
    xlab(expression(atop('Motorized travel time to health care', paste('in minutes, 2019'))))
  

dhs_access_plot = cbind(df_access, region, positives, num_tested) 
dhs_access_plot$rate = (dhs_access_plot$positives/dhs_access_plot$num_tested) 
dhs_access_plot = dhs_access_plot  %>%  pivot_longer(!c(region, positives, num_tested, rate),names_to='x_label', values_to='values')
df_list = split(dhs_access_plot, dhs_access_plot$x_label)


xlab=list('Motorized travel time to health care')

#unadjusted positives
plots = df_list %>%  {purrr::map2(., xlab, ~ggplot(.x,aes(x=values, y=positives))+
                                            geom_point(shape=42, size= 3, color = "turquoise4", alpha = 0.5) +
                                            geom_smooth(aes(fill = "Trend"), se = FALSE, color = "tan4", method = 'glm', method.args = list(family = poisson(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))+
                                            geom_smooth(aes(color = "Confidence Interval"), fill = "tan3", linetype = 0, method = 'glm', method.args = list(family = poisson(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))+
                                            theme_manuscript()+
                                            labs(x = expression(atop('Motorized travel time to health care', paste('in minutes, 2019'))), y ='malaria test positive')+
                                            guides(fill =FALSE, color =FALSE))}


p_all=p +plots[[1]]



#chopped off boundaries 
dhs_access_plot = cbind(df_access, region, positives, num_tested) 
is.na(dhs_access_plot['motor_travel_healthcare'])<- dhs_access_plot['motor_travel_healthcare'] > 25
dhs_access_plot$rate = (dhs_access_plot$positives/dhs_access_plot$num_tested) 
dhs_access_plot = dhs_access_plot  %>%  pivot_longer(!c(region, positives, num_tested, rate),names_to='x_label', values_to='values')
df_list = split(dhs_access_plot, dhs_access_plot$x_label)

plots = df_list %>%  {purrr::map2(., xlab, ~ggplot(.x,aes(x=values, y=positives))+
                                    geom_point(shape=42, size= 3, color = "turquoise4", alpha = 0.5) +
                                    geom_smooth(aes(fill = "Trend"), se = FALSE, color = "tan4", method = 'glm', method.args = list(family = poisson(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))+
                                    geom_smooth(aes(color = "Confidence Interval"), fill = "tan3", linetype = 0, method = 'glm', method.args = list(family = poisson(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))+
                                    theme_manuscript()+
                                    labs(x = expression(atop('Motorized travel time to health care', paste('in minutes, 2019 (x-axis is limited values >=25)'))), y ='malaria test positive')+
                                    guides(fill =FALSE, color =FALSE))}

p_all2 = p_all + plots[[1]]

plots = df_list %>%  {purrr::map2(., xlab, ~ggplot(.x,aes(x=values, y=rate))+
                                    geom_point(shape=42, size= 3, color = "turquoise4", alpha = 0.5) +
                                    geom_smooth(aes(fill = "Trend"), se = FALSE, color = "tan4", method = 'glm', method.args = list(family = poisson(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))+
                                    geom_smooth(aes(color = "Confidence Interval"), fill = "tan3", linetype = 0, method = 'glm', method.args = list(family = poisson(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))+
                                    theme_manuscript()+
                                    labs(x = expression(atop('Motorized travel time to health care', paste(' in minutes, 2019 (x-axis is limited values >=25)'))) , y ='malaria test positive rate')+
                                    guides(fill =FALSE, color =FALSE))}

p_all3 = p_all2 +  plots[[1]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))

ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_accessibility_variable_distribution_bivariate.pdf'), p_all3, width = 8.2, height =4.5)




## --------------------------------------------------------------------------------------------------------------------------
### Environmental factors variable distribution, cumulative distribution, correlation and relationship with malaria prevalence 
## --------------------------------------------------------------------------------------------------------------------------
#variable distribution and cumulative distribution 
df_env = data.frame(`Precipitation` = df_all$precipitation_monthly_0m, `Temperature` = df_all$temperature_monthly_0m, `Surface soil moisture` =df_all$soil_wetness_0m,
                       `Distance to water bodies` = df_all$dist_water_bodies_0m, `Elevation` = df_all$elevation_1000m, `Enhanced Vegetation Index`= df_all$EVI_0m)

df_env_long = df_env %>%  pivot_longer(everything(),names_to='x_label', values_to='values')

df_list =split(df_env_long, df_env_long$x_label)
df_list_ordered = list(df_list$Precipitation,df_list$Temperature, df_list$Surface.soil.moisture,df_list$Distance.to.water.bodies,
                       df_list$Elevation, df_list$Enhanced.Vegetation.Index)

x=list('values')
fill = list('dodgerblue3')
color = list('dodgerblue3')
xlab=list(expression(atop('Precipitation', paste('(meters depth)'))), expression(atop('Temperature', paste('(°C)'))),expression(atop('Surface soil', paste('moisture (GSM)'))),expression(atop('Distance to water', paste('bodies (meters)'))), expression(atop('Elevation', paste('(meters)'))),
          expression(atop('Enhanced Vegetation',  paste('Index'))))
bins = list(25)



p = pmap(list(df_list_ordered,fill, color, x, xlab, bins), cdf_hist)
p=p[[1]]+ p[[2]]+ p[[3]]+p[[4]] + p[[5]]+ p[[6]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_environmental_variable_distribution.pdf'), p, width = 8.5, height =5)



#correlation 
colnames(df_behave)= c('Precipitation',
                       'Temperature','Surface soil moisture',
                       'Distance to water bodies', 'Elevation', "Enhanced Vegetation Index")
df_env_reverse=df_env[,order(ncol(df_env):1)]


#replace nas with their means 
for(i in 1:ncol(df_env)){
  df_env_reverse[is.na(df_env_reverse[,i]), i] = mean(df_env_reverse[,i], na.rm = TRUE)
}

#correlation matrix 
corr = round(cor(df_env_reverse), 1)

# Compute a matrix of correlation p-values
p.mat = cor_pmat(df_env_reverse)


corr= ggcorrplot(corr, lab = TRUE, legend.title = "Correlation coefficient")+ 
  theme_corr()
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), 'correlation_coefficients_environmental.pdf'), corr, width = 8.5, height = 4.5)


## ----------------------------------------------------------------
### Read in computed DHS cluster data and generate related figures - will bring this up later during code cleaning   
## ----------------------------------------------------------------
dhs$positives_prop = round(dhs$positives/dhs$child_6_59_tested_malaria, 1)
summary(dhs$positives_prop)

#figure 1
p1 = igv.lm.point(dhs$num_child_6_59, dhs$child_6_59_tested_malaria,dhs$dhs_year,  "Survey year", 'Number of children 6 - 59 months', 'Number of children 6 - 59 months \n tested for malaria')
p1= p1 +geom_smooth(method=lm, color = "black")+ theme(legend.position = 'none')
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_Figure_1_sample_overview.pdf'), p1, width = 13, height = 9)



#values used in manuscript texts 
table(dhs$dhs_year)
nrow(!is.na(subset(dhs, dhs_year ==2010 & num_child_6_59 >=20)))
nrow(!is.na(subset(dhs, dhs_year ==2015 & num_child_6_59 >=20)))
nrow(!is.na(subset(dhs, dhs_year ==2018 & num_child_6_59 >=20)))

#figure 2a
df_tested = data.frame(values = dhs$child_6_59_tested_malaria, category = 'tested')
df_positives = data.frame(values = dhs$positives, category = 'positives')
df_all = rbind(df_tested, df_positives)
p2 = hist_fun(df_all, df_all$values, df_all$category, 'Number of children 6 - 59 months', 'Count', c("Positive tests", "Tested"))



#figure 2b
#examine the number of children tested 
p3 = igv.lm.point(dhs$child_6_59_tested_malaria, dhs$positives, dhs$dhs_year, 'Survey year', 'Number of children 6 - 59 months \n tested for malaria', 'Number of positive tests' )
p3_ = p3 + geom_abline(slope=1, intercept=c(0,0), size = 0.9) +geom_smooth(method=lm, color = "black")



#figure 2c
#load spatial points
sf18 = st_read(file.path(DHSData, "Downloads", "NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp"),) 
sf15 = st_read(file.path(DHSData, "Downloads", "NG_2015_MIS_06192019/NGGE71FL/NGGE71FL.shp"),) 
sf10 = st_read(file.path(DHSData, "Downloads", "NG_2010_MIS_06192019/NGGE61FL/NGGE61FL.shp"),) 
sf_all = rbind(sf18, sf15, sf10) %>%filter(URBAN_RURA == "U") %>%  rename(v001 = DHSCLUST)


#data wrangling
dhs = dhs %>%  dplyr::select(v001, positives, child_6_59_tested_malaria, DHSYEAR=dhs_year)
map = sf_all %>% left_join(dhs, by=c('v001', 'DHSYEAR'))  %>%  filter(LATNUM != 0) 
map$positives_prop = round(map$positives/map$child_6_59_tested_malaria, 1)
map$positives_cut = cut(map$positives_prop, breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), include.lowest = TRUE)
df_count = map %>% dplyr::select(positives_cut) %>%  group_by(positives_cut) %>%  summarize(`Count` = n())


#big map 
map_big = gmap_fun(state_sf, map, labels=c(paste0('0 - 0.2',  ' (', df_count$Count[[1]], ')'), 
                                           paste0('0.3 - 0.4',  ' (', df_count$Count[[2]], ')'), paste0('0.5 - 0.6',  ' (', df_count$Count[[3]], ')'), 
                                           paste0('0.7 - 0.8',  ' (', df_count$Count[[4]], ')'), paste0('0.9 - 1.0',  ' (', df_count$Count[[5]], ')'), 
                                           'Missing data'),
                   map$positives_cut, 'Test positivity rate (overall count)')


#Lagos 
df_lagos = dplyr::filter(state_sf, (NAME_1 %in% c('Lagos')))
map_lagos = dplyr::filter(map, (ADM1NAME %in% c('LAGOS')))
map_lag = gmap_fun(df_lagos, map_lagos, labels=c('0 - 0.2', '0.3 - 0.4', '0.5 - 0.6', '0.7 - 0.8', '0.9 - 1.0', 'Missing data'),
                   map_lagos$positives_cut, 'Test positivity rate')
map_lag = map_lag + theme(legend.position = 'none', panel.border = element_rect(colour = "black", fill=NA, size=0.5))+ xlab('Lagos')

#Anambra 
df_anambra = dplyr::filter(state_sf, (NAME_1 %in% c('Anambra')))
map_anambra = dplyr::filter(map, (ADM1NAME %in% c('ANAMBRA')))
map_anam = gmap_fun(df_anambra, map_anambra, labels=c('0 - 0.2', '0.3 - 0.4', '0.5 - 0.6', '0.7 - 0.8', '0.9 - 1.0', 'Missing data'),
                    map_anambra$positives_cut, 'Test positivity rate')
map_anam = map_anam + theme(legend.position = 'none', panel.border = element_rect(colour = "black", fill=NA, size=0.5))+ xlab('Anambra')


#rivers 
df_rivers = dplyr::filter(state_sf, (NAME_1 %in% c('Rivers')))
map_rivers = dplyr::filter(map, (ADM1NAME %in% c('RIVERS')))
map_riv = gmap_fun(df_rivers, map_rivers, labels=c('0 - 0.2', '0.3 - 0.4', '0.5 - 0.6', '0.7 - 0.8', '0.9 - 1.0', 'Missing data'),
                   map_rivers$positives_cut, 'Test positivity rate')
map_riv = map_riv + theme(legend.position = 'none', panel.border = element_rect(colour = "black", fill=NA, size=0.5))+ xlab('Rivers')


patch1 = ((map_lag /(map_anam + map_riv))| map_big)+ plot_layout(ncol = 2)
patch2 = (p2+ p3_)/ patch1 + plot_layout(nrow = 2)+  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold', size = 16))
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_Figure_2_low_positivity_viz.pdf'), patch2, width = 13, height = 9)


map_low_values = map %>% na.omit(positives) %>%  filter(positives_prop == 0) %>%  group_by(ADM1NAME) %>%  summarise(n())
map_low = map_low_values %>%  filter(`n()` >15)
cluster = map %>% na.omit(positives)



#figure 3
#trends by DHS year 
trend_data= dhs %>%  mutate(positives_prop = positives/child_6_59_tested_malaria)
table(trend_data$first_interview_month)

trend_data$month_year = paste0(trend_data$first_interview_month, "_", trend_data$dhs_year)
table(trend_data$month_year)

trend_data_10 = trend_data[trend_data$first_interview_month ==10,]
p_all_10 = gdensity_fun(trend_data_10, trend_data_10$positives_prop, trend_data_10$dhs_year, "Survey year", 
                        'Test positivity rate for clusters sampled in october', 'Density')

trend_data_11 = trend_data[trend_data$first_interview_month ==11,]
p_all_11 = gdensity_fun(trend_data_11, trend_data_11$positives_prop, trend_data_11$dhs_year, "Survey year", 
                        'Test positivity rate for clusters sampled in November', 'Density')


trend_data_12 = trend_data[trend_data$first_interview_month ==12,]
p_all_12 = gdensity_fun(trend_data_12, trend_data_12$positives_prop, trend_data_12$dhs_year, "Survey year", 
                        'Test positivity rate for clusters sampled in November', 'Density')

all_plots = p_all_10 / p_all_11 / p_all_12 +  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold', size = 16))
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_Figure_3_malaria_tests_positivity_trends.pdf'), all_plots, width = 13, height = 9)
data = data.frame(pos =trend_data_12[trend_data_12$dhs_year == 2010,'positives_prop'])
data_ = data %>%  filter(pos == 0)












































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

cdf_hist = function(df, fill,color, x, xlab){
  hist=ggplot(df, aes(x =.data[[x]]))+geom_histogram(alpha = 0.4, position="identity")
  max_y=max(ggplot_build(hist)$data[[1]]$count)
  ggplot(df, aes(.data[[x]]))+
    geom_histogram(fill=fill, color= color, alpha = 0.4, position="identity") +
    stat_ecdf(aes_(y =bquote(..y..* .(max_y)), color =color))+
    scale_y_continuous(name= 'Count', sec.axis=sec_axis(trans = ~./max_y, name = 'Cumulative percentage', labels = percent))+
    theme_manuscript()+theme(legend.position = 'none')+
    xlab(xlab)
}


x=list('values')
fill = list('#8971B3')
color = list('#8971B3')
xlab=list('% with post-primary education',
          '% in the rich wealth quintiles','% in homes with improved flooring',
          '% in homes with a metal or zinc roof', '% in homes with an improved wall type',
          '% living in improved housing (2000)',
          '% living in improved housing (2015)')




p = pmap(list(df_list_ordered,fill, color, x, xlab), cdf_hist)
all_p=p[[1]]+ p[[2]]+ p[[3]]+p[[4]]+ p[[5]]+p[[6]]+p[[7]]
all_p
ggsave(paste0(ResultDir, '/updated_figures', Sys.Date(), 'social_variable_distribution.pdf'), all_p, width =13, height =9)

#correlation 

dhs_social_ordered = data.frame(`improved housing in 2015` = df_all$housing_2015_4000m, 
                                `improved housing in 2000` =df_all$housing_2000_4000m, `Improved wall` = df_all$wall_type,
                                `Improved roofing materials` = df_all$roof_type,  `Improved flooring` =df_all$floor_type,Wealth = df_all$wealth, 
                                `Educational attainment` = df_all$edu_a) %>%  mutate(improved.housing.in.2000 = improved.housing.in.2000*100,
                                                                                            improved.housing.in.2015= improved.housing.in.2015*100)
#replace nas with their means 
for(i in 1:ncol(dhs_social_ordered)){
  dhs_social_ordered[is.na(dhs_social_ordered[,i]), i] = mean(dhs_social_ordered[,i], na.rm = TRUE)
}

#correlation matrix 
corr = round(cor(dhs_social_ordered), 1)

# Compute a matrix of correlation p-values
p.mat = cor_pmat(dhs_social_ordered)


corr_social= ggcorrplot(corr, lab = TRUE, legend.title = "Correlation coefficient")+ 
  theme_corr()
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), 'correlation_coefficients_social.pdf'), corr_social, width = 13, height = 9)

#relationship with malaria positives
positives = dhs$positives 
dhs_year=dhs$dhs_year
num_tested =dhs$child_6_59_tested_malaria
dhs_social_plot = cbind(dhs_social, dhs_year, positives, num_tested)
dhs_social_plot = dhs_social_plot  %>%  pivot_longer(!c(dhs_year, positives, num_tested),names_to='x_label', values_to='values')
df_list = split(dhs_social_plot, dhs_social_plot$x_label)
df_list_ordered = list(df_list$Educational.attainment,df_list$Wealth,
                       df_list$Improved.flooring, df_list$Improved.roofing.materials, df_list$Improved.wall, df_list$improved.housing.in.2000,
                       df_list$improved.housing.in.2015)


#create values for geom_smooth
for(i in 1:length(df_list_ordered)){
    mod= glm(positives ~ values +offset(log(num_tested)), data=df_list_ordered[[i]], family = "poisson")
    mod.p =predict(mod, df_list_ordered[[1]],type = "link", se.fit = TRUE)#type = "link", se.fit = TRUE
    critval <- 1.96 ## approx 95% CI
    upr <- mod.p$fit + (critval * mod.p$se.fit)
    lwr <- mod.p$fit - (critval * mod.p$se.fit)
    fit <- mod.p$fit
    fit2 <- mod$family$linkinv(fit)
    upr2 <- mod$family$linkinv(upr)
    lwr2 <- mod$family$linkinv(lwr)
    df_list_ordered[[i]]$lwr <- round(lwr2, 0) 
    df_list_ordered[[i]]$upr <- round(upr2, 0) 
    df_list_ordered[[i]]$fit <- round(fit2, 0) 
    
}


ggplot(data=df_list_ordered[[1]]) + geom_point(aes(x=values, y = positives), size = 2, colour = "blue") +  
  geom_smooth(data=df_list_ordered[[1]], aes(x=values, y=fit, ymin=lwr, ymax=upr), size = 1.5, 
              colour = "red", se = TRUE, stat = "smooth", method = 'glm', method.args=list(family ='poisson'))



plots = df_list_ordered %>%  {map2(., xlab, ~ggplot(.x)+
                                   geom_point(mapping=aes(x=values, y=positives), shape=42, size= 5, color = "#f64b77", alpha = 0.5, position ='jitter') +
                                     geom_smooth(.x, mapping=aes(x=values, y=fit, ymin=lwr, ymax=upr, fill = "Trend"), stat="smooth", se = FALSE, color = "#644128", method = 'glm', method.args=list(family ='poisson'))+
                                  geom_smooth(.x, mapping=aes(x=values, y=fit, ymin=lwr, ymax=upr, color = "Confidence Interval"), stat="smooth", fill = "#a56c56", linetype = 0, method = 'glm', method.args=list(family ='poisson'))+
                                   theme_manuscript()+
                                   labs(x = .y, y ='malaria positives')+
                                     guides(fill =FALSE, color =FALSE))}

social_p<- plots[[1]]+plots[[2]]+ plots[[3]]+ plots[[4]]+ plots[[5]]+ plots[[6]]+ plots[[7]]
social_p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_bivariate_social.pdf'), social_p, width = 14, height =9)


#effect list for social variables 

effect_list = list()

for(i in 1:length(df_list_ordered)){
  
  m = glm(positives ~ values +offset(log(num_tested)), data=df_list_ordered[[i]], family = "poisson")
  variable = unique(df_list_ordered[[i]]$x_label)
  slope = m$coefficients[[2]]
  std_error = summary(m)$coefficients[, 2][[2]]
  lci = slope - std_error
  uci = slope + std_error
  index = i
  dat = data.frame(index =i, beta = slope, LCI = lci, UCL = uci, variable=variable)
  effect_list = append(effect_list, list(dat))
}

all_effect =plyr::ldply(effect_list)

#effect plot 
xname <- expression(paste("Slope estimate"))

p <- ggplot(data=all_effect, aes(y=index, x=beta, xmin=LCI, xmax=UCL))+ 
  geom_point(shape = 15, color='#644128', size = 3)+ 
  geom_errorbarh(height=.1, color ="#a56c56")+
  scale_x_continuous(limits=c(-0.07,0.01), breaks = c(-0.07,-0.06, -0.05, -0.04, -0.03, -0.02, -0.01, 0, 0.01), name=xname)+
  scale_y_continuous(name = "", breaks=1:7, labels = all_effect$variable, trans = 'reverse')+
  geom_vline(xintercept=0, color='black', linetype="dashed", alpha=.5)+
  theme_manuscript()
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_slope_estimate_social.pdf'), p, width = 14, height =9)









plots_by_region = df_list_ordered %>%  {map2(., xlab, ~ggplot(.x, aes(x=values, y=log(positives), group=dhs_year))+
                                               geom_point(aes(color =dhs_year, fill=dhs_year), alpha=0.7, shape=21) +
                                               geom_smooth(aes(color =dhs_year, fill=dhs_year), method = 'glm', method.args=list(family ='poisson'))+
                                               theme_manuscript()+
                                               labs(x = .y, y ='positive malaria test'))}

plots_by_region[[1]]+plots_by_region[[2]]+ plots_by_region[[3]]+ plots_by_region[[4]]+ plots_by_region[[5]]+ plots_by_region[[6]]+ plots_by_region[[7]]


## ----------------------------------------------------------------
### Examining correlations among numeric variables 
## ----------------------------------------------------------------
#create the demographic correlation dataset 
dhs_demo = data.frame(`Population density` = df_all$pop_density_0m, `U5 population density` = df_all$pop_den_U5_FB_4000m, `Pregnant women` =df_all$preg_women,
                      `Female population` = df_all$all_female_sex, `Median household size` = df_all$household_size, `Median age` =df_all$median_age)

#replace nas with their means 
for(i in 1:ncol(dhs_demo)){
  dhs_demo[is.na(dhs_demo[,i]), i] = mean(dhs_demo[,i], na.rm = TRUE)
}

#correlation matrix 
corr = round(cor(dhs_demo), 1)

# Compute a matrix of correlation p-values
p.mat = cor_pmat(dhs_demo)


corr_demo= ggcorrplot(corr, lab = TRUE, legend.title = "Correlation coefficient")+ 
  theme_corr()

#create the socioeconomic correlation dataset 
dhs_social = data.frame(`Educational attainment` = df_all$edu_a, Wealth = df_all$wealth, `Improved flooring` =df_all$floor_type,
                        `Improved roofing materials` = df_all$roof_type, `Improved wall` = df_all$wall_type, `improved housing in 2000` =df_all$housing_2000_4000m,
                        `improved housing in 2015` = df_all$housing_2015_4000m)





#create the behavioral correlation dataset 
dhs_behvioral = data.frame(`Net use` = df_all$net_use, `Children's net use` =df_all$net_use_child, `Medical treatment` =df_all$med_treat_fever,
                      `Effective fever treatment` = df_all$ACT_use_U5)

#replace nas with their means 
for(i in 1:ncol(dhs_behvioral)){
  dhs_behvioral[is.na(dhs_behvioral[,i]), i] = mean(dhs_behvioral[,i], na.rm = TRUE)
}

#correlation matrix 
corr = round(cor(dhs_behvioral), 1)

# Compute a matrix of correlation p-values
p.mat = cor_pmat(dhs_behvioral)


corr_behvioral= ggcorrplot(corr, lab = TRUE, legend.title = "Correlation coefficient")+ 
  theme_corr()


#create the accessibility correlation dataset 
dhs_access = data.frame(`Motorized travel to health facility` =df_all$motorized_travel_healthcare_2019_2000m, 
                           `Minutes to the nearest city` = df_all$minutes_nearest_city_1000m)

#replace nas with their means 
for(i in 1:ncol(dhs_access)){
  dhs_access[is.na(dhs_access[,i]), i] = mean(dhs_access[,i], na.rm = TRUE)
}

#correlation matrix 
corr = round(cor(dhs_access), 1)

# Compute a matrix of correlation p-values
p.mat = cor_pmat(dhs_access)


corr_access= ggcorrplot(corr, lab = TRUE, legend.title = "Correlation coefficient")+ 
  theme_corr()


#create the environment correlation dataset 
dhs_environment = data.frame(Precipitation =df_all$precipitation_monthly_0m, Temperature = df_all$temperature_monthly_0m,
                        `Surface soil moisture` = df_all$soil_wetness_0m, `Distance to water bodies` = df_all$dist_water_bodies_0m,
                        Elevation = df_all$elevation_1000m, `Enhanced vegetation index` = df_all$EVI_0m)

#replace nas with their means 
for(i in 1:ncol(dhs_environment)){
  dhs_environment[is.na(dhs_environment[,i]), i] = mean(dhs_environment[,i], na.rm = TRUE)
}

#correlation matrix 
corr = round(cor(dhs_environment), 1)

# Compute a matrix of correlation p-values
p.mat = cor_pmat(dhs_environment)


corr_environment= ggcorrplot(corr, lab = TRUE, legend.title = "Correlation coefficient")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        axis.text.x = element_text(size = 16, color = "black"), 
        axis.text.y = element_text(size = 16, color = "black"))

## ----------------------------------------------------------------
### Read in computed DHS cluster data and generate related figures  
## ----------------------------------------------------------------

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

#read in state shape file 
stateshp = readOGR(file.path(DataDir, "shapefiles","gadm36_NGA_shp"), layer ="gadm36_NGA_1",use_iconv=TRUE, encoding= "UTF-8")
state_sf = st_as_sf(stateshp)


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





## ----------------------------------------------------------------
### bivariate plots for DHS and geospatial variables   
## ----------------------------------------------------------------
df = read.csv(file.path(CsvDir, "all_DHS_variables_urban_malaria.csv"), header = T, sep = ',') 
df= df %>%  mutate(positives_prop = positives/child_6_59_tested_malaria)
df$edu_cut = cut(df$edu_a, breaks=c(0, 10, 20, 30, 40, 50, 60,70, 80,90, 100), include.lowest=TRUE)


ggplot(df) +
  geom_histogram(aes(x = positives_prop), bins = 4, colour = "black", fill = "white") +
  facet_wrap(~edu_cut)

df_check = df %>%  group_by(edu_cut) %>%  summarize(mean_positives=mean(positives, na.rm=TRUE), sd_positives=sd(positives, na.rm=TRUE),
                                                     mean_positives_prop=mean(positives_prop, na.rm=TRUE), sd_positives_prop=sd(positives_prop, na.rm=TRUE), n = n())

ggplot(df, aes(floor_type, log(positives))) +
  geom_point() +
  geom_smooth()+
  facet_wrap(~region)




























#___________________________________________ Demo Plots______________________________________________________

#tools

labels_data = list("Educational attainment", "Floor type", "Household size", "Housing quality", "Mean age", 
                    "Median age", "Net use", "Roof type", "Under five population" ,"Wall type",
                    "Wealth", "Fever", "Pregnant women", "Female population", "Under five Female population", 
                    "Under five net use", "Malaria prevalence", "Fever treatment", "Under five ACT use","Travel time to city", 
                    "Building density", "Dominant vector", "Elevation", "Time to travel one metre 2015","Housing quality 2000", 
                    "Housing quality 2015", "Time to travel one metre 2019","Travel time to healthcare", "Under five population density (FB)", "Population density",
                    "Secondary vector", "Temperature", "Time to walk one metre", "Walk time to healthcare")

xlab_data = list("percent", "Percentage of reproductive age women with secondary or higher educational attainment", "number", "percent", "number", 
                  "number", "percent", "percent","percent" ,"percent", 
                  "percent", "percent", "percent", "percent", "percent",
                  "percent", "percent", "percent", "percent","Mean minutes", 
                  "Mean", "Mean", "Mean", "Mean minutes", "percent",
                  "percent", "Mean minutes","Mean minutes", "Mean", "Mean",
                  "Mean", "Mean", "Mean minutes", "Mean minutes")

colr_data = rep(c("violetred", "darkorchid","green4","tan4","turquoise"), times =c(9,9,5,7,3))


#histogram

hist_fun2 =function(df, xmin, xmax){
  p= ggplot(df, aes_string(x=names(df)[var_list[[2]]])) + 
    geom_histogram(bins = 30, alpha = 0.7, position="identity", color = "violetred4", fill = colr_data[colr_list[[1]]])+
    theme_manuscript()+ 
    labs (title = labels_data[label_list[[2]]], x = "values") +
    xlab(xlab_data[2]) +
    ylab("Count")+
    scale_y_continuous(expand = c(0.03, 0))+
    scale_x_continuous(limits = c(xmin, xmax), expand = c(0.01, 0))
  
}

hist_fun2(clu_df_cont, 0, 100)


#map
df_10_18_fin = df_10_18_fin %>% filter(LONGNUM > 0.000000)

df_10_18_fin$ed_cut = cut(df_10_18_fin$edu_a, breaks=c(0,20, 40, 60, 80, 100, NA), include.lowest = TRUE)
p= ggplot() + 
  geom_sf(data =state_sf, color='lightgrey')+
  geom_point(df_10_18_fin, mapping = aes(x = LONGNUM, y = LATNUM, color = ed_cut), size =8, alpha =0.7)+
  #viridis::scale_fill_viridis(option="plasma", discrete=TRUE, labels=labels, na.value ='grey', limits=c('[0,20]', '(20,40]', '(40,60]', '(60,80]', '(80,100]', NA)) +
  theme_bw()+
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA), 
        legend.title.align=0.5,
        legend.title=element_text(size=16, colour = 'black'), 
        legend.text =element_text(size = 16, colour = 'black'),
        legend.key.height = unit(0.65, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  labs (title = labels_data[label_list[[2]]], x = "values", size=20) +
  xlab("") +
  ylab("")+
  guides(color=guide_legend(""))+
  scale_color_manual(values = c("#00AFBB", "burlywood4", "#FC4E07", "#E7B800", "#00AFBB", "lightgrey"), 
                     labels = c("0 - 20", "20 - 40", "40 - 60", "60 - 80", "80 - 100", "NA"))

p


#______________________________
df_10_18_fin = df_10_18_fin %>% filter(LONGNUM > 0.000000)

gmap_fun = function(polygon_name, point_data, labels, fill, legend_title){
  ggplot(polygon_name) +
    geom_sf(color='lightgrey')+
    geom_point(data = point_data,
               aes(fill=fill,  geometry = geometry),
               stat = "sf_coordinates", alpha = 0.45, size=3, shape=21
    ) +
    #viridis::scale_fill_viridis(option='C', discrete=TRUE, labels=labels, na.value ='grey', limits=c('[0,20]', '(20,40]', '(40,60]', '(60,80]', '(80,100]', NA)) +
    map_theme() + 
    guides(fill = guide_legend(title=legend_title, override.aes = list(size = 5)))+
    xlab("")+
    ylab("")
}




#________________________________all coveriates histogram plots 200m buffer_________________________________


clu_df_cont = clu_df_10_18[ , -which(names(clu_df_10_18) %in% c("shstate", "region", "X"))]
clu_df_cont$y = ifelse(clu_df_cont$p_test < 0.1, 0,1) %>% (as.numeric)
clu_df_cont= na.omit(clu_df_cont)

labels_data = list("Education", "Floor type", "Household size", "Housing quality", "Mean age", 
                    "Median age", "Net use", "Roof type", "Under five population" ,"Wall type",
                    "Wealth", "Fever", "Pregnant women", "Female population", "Under five Female population", 
                    "Under five net use", "Malaria prevalence", "Fever treatment", "Under five ACT use","Travel time to city", 
                    "Building density", "Dominant vector", "Elevation", "Time to travel one metre 2015","Housing quality 2000", 
                    "Housing quality 2015", "Time to travel one metre 2019","Travel time to healthcare", "Under five population density (FB)", "Population density",
                    "Secondary vector", "Temperature", "Time to walk one metre", "Walk time to healthcare")

xlab_data = list("percent per cl", "percent", "number", "percent", "number", 
                  "number", "percent", "percent","percent" ,"percent", 
                  "percent", "percent", "percent", "percent", "percent",
                  "percent", "percent", "percent", "percent","Mean minutes", 
                  "Mean", "Mean", "Mean", "Mean minutes", "percent",
                  "percent", "Mean minutes","Mean minutes", "Mean", "Mean",
                  "Mean", "Mean", "Mean minutes", "Mean minutes")

colr_data = rep(c("orangered", "dodgerblue", "darkorchid","green4","tan4","turquoise"), times =c(1,9,9,5,7,3))


label_list = c(17,1,11,4,25,26,2,8,10,21,
                3,5,6,9,29,15,14,13,30,
                12,7,16,18,19,
                23,20,24,27,33,28,34,
                22,31,32)

var_list = c(label_list+3)
colr_list = c(1:34)
plot_list = list()

for (i in 1:34) { 
  p= ggplot(clu_df_cont, aes_string(x=names(clu_df_cont)[var_list[[i]]])) + 
    geom_histogram(fill = colr_data[colr_list[[i]]])+
    theme_manuscript() +
    labs (title = labels_data[label_list[[i]]], x = "values") +
    xlab(xlab_data[label_list[[i]]]) +
    ylab("")
  plot_list[[i]]=p
}


variable1 = ggarrange(NULL,NULL,get_legend(plot_list[[2]] + theme(legend.position="bottom")),NULL,NULL,
                       NULL,NULL,plot_list[[1]],NULL,NULL, 
                       NULL,NULL,text_grob("Socioeconmic factors", face = "italic", size = 16, color = "dodgerblue"),NULL,NULL,
                       plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]],plot_list[[6]],plot_list[[7]],
                       plot_list[[8]],plot_list[[9]],plot_list[[10]],NULL,
                       NULL,NULL,text_grob("Demographic factors", face = "italic", size = 16, color = "darkorchid"),NULL,NULL,
                       plot_list[[11]],plot_list[[12]],plot_list[[13]],plot_list[[14]],plot_list[[15]],plot_list[[16]],
                       plot_list[[17]],plot_list[[18]],plot_list[[19]],NULL,
                       NULL,NULL,text_grob("Behavioral factors", face = "italic", size = 16, color = "green4"),NULL,NULL,
                       plot_list[[20]],plot_list[[21]],plot_list[[22]],plot_list[[23]],plot_list[[24]],
                       NULL,NULL,text_grob("Accessibility factors", face = "italic", size = 16, color = "tan4"),NULL,NULL,
                       plot_list[[25]],plot_list[[26]],plot_list[[27]],plot_list[[28]],plot_list[[29]],plot_list[[30]],
                       NULL,NULL,NULL,plot_list[[31]],
                       NULL,NULL,text_grob("Environmental factors", face = "italic", size = 16, color = 'turquoise'),NULL,NULL,
                       NULL,plot_list[[32]],plot_list[[33]],plot_list[[34]],NULL,
                       nrow = 15, ncol= 5, heights = c(0.2,1,00.2,1,1,0.2, 1,1,0.2,1,0.2,1,1,0.2,1, widths = c(1,1,1,1,1)))


variables = annotate_figure(variable1, top = text_grob("Distribution of covariates", 
                                                        color = "Black", face = "bold", size = 14),
                             left = text_grob("Count", color = "Black", size = 14,  rot = 90))
ggsave(paste0(HisDir, '/', Sys.Date(),  'histograms.pdf'), variables, width=13, height=20)

#__________________________

for (i in 1:34) { 
  #clu_df_cont$colors = ifelse(clu_df_cont$p_test < 0.1, "blue", "green")
  p= ggplot(clu_df_cont, aes_string(x=names(clu_df_cont)[var_list[[i]]])) + 
    geom_histogram(aes(position="stack", group = y, fill=y))+
    theme_manuscript() +
    labs (title = labels_data[label_list[[i]]], x = "values") +
    xlab(xlab_data[label_list[[i]]]) +
    ylab("")
  plot_list[[i]]=p
}

variable1 = ggarrange(NULL,NULL,get_legend(plot_list[[2]] + theme(legend.position="bottom")),NULL,NULL,
                       NULL,NULL,plot_list[[1]],NULL,NULL, 
                       NULL,NULL,text_grob("Socioeconmic factor", face = "italic", size = 10, color = "dodgerblue"),NULL,NULL,
                       plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]],plot_list[[6]],plot_list[[7]],
                       plot_list[[8]],plot_list[[9]],plot_list[[10]],NULL,
                       NULL,NULL,text_grob("Demographic factors", face = "italic", size = 10, color = "darkorchid"),NULL,NULL,
                       plot_list[[11]],plot_list[[12]],plot_list[[13]],plot_list[[14]],plot_list[[15]],plot_list[[16]],
                       plot_list[[17]],plot_list[[18]],plot_list[[19]],NULL,
                       NULL,NULL,text_grob("Behavioral factors", face = "italic", size = 10, color = "green4"),NULL,NULL,
                       plot_list[[20]],plot_list[[21]],plot_list[[22]],plot_list[[23]],plot_list[[24]],
                       NULL,NULL,text_grob("Accessibility factors", face = "italic", size = 10, color = "tan4"),NULL,NULL,
                       plot_list[[25]],plot_list[[26]],plot_list[[27]],plot_list[[28]],plot_list[[29]],plot_list[[30]],
                       NULL,NULL,NULL,plot_list[[31]],
                       NULL,NULL,text_grob("Environmental factors", face = "italic", size = 10, color = 'turquoise'),NULL,NULL,
                       NULL,plot_list[[32]],plot_list[[33]],plot_list[[34]],NULL,
                       nrow = 15, ncol= 5, heights = c(0.5,1,00.2,1,1,0.2, 1,1,0.2,1,0.2,1,1,0.2,1, widths = c(1,1,1,1,1)))


variables = annotate_figure(variable1, top = text_grob("Distribution of covariates", 
                                                        color = "Black", face = "bold", size = 14),
                             left = text_grob("Count", color = "Black", size = 14,  rot = 90))
ggsave(paste0(HisDir, '/', Sys.Date(),  'histograms_y.pdf'), variables, width=13, height=13)



#All buffers plot - according to theme

fill0 =  c("orangered", "dodgerblue", "darkorchid","green4","tan4", "turquoise", "orangered", "dodgerblue", "darkorchid","green4","tan4", "turquoise")
fill1 = c("dodgerblue", "darkorchid","green4","tan4", "turquoise", "orangered", "dodgerblue", "darkorchid","green4","tan4", "turquoise")
fill2 = c("darkorchid", "darkgoldenrod1", "red1","cornflowerblue","hotpink","darkorchid", "darkgoldenrod1", "red1","cornflowerblue","hotpink")
fill3 = c("green4", "chocolate2", "chocolate3","gold","cornflowerblue","chocolate1", "chocolate2", "chocolate3","chocolate4","chocolate")
fill4 = c("magenta", "limegreen", "gold2","gray48","tan4","magenta", "limegreen", "gold2","gray48","tan4")
fill5 = c("lightcoral", "darkgoldenrod", "brown1","darkslategray4","turquoise","lightcoral", "darkgoldenrod", "brown1","darkslategray4","turquoise")



#All buffers plot - grouped in two


labels_data = list("Malaria prevalence",
                    "Education","wealth","Housing quality","Floor type", "Roof type",
                    "Wall type","Household size","Mean age","Median age","Pregnant women",
                    "Net use","Under five net use","Fever cases","Fever treatment","Under five ACT use",
                    "Female population","Under five population","Under five Female population",
                    
                    "Population density","Under five population density (FB)","Housing quality 2000","Housing quality 2015","building",
                    "Elevation","Travel time to city","Time to travel one metre 2015","Time to travel one metre 2019","Time to walk one metre",
                    "Travel time to healthcare","Walk time to healthcare","Dominant vector","Secondary vector","Temperature", 
                    "v001")

name_list = c("test", 
               "edu","wealth","housing_q","floor", "roof_type", 
               "wall","household_size","mean_age","median_age","preg_women",
               "net_use_all","net_use_child","fever_cases","med_treat_fever","ACT_use_U5",
               "all_female_sex","U5_pop","female_child_sex",
               
               "pop_density","U5_FB","housing_2000","housing_2015","building", 
               "elev_","minutes_to_city","travel_metre_2015","travel_metre_2019","walking_metre", 
               "travel_healthcare","walking_healthcare","dominant","secondary_vector","temp_all_yrs",
               "v001")


fill_list = rep(list(fill0, fill2,fill5), times =c(1,18,15))
label_list = c(1:34)


for (i in 1:34) { 
  melteddf = melt(dplyr::select(clu_df_cont, "v001", matches(name_list[[i]])), id="v001", na.rm=T)
  fill_select = colr_list[i]
  p= ggplot(melteddf, aes_string(x= "value", fill = "variable", color = "variable")) +
    geom_freqpoly(size = 0.7) +
    theme_manuscript()+
    labs (title = labels_data[label_list[[i]]], x = "values") +
    scale_color_manual(labels = c("0m", "1000m", "2000m", "3000m","4000m"), 
                       values = fill_list[[i]]) +
    guides(color=guide_legend("Legend/Buffers")) +
    xlab(xlab_data[label_list[[i]]]) +
    ylab("")
  plot_list[[i]]=p  
  
}


variable1 = ggarrange(NULL,NULL,plot_list[[1]],NULL,NULL, 
                       NULL,NULL,text_grob("DHS covariates", face = "italic", size = 10, color = "darkorchid"),NULL,NULL,
                       plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]],plot_list[[6]],
                       plot_list[[7]],plot_list[[8]],plot_list[[9]],plot_list[[10]],plot_list[[11]], 
                       plot_list[[12]],plot_list[[13]],plot_list[[14]],plot_list[[15]],plot_list[[16]],
                       plot_list[[17]],plot_list[[18]], plot_list[[19]],NULL,NULL,
                       
                       NULL,NULL,text_grob("Geospatial covariates", face = "italic", size = 10, color = "turquoise"),NULL,NULL,
                       NULL,NULL,get_legend(plot_list[[20]] + theme(legend.position="bottom")),NULL,NULL,
                       plot_list[[20]],plot_list[[21]],plot_list[[22]],plot_list[[23]],plot_list[[24]],
                       
                       
                       plot_list[[25]],plot_list[[26]],plot_list[[27]],plot_list[[28]],plot_list[[29]],
                       plot_list[[30]],plot_list[[31]],plot_list[[32]],plot_list[[33]],plot_list[[34]],
                       
                       nrow = 12, ncol= 5, heights = c(1,0.2,1,1,1,1,0.2,0.2,1,1,1, widths = c(1,1,1,1,1)))

variables = annotate_figure(variable1, top = text_grob("Distribution of covariates", 
                                                        color = "Black", face = "bold", size = 14),
                             left = text_grob("Count", color = "Black", size = 14,  rot = 90))
ggsave(paste0(HisDir, '/', Sys.Date(),  'freqpoly_durce_grouped.pdf'), variables, width=13, height=13)



#________________________________ geospatial coveriates plots______________________________

#make an urban map of all cluster values 

u_df_18_fin = df_10_18_fin %>% filter(DHSYEAR == 2018) 
u_df_15_fin = df_10_18_fin %>% filter(DHSYEAR == 2015) 
u_df_10_fin = df_10_18_fin %>% filter(DHSYEAR == 2010)

#read in state shape file 
stateshp = readOGR(file.path(DataDir, "shapefiles","gadm36_NGA_shp"), layer ="gadm36_NGA_1",
                    use_iconv=TRUE, encoding= "UTF-8")
state_sf = st_as_sf(stateshp)

#make cluster maps 

clustermap=function(cluster_shp, title){
  tm_shape(state_sf) + #this is the health district shapfile with DS estimates info
    tm_polygons()+
    tm_shape(cluster_shp)+ #this is the points shape file with LLIN and number of kids info by cluster 
    tm_bubbles(size =0.2, col = "p_test", 
               border.col= "black", palette="seq",textNA = "Missing",
               breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=T)+
    tm_layout(aes.palette = list(seq ="-RdYlBu"), title = title)+
    tm_legend(legend.title.size = 0.8, legend.just="top")
}

map_18=clustermap(u_df_18_fin, "2018 malaria prevalence by cluster (DHS)")
map_15 =clustermap(u_df_15_fin, "2015 malaria prevalence by cluster (DHS)")
map_10 =clustermap(u_df_10_fin, "2010 malaria prevalence by cluster (DHS)")


urban_map=tmap_arrange(map_18, map_15, map_10)


tmap_save(tm =urban_map, filename = file.path(ResultDir, "maps", "urban_malaria_maps.pdf"), 
          width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)



#make maps for all variable using their cluster values 


for (i in 1:34){ 
  m=tm_shape(state_sf) + #this is the health district shapfile with DS estimates info
    tm_polygons()+
    tm_shape(df_10_18_fin)+ #this is the points shape file with LLIN and number of kids info by cluster 
    tm_bubbles(size=0.1, col = names(clu_df_cont)[grepl(name_list[[i]], names(clu_df_cont))], 
               border.col= "black", palette="seq",textNA = "Missing", title.col ="     ")+
    tm_layout(aes.palette = list(seq ="-RdYlBu"), main.title = labels_data[label_list[[i]]], 
              main.title.position = "center", main.title.size =0.8,
              legend.position = c("right", "top"))
  
  plot_list[[i]]=m
  
  
}


map_plpot =tmap_arrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]],
                         plot_list[[6]],plot_list[[7]],plot_list[[8]],plot_list[[9]],plot_list[[10]],
                         plot_list[[11]], plot_list[[12]],plot_list[[13]],plot_list[[14]],plot_list[[15]],
                         plot_list[[16]], plot_list[[17]], plot_list[[18]], plot_list[[19]],plot_list[[20]],
                         plot_list[[20]],plot_list[[21]],plot_list[[22]],plot_list[[23]],plot_list[[24]],
                         plot_list[[25]],plot_list[[26]],plot_list[[27]],plot_list[[28]],plot_list[[29]],
                         plot_list[[30]],plot_list[[31]],plot_list[[32]],plot_list[[33]],plot_list[[34]],nrow = 7, ncol= 5)

tmap_save(tm =map_plpot, filename = file.path(ResultDir, "maps", "dependent_dhscov_maps.pdf"), 
          width=13, height=18, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE, add.titles = "Distribution of Covariates")



p= ggplot() + 
  geom_sf(data =state_sf)+
  geom_point(df_10_18_fin, mapping = aes(x = LONGNUM, y = LATNUM, color = edu_a))+
  map_theme() +
  guides(color=guide_legend("Legend/Buffers")) +
  xlab(xlab_data[label_list[[i]]]) +
  ylab("")

p

#______________________________
df_10_18_fin = df_10_18_fin %>% filter(LONGNUM > 0.000000)

gmap_fun = function(polygon_name, point_data, labels, fill, legend_title){
  ggplot(polygon_name) +
    geom_sf(color='lightgrey')+
    geom_point(data = point_data,
               aes(fill=fill,  geometry = geometry),
               stat = "sf_coordinates", alpha = 0.45, size=3, shape=21
    ) +
    #viridis::scale_fill_viridis(option='C', discrete=TRUE, labels=labels, na.value ='grey', limits=c('[0,20]', '(20,40]', '(40,60]', '(60,80]', '(80,100]', NA)) +
    map_theme() + 
    guides(fill = guide_legend(title=legend_title, override.aes = list(size = 5)))+
    xlab("")+
    ylab("")
}




for (i in 1:34) { 
  p = gmap_fun(state_sf, df_10_18_fin, labels_data[label_list[[i]]], names(clu_df_cont)[var_list[[i]]], "legend") +
  
  plot_list[[i]]=p 
  
}




variable1 = ggarrange(NULL,plot_list[[1]],NULL,
                       NULL,text_grob("DHS covariates", face = "italic", size = 10, color = "darkorchid"),NULL,
                       plot_list[[2]],plot_list[[3]],plot_list[[4]],
                       plot_list[[5]],plot_list[[6]],plot_list[[7]],
                       plot_list[[8]],plot_list[[9]],plot_list[[10]],
                       plot_list[[11]],plot_list[[12]],plot_list[[13]],
                       plot_list[[14]],plot_list[[15]],plot_list[[16]],
                       plot_list[[17]],plot_list[[18]], plot_list[[19]],
                       
                       
                       NULL,text_grob("Geospatial covariates", face = "italic", size = 10, color = "turquoise"),NULL,
                       plot_list[[20]],plot_list[[21]],plot_list[[22]],
                       plot_list[[23]],plot_list[[24]],plot_list[[25]],
                       
                       
                       plot_list[[26]],plot_list[[27]],plot_list[[28]],
                       plot_list[[29]],plot_list[[30]],plot_list[[31]],
                       plot_list[[32]],plot_list[[33]],plot_list[[34]],
                       
                       nrow = 14, ncol= 3, heights = c(1,0.2,1,1,1,1,1,1,0.2,1,1,1,1,1))

variables = annotate_figure(variable1, top = text_grob("Distribution of covariates", 
                                                        color = "Black", face = "bold", size = 14),
                             left = text_grob("Count", color = "Black", size = 14,  rot = 90))
ggsave(paste0(MapsDir, '/', Sys.Date(),  'maps_grouped.pdf'), variables, width=13, height=30)







#________________________ plotting goespatials averages per state

state_sf = state_sf %>% mutate(NAME_1 = case_when(NAME_1 == "Federal Capital Territory" ~ "Fct Abuja",
                                                   NAME_1 == "Nassarawa" ~ "Nasarawa",
                                                   TRUE ~ as.character(NAME_1)))%>% mutate(NAME_1 = tolower(NAME_1))


state_mean = clu_df_10_18 %>% group_by(names(clu_df_cont[3:38])) %>% summarise(Mean_sales = mean(Sales))


sf = left_join(state_mean, state_sf, by =c("shstate" = "NAME_1"))




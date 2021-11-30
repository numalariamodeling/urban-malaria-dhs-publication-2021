
# for (i in 1:length(vars)) {
#   df <- map2(dhs, raster, get_crs)
#   df <- pmap(list(raster, df, vars[i]), extract_fun)
#   df <- plyr::ldply(df)
#   var_name <- paste0('pop_den_FB_', as.character(vars[i]), 'm')
#   df <- extrclean.fun(df, var_name)
#   write.csv(df, file =file.path(GeoDir, paste0('pop_density_FB_', as.character(vars[i]), 
#                                                'm_buffer', "_DHS_10_15_18.csv")),row.names = FALSE)
# }





# for (i in 1:length(vars)) {
#   df <- map2(dhs, raster, get_crs)
#   df <- pmap(list(raster, df, vars[i]), extract_fun)
#   df <- plyr::ldply(df)
#   var_name <- paste0('pop_den_U5_FB_', as.character(vars[i]), 'm')
#   df <- extrclean.fun(df, var_name)
#   write.csv(df, file =file.path(GeoDir, paste0('pop_density_U5_FB_', as.character(vars[i]), 
#                                                'm_buffer', "_DHS_10_15_18.csv")),row.names = FALSE)
# }







tests=ggplot(clu_df_10_18, aes(x = child_6_59_tested_malaria))+
  geom_histogram(bins = 25, fill = '#019875FF') + 
  scale_x_continuous(expand = c(0.03, 0)) +
  scale_y_continuous(expand = c(0.03, 0)) +
  theme_bw() + 
  labs(x = 'Malaria tests by microscopy', y = 'Count')+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.text.x = element_text(size = 16, color = "black"), 
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size =16))

sum(clu_df_10_18$child_6_59_tested_malaria, na.rm = TRUE)
summary(clu_df_10_18$child_6_59_tested_malaria)
sd(clu_df_10_18$child_6_59_tested_malaria, na.rm = TRUE)

#examine the number of positives 
positives=ggplot(clu_df_10_18, aes(x = positives))+
  geom_histogram(bins = 20, fill = '#FECEA8FF') + 
  scale_x_continuous(expand = c(0.03, 0)) +
  scale_y_continuous(expand = c(0.03, 0)) +
  theme_bw() + 
  labs(x = 'Number of positive malaria tests by microscopy', y = 'Count')+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.text.x = element_text(size = 16, color = "black"), 
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size =16))

table(clu_df_10_18$positives)
summary(clu_df_10_18$positives)
sd(clu_df_10_18$positives, na.rm = TRUE)

#__________________________________Loading Spactial pointd

# urban cluster points




# join dhs variables to cluster points by year 
#df_10_18_fin <- left_join(sf_10_18, clu_df_10_18, by = 
#c("DHSCLUST" = "v001", "DHSYEAR" = "dhs_year"))


#what is their spatial distribution?





summary(map$positives_prop)

#plot(clu_df_10_18$num_child_6_59, clu_df_10_18$child_6_59_tested_malaria)# make plot and add to methods 


#table data 
df <- map %>% select(positives_cut) %>%  group_by(positives_cut) %>%  summarize(`Count` = n())  
st_geometry(df)<- NULL
colnames(df)[1]<- 'Category'

#map data 

#levels(map$positives_cut) <- c(levels(map$positives_cut),"Missing data")
#map$positives_cut[is.na(map$positives_cut)] <- "Missing data"

df_all <- read.csv(file.path(CsvDir, "all_cluster_variables_urban_malaria_all_buffers.csv"), 
                   header = T, sep = ',')

df_all$housing_2000_2000m <- clu_df_10_18$housing_2000_2000m *100
df_all$housing_2015_2000m <- clu_df_10_18$housing_2015_2000m *100


# Binarize response:
df_all$y <- ifelse(df_all$p_test < 0.1, 0,1)

#na count
missing_values <- sapply(df_all, function(x) sum(is.na(x)))

clu_df_10_18 <- rename.vars(df_all, from = c("net_use", "fever"), 
                            to = c("nfever_cases", "net_use_all"))
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



#Dominant vector

#loading raster files

files <- list.files(path = file.path(RastDir , "vector") ,pattern = "*GA.tiff$", full.names = TRUE, recursive = TRUE)
files<- files[(grep('Dominant_Vector', files))]
raster<-sapply(files, raster, simplify = F)


for (i in 1:length(vars)) {
  var_name <- paste0('dominant_vector_', as.character(vars[i]), 'm')
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- df %>%  map(~rename_with(., .fn=~paste0(var_name), .cols = contains('Dominant_Vector')))
  df <- plyr::ldply(df)%>% dplyr::select(-c(ID))
  write.csv(df, file = file.path(GeoDir, paste0('dominant_vector_', as.character(vars[i]), 
                                                'm_buffer', "_DHS_10_15_18.csv")),row.names = FALSE)
}



#Secondary vector

files <- list.files(path = file.path(RastDir , "vector") ,pattern = "*GA.tiff$", full.names = TRUE, recursive = TRUE)
files<- files[(grep('Secondary', files))]
raster<-sapply(files, raster, simplify = F)


for (i in 1:length(vars)) {
  var_name <- paste0('secondary_vector_', as.character(vars[i]), 'm')
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- df %>%  map(~rename_with(., .fn=~paste0(var_name), .cols = contains('Secondary')))
  df <- plyr::ldply(df)%>% dplyr::select(-c(ID))
  write.csv(df, file = file.path(GeoDir, paste0('secondary_vector_', as.character(vars[i]), 
                                                'm_buffer', "_DHS_10_15_18.csv")),row.names = FALSE)
}



#DHS
dhs = read.csv(file.path(CsvDir, "all_DHS_variables_urban_malaria.csv"), header = T, sep = ',') %>% dplyr::select(-X)


#correlation coefficients for DHS variables 
x = dhs %>% dplyr::select(-c(p_test,positives, first_interview_month, dhs_year, shstate, v001, region, num_child_6_59, mean_age, female_child_sex, fever, U5_pop, housing_q)) #removes categorical variables and malaria prevalence 

#replace nas with their means 
for(i in 1:ncol(x)){
  x[is.na(x[,i]), i] = mean(x[,i], na.rm = TRUE)
}

#correlation matrix 
corr = round(cor(x), 1)

# Compute a matrix of correlation p-values
p.mat = cor_pmat(x)


corrPlot= ggcorrplot(corr, lab = TRUE, legend.title = "Correlation coefficient")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.9))
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_methods_figures_correlation_coefficients_DHS.pdf'), corrPlot, width = 13, height = 9)


#geospatial
#examine distribution of geospatial variables, select buffers with fewer negative and NA values and run correlation coefficient 
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

#create a geospatial data based on results of nas and negatives check 

df_sp = data.frame(v001 = df_geo[[1]]$v001, dhs_year = df_geo[[1]]$dhs_year, elevation_1000m = df_geo[[2]]$elev_merit_1000m,
                   housing_2000_4000m = df_geo[[5]]$housing_2000_4000m,  housing_2015_4000m = df_geo[[5]]$housing_2015_4000m,
                   minutes_nearest_city_1000m = df_geo[[2]]$minutes_to_city_1000m, minutes_travel_metre_2015_1000m = df_geo[[2]]$minutes_travel_metre_2015_1000m,
                   minutes_travel_metre_2019_2000m = df_geo[[3]]$minutes_travel_metre_2019_2000m, minutes_walking_healthcare_2000m = df_geo[[3]]$minutes_walking_healthcare_2000m,
                   minutes_walking_metre_2000m = df_geo[[3]]$minutes_walking_metre_2000m, motorized_travel_healthcare_2019_2000m = df_geo[[3]]$motorized_travel_healthcare_2019_2000m,
                   pop_den_U5_FB_4000m = df_geo[[5]]$pop_den_U5_FB_4000m, pop_density_0m = df_geo[[1]]$pop_density_0m,
                   precipitation_monthly_0m = df_geo[[1]]$preci_monthly_0m,soil_wetness_0m = df_geo[[1]]$soil_wetness_0m, 
                   temperature_monthly_0m = df_geo[[1]]$temp_survey_month_0m, dist_water_bodies_0m = df_geo[[1]]$dist_water_bodies_0m, EVI_0m = df_geo[[1]]$EVI_0m) 

df_sp = df_sp %>% dplyr::select(-c(dhs_year, v001, minutes_travel_metre_2015_1000m, minutes_travel_metre_2019_2000m, minutes_walking_metre_2000m, minutes_walking_healthcare_2000m)) %>% mutate(housing_2015_4000m = housing_2015_4000m*100,  housing_2000_4000m =  housing_2000_4000m *100,) #removes categorical variables and malaria prevalence 


#replace nas with their means 
for(i in 1:ncol(df_sp)){
  df_sp[is.na(df_sp[,i]), i] = mean(df_sp[,i], na.rm = TRUE)
}

#correlation matrix 
corr = round(cor(df_sp), 1)

# Compute a matrix of correlation p-values
p.mat = cor_pmat(df_sp)


corrPlot= ggcorrplot(corr, lab=TRUE, legend.title = "Correlation coefficient")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.9))
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_methods_figures_correlation_coefficients_geospatial.pdf'), corrPlot, width = 13, height = 9)
#investigate temperature all years and temperature monthly extraction. Same for precipitation.

#based on the correlation coefficient plot, we drop variables from the DHS and geospatial dataframes seperately 
dhs = read.csv(file.path(CsvDir, "all_DHS_variables_urban_malaria.csv"), header = T, sep = ',') %>% dplyr::select(-X)
dhs = dhs %>%  dplyr::select(-c(U5_pop, female_child_sex, wall_type, floor_type, housing_q, net_use, household_size, mean_age, p_test, fever))#p_test or test positivity rate is dropped since it is not used in the model

df_sp = data.frame(v001 = df_geo[[1]]$v001, dhs_year = df_geo[[1]]$dhs_year, elevation_1000m = df_geo[[2]]$elev_merit_1000m,
                   housing_2000_4000m = df_geo[[5]]$housing_2000_4000m,  housing_2015_4000m = df_geo[[5]]$housing_2015_4000m,
                   minutes_nearest_city_1000m = df_geo[[2]]$minutes_to_city_1000m, minutes_travel_metre_2015_1000m = df_geo[[2]]$minutes_travel_metre_2015_1000m,
                   minutes_travel_metre_2019_2000m = df_geo[[3]]$minutes_travel_metre_2019_2000m, minutes_walking_healthcare_2000m = df_geo[[3]]$minutes_walking_healthcare_2000m,
                   minutes_walking_metre_2000m = df_geo[[3]]$minutes_walking_metre_2000m, motorized_travel_healthcare_2019_2000m = df_geo[[3]]$motorized_travel_healthcare_2019_2000m,
                   pop_den_U5_FB_4000m = df_geo[[5]]$pop_den_U5_FB_4000m, pop_density_0m = df_geo[[1]]$pop_density_0m,
                   precipitation_monthly_0m = df_geo[[1]]$preci_monthly_0m,soil_wetness_0m = df_geo[[1]]$soil_wetness_0m, 
                   temperature_monthly_0m = df_geo[[1]]$temp_survey_month_0m, dist_water_bodies_0m = df_geo[[1]]$dist_water_bodies_0m, EVI_0m = df_geo[[1]]$EVI_0m) 

df_sp = df_sp %>%  dplyr::select(-c(minutes_walking_healthcare_2000m, minutes_walking_metre_2000m, housing_2000_4000m, minutes_travel_metre_2015_1000m, minutes_travel_metre_2019_2000m, elevation_1000m))


df_all <- left_join(dhs, df_sp, by =c('v001', 'dhs_year'))

#compute correlation coefficient for all dhs and geospatial variables 
x = df_all %>% dplyr::select(-c(positives, first_interview_month, dhs_year, shstate, v001, region, num_child_6_59)) #removes categorical variables and malaria prevalence 

#replace nas with their means 
for(i in 1:ncol(x)){
  x[is.na(x[,i]), i] = mean(x[,i], na.rm = TRUE)
}

#correlation matrix 
corr = round(cor(x), 1)

# Compute a matrix of correlation p-values
p.mat = cor_pmat(x)


corrPlot= ggcorrplot(corr, lab = TRUE, legend.title = "Correlation coefficient")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.9))
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_methods_figures_correlation_coefficients_DHS_geospatial_all.pdf'), corrPlot, width = 13, height = 9)

#final dataset of covariates and independent variable 
write_csv(df_all, paste0(CsvDir, '/final_dataset/final_dataset.csv'))


# #create values for geom_smooth
# for(i in 1:length(df_list_ordered)){
#   mod= glm(positives ~ ns(values,3) +offset(log(num_tested)), data=df_list_ordered[[i]], family = "quasipoisson")
#   mod.p =predict(mod, df_list_ordered[[1]],type = "link", se.fit = TRUE)#type = "link", se.fit = TRUE
#   critval <- 1.96 ## approx 95% CI
#   upr <- mod.p$fit + (critval * mod.p$se.fit)
#   lwr <- mod.p$fit - (critval * mod.p$se.fit)
#   fit <- mod.p$fit
#   fit2 <- mod$family$linkinv(fit)
#   upr2 <- mod$family$linkinv(upr)
#   lwr2 <- mod$family$linkinv(lwr)
#   df_list_ordered[[i]]$lwr <- round(lwr2, 0) 
#   df_list_ordered[[i]]$upr <- round(upr2, 0) 
#   df_list_ordered[[i]]$fit <- round(fit2, 0) 
#   
# }


# # 
# mod <-gam(positives ~ s(values) + offset(log(num_tested)), data=df_list_ordered[[6]], family ='poisson')
# plot(mod)




# plots = df_list_ordered %>%  {purrr::map2(., xlab, ~ggplot(.x)+
#                                      geom_point(mapping=aes(x=values, y=positives), shape=42, size= 5, color = "#f64b77", alpha = 0.5, position ='jitter') +
#                                      geom_smooth(.x, mapping=aes(x=values, y=fit, ymin=lwr, ymax=upr, fill = "Trend"), stat="smooth", se = FALSE, color = "#644128", method = 'gam', method.args = list(family = quasipoisson(link = "log")), formula = y ~ ns(x, 3))+
#                                      geom_smooth(.x, mapping=aes(x=values, y=fit, ymin=lwr, ymax=upr, color = "Confidence Interval"), stat="smooth", fill = "#a56c56", linetype = 0, method = 'gam', method.args=list(family ='poisson'), formula = y ~ s(x, bs = "cs"))+
#                                      theme_manuscript()+
#                                      labs(x = .y, y ='malaria positives')+
#                                      guides(fill =FALSE, color =FALSE))}
# 
# demo_p<- plots[[1]]+plots[[2]]+ plots[[3]]+ plots[[4]]+ plots[[5]]+ plots[[6]]
# demo_p

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



plots_rate = df_list_ordered %>%  {map2(., xlab, ~ggplot(.x,aes(x=values, y=rate))+
                                          geom_point(shape=42, size= 5, color = "#f64b77", alpha = 0.7) +
                                          geom_smooth(aes(fill = "Trend"), se = FALSE, color = "#644128", method = 'loess')+
                                          geom_smooth(aes(color = "Confidence Interval"), fill = "#a56c56", linetype = 0, method = 'loess')+
                                          theme_manuscript()+
                                          labs(x = .y, y ='malaria test positive rate')+
                                          guides(fill =FALSE, color =FALSE))}

social_p_rate<- plots_rate[[1]]+plots_rate[[2]]+ plots_rate[[3]]+ plots_rate[[4]]+ plots_rate[[5]]+ plots_rate[[6]]+ plots_rate[[7]]
social_p_rate


plots_by_year = df_list_ordered %>%  {map2(., xlab, ~ggplot(.x,aes(x=values, y=rate, color = as.factor(dhs_year), fill = as.factor(dhs_year), group = as.factor(dhs_year)))+
                                             geom_point(shape=42, size= 5, alpha = 0.6) +
                                             geom_smooth(method = 'glm', method.args = list(family = quasipoisson(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))+
                                             scale_color_viridis(discrete = TRUE)+
                                             scale_fill_viridis(discrete = TRUE)+
                                             theme_manuscript()+
                                             theme(legend.title = element_blank())+
                                             labs(x = .y, y ='malaria test positive rate'))}

social_p_year = plots_by_year[[1]]+plots_by_year[[2]]+ plots_by_year[[3]]+ plots_by_year[[4]]+ plots_by_year[[5]]+ plots_by_year[[6]]+ plots_by_year[[7]]& theme(legend.position = "bottom", legend.title = element_blank())
social_p_year=social_p_year+ plot_layout(guides = "collect")
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_bivariate_social_rate_by_year.pdf'), social_p_year, width = 14, height =9)
map<-ggplot(state_map) +
  geom_sf(aes(fill = p_test))+
  viridis::scale_fill_viridis(option="D")+
  map_theme()+ 
  geom_text(
    data = state_map,
    aes(label =  NAME_1, geometry = geometry),color ='white',
    stat = "sf_coordinates"
  )+ #geom_sf_text(aes(label=name_long))+ 
  xlab('')+
  ylab('')

state_map = left_join(state_sf, state_df_2, by =c('NAME_1'))


ggsave('state_urban_malaria.pdf', map)

# state_df=df$`C:/Users/ido0493/Box/NU-malaria-team/data/nigeria_dhs/data_analysis/data/DHS/Computed_cluster_information/urban_malaria_covariates/DHS_survey_extract/p_test_PfPR_urban_state_DHS_10_15_18.csv`
# 
# state_df =  state_df %>%  mutate(dhs_year = str_split(.id, "_", simplify = T)[, 4]) %>%  filter(dhs_year == '2018')


stateshp = readOGR(file.path(DataDir, "shapefiles","gadm36_NGA_shp"), layer ="gadm36_NGA_1",use_iconv=TRUE, encoding= "UTF-8")
state_sf = st_as_sf(stateshp)

state_val<-raster::extract(raster[[3]],state_sf, buffer = buffer, fun = mean, df =TRUE) %>% 
  mutate(NAME_1 = state_sf$NAME_1)

state_map = left_join(state_sf, state_val)

map=ggplot(state_map) +
  geom_sf(aes(fill =gpw_v4_population_density_rev11_2020_1_deg))+
  scale_fill_viridis_c(option = "C") +
  map_theme()+
  geom_text(
    data = state_map,
    aes(label =  NAME_1, geometry = geometry),color ='white',
    stat = "sf_coordinates"
  )+ #geom_sf_text(aes(label=name_long))+ 
  xlab('')+
  ylab('')


ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_pop_density_by_state.pdf'), map, width = 8.5, height =7)


state_df_2 = state_df %>% mutate(NAME_1 = str_to_title(state), NAME_1 = ifelse(NAME_1 == 'Fct Abuja', 'Federal Capital Territory',
                                                                               ifelse(NAME_1 == 'Nasarawa', 'Nassarawa', NAME_1))) %>% dplyr::select(-state)


state_edu_a=read.csv(file.path(DataIn, 'DHS_survey_extract', 'edu_a_all_state_DHS_PR_10_15_18.csv')) %>% 
  mutate(dhs_year = str_split(.id, "_", simplify = T)[, 4]) %>%  dplyr::select(-.id) %>% filter(dhs_year == '2018')

stateshp = readOGR(file.path(DataDir, "shapefiles","gadm36_NGA_shp"), layer ="gadm36_NGA_1",use_iconv=TRUE, encoding= "UTF-8")
state_sf = st_as_sf(stateshp)

state_edu_a_2 = state_edu_a %>% mutate(NAME_1 = str_to_title(state), NAME_1 = ifelse(NAME_1 == 'Fct Abuja', 'Federal Capital Territory',
                                                                                     ifelse(NAME_1 == 'Nasarawa', 'Nassarawa', NAME_1))) %>% dplyr::select(-state)

state_map = left_join(state_sf, state_edu_a_2)

library("ggsci")
map=ggplot(state_map) +
  geom_sf(aes(fill =edu_a))+
  scale_fill_gsea(reverse = T) +
  map_theme()+
  geom_text(
    data = state_map,
    aes(label =  NAME_1, geometry = geometry),color ='white',
    stat = "sf_coordinates"
  )+ #geom_sf_text(aes(label=name_long))+ 
  xlab('')+
  ylab('')


ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_edu_by_state-2018.pdf'), map, width = 8.5, height =7)

# library(glmmTMB)
# data(sleepstudy,package="lme4")
# 
# library(splines)
# m1 <- glmmTMB(Reaction~ns(Days,5)+(1|Subject), data=sleepstudy)
# sleepstudy$pred <- predict(m1)
# 
# library(ggplot2)
# ggplot(sleepstudy,aes(x=Days))+geom_point(aes(y=Reaction))+geom_line(aes(y=pred))


# 
# library(glmmTMB)
# data(sleepstudy,package="lme4")
# 
# library(splines)
# data=df_list_ordered[[4]] %>% drop_na(positives)
# min=min(data$values)
# max=max(data$values)
# 
# data_e= data[1:20, c('positives', 'values')]
# dput(data_e)
# 
# m1 <- glmmTMB(positives~ns(values, 3, knots = seq(min(values),max(values),length =4)[2:3]), data=data_e,  ziformula=~1,family=poisson)
# #m1 <- glmmTMB(positives~ns(values, 3)+offset(log(num_tested)), data=data,  ziformula=~1,family=poisson)
# a=ggpredict(m1, terms="values [all]", type ='zero_inflated') %>% plot(rawdata = TRUE, jitter = .01) + theme_manuscript()
# 
# colnames(m1$frame)[2]='values'
# 
# m2 <- glm(positives~ns(values, 3, knots = seq(min(values),max(values),length =4)[2:3]), data=data, family=poisson)
# b=ggpredict(m2, terms = "values") %>% plot(rawdata = TRUE, jitter = .01)
# 
# a +b
# 
# library(ggplot2)
# ggplot(data,aes(x=values))+geom_point(aes(y=positives))+geom_line(aes(y=pred))


# what would the cluster point value be in 2018 for June and July?

files <- list.files(path = file.path(DataDir, "Raster_files", 'rainfall_monthly', '2010', 'june_july_2010') , pattern = ".tif$", full.names = TRUE, recursive = TRUE)
files<- files[(grep('month_07', files))]
raster<-sapply(files, raster, simplify = F)

for (i in 1:length(vars)) {
  var_name <- c(paste0('precipitation', as.character(vars[i]), 'm'))
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- df %>%  map(~rename_with(., .fn=~paste0(var_name), .cols = contains('month_07')))
  df <- plyr::ldply(df) %>% dplyr::select(-c(ID))
  write.csv(df, file =file.path(GeoDir, paste0('precipitation_month_07_10_', as.character(vars[i]), 
                                               'm_buffer', "_DHS_10_15_18.csv")),row.names = FALSE)
}

j18 = cdf_hist(df, 'dodgerblue3', 'dodgerblue3','precipitation0m',  'Precipitation, 2010', 25)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), 'cluster_precipitation_july_2010.pdf'),j18, width = 8.5, height =5)


# LGAshp = readOGR(file.path(DataDir, "shapefiles","Nigeria_LGAs_shapefile_191016"), layer ="NGA_LGAs",use_iconv=TRUE, encoding= "UTF-8")
# LGA_sf = st_as_sf(LGAshp)


# df_lagos = dplyr::filter(LGA_sf, (State %in% c('Lagos')))
# map_lag = gmap_fun(df_lagos, map_lagos, labels=c('0 - 10','11-15', '16 - 20', '21 - 25', '26 - 30', '31 - 40', '41 - 50', '51 - 60', '61 - 70', '71 - 80', '81 - 90', '91 - 100'),
#                    map_lagos$net_cut, 'net use rate')
# 
# map_lag + geom_text_repel(
#   data = df_lagos,
#   aes(label = LGA, geometry = geometry),color ='black',
#   stat = "sf_coordinates",
#   min.segment.length = 0, size = 3, force = 1)+ #geom_sf_text(aes(label=name_long))+
#   xlab('')+
#   ylab('')

x <- df %>% dplyr::select(-c(p_test, interview_month, dhs_year, shstate, v001, region)) #removes categorical variables and malaria prevalence 

#replace nas with their means 
for(i in 1:ncol(x)){
  x[is.na(x[,i]), i] <- mean(x[,i], na.rm = TRUE)
}

#correlation matrix 
corr <- round(cor(x), 1)

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(x)


corrPlot<- ggcorrplot(corr, lab = TRUE, legend.title = "Correlation coefficient")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.9))
ggsave(paste0(illustrations_dir, '/modeling_plots/correlation_all.pdf'), plot =corrPlot, height = 13, width = 13)

corrPlot<- ggcorrplot(corr, lab=TRUE, legend.title = "Correlation coefficient")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.9))
ggsave(paste0(illustrations_dir, '/modeling_plots/correlation_all_no_label.pdf'), plot =corrPlot, height = 13, width = 13)



#correlation with reduced data 
x_reduced <- x %>% dplyr::select(-c(mean_age, net_use, housing_2000_2000m,housing_q, minutes_walking_healthcare_2000m, 
                                    minutes_travel_metre_2015_2000m, female_child_sex, minutes_travel_metre_2019_2000m, floor_type, wall_type)) #removes highly correlated variables


#correlation matrix 
corr <- round(cor(x_reduced), 1)

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(x_reduced)


corrPlot<- ggcorrplot(corr, lab = TRUE, legend.title = "Correlation coefficient")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.9))
ggsave(paste0(illustrations_dir, '/modeling_plots/correlation_reduced.pdf'), plot =corrPlot, height = 13, width = 13)

corrPlot<- ggcorrplot(corr, legend.title = "Correlation coefficient")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.9))
ggsave(paste0(illustrations_dir, '/modeling_plots/correlation_reduced_no_label.pdf'), plot =corrPlot, height = 13, width = 13)




# ------------------------------------------
### Make data 
## -----------------------------------------
prevalence <- ifelse(df$p_test>=0.1,1, 0)

df_x <- df %>%  dplyr::select(interview_month, dhs_year, shstate, region)

df_x$dhs_year<-as.factor(df_x$dhs_year)
df_x$shstate<-as.factor(df_x$shstate)
df_x$region<-as.factor(df_x$region)
df_x$region<-as.factor(df_x$region)

df <- cbind(df_x, x, prevalence)

#removing all collinear variables, malaria prevalence and cluster id
x_reduced <- df %>% dplyr::select(-c(mean_age, net_use, housing_2000_2000m,housing_q, minutes_walking_healthcare_2000m, 
                                     minutes_travel_metre_2015_2000m, female_child_sex, minutes_travel_metre_2019_2000m, floor_type, wall_type)) #removes highly correlated variables


df<- cbind(df_x, x_reduced, prevalence)






# ------------------------------------------
### try bas package 
## -----------------------------------------

BAS_model = bas.glm(prevalence ~ ., data=df,
                    family=binomial(),
                    method="MCMC", n.models=20000,
                    betaprior=bic.prior(n = nrow(df)),
                    modelprior=uniform())


summary(BAS_model)


coef <- coef(BAS_model)


#maybe we imput?//
for(i in 1:ncol(x_reduced)){
  if (is.numeric(x_reduced)){
    x_reduced[is.na(x_reduced[,i]), i] <- mean(x_reduced[,i], na.rm = TRUE)}
}



# ------------------------------------------
### try INLA with BMA and beta regression
## -----------------------------------------







urbandataset$y <- ifelse(urbandataset$p_test < 0.1, 0,1) # delete this


pfpr <- read.csv(file=file.path(DataDir, 'urban_malaria_cluster_est', 'pfpr_DHS_10_15_18.csv'))
pfpr$y <- ifelse(pfpr$hml32 *100 < 10, 0, 1) 
hist(pfpr$y)


edu <- read.csv(file=file.path(DataDir, 'urban_malaria_cluster_est', 'edu_a_DHS_10_15_18.csv'))
hist(edu$edu_a)

df <- pfpr %>%left_join(edu, by=c('.id', 'hv001'))

df$edu_a = df$edu_a*100
df

edu_glm<- glm(y~ 1+ edu_a, data = df,
              family = "binomial")

summary(edu_glm)
exp(coef(edu_glm))** 5 # we would need to convert all our proportions to percentages to make the interpretations less clunky and use exponents of 5 unit increases

w_glm <- glm(y~ 1+ wealth_2,
             data = urbandataset, family = "binomial")

summary(w_glm)
exp(coef(w_glm))

results_w_glm = coefficients(summary(w_glm))
colnames(results_w_glm)[2] = "SE"
results_w_glm_df <- data.frame(results_w_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_w_glm_df["vars"] = c("(Intercept_c)", "wealth_2_c")

#### education 

e_glm <- glm(y~ 1+ edu_a,
             data = urbandataset, family = "binomial")

summary(e_glm)
exp(coef(e_glm))

results_e_glm = coefficients(summary(e_glm))
colnames(results_e_glm)[2] = "SE"
results_e_glm_df <- data.frame(results_e_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_e_glm_df["vars"] = c("(Intercept_c)", "edu_a_c")

###sex
sex_fglm <- glm(y~ 1+ sex_f,
                data = urbandataset, family = "binomial")

summary(sex_fglm)
exp(coef(sex_fglm))

results_sex_fglm = coefficients(summary(sex_fglm))
colnames(results_sex_fglm)[2] = "SE"
results_sex_fglm_df <- data.frame(results_sex_fglm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_sex_fglm_df["vars"] = c("(Intercept_c)", "sex_f_c")

#ACT
ACT_use_u5_glm <- glm(y~ 1+ ACT_use_u5,
                      data = urbandataset, family = "binomial")

summary(ACT_use_u5_glm)
exp(coef(ACT_use_u5_glm))

results_ACT_use_u5_glm = coefficients(summary(ACT_use_u5_glm))
colnames(results_ACT_use_u5_glm)[2] = "SE"
results_ACT_use_u5_glm_df <- data.frame(results_ACT_use_u5_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_ACT_use_u5_glm_df["vars"] = c("(Intercept_c)", "ACT_use_u5_c")

#net use
net_use_glm <- glm(y~ 1+ net_use,
                   data = urbandataset, family = "binomial")

summary(net_use_glm)
exp(coef(net_use_glm))

results_net_use_glm = coefficients(summary(net_use_glm))
colnames(results_net_use_glm)[2] = "SE"
results_net_use_glm_df <- data.frame(results_net_use_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_net_use_glm_df["vars"] = c("(Intercept_c)", "net_use_c")

#humidity
humidindex_glm <- glm(y~ 1+ humidindex,
                      data = urbandataset, family = "binomial")

summary(humidindex_glm)
exp(coef(humidindex_glm))

results_humidindex_glm = coefficients(summary(humidindex_glm))
colnames(results_humidindex_glm)[2] = "SE"
results_humidindex_glm_df <- data.frame(results_humidindex_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_humidindex_glm_df["vars"] = c("(Intercept_c)", "humidindex_c")

#precipitation
annual_precipitation_glm <- glm(y~ 1+ annual_precipitation,
                                data = urbandataset, family = "binomial")

summary(annual_precipitation_glm)
exp(coef(annual_precipitation_glm))

results_annual_precipitation_glm = coefficients(summary(annual_precipitation_glm))
colnames(results_annual_precipitation_glm)[2] = "SE"
results_annual_precipitation_glm_df <- data.frame(results_annual_precipitation_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_annual_precipitation_glm_df["vars"] = c("(Intercept_c)", "annual_precipitation_c")

#hh size
hh_size_glm <- glm(y~ 1+ hh_size,
                   data = urbandataset, family = "binomial")

summary(hh_size_glm)
exp(coef(hh_size_glm))

results_hh_size_glm = coefficients(summary(hh_size_glm))
colnames(results_hh_size_glm)[2] = "SE"
results_hh_size_glm_df <- data.frame(results_humidindex_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_humidindex_glm_df["vars"] = c("(Intercept_c)", "hh_size_c")

#hh age
hh_members_age_glm <- glm(y~ 1+ hh_members_age,
                          data = urbandataset, family = "binomial")

summary(hh_members_age_glm)
exp(coef(hh_members_age_glm))

results_hh_members_age_glm = coefficients(summary(hh_members_age_glm))
colnames(results_hh_members_age_glm)[2] = "SE"
results_hh_members_age_glm_df <- data.frame(results_hh_members_age_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_hh_members_age_glm_df["vars"] = c("(Intercept_c)", "hh_members_age_c")

#pop dens
pop_count_glm <- glm(y~ 1+ pop_count,
                     data = urbandataset, family = "binomial")

summary(pop_count_glm)
exp(coef(pop_count_glm))

results_pop_count_glm = coefficients(summary(pop_count_glm))
colnames(results_pop_count_glm)[2] = "SE"
results_pop_count_glm_df <- data.frame(results_pop_count_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_pop_count_glm_df["vars"] = c("(Intercept_c)", "pop_count_c")

#building count
build_count_glm <- glm(y~ 1+ build_count,
                       data = urbandataset, family = "binomial")

summary(build_count_glm)
exp(coef(build_count_glm))

results_build_count_glm = coefficients(summary(build_count_glm))
colnames(results_build_count_glm)[2] = "SE"
results_build_count_glm_df <- data.frame(results_build_count_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_build_count_glm_df["vars"] = c("(Intercept_c)", "build_count_c")

#merging univeriate results
univ_merge <- dplyr::bind_rows(results_w_glm_df, results_e_glm_df, results_sex_fglm_df, results_net_use_glm_df, results_humidindex_glm_df, 
                               results_annual_precipitation_glm_df, results_humidindex_glm_df, results_hh_members_age_glm_df, results_pop_count_glm_df, 
                               results_ACT_use_u5_glm_df, results_build_count_glm_df)

univ_merge <- univ_merge[univ_merge$vars != "(Intercept_c)",]
write_csv(univ_merge, file.path(Man_Dir, 'univ_merged_odds.csv'))

#############################################################################################
########################multivariable model comparisons####################################


###############multivariable model with all variables ####################
model1 <- glm(y ~ edu_a + wealth_2 +  annual_precipitation +  net_use + sex_f + hh_size + log_pop_den  + hh_members_age
              + ACT_use_u5 + humidindex, data = urbandataset, binomial)

summary(model1)


##################significant model ##################333

model3 <- glm(y ~ edu_a + wealth_2, data = urbandataset, binomial) 


#multivariable model comparisons adding  annual_precipitation

model3 <- glm(y ~ edu_a  + wealth_2 + annual_precipitation, data = urbandataset, binomial) 


delta_coef_precip <- abs((coef(model3)-coef(model1)[1:3])/
                           coef(model1)[1:3]) 

delta_coef_precip <- as.data.frame(round(delta_coef_precip, 3))

lrtest3 <- as.data.frame(lrtest(model2, model3))
lrtest3$model <- "wealth_model"


#multivariable model comparisons adding hh_size 
model1a <- glm(y ~ edu_a  + wealth_2 + hh_size + sex_f  + annual_precipitation  + log_pop_den  + hh_members_age
               + net_use + ACT_use_u5 + humidindex, data = urbandataset, binomial)
summary(model1a)

model4 <- glm(y ~ edu_a + wealth_2 + hh_size , data = urbandataset, binomial) 
summary(model4)

delta_coef_hh_size <- abs((coef(model4)-coef(model1a)[1:3])/
                            coef(model1a)[1:3]) 

delta_coef_hh_size <- as.data.frame(round(delta_coef_hh_size, 3))


lrtest4 <- as.data.frame(lrtest(model2, model4))
lrtest4$model <- "hh_size_model"

#multivariable model comparisons adding log_pop_den 
model1b <- glm(y ~ edu_a +  + wealth_2 + log_pop_den + sex_f + annual_precipitation + hh_size + hh_members_age
               + net_use + ACT_use_u5 + humidindex, data = urbandataset, binomial)
summary(model1a)

model5 <- glm(y ~ edu_a + wealth_2 + log_pop_den, data = urbandataset, binomial) 
summary(model5)

delta_coef_pop_den <- abs((coef(model5)-coef(model1b)[1:3])/
                            coef(model1b)[1:3]) 

delta_coef_pop_den <- as.data.frame(round(delta_coef_pop_den, 3))


lrtest5 <- as.data.frame(lrtest(model2, model5))
lrtest5$model <- "pop_den_model"


#multivariable model comparisons adding hh_members_age
model1c <- glm(y ~ edu_a + wealth_2 + hh_members_age + sex_f + annual_precipitation + log_pop_den + hh_size 
               + net_use + ACT_use_u5 + humidindex, data = urbandataset, binomial)
summary(model1a)

model6 <- glm(y ~ edu_a + wealth_2 + hh_members_age, data = urbandataset, binomial) 
summary(model6)

delta_coef_hh_age <- abs((coef(model6)-coef(model1c)[1:3])/
                           coef(model1c)[1:3]) 

delta_coef_hh_age <- as.data.frame(round(delta_coef_hh_age, 3))


lrtest6 <- as.data.frame(lrtest(model2, model6))
lrtest6$model <- "hh_age_model"

#multivariable model comparisons adding net_use
model1d <- glm(y ~ edu_a + wealth_2 + net_use + sex_f + annual_precipitation  + 
                 hh_members_age + log_pop_den + hh_size + ACT_use_u5 + humidindex, 
               data = urbandataset, binomial)
summary(model1d)

model7 <- glm(y ~ edu_a + wealth_2 + net_use, data = urbandataset, binomial) 
summary(model7)

delta_coef_net_use <- abs((coef(model7)-coef(model1d)[1:3])/
                            coef(model1d)[1:3]) 

delta_coef_net_use <- as.data.frame(round(delta_coef_net_use5, 3))


lrtest7 <- as.data.frame(lrtest(model2, model7))
lrtest7$model <- "u5_net_model"


#multivariable model comparisons adding ACT_use_u5  
model1f <- glm(y ~ edu_a  + wealth_2 + ACT_use_u5 + net_use + sex_f  + 
                 annual_precipitation + hh_members_age + log_pop_den + 
                 hh_size + humidindex, data = urbandataset, binomial)

model9 <- glm(y ~ edu_a + wealth_2 + ACT_use_u5, data = urbandataset, binomial) 
summary(model9)

delta_coef_ACT <- abs((coef(model9)-coef(model1f)[1:3])/
                        coef(model1f)[1:3])  

delta_coef_ACT <- as.data.frame(round(delta_coef_ACT, 3))


lrtest9 <- as.data.frame(lrtest(model2, model9))
lrtest9$model <- "ACT_model"


#multivariable model comparisons adding  humidindex
model1g <- glm(y ~ edu_a   + wealth_2 +  humidindex+ sex_f + annual_precipitation + 
                 net_use + hh_members_age + log_pop_den + hh_size, data = urbandataset, binomial)

model10 <- glm(y ~ edu_a  + wealth_2 +  humidindex, data = urbandataset, binomial) 
summary(model10)

delta_coef_humidindex <- abs((coef(model10)-coef(model1g)[1:3])/
                               coef(model1g)[1:3]) 

delta_coef_humidindex <- as.data.frame(round(delta_coef_humidindex, 3))


lrtest10 <- as.data.frame(lrtest(model2, model10))
lrtest10$model <- "humidindex_model10"





delta_coef_build <- abs((coef(model3)-coef(model1g)[1:3])/
                          coef(model1g)[1:3]) 


delta_coef_build <- as.data.frame(round(delta_coef_build, 3))

#Creating dataframe for computed coeficient difference 
delta_coef_df <- merge(delta_coef_pop_den, delta_coef_annual_precipitation, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_hh_size, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_ACT, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_hh_age, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_humidindex, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_net_use, by="row.names", all=TRUE)

delta_coef_df <- na.omit(delta_coef_df)

#Visualizing the percentage change that each  additioin variable makes to the education
#variable after being added to  the model. 
delta_df <- delta_coef_df[c(2), ]


#plot of delta_coef
df1 <- melt(delta_df,"Row.names")

g1 <- ggplot(df1, aes(x = variable, y = value)) +
  geom_bar(aes(fill= variable),stat="identity", position ="dodge") + 
  theme_bw()+ 
  scale_y_continuous(breaks = seq(0, 0.55, by = 0.1), limits=c(0,0.55))+
  theme(axis.text.x = element_text(angle=-40, hjust=.1))


g1

#Visualizing the percentage change that each  additioin variable makes to the Wealth
#variable after being added to  the model. 

delta_df <- delta_coef_df[c(3), ]

#plot of delta_coef
df1 <- melt(delta_df,"Row.names")

g2 <- ggplot(df1, aes(x = variable, y = value)) +
  geom_bar(aes(fill= variable),stat="identity", position ="dodge") + 
  theme_bw()+ 
  scale_y_continuous(breaks = seq(0, 0.55, by = 0.1), limits=c(0,0.55))+
  theme(axis.text.x = element_text(angle=-40, hjust=.1))


g2


#We see that Humidity index and household size did not change the significant variables'
#estimate by >10. 

#Therefore, our multivariable model of choice is below. 


u_glm <- glm(y~ 1+ wealth_2+ edu_a + sex_f+  ACT_use_u5 + build_count +
               pop_count + hh_members_age + net_use  + annual_precipitation,
             data = urbandataset, family = "binomial")
summary(u_glm)

printCrudeAndAdjustedModel(u_glm)[-1,]




#m2_simres <- simulateResiduals(m2)
# plot(m2_simres)
# summary(residuals(m2_simres))
# 
# 
# 
# All_models=MuMIn::dredge(m2)


#autmate model selection 
# map2 = map %>% dplyr::select(positives, edu_a, wealth, housing_2015_4000m, roof_type,child_6_59_tested_malaria,
#                              pop_density_0m,pop_den_U5_FB_4000m,preg_women,all_female_sex,median_age,household_size,
#                              net_use, net_use_child, med_treat_fever, ACT_use_U5,
#                              motorized_travel_healthcare_2019_2000m, 
#                              precipitation_monthly_0m,
#                              temperature_monthly_0m, soil_wetness_0m, dist_water_bodies_0m, 
#                              elevation_1000m, EVI_0m,
#                              lat, lon, first_interview_month, dhs_year) %>%  na.omit()
# map2$pos <- numFactor(scale(map2$lat), scale(map2$lon)) # first we need to create a numeric factor recording the coordinates of the sampled locations
# map2$ID <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
# map2$month_year = factor(paste(map2$first_interview_month, '_', map2$dhs_year))
# levels(map2$month_year)
# map2$ID2 <- factor(rep(1, nrow(map2)))# then create a dummy group factor to be used as a random term
# 
# m2 <- glmmTMB(positives~ns(edu_a, 3)+ ns(wealth, 3)+ns(pop_density_0m, 2) + ns(median_age, 2)+ 
#                 ns(med_treat_fever, knots = seq(min(med_treat_fever),max(med_treat_fever),length =4)[2:3])+
#                 ns(precipitation_monthly_0m, 3) + ns(EVI_0m, 3)+
#                 + offset(log(child_6_59_tested_malaria)) +
#                 mat(pos + 0 | ID) + ar1(month_year + 0 | ID2), data=map2,  ziformula=~1,family=poisson)
# summary(m2) 


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

rm(list=ls())
#memory.limit(size = 50000)

# ----------------------------------------------------
### Directories
# ----------------------------------------------------

user = Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive,"OneDrive", "urban_malaria")
ExDir <- file.path(NuDir, "extracted_data")
ProjectDir = file.path(NuDir, 'data', 'nigeria','nigeria_dhs' , 'data_analysis')
DataDir = file.path(ProjectDir, "data")
DHSData = file.path(DataDir, 'DHS')
DataIn = file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'DHS_survey_extract')
ResultDir =file.path(ProjectDir, "results", "research_plots")
HisDir =file.path(ResultDir, "histograms")
MapsDir = file.path(ResultDir, "maps")
GeoDir <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'geospatial_covariates')
CsvDir = file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'cleaned_cluster_covariates_all', 'New_082321')


# ----------------------------------------------------
### Required functions and settings
## ----------------------------------------------------
library(ggridges)
source("./other_functions/descriptive_analysis_functions.R")


## ----------------------------------------------------------------
### Read in computed DHS cluster data and generate related figures  
## ----------------------------------------------------------------
#read in dhs file 
dhs = read.csv(file.path(ExDir, "cleaned_datasets","all_DHS_variables_urban_malaria.csv"), header = T, sep = ',') %>% dplyr::select(-X)
dhs$positives_prop = round(dhs$positives/dhs$child_6_59_tested_malaria, 1) %>% as.numeric
summary(dhs$child_6_59_tested_malaria)


reg_month <- dhs %>%drop_na(positives) %>% group_by(dhs_year, region,first_interview_month) %>% 
  summarise(prop = sum(positives)/sum(child_6_59_tested_malaria), clusters = n())



#figure 1a
dhs = dhs %>%drop_na(positives)
df=data.frame(x=c('2010', '2015', '2018', "2021"), y =c(81, 136, 560, 195))
pdf('clusters_dhs.pdf')
barplot(height=df$y, names=df$x, 
        col=rgb(0.8,0.1,0.1,0.6),
        xlab="Year", 
        ylab="Number of Clusters", 
)
dev.off()

#figure 1b
p1 = igv.lm.point(dhs, dhs$num_child_6_59, dhs$child_6_59_tested_malaria,dhs$dhs_year,  "Survey year", 'Number of children 6 - 59 months', 'Number of children 6 - 59 months \n tested for malaria')
p1= p1 +geom_smooth(method=lm, color = "black")+ theme(legend.position = 'none')
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_Figure_1_sample_overview.pdf'), p1, width = 13, height = 9)


#figure 1a
p1a <- ggplot(df, aes(x= x,  y = y), fill= dhs_year) + 
  geom_col(aes(fill = x))+
  labs (x = "Year of survey", y = "Number of urban clusters", title = "") +
  scale_fill_manual(values=c("#5560AB",  "#FAAF43", "#EE3C96", "#1390A1")) +
  theme_manuscript()+
  theme(legend.position = "none")

p1a

#figure 1b
dhs_1 <- dhs %>% mutate(dhs_year = as.character(dhs$dhs_year))
p1b <- ggplot(data = dhs_1, aes(x= dhs_year,  y = child_6_59_tested_malaria), fill= dhs_year) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(color = dhs_year), width = 0.08)+
  labs (x = "Year of survey", y = "Children 6 - 59 months tested for malaria per urban cluster", title = "") +
  scale_color_manual(values=c("#5560AB",  "#FAAF43", "#EE3C96", "lightseagreen")) +
  theme_manuscript()+ 
  theme(legend.position = "none")

p1 <- p1a + p1b 
p1
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_Figure_1_sample_overview_complete.pdf'), p1, width = 7.2, height = 3.4)

#values used in manuscript texts 
table(dhs$dhs_year)
nrow(!is.na(subset(dhs, dhs_year ==2010 & num_child_6_59 >=20)))
nrow(!is.na(subset(dhs, dhs_year ==2015 & num_child_6_59 >=20)))
nrow(!is.na(subset(dhs, dhs_year ==2018 & num_child_6_59 >=20)))
nrow(!is.na(subset(dhs, dhs_year ==2021 & num_child_6_59 >=20)))

#figure 2a
df_tested = data.frame(values = dhs$child_6_59_tested_malaria, category = 'tested')
df_positives = data.frame(values = dhs$positives, category = 'positives')
df_all = rbind(df_tested, df_positives)
p2 = hist_fun(df_all, df_all$values, df_all$category, 'Number of children 6 - 59 months', 'Count', c("Positive tests", "Tested"))+
  theme(legend.position = c(0.6, 0.7))+  
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) + 
  guides(fill=guide_legend(title= str_wrap("2010, 2015,2018, and 2021 DHS and MIS surveys", width = 25)))
p2  



#figure 2b
#examine the number of children tested 
p3 = igv.lm.point(dhs, dhs$child_6_59_tested_malaria, dhs$positives, dhs$dhs_year, 'Survey year', 'Number of children 6 - 59 months \n tested for malaria', 'Number of positive tests' )
p3_ = p3 + geom_abline(slope=1, intercept=c(0,0), size = 0.9) +geom_smooth(method=lm, color = "black")

p_2b=ggplot(data=dhs, mapping=aes(x= positives_prop, y= factor(dhs_year), fill =factor(dhs_year), height = ..density..)) +
  geom_density_ridges(trim = TRUE) +
  scale_fill_manual(values = c("#5560AB",  "#FAAF43", "#EE3C96", "lightseagreen")) +
  theme_manuscript() + 
  theme(legend.position = "none") + 
  ylab("") +
  xlab("Test positivity rate") + xlim(0,1)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '2b_tst_positiv_yr_srvy.pdf'), p_2b, width = 3.653, height = 3.123)





#figure 2c
#load spatial points
sf21 = st_read(file.path(DHSData, "Downloads", "NG_2021_MIS_12052022_1735_141460/NGGE81FL/NGGE81FL.shp"),)
sf18 = st_read(file.path(DHSData, "Downloads", "NG_2018_DHS_08142023_1348_141460/NGGE7BFL/NGGE7BFL.shp"),) 
sf15 = st_read(file.path(DHSData, "Downloads", "NG_2015_MIS_06192019/NGGE71FL/NGGE71FL.shp"),) 
sf10 = st_read(file.path(DHSData, "Downloads", "NG_2010_MIS_06192019/NGGE61FL/NGGE61FL.shp"),) 
sf_all = rbind(sf21, sf18, sf15, sf10) %>%filter(URBAN_RURA == "U") %>%  dplyr::rename(v001 = DHSCLUST)


#data wrangling
dhs_ = dhs %>%  dplyr::select(v001, positives, child_6_59_tested_malaria, DHSYEAR=dhs_year, net_use, net_use_child.x, positives_prop, 
                              first_interview_month)
map = sf_all %>% left_join(dhs_, by=c('v001', 'DHSYEAR'))  %>%  filter(LATNUM != 0) %>% mutate(ADM1NAME = toupper(ADM1NAME))
map$positives_cut = cut(map$positives_prop, breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), include.lowest = TRUE)
df_count = map %>% dplyr::select(positives_cut) %>%  group_by(positives_cut) %>%  summarize(`Count` = n())
map$net_cut = cut(map$net_use, breaks=c(0,10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100), include.lowest = TRUE)

stateshp = st_read(file.path(DataDir, "shapefiles","gadm36_NGA_shp", "gadm36_NGA_1.shp")) #, layer ="gadm36_NGA_1",use_iconv=TRUE, encoding= "UTF-8")
state_sf = st_as_sf(stateshp)


#big map 
map_big = gmap_fun(state_sf, map, labels=c(paste0('0 - 0.2',  ' (', df_count$Count[[1]], ')'), 
                                           paste0('0.3 - 0.4',  ' (', df_count$Count[[2]], ')'), paste0('0.5 - 0.6',  ' (', df_count$Count[[3]], ')'), 
                                           paste0('0.7 - 0.8',  ' (', df_count$Count[[4]], ')'), paste0('0.9 - 1.0',  ' (', df_count$Count[[5]], ')'), 
                                           'Missing data'),
                   map$positives_cut, 'Test positivity rate (overall count)') #+ theme(legend.position = "none")



#borno
map_abia <- state_map('Abia', 'ABIA', 'Test positivity rate')

#Lagos 
map_lag <- state_map('Lagos', 'LAGOS', 'Test positivity rate')

#Anambra 
map_akwa <- state_map('Akwa Ibom', 'AKWA IBOM', 'Test positivity rate')

#rivers 
map_riv <- state_map('Rivers', 'RIVERS', 'Test positivity rate') #+ theme(legend.position = "right")

patch1 = ( map_big|(map_lag /(map_abia + map_riv)))+ plot_layout(ncol = 2)
patch2 = (p2+ p_2b)/ patch1 + plot_layout(nrow = 2) #+  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold', size = 16))
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_Figure_2_low_positivity_viz.pdf'), patch2, width = 7.7, height = 6.3)

patch3 = ((map_lag /(map_abia + map_riv)))+ plot_layout(nrow = 2)
patch3
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_Figure_2_low_positivity_viz_states.pdf'), patch3, width = 4, height = 3.7)


map_low_values = map %>% na.omit(positives) %>%  filter(positives_prop == 0) %>%  group_by(ADM1NAME) %>%  summarise(n())
map_low = map_low_values %>%  filter(`n()` >15)
cluster = map %>% na.omit(positives)


#figure 3
#trends by DHS year 
trend_data= dhs

trend_data$month_year = paste0(trend_data$first_interview_month, "_", trend_data$dhs_year)
table(trend_data$month_year)


trend_data_10 = trend_data[trend_data$first_interview_month ==10,]
p_all_10 = gdensity_fun(trend_data_10, trend_data_10$positives_prop, trend_data_10$dhs_year, "Survey year", 
                        'Test positivity rate for clusters sampled in October', 'Density') + xlim(0,1)

check = trend_data_10 %>%  filter(dhs_year == '2021')
summary(check$positives_prop)


trend_data_11 = trend_data[trend_data$first_interview_month ==11,]
p_all_11 = gdensity_fun(trend_data_11, trend_data_11$positives_prop, trend_data_11$dhs_year, "Survey year", 
                        'Test positivity rate for clusters sampled in November', 'Density') + xlim(0,1)


trend_data_12 = trend_data[trend_data$first_interview_month ==12,]
p_all_12 = gdensity_fun(trend_data_12, trend_data_12$positives_prop, trend_data_12$dhs_year, "Survey year", 
                        'Test positivity rate for clusters sampled in December', 'Density') + xlim(0,1)

all_plots = p_all_10 / p_all_11 / p_all_12 +  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold', size = 16))
all_plots
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_Figure_3_malaria_tests_positivity_trends.pdf'), all_plots, width = 7, height = 6)
data = data.frame(pos =trend_data_12[trend_data_12$dhs_year == 2018,'positives_prop'])
data_ = data %>%  filter(pos == 0)



#figure 3d-e
#Year big map
map$DHSYEAR_cut = cut(map$DHSYEAR, breaks=c(0, 2010, 2015, 2018, 2021), include.lowest = TRUE) 

df_year = map %>% dplyr::select(DHSYEAR) %>%  group_by(DHSYEAR) %>%  summarize(`Count` = n())


map_big_year = gmap_fun2(state_sf, map, labels=c(paste0('2010',  ' (', df_year$Count[[1]], ')'), 
                                                 paste0('2015',  ' (', df_year$Count[[2]], ')'), 
                                                 paste0('2018',  ' (', df_year$Count[[3]], ')'), paste0('2021',  ' (', df_year$Count[[4]], ')')),
                         map$DHSYEAR_cut, 'Clusters (overall count)') + theme(legend.position = "bottom") #+ facet_wrap(~ DHSYEAR_cut)

map_big_year

#Year big map
map_months <- map %>% filter(!is.na(first_interview_month))
map_months$month_cut = cut(map_months$first_interview_month, breaks=c(0, 8, 9, 10, 11, 12), include.lowest = TRUE) 

df_month = map_months %>% dplyr::select(first_interview_month) %>%  group_by(first_interview_month) %>%  summarize(`Count` = n())


map_big_month = gmap_fun2(state_sf, map_months, labels=c(paste0('8',  ' (', df_month$Count[[1]], ')'), 
                                                         paste0('9',  ' (', df_month$Count[[2]], ')'), 
                                                         paste0('10',  ' (', df_month$Count[[3]], ')'), 
                                                         paste0('11',  ' (', df_month$Count[[4]], ')'), 
                                                         paste0('12',  ' (', df_month$Count[[5]], ')')),
                          map_months$month_cut, 'Clusters (overall count)') + theme(legend.position = "bottom") + 
  scale_fill_manual(values = c('darkred',"darkseagreen", "#8971B3", "royalblue","yellow1")) #+ facet_wrap(~ first_interview_month)

map_big_month

over_years_months <- map_big_year  + map_big_month
over_years_months
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_Figure_3d_clusters.pdf'), over_years_months,width = 7.6, height = 3.8)
#regional data 

#month and region 
reg_month <- as.data.frame(table(map_months$DHSREGNA, map_months$DHSYEAR,map_months$first_interview_month))

reg_month <- map_months %>% group_by(DHSYEAR, DHSREGNA,first_interview_month) %>% 
  summarise(prop = sum(positives)/sum(child_6_59_tested_malaria), clusters = n())


  
  
  
reg_ <- as.data.frame(table(map_months$first_interview_month))

#
df = dhs %>% mutate(group = ifelsemap_months(positives_prop > 0, 0, 1))%>%  group_by(region, group) %>% 
  summarise(number = n()) %>%  drop_na() %>% mutate(freq = number / sum(number))

df$region = factor(df$region, levels = c('south south', 'north east', 'north central', 'south east', 
                                         'south west', 'north west'))

x_label =  c('SS', 'NE', 'NC', 'SE', 'SW', "NW")

p_2d=ggplot(df, aes(fill=as.factor(group), y=number, x=region)) + 
  geom_bar(position="fill", stat="identity", alpha = 0.7)+
  scale_fill_manual(labels = c("> zero U5 test positivity rate", "equal to zero U5 test positivity rate"), values = c("firebrick", "darksalmon"))+
  scale_x_discrete(labels=x_label)+
  theme_manuscript() + 
  theme(legend.title = element_blank())+
  ylab('Proportion of clusters by U5 malaria test positivity rate category')

ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_Figure_2_regional_difference.pdf'), p_2d, width = 7.5, height = 3)



## ----------------------------------------------------------------
### Creating analysis data  
## ----------------------------------------------------------------
#read in geospatial dataset and create final data 
files <- list.files(path = file.path(ExDir, 'cleaned_datasets') , pattern = '.csv', full.names = TRUE, recursive = TRUE)
files = files[-grep('all_DHS_variables_urban|2018_mobility_DHS_variables_urban_malaria', files)]
df_geo = sapply(files, read.csv, simplify = F)
df_geo <- lapply(df_geo, function(x) x[!duplicated(x[c("v001", "dhs_year")]), ])



#find the number of NAs per column in geospatial data 
df_nas = df_geo %>%  map(~summarise_all(., funs(sum(is.na(.)))))
df_nas[[1]] <- df_nas[[1]] #%>%  dplyr::select(-precipitation0m, -precipitation0m.x, -precipitation0m.y)
names(df_nas[[1]])<- gsub(pattern = '\\_0m$', replacement = '_nas', x = names(df_nas[[1]]))#remove _0m from the 0m buffer dataset
for(i in seq_along(df_nas)){
  names(df_nas[[i]])<- names(df_nas[[i]])
}
df_nas = bind_rows(df_nas, .id ='column label')


#count the number of negatives per column in geospatial data 
df_neg = df_geo %>%  map(~summarise_all(., funs(sum((.) < 0, na.rm=TRUE))))
df_neg[[1]] <- df_neg[[1]] #%>%  dplyr::select(-precipitation0m, -precipitation0m.x, -precipitation0m.y)
names(df_neg[[1]])<- gsub(pattern = '\\_0m$', replacement = '_negs', x = names(df_neg[[1]]))#remove _0m from the 0m buffer dataset
for(i in seq_along(df_neg)){
  names(df_neg[[i]])<- names(df_neg[[i]])
}
df_neg = bind_rows(df_neg, .id ='column label')

df_na_neg = cbind(df_nas, df_neg)
df_na_neg = df_na_neg[,order(colnames(df_na_neg))]
df_na_neg

#select geospatial dataset values with fewest 
df_sp = data.frame(v001 = df_geo[[1]]$v001, dhs_year = df_geo[[1]]$dhs_year, elevation_4000m = df_geo[[5]]$elevation_4000m,
                   housing_2000_4000m = df_geo[[5]]$housing_2000_4000m,  housing_2015_4000m = df_geo[[5]]$housing_2015_4000m,
                   minutes_nearest_city_1000m = df_geo[[2]]$access_to_cities_2000m, minutes_travel_metre_2015_1000m = df_geo[[2]]$minutes_travel_metre_2015_1000m,
                   minutes_travel_metre_2019_2000m = df_geo[[3]]$minutes_travel_metre_2019_2000m, minutes_walking_healthcare_2000m = df_geo[[3]]$minutes_walk_healthcare_2000m,
                   minutes_walking_metre_2000m = df_geo[[3]]$minutes_walking_metre_2000m, motorized_travel_healthcare_2019_2000m = df_geo[[3]]$motorized_travel_healthcare_2019_2000m,
                   pop_den_U5_FB_4000m = df_geo[[5]]$pop_den_U5_FB_4000m, pop_density_0m = df_geo[[5]]$pop_den_4000m,
                   precipitation_monthly_0m = df_geo[[5]]$preci_monthly_4000m, soil_wetness_0m = df_geo[[1]]$soil_wetness_0m, 
                   temperature_monthly_0m = df_geo[[1]]$temp_survey_month_0m.x, dist_water_bodies_0m = df_geo[[1]]$dist_water_bodies_0m, EVI_0m = df_geo[[4]]$EVI_3000m) 


df_all <- left_join(dhs, df_sp, by =c('v001', 'dhs_year'))
write.csv(df_all, file=file.path(ExDir, 'cleaned_datasets','final_dataset_multivariate_analysis', 'multivariate_analysis_dataset.csv'))

#define variables to be used throughout analysis
positives = df_all$positives 
dhs_year=df_all$dhs_year
num_tested =df_all$child_6_59_tested_malaria
region = df_all$region
interview_month = df_all$first_interview_month
edu_cat = cut(as.numeric(df_all$edu_a), breaks = c(0, 5, 10, 15, 100), include.lowest =TRUE)



df_all <-read.csv(file.path(ExDir, 'cleaned_datasets', 'final_dataset_multivariate_analysis',"multivariate_analysis_dataset.csv"), header = T, sep = ',')



## -----------------------------------------------------------------------------------------------------------------------
### Socio-economic variable distribution, cumulative distribution, correlation and relationship with malaria prevalence 
## -----------------------------------------------------------------------------------------------------------------------

#variable distribution and cumulative distribution 
dhs_social = data.frame(`Educational attainment` = df_all$edu_a, Wealth = df_all$wealth, `Improved flooring` =df_all$floor_type,
                        `Improved roofing materials` = df_all$roof_type, `Improved wall` = df_all$wall_type, `improved housing in 2000` =df_all$housing_2000_4000m,
                        `improved housing in 2015` = df_all$housing_2015_4000m) %>%  mutate(improved.housing.in.2000 = improved.housing.in.2000*100,
                                                                                            improved.housing.in.2015= improved.housing.in.2015*100) 
#Figure 4
dhs_social_long = dhs_social %>%  pivot_longer(everything(),names_to='x_label', values_to='values')

df_list =split(dhs_social_long, dhs_social_long$x_label)
df_list_ordered = list(df_list$Educational.attainment,df_list$Wealth,
                       df_list$Improved.flooring, df_list$Improved.roofing.materials, df_list$Improved.wall, df_list$improved.housing.in.2000,
                       df_list$improved.housing.in.2015)


xlab=list(expression(atop('% with post-primary', paste('education'))),
          expression(atop('% in the rich wealth', paste('quintiles'))),expression(atop('% in homes with improved', paste(' flooring'))),
          expression(atop('% in homes with a metal', paste('or zinc roof'))), expression(atop('% in homes with an', paste('improved wall type'))),
          expression(atop('% living in improved', paste('housing (2000)'))),
          expression(atop('% living in improved', paste('housing (2015)'))))


p = pmap(list(df_list_ordered,'#8971B3', '#8971B3', 'values', xlab, 30), cdf_hist)
p=p[[1]]+ p[[2]]+ p[[3]]+p[[4]]+ p[[5]]+p[[6]]+p[[7]]+ plot_annotation(tag_levels = 'A')& theme(plot.tag = element_text(size = 12, face = 'bold'))
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), 'social_variable_distribution.pdf'), p, width =8, height =6)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), 'social_variable_distribution.png'), p, width =8, height =6)

#correlation 
#Supplement pub. figure 1
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
corr
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), 'correlation_coefficients_social.pdf'), corr, width = 8, height = 5)


#relationship with malaria positives
dhs_social_plot = cbind(dhs_social, dhs_year, positives, num_tested)
dhs_social_plot$rate = (dhs_social_plot$positives/dhs_social_plot$num_tested) 
dhs_social_plot = dhs_social_plot  %>%  pivot_longer(!c(dhs_year, positives, num_tested, rate),names_to='x_label', values_to='values')
df_list = split(dhs_social_plot, dhs_social_plot$x_label)
df_list_ordered = list(df_list$Educational.attainment,df_list$Wealth,
                       df_list$Improved.flooring, df_list$Improved.roofing.materials, df_list$Improved.wall, df_list$improved.housing.in.2000,
                       df_list$improved.housing.in.2015)

#Supplement pub. figure 2
plots <- plots_fun(df_list_ordered,'positives', "#f64b77", "#644128", "#a56c56", poisson,"malaria positives")

p<- plots[[1]]+plots[[2]]+ plots[[3]]+ plots[[4]]+ plots[[5]]+ plots[[6]] + plots[[7]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p

ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_bivariate_social.pdf'), p, width = 8, height =7)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_bivariate_social.png'), p, width = 8, height =7)

#Supplement pub. figure 3
#rate 
plots <- plots_fun(df_list_ordered,'rate', "#f64b77", "#644128", "#a56c56", quasipoisson, "malaria positivity rate")

p<- plots[[1]]+plots[[2]]+ plots[[3]]+ plots[[4]]+ plots[[5]]+ plots[[6]]+ plots[[7]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p

ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_bivariate_social_rate.pdf'), p, width = 8, height =7)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_bivariate_social_rate.png'), p, width = 8, height =7)

## -----------------------------------------------------------------------------------------------------------------------
### Demographic variable distribution, cumulative distribution, correlation and relationship with malaria prevalence 
## -----------------------------------------------------------------------------------------------------------------------
#variable distribution and cumulative distribution 
demo_numeric = data.frame(`Population density` = df_all$pop_density_0m, `U5 population density` = df_all$pop_den_U5_FB_4000m, `Pregnant women` =df_all$preg_women,
                          `Female population` = df_all$all_female_sex, `Household size` = df_all$household_size, `Median age` =df_all$median_age)

#Figure 5 A-F                   
demo_numeric_long = demo_numeric %>%  pivot_longer(everything(),names_to='x_label', values_to='values')

df_list =split(demo_numeric_long, demo_numeric_long$x_label)
df_list_ordered = list(df_list$Population.density,df_list$U5.population.density, df_list$Pregnant.women,
                       df_list$Female.population, df_list$Household.size, df_list$Median.age)


xlab=list('all age population density',
          expression(atop('Population density,', paste('children 5 years and under'))),'% of pregnant women',
          '% of females', 'Median household size',
          'Median age')


p = pmap(list(df_list_ordered,'#00A08A', '#00A08A', 'values', xlab, 25), cdf_hist)
p=p[[1]]+ p[[2]]+ p[[3]]+p[[4]]+ p[[5]]+p[[6]] 
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_demo_variable_distribution.pdf'), p, width = 7.6, height = 4)

#maps for categorical variable 
clu_name_state = df_all %>%  group_by(shstate) %>%  summarise(n = n()) %>%
  mutate(NAME_1 = str_to_title(shstate), NAME_1 = ifelse(NAME_1 == 'Fct-Abuja', 'Federal Capital Territory',
                                                         ifelse(NAME_1 == 'Nasarawa', 'Nassarawa', NAME_1)))

#read in state shape file 
stateshp = readOGR(file.path(DataDir, "shapefiles","gadm36_NGA_shp"), layer ="gadm36_NGA_1",use_iconv=TRUE, encoding= "UTF-8")
state_sf = st_as_sf(stateshp)

#Figure 5 G
#map by state
state_map = left_join(state_sf, clu_name_state, by =c('NAME_1'))
state_map$nun_cut = cut(state_map$n, breaks = c(0, 10, 21, 32, 43, 54, 76, 85), include.lowest = TRUE)
map_demo1=ggplot(state_map) +
  geom_sf(aes(fill = nun_cut))+ 
  brightness(viridis::scale_fill_viridis(option="E", discrete =TRUE, labels = c('0 - 10', '11 - 21', '22 - 32', '33 - 43', '44 - 54', '55 - 75', "76 - 95")), 0.8)+
  map_theme()+
  guides(fill = guide_legend(title= 'Number of clusters sampled\n per state', override.aes = list(size = 5))) 
map_demo1
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_distribution_clusters_by_state.pdf'), map_demo1, width = 14, height =9)

#Figure 5 H
#map by geopolitical region
clu_num_geo = df_all %>%  group_by(region) %>%  summarise(n = n())
clu_num_geo_ = left_join(df_all, clu_num_geo, by=c('region')) %>%  dplyr::select(region, shstate, n) %>%  
  mutate(NAME_1 = str_to_title(shstate), NAME_1 = ifelse(NAME_1 == 'Fct-Abuja', 'Federal Capital Territory',
                                                         ifelse(NAME_1 == 'Nasarawa', 'Nassarawa', NAME_1)))
geo_map = left_join(state_sf, clu_num_geo_, by =c('NAME_1'))

map_demo2=ggplot(geo_map) +
  geom_sf(aes(fill =as.factor(n)))+ 
  brightness(viridis::scale_fill_viridis(option="E", discrete = TRUE), 0.8)+
  map_theme()+
  guides(fill = guide_legend(title= 'Number of clusters sampled\n per geopolitical region', override.aes = list(size = 5)))
#map_demo2
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_distribution_clusters_by_GPZ.pdf'), map_demo2, width = 14, height =9)

map_demo1 = map_demo1 + theme(legend.position = "none")
map_demo2 = map_demo2 + theme(legend.position = "none")
p_demo_all = p/(map_demo1 + map_demo2)  + plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold')) 
#p_demo_all

ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_figure_S5.pdf'), p_demo_all, width = 6, height =6)

#correlation
#supplementpub Figure 4
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
corr
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), 'correlation_coefficients_demo.pdf'), corr, width = 8, height = 5)



#relationship with malaria positives
dhs_demo_plot = cbind(demo_numeric, dhs_year, positives, num_tested) 
is.na(dhs_demo_plot['Population density'])<- dhs_demo_plot['Population density'] > 3000
is.na(dhs_demo_plot['Population density,\n children 5 years and under'])<- dhs_demo_plot['Population density,\n children 5 years and under'] > 10
is.na(dhs_demo_plot['Median age'])<- dhs_demo_plot['Median age'] > 30
is.na(dhs_demo_plot['% of pregnant women'])<- dhs_demo_plot['% of pregnant women'] > 30
dhs_demo_plot$rate = (dhs_demo_plot$positives/dhs_demo_plot$num_tested) 
dhs_demo_plot = dhs_demo_plot  %>%  pivot_longer(!c(dhs_year, positives, num_tested, rate),names_to='x_label', values_to='values')
df_list = split(dhs_demo_plot, dhs_demo_plot$x_label)
df_list_ordered = list(df_list$`Population density`,df_list[[6]],df_list$`% of pregnant women`, df_list$`% of females`, df_list$`Median household size`, df_list$`Median age`)


#supplementpub Figure 5
#unadjusted positives chopped boundary 
plots <- plots_fun2(df_list_ordered, 'positives', "dodgerblue3", "darkred", "darksalmon", poisson,"malaria positives")


p<- plots[[1]]+plots[[2]]+ plots[[3]]+ plots[[4]]+ plots[[5]]+ plots[[6]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_demo_bivariate_chopped_bondary.png'), p, width = 8.5, height =4.5)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_demo_bivariate_chopped_bondary.pdf'), p, width = 8.5, height =4.5)

#supplementpub Figure 6
#malaria rate chopped boundary 
plots <- plots_fun2(df_list_ordered, 'rate', "dodgerblue3", "darkred", "darksalmon", quasipoisson,"malaria positivity rate")

p<- plots[[1]]+plots[[2]]+ plots[[3]]+ plots[[4]]+ plots[[5]]+ plots[[6]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_demo_rate_bivariate_chopped_boundary.pdf'), p, width = 8.5, height =4.5)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_demo_rate_bivariate_chopped_boundary.png'), p, width = 8.5, height =4.5)


#unadjusted positives full boundary 
dhs_demo_plot = cbind(demo_numeric, dhs_year, positives, num_tested) 
dhs_demo_plot$rate = (dhs_demo_plot$positives/dhs_demo_plot$num_tested) 
dhs_demo_plot = dhs_demo_plot  %>%  pivot_longer(!c(dhs_year, positives, num_tested, rate),names_to='x_label', values_to='values')
df_list = split(dhs_demo_plot, dhs_demo_plot$x_label)
df_list_ordered = list(df_list$`Population density`,df_list[[6]],
                       df_list$`% of pregnant women`, df_list$`% of females`, df_list$`Median household size`, df_list$`Median age`)


#supplementpub Figure 7
#unadjusted positives expanded boundary 
plots <- plots_fun2(df_list_ordered, 'positives', "dodgerblue3", "darkred", "darksalmon", poisson,"malaria positives")


p<- plots[[1]]+plots[[2]]+ plots[[3]]+ plots[[4]]+ plots[[5]]+ plots[[6]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_demo_bivariate_expanded_boundary.pdf'), p, width = 8.5, height =4.5)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_demo_bivariate_expanded_boundary.png'), p, width = 8.5, height =4.5)


#relationship with demo categorical variables 
#state data 
state_rship = glm(positives ~ as.factor(shstate)+ offset(log(num_tested)), data = df_all, family ='poisson')
state_dat =data.frame(coefficient=state_rship$coefficients, std_error = summary(state_rship)$coefficients[, 2])
state_dat$names = rownames(state_dat)
state_dat = state_dat %>%  mutate(lci = coefficient - std_error, uci = coefficient + std_error) %>% 
  filter(names !='(Intercept)') %>%  mutate(index = 1:36)

#supplementpub Figure 8
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

#supplementpub Figure 9
#effect plot - GPZ 
p = forest_fun(GPZ_dat, "forestgreen", "darkseagreen", xname,1:5, GPZ_dat$names)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_slope_estimate_GPZ_comparison.pdf'), p, width = 8.5, height =4.5)


#2018 data alone 
occ_df = read.csv(file.path(ExDir, "cleaned_datasets", "2018_mobility_DHS_variables_urban_malaria.csv"), header = T, sep = ',') %>% dplyr::select(-X, -agri_worker_both, -agri_worker_man, -agri_worker_woman)
df_18 = df_all %>%  filter(dhs_year == '2018') %>%  dplyr::select(v001, positives)
df_18_all = left_join(df_18, occ_df, by =c('v001'))
occ_df = occ_df %>%  dplyr::select(agri_worker_partner,last_work_man, last_work_partner, last_work_woman, seasonal_work_man, seasonal_work_woman)
#variable distribution and cumulative distribution 

occ_long = occ_df %>%  pivot_longer(everything(),names_to='x_label', values_to='values')

df_list =split(occ_long, occ_long$x_label)
df_list_ordered = list(df_list$agri_worker_partner, df_list$seasonal_work_man, df_list$seasonal_work_woman)


xlab=list('% of partners that are agricultural workers',
          '% of male seasonal workers','% of female seasonal workers')


p = pmap(list(df_list_ordered,'#00A08A', '#00A08A', 'values', xlab, 25), cdf_hist)
p=p[[1]]+ p[[2]]+ p[[3]]
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_occupation_positive_distribution.pdf'), p, width = 8.5, height =2.25)

#relationship with malaria positives
dhs_occ_plot = df_18_all  %>% dplyr::select(positives, agri_worker_partner,seasonal_work_man, seasonal_work_woman) %>%  pivot_longer(!c(positives),names_to='x_label', values_to='values')
df_list = split(dhs_occ_plot , dhs_occ_plot$x_label)


#supplementpub Figure 10
#unadjusted positives chopped boundary 
plots <- plots_fun2(df_list, 'positives', "dodgerblue3", "darkred", "darksalmon", poisson,"malaria positives")

p<- plots[[1]]+plots[[2]]+ plots[[3]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p

ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_occupation_positive_distribution.pdf'), p, width = 8.5, height =2.25)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_occupation_positive_distribution.png'), p, width = 8.5, height =2.25)



## --------------------------------------------------------------------------------------------------------------------------
### Behavioral factors variable distribution, cumulative distribution, correlation and relationship with malaria prevalence 
## --------------------------------------------------------------------------------------------------------------------------

#variable distribution and cumulative distribution 
df_behave = data.frame(`Net use access` = df_all$net_use_access,`Net use` = df_all$net_use, `Child net use` = df_all$net_use_child.x, `Medical treatment for fever` =df_all$med_treat_fever,
                       `Effective fever treatment` = df_all$ACT_use_U5)

#Figure 6
df_behave_long = df_behave %>%  pivot_longer(everything(),names_to='x_label', values_to='values')

df_list =split(df_behave_long, df_behave_long$x_label)
df_list_ordered = list(df_list$Net.use.access, df_list$Net.use,df_list$Child.net.use, df_list$Medical.treatment.for.fever,
                       df_list$Effective.fever.treatment)


xlab=list('% of individuals using bednets given access', '% of individuals using bednets',
          expression(atop('% of children 6- 59 months using bednets', paste('among those tested for microscopy'))),expression(atop('% of U5 children that sought', paste('medical treatment for fever'))),
          expression(atop('% of U5 children with fever that received', paste('an artemisinin-based combination therapy'))))


p_list = pmap(list(df_list_ordered,'salmon', 'salmon', 'values', xlab, 25), cdf_hist)
p =p_list [[2]]+ p_list [[3]]+p_list [[4]] +p_list [[5]] + plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p

ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_behavioral_variable_distribution.pdf'), p, width = 8, height =4.5)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_behavioral_variable_distribution.png'), p, width = 8, height =4.5)


p = p_list [[1]]
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_behavioral_variable_distribution_netaccess.pdf'), p, width = 4, height =2.25)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_behavioral_variable_distribution_netaccess.png'), p, width = 4, height =2.25)




#supplementpub Figure 11
#correlation 
colnames(df_behave)= c('% of individuals using bednets given access','% of individuals using bednets',
                       '% of children using bednets','% of U5 children that sought \n  medical treatment for fever',
                       '% of U5 children with fever that received \n an artemisinin-based combination therapy')
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
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), 'correlation_coefficients_behave.pdf'), corr, width = 8.5, height = 6)


#supplementpub Figure 12
dhs_behave_plot = cbind(df_behave, region, positives, num_tested) 
dhs_behave_plot$rate = (dhs_behave_plot$positives/dhs_behave_plot$num_tested) 
dhs_behave_plot = dhs_behave_plot  %>%  pivot_longer(!c(region, positives, num_tested, rate),names_to='x_label', values_to='values')
df_list = split(dhs_behave_plot, dhs_behave_plot$x_label)
df_list_ordered = list(df_list$`% of individuals using bednets given access`,df_list$`% of individuals using bednets`,df_list$`% of children using bednets`, df_list[[3]], df_list[[4]])

#unadjusted positives
plots <- plots_fun(df_list_ordered,'positives', "purple2", "deeppink4", "deeppink", poisson,"malaria positives")

p<- plots[[2]]+ plots[[3]]+ plots[[4]]+ plots[[5]]+plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_behavioral_variable_bivariate_positives.pdf'), p, width = 8.5, height =4.5)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_behavioral_variable_bivariate_positives.png'), p, width = 8.5, height =4.5)

#supplementpub Figure 13
#rate
plots <- plots_fun(df_list_ordered,'rate', "purple2", "deeppink4", "deeppink", quasipoisson,"malaria positivity rate")

p= plots[[2]]+ plots[[3]]+ plots[[4]]+ plots[[5]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_behavioral_variable_rate_bivariate_positives.pdf'), p, width = 8.5, height =4.5)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_behavioral_variable_rate_bivariate_positives.png'), p, width = 8.5, height =4.5)

#supplementpub Figure 14
#region adjustment 
plots = df_list_ordered %>%  {purrr::map2(., xlab, ~ggplot(.x,aes(x=values, y=positives, color=region, group = region, fill=region))+
                                            geom_point(shape=42, size= 5, alpha = 0.7) +
                                            geom_smooth(method = 'glm', method.args = list(family = poisson(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))+
                                            theme_manuscript()+
                                            labs(x = .y, y ='malaria positives')+
                                            guides(fill =FALSE))}

p= plots[[2]]+ plots[[3]]+ plots[[4]]+ plots[[5]]
p = p+ plot_layout(guides = "collect")+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'), legend.position = 'bottom')
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_behavioral_variable_bivariate_positives_region.pdf'), p, width = 8.5, height =7)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_behavioral_variable_bivariate_positives_region.png'), p, width = 8.5, height =7)


## --------------------------------------------------------------------------------------------------------------------------
### Accessibility factors variable distribution, cumulative distribution, correlation and relationship with malaria prevalence 
## --------------------------------------------------------------------------------------------------------------------------

#Figure 7
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
p

ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_accessibility_hist_sfig_19.pdf'), p, width = 4, height =2.25)


dhs_access_plot = cbind(df_access, region, positives, num_tested) 
dhs_access_plot$rate = (dhs_access_plot$positives/dhs_access_plot$num_tested) 
dhs_access_plot = dhs_access_plot  %>%  pivot_longer(!c(region, positives, num_tested, rate),names_to='x_label', values_to='values')
df_list = split(dhs_access_plot, dhs_access_plot$x_label)


xlab=list('Motorized travel time to health care')

#unadjusted positives

plots <- plots_fun3(df_list,'positives', "turquoise4", "tan4", "tan3", poisson, expression(atop('Motorized travel time to health care', paste('in minutes, 2019'))),"malaria positives")

p_all= plots[[1]]
p_all


#chopped off boundaries 
dhs_access_plot = cbind(df_access, region, positives, num_tested) 
is.na(dhs_access_plot['motor_travel_healthcare'])<- dhs_access_plot['motor_travel_healthcare'] > 25
dhs_access_plot$rate = (dhs_access_plot$positives/dhs_access_plot$num_tested) 
dhs_access_plot = dhs_access_plot  %>%  pivot_longer(!c(region, positives, num_tested, rate),names_to='x_label', values_to='values')
df_list = split(dhs_access_plot, dhs_access_plot$x_label)

plots <- plots_fun3(df_list,'positives', "turquoise4", "tan4", "tan3", poisson, expression(atop('Motorized travel time to health care', paste('in minutes, 2019'))),"malaria positives")

p_all2 = p_all + plots[[1]]

dhs_access_plot = cbind(df_access, region, positives, num_tested) 
is.na(dhs_access_plot['motor_travel_healthcare'])<- dhs_access_plot['motor_travel_healthcare'] > 20
dhs_access_plot$rate = (dhs_access_plot$positives/dhs_access_plot$num_tested) 
dhs_access_plot = dhs_access_plot  %>%  pivot_longer(!c(region, positives, num_tested, rate),names_to='x_label', values_to='values')
df_list = split(dhs_access_plot, dhs_access_plot$x_label)

##supplementpub Figure 15
plots <- plots_fun3(df_list,'rate', "turquoise4", "tan4", "tan3", poisson, expression(atop('Motorized travel time to health care', paste('in minutes, 2019'))),'malaria positivity rate')

p_all3 = p_all2 +  plots[[1]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))

ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_accessibility_variable_distribution_bivariate.pdf'), p_all3, width = 8, height =2.25)


#2018 data alone 
acc_df = read.csv(file.path(ExDir, "cleaned_datasets", "2018_mobility_DHS_variables_urban_malaria.csv"), header = T, sep = ',') %>% dplyr::select(v001, dhs_year, trips_man, trips_woman, duration_travel_woman, duration_travel_man)
df_18 = df_all %>%  filter(dhs_year == '2018') %>%  dplyr::select(v001, positives)
df_18_all = left_join(df_18, acc_df, by =c('v001'))
acc_df = acc_df %>%  dplyr::select(-c(v001, dhs_year))
#variable distribution and cumulative distribution 

acc_long = acc_df %>%  pivot_longer(everything(),names_to='x_label', values_to='values')
df_list =split(acc_long, acc_long$x_label)
#df_list_ordered = list(df_list$agri_worker_partner, df_list$seasonal_work_man, df_list$seasonal_work_woman)


xlab=list('% of men away for more than one month in the last 12 months', '% of women away for more than one month in the last 12 months',
          'Number of times males were away from home in the last 12 months','Number of times women were away from home in the last 12 months')


p = pmap(list(df_list,'#00A08A', '#00A08A', 'values', xlab, 25), cdf_hist)
p=p[[1]]+ p[[2]]+ p[[3]]+ p[[4]]
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_trips_duration_distribution.pdf'), p, width = 8.5, height =2.25)

#relationship with malaria positives
dhs_acc_plot = df_18_all  %>% dplyr::select(duration_travel_woman, duration_travel_man,trips_man,trips_woman, positives) %>%  pivot_longer(!c(positives),names_to='x_label', values_to='values')
df_list = split(dhs_acc_plot, dhs_acc_plot$x_label)


#unadjusted positives chopped boundary 

plots <- plots_fun2(df_list, 'positives', "dodgerblue3", "darkred", "darksalmon", poisson,"malaria positives")

p<- plots[[1]]+plots[[2]]+ plots[[3]] + plots[[4]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p

ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_trips_duration_bivariate_distribution.pdf'), p, width = 8.5, height =2.25)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_trips_duration_bivariate_distribution.png'), p, width = 8.5, height =2.25)


## --------------------------------------------------------------------------------------------------------------------------
### Environmental factors variable distribution, cumulative distribution, correlation and relationship with malaria prevalence 
## --------------------------------------------------------------------------------------------------------------------------
#variable distribution and cumulative distribution 
df_env = data.frame(`Precipitation` = df_all$precipitation_monthly_0m, `Temperature` = df_all$temperature_monthly_0m, `Surface soil moisture` =df_all$soil_wetness_0m,
                    `Distance to water bodies` = df_all$dist_water_bodies_0m, `Elevation` = df_all$elevation_4000m, `Enhanced Vegetation Index`= df_all$EVI_0m) 

df_env$Elevation <- replace(df_env$Elevation, which(df_env$Elevation < 0), NA)

df_env_long = df_env %>%  pivot_longer(everything(),names_to='x_label', values_to='values')

df_list =split(df_env_long, df_env_long$x_label)
df_list_ordered = list(df_list$Precipitation,df_list$Temperature, df_list$Surface.soil.moisture,df_list$Distance.to.water.bodies,
                       df_list$Elevation, df_list$Enhanced.Vegetation.Index)


xlab=list(expression(atop('Total precipitation', paste('(meters depth)'))), expression(atop('Temperature', paste('(°C)'))),expression(atop('Surface soil', paste('moisture (GSM)'))),expression(atop('Distance to water', paste('bodies (meters)'))), expression(atop('Elevation', paste('(meters)'))),
          expression(atop('Enhanced Vegetation',  paste('Index'))))

#Figure 8
p = pmap(list(df_list_ordered,'dodgerblue3', 'dodgerblue3', 'values', xlab, 25), cdf_hist)
p=p[[1]]+ p[[2]]+ p[[3]]+p[[4]] + p[[5]]+ p[[6]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_environmental_variable_distribution.pdf'), p, width = 8, height =4.6)

#supplementpub Figure 16
files <- list.files(path = file.path(GeoDir) , pattern = "month_07", full.names = TRUE, recursive = TRUE)
files<-sapply(files, read.csv, simplify = F)
preci_2010 = cdf_hist(files[[1]] %>% filter(dhs_year == 2010), 'dodgerblue3', 'dodgerblue3','month_07_0m',  expression(atop('Total precipitation, July 2010', paste('(meters depth)'))), 25) + xlim(0,0.02)
preci_2015 = cdf_hist(files[[1]] %>% filter(dhs_year == 2015), 'dodgerblue3', 'dodgerblue3','month_07_0m',  expression(atop('Total precipitation, July 2015', paste('(meters depth)'))), 25) + xlim(0,0.02)
preci_2018 = cdf_hist(files[[1]] %>% filter(dhs_year == 2018), 'dodgerblue3', 'dodgerblue3','month_07_0m',  expression(atop('Total precipitation, July 2018', paste('(meters depth)'))), 25) + xlim(0,0.02)
preci_2021 = cdf_hist(files[[1]] %>% filter(dhs_year == 2021), 'dodgerblue3', 'dodgerblue3','month_07_0m',  expression(atop('Total precipitation, July 2021', paste('(meters depth)'))), 25) + xlim(0,0.02)
all_precip = preci_2010 + preci_2015 + preci_2018 + preci_2021
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_precipitation_july_all_DHS.pdf'), all_precip, width = 8, height =4.6)

#supplementpub Figure 17
#correlation 
colnames(df_env)= c('Total precipitation',
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
corr
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), 'correlation_coefficients_environmental.pdf'), corr, width = 8.5, height = 4.5)

#supplementpub Figure 18
dhs_env_plot = cbind(df_env, region, positives, num_tested) 
dhs_env_plot$rate = (dhs_env_plot$positives/dhs_env_plot$num_tested) 
dhs_env_plot = dhs_env_plot  %>%  pivot_longer(!c(region, positives, num_tested, rate),names_to='x_label', values_to='values')
df_list = split(dhs_env_plot, dhs_env_plot$x_label)
names(df_list) <- c("Distance.to.water.bodies", "Elevation", "Enhanced.Vegetation.Index", "Surface.soil.moisture", "Temperature", "Precipitation")
df_list_ordered = list(df_list$Precipitation,df_list$Temperature, df_list$Surface.soil.moisture, df_list$Distance.to.water.bodies,
                       df_list$Elevation, df_list$Enhanced.Vegetation.Index)

#unadjusted positives
plots <- plots_fun(df_list_ordered,'positives', "slateblue4", "yellow4", "yellow1", poisson,"malaria positives")

p<- plots[[1]]+plots[[2]]+ plots[[3]]+ plots[[4]]+ plots[[5]]+ plots[[6]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_environmental_variable_bivariate_positives.pdf'), p, width = 8.5, height =4.5)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_environmental_variable_bivariate_positives.png'), p, width = 8.5, height =4.5)

#supplement pub Figure 19
#rate
names(df_env) <- c("Precipitation", "Temperature", "Surface.soil.moisture", "Distance.to.water.bodies","Elevation", "Enhanced.Vegetation.Index")
dhs_env_plot = cbind(df_env, region, positives, num_tested) 
is.na(dhs_env_plot$Precipitation) = dhs_env_plot$Precipitation > 400
is.na(dhs_env_plot$Elevation) = dhs_env_plot$Elevation > 750
is.na(dhs_env_plot$Elevation) = dhs_env_plot$Elevation < 0
dhs_env_plot$rate = (dhs_env_plot$positives/dhs_env_plot$num_tested) 
dhs_env_plot = dhs_env_plot  %>%  pivot_longer(!c(region, positives, num_tested, rate),names_to='x_label', values_to='values')
df_list = split(dhs_env_plot, dhs_env_plot$x_label)
df_list_ordered = list(df_list$Precipitation,df_list$Temperature, df_list$Surface.soil.moisture, df_list$Distance.to.water.bodies,
                       df_list$Elevation, df_list$Enhanced.Vegetation.Index)

xlab=list(expression(atop('Precipitation', paste('(meters depth)'))), expression(atop('Temperature', paste('(?C)'))),expression(atop('Surface soil', paste('moisture (GSM)'))),expression(atop('Distance to water', paste('bodies (meters)'))), expression(atop('Elevation', paste('(meters)'))),
          expression(atop('Enhanced Vegetation',  paste('Index'))))


plots <- plots_fun(df_list_ordered,'rate', "slateblue4", "yellow4", "yellow4", quasipoisson,"malaria positivity rate")

p<- plots[[1]]+plots[[2]]+ plots[[3]]+ plots[[4]]+ plots[[5]]+ plots[[6]]+ plot_annotation(tag_levels = 'A')& 
  theme(plot.tag = element_text(size = 12, face = 'bold'))
p

ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_environmental_variable_bivariate_positive_rate_choppedx.pdf'), p, width = 8, height =4.5)
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_environmental_variable_bivariate_positive_rate_choppedx.png'), p, width = 8, height =4.5)






























































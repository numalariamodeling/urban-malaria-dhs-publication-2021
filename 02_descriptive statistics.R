rm(list=ls())
#memory.limit(size = 50000)

# ----------------------------------------------------
### Directories
# ----------------------------------------------------

user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
ProjectDir <- file.path(NuDir, 'data', 'nigeria_dhs' , 'data_analysis')
DataDir <- file.path(ProjectDir, "data")
DHSData <- file.path(DataDir, 'DHS')
DataIn <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'DHS_survey_extract')
ResultDir <-file.path(ProjectDir, "results", "research_plots")
HisDir <-file.path(ResultDir, "histograms")
MapsDir <- file.path(ResultDir, "maps")
CsvDir <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'cleaned_cluster_covariates_all', 'New_082321')


# ----------------------------------------------------
### Required functions and settings
## ----------------------------------------------------
source("./functions/descriptive_analysis_functions.R")


## ----------------------------------------------------------------
### Read in computed DHS cluster data and generate related figures  
## ----------------------------------------------------------------
df <- read.csv(file.path(CsvDir, "all_DHS_variables_urban_malaria.csv"), header = T, sep = ',') 

#figure 1
p1 = igv.lm.point(df$num_child_6_59, df$child_6_59_tested_malaria,df$dhs_year,  "Survey year", 'Number of children 6 - 59 months', 'Number of children 6 - 59 months \n tested for malaria')
p1= p1 +geom_smooth(method=lm, color = "black")+ theme(legend.position = 'none')
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_Figure_1_sample_overview.pdf'), p1, width = 13, height = 9)



#values used in manuscript texts 
table(df$dhs_year)
nrow(!is.na(subset(df, dhs_year ==2010 & num_child_6_59 >=20)))
nrow(!is.na(subset(df, dhs_year ==2015 & num_child_6_59 >=20)))
nrow(!is.na(subset(df, dhs_year ==2018 & num_child_6_59 >=20)))

#figure 2a
df_tested = data.frame(values = df$child_6_59_tested_malaria, category = 'tested')
df_positives = data.frame(values = df$positives, category = 'positives')
df_all = rbind(df_tested, df_positives)
p2 = hist_fun(df_all, df_all$values, df_all$category, 'Number of children 6 - 59 months', 'Count', c("Positive tests", "Tested"))


  
#figure 2b
#examine the number of children tested 
p3 = igv.lm.point(df$child_6_59_tested_malaria, df$positives, df$dhs_year, 'Survey year', 'Number of children 6 - 59 months \n tested for malaria', 'Number of positive tests' )
p3_ = p3 + geom_abline(slope=1, intercept=c(0,0), size = 0.9) +geom_smooth(method=lm, color = "black")



#figure 2c
#load spatial points
sf18 <- st_read(file.path(DHSData, "Downloads", "NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp"),) 
sf15 <- st_read(file.path(DHSData, "Downloads", "NG_2015_MIS_06192019/NGGE71FL/NGGE71FL.shp"),) 
sf10 <- st_read(file.path(DHSData, "Downloads", "NG_2010_MIS_06192019/NGGE61FL/NGGE61FL.shp"),) 
sf_all <- rbind(sf18, sf15, sf10) %>%filter(URBAN_RURA == "U") %>%  rename(v001 = DHSCLUST)

#read in state shape file 
stateshp <- readOGR(file.path(DataDir, "shapefiles","gadm36_NGA_shp"), layer ="gadm36_NGA_1",use_iconv=TRUE, encoding= "UTF-8")
state_sf <- st_as_sf(stateshp)


#data wrangling
df <- read.csv(file.path(CsvDir, "all_DHS_variables_urban_malaria.csv"), header = T, sep = ',') 
df <- df %>%  dplyr::select(v001, positives, child_6_59_tested_malaria, DHSYEAR=dhs_year)
map <- sf_all %>% left_join(df, by=c('v001', 'DHSYEAR'))  %>%  filter(LATNUM != 0) 
map$positives_prop <- round(map$positives/map$child_6_59_tested_malaria, 1)
map$positives_cut <- cut(map$positives_prop, breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), include.lowest = TRUE)
df_count <- map %>% dplyr::select(positives_cut) %>%  group_by(positives_cut) %>%  summarize(`Count` = n())


#big map 
map_big <- gmap_fun(state_sf, map, labels=c(paste0('0 - 0.2',  ' (', df_count$Count[[1]], ')'), 
          paste0('0.3 - 0.4',  ' (', df_count$Count[[2]], ')'), paste0('0.5 - 0.6',  ' (', df_count$Count[[3]], ')'), 
          paste0('0.7 - 0.8',  ' (', df_count$Count[[4]], ')'), paste0('0.9 - 1.0',  ' (', df_count$Count[[5]], ')'), 
          'Missing data'),
            map$positives_cut, 'Test positivity rate (overall count)')


#Lagos 
state_lagos = dplyr::filter(state_sf, (NAME_1 %in% c('Lagos')))
map_lagos = dplyr::filter(map, (ADM1NAME %in% c('LAGOS')))
map_lag <- gmap_fun(state_lagos, map_lagos, labels=c('0 - 0.2', '0.3 - 0.4', '0.5 - 0.6', '0.7 - 0.8', '0.9 - 1.0', 'Missing data'),
                    map_lagos$positives_cut, 'Test positivity rate')
map_lag <- map_lag + theme(legend.position = 'none', panel.border = element_rect(colour = "black", fill=NA, size=0.5))+ xlab('Lagos')

#Anambra 
state_anambra = dplyr::filter(state_sf, (NAME_1 %in% c('Anambra')))
map_anambra = dplyr::filter(map, (ADM1NAME %in% c('ANAMBRA')))
map_anam <- gmap_fun(state_anambra, map_anambra, labels=c('0 - 0.2', '0.3 - 0.4', '0.5 - 0.6', '0.7 - 0.8', '0.9 - 1.0', 'Missing data'),
                     map_anambra$positives_cut, 'Test positivity rate')
map_anam <- map_anam + theme(legend.position = 'none', panel.border = element_rect(colour = "black", fill=NA, size=0.5))+ xlab('Anambra')


#rivers 
state_rivers = dplyr::filter(state_sf, (NAME_1 %in% c('Rivers')))
map_rivers = dplyr::filter(map, (ADM1NAME %in% c('RIVERS')))
map_riv <- gmap_fun(state_rivers, map_rivers, labels=c('0 - 0.2', '0.3 - 0.4', '0.5 - 0.6', '0.7 - 0.8', '0.9 - 1.0', 'Missing data'),
                    map_rivers$positives_cut, 'Test positivity rate')
map_riv <- map_riv + theme(legend.position = 'none', panel.border = element_rect(colour = "black", fill=NA, size=0.5))+ xlab('Rivers')


patch1 <- ((map_lag /(map_anam + map_riv))| map_big)+ plot_layout(ncol = 2)
patch2 <- (p2+ p3_)/ patch1 + plot_layout(nrow = 2)+  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold', size = 16))
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_Figure_2_low_positivity_viz.pdf'), patch2, width = 13, height = 9)


map_low_values <- map %>% na.omit(positives) %>%  filter(positives_prop == 0) %>%  group_by(ADM1NAME) %>%  summarise(n())
map_low <- map_low_values %>%  filter(`n()` >15)
cluster <- map %>% na.omit(positives)



#figure 3
#trends by DHS year 
df <- read.csv(file.path(CsvDir, "all_DHS_variables_urban_malaria.csv"), header = T, sep = ',') 
trend_data<- df %>%  mutate(positives_prop = positives/child_6_59_tested_malaria)
table(trend_data$first_interview_month)

trend_data$month_year <- paste0(trend_data$first_interview_month, "_", trend_data$dhs_year)
table(trend_data$month_year)

trend_data_10 <- trend_data[trend_data$first_interview_month ==10,]
p_all_10 <- gdensity_fun(trend_data_10, trend_data_10$positives_prop, trend_data_10$dhs_year, "Survey year", 
'Test positivity rate for clusters sampled in october', 'Density')

trend_data_11 <- trend_data[trend_data$first_interview_month ==11,]
p_all_11 <- gdensity_fun(trend_data_11, trend_data_11$positives_prop, trend_data_11$dhs_year, "Survey year", 
                         'Test positivity rate for clusters sampled in November', 'Density')


trend_data_12 <- trend_data[trend_data$first_interview_month ==12,]
p_all_12 <- gdensity_fun(trend_data_12, trend_data_12$positives_prop, trend_data_12$dhs_year, "Survey year", 
                         'Test positivity rate for clusters sampled in November', 'Density')

all_plots <- p_all_10 / p_all_11 / p_all_12 +  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold', size = 16))
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_Figure_3_malaria_tests_positivity_trends.pdf'), all_plots, width = 13, height = 9)
data <- data.frame(pos =trend_data_12[trend_data_12$dhs_year == 2010,'positives_prop'])
data_ <- data %>%  filter(pos == 0)




## ----------------------------------------------------------------
### Covariate plots for DHS and geospatial variables   
## ----------------------------------------------------------------




## ----------------------------------------------------------------
### bivariate plots for DHS and geospatial variables   
## ----------------------------------------------------------------
df <- read.csv(file.path(CsvDir, "all_DHS_variables_urban_malaria.csv"), header = T, sep = ',') 
df<- df %>%  mutate(positives_prop = positives/child_6_59_tested_malaria)
df$positives_cut <- cut(df$positives_prop, breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), include.lowest = TRUE)

ggplot(df) +
  geom_histogram(aes(x = edu_a), bins = 4, colour = "black", fill = "white") +
  facet_wrap(~positives_prop)

df_check <- df %>%  group_by(edu_a) %>%  summarize(mean=mean(positives), sd=sd(positives), n=n())

ggplot(df) +
  geom_histo(aes(x = edu_a), bins = 4, colour = "black", fill = "white") +
  facet_wrap(~positives_prop)








































#________________________________all coveriates histogram plots 200m buffer_________________________________


clu_df_cont <- clu_df_10_18[ , -which(names(clu_df_10_18) %in% c("shstate", "region", "X"))]
clu_df_cont$y <- ifelse(clu_df_cont$p_test < 0.1, 0,1) %>% (as.numeric)


labels_data <- list("Education", "Floor type", "Household size", "Housing quality", "Mean age", 
                    "Median age", "Net use", "Roof type", "Under five population" ,"Wall type",
                    "Wealth", "Fever", "Pregnant women", "Female population", "Under five Female population", 
                    "Under five net use", "Malaria prevalence", "Fever treatment", "Under five ACT use","Travel time to city", 
                    "Building density", "Dominant vector", "Elevation", "Time to travel one metre 2015","Housing quality 2000", 
                    "Housing quality 2015", "Time to travel one metre 2019","Travel time to healthcare", "Under five population density (FB)", "Population density",
                    "Secondary vector", "Temperature", "Time to walk one metre", "Walk time to healthcare")

xlab_data <- list("percent", "percent", "number", "percent", "number", 
                  "number", "percent", "percent","percent" ,"percent", 
                  "percent", "percent", "percent", "percent", "percent",
                  "percent", "percent", "percent", "percent","Mean minutes", 
                  "Mean", "Mean", "Mean", "Mean minutes", "percent",
                  "percent", "Mean minutes","Mean minutes", "Mean", "Mean",
                  "Mean", "Mean", "Mean minutes", "Mean minutes")

colr_data <- rep(c("orangered", "dodgerblue", "darkorchid","green4","tan4","turquoise"), times =c(1,9,9,5,7,3))


label_list <- c(17,1,11,4,25,26,2,8,10,21,
                3,5,6,9,29,15,14,13,30,
                12,7,16,18,19,
                23,20,24,27,33,28,34,
                22,31,32)

var_list <- c(label_list+3)
colr_list <- c(1:34)
plot_list = list()

for (i in 1:34) { 
  p<- ggplot(clu_df_cont, aes_string(x=names(clu_df_cont)[var_list[[i]]])) + 
    geom_histogram(fill = colr_data[colr_list[[i]]])+
    theme_minimal()+
    theme(panel.border = element_rect(fill = NA,color = "black", size=0.2, linetype = 'solid'),
          text=element_text(size=7), 
          axis.title.x = element_text(size=7, hjust = 0.5, vjust = +3),
          title = element_text(size=7),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(margin = margin(r = -1.7)),
          axis.text.y = element_text(margin = margin(r = -1.7))) +
    labs (title = labels_data[label_list[[i]]], x = "values") +
    xlab(xlab_data[label_list[[i]]]) +
    ylab("")
  plot_list[[i]]<-p
}


variable1 <- ggarrange(NULL,NULL,get_legend(plot_list[[2]] + theme(legend.position="bottom")),NULL,NULL,
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


variables <- annotate_figure(variable1, top = text_grob("Distribution of covariates", 
                                                        color = "Black", face = "bold", size = 14),
                             left = text_grob("Count", color = "Black", size = 14,  rot = 90))
ggsave(paste0(HisDir, '/', Sys.Date(),  'histograms.pdf'), variables, width=13, height=13)

#__________________________

for (i in 1:34) { 
  #clu_df_cont$colors <- ifelse(clu_df_cont$p_test < 0.1, "blue", "green")
  p<- ggplot(clu_df_cont, aes_string(x=names(clu_df_cont)[var_list[[i]]])) + 
    geom_histogram(aes(position="stack", group = y, fill=y))+
    theme_minimal()+
    theme(panel.border = element_rect(fill = NA,color = "black", size=0.2, linetype = 'solid'),
          text=element_text(size=7), 
          axis.title.x = element_text(size=7, hjust = 0.5, vjust = +3),
          title = element_text(size=7),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(margin = margin(r = -1.7)),
          axis.text.y = element_text(margin = margin(r = -1.7)),
          legend.position = "none") +
    labs (title = labels_data[label_list[[i]]], x = "values") +
    xlab(xlab_data[label_list[[i]]]) +
    ylab("")
  plot_list[[i]]<-p
}

variable1 <- ggarrange(NULL,NULL,get_legend(plot_list[[2]] + theme(legend.position="bottom")),NULL,NULL,
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


variables <- annotate_figure(variable1, top = text_grob("Distribution of covariates", 
                                                          color = "Black", face = "bold", size = 14),
                               left = text_grob("Count", color = "Black", size = 14,  rot = 90))
ggsave(paste0(HisDir, '/', Sys.Date(),  'histograms_y.pdf'), variables, width=13, height=13)



#All buffers plot - according to theme

fill0 <-  c("orangered", "dodgerblue", "darkorchid","green4","tan4", "turquoise", "orangered", "dodgerblue", "darkorchid","green4","tan4", "turquoise")
fill1 <- c("dodgerblue", "darkorchid","green4","tan4", "turquoise", "orangered", "dodgerblue", "darkorchid","green4","tan4", "turquoise")
fill2 <- c("darkorchid", "darkgoldenrod1", "red1","cornflowerblue","hotpink","darkorchid", "darkgoldenrod1", "red1","cornflowerblue","hotpink")
fill3 <- c("green4", "chocolate2", "chocolate3","gold","cornflowerblue","chocolate1", "chocolate2", "chocolate3","chocolate4","chocolate")
fill4 <- c("magenta", "limegreen", "gold2","gray48","tan4","magenta", "limegreen", "gold2","gray48","tan4")
fill5 <- c("lightcoral", "darkgoldenrod", "brown1","darkslategray4","turquoise","lightcoral", "darkgoldenrod", "brown1","darkslategray4","turquoise")



#All buffers plot - grouped in two


labels_data <- list("Malaria prevalence",
                    "Education","wealth","Housing quality","Floor type", "Roof type",
                    "Wall type","Household size","Mean age","Median age","Pregnant women",
                    "Net use","Under five net use","Fever cases","Fever treatment","Under five ACT use",
                    "Female population","Under five population","Under five Female population",
                    
                    "Population density","Under five population density (FB)","Housing quality 2000","Housing quality 2015","building",
                    "Elevation","Travel time to city","Time to travel one metre 2015","Time to travel one metre 2019","Time to walk one metre",
                    "Travel time to healthcare","Walk time to healthcare","Dominant vector","Secondary vector","Temperature", 
                    "v001")

name_list <- c("test", 
               "edu","wealth","housing_q","floor", "roof_type", 
               "wall","household_size","mean_age","median_age","preg_women",
               "net_use_all","net_use_child","fever_cases","med_treat_fever","ACT_use_U5",
               "all_female_sex","U5_pop","female_child_sex",
               
               "pop_density","U5_FB","housing_2000","housing_2015","building", 
               "elev_","minutes_to_city","travel_metre_2015","travel_metre_2019","walking_metre", 
               "travel_healthcare","walking_healthcare","dominant","secondary_vector","temp_all_yrs",
               "v001")


fill_list <- rep(list(fill0, fill2,fill5), times =c(1,18,15))
label_list <- c(1:34)


for (i in 1:34) { 
  melteddf <- melt(dplyr::select(clu_df_cont, "v001", matches(name_list[[i]])), id="v001", na.rm=T)
  fill_select <- colr_list[i]
  p<- ggplot(melteddf, aes_string(x= "value", fill = "variable", color = "variable")) +
    geom_freqpoly(size = 0.7) +
    theme_minimal()+
    theme(panel.border = element_rect(fill = NA,color = "black", size=0.2, linetype = 'solid'),
          text=element_text(size=7), 
          axis.title.x = element_text(size=7, hjust = 0.5, vjust = +3),
          title = element_text(size=7),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(margin = margin(r = -1.7)),
          axis.text.y = element_text(margin = margin(r = -1.7)),
          legend.position = "none") +
    labs (title = labels_data[label_list[[i]]], x = "values") +
    scale_color_manual(labels = c("0m", "1000m", "2000m", "3000m","4000m"), 
                       values = fill_list[[i]]) +
    guides(color=guide_legend("Legend/Buffers")) +
    xlab(xlab_data[label_list[[i]]]) +
    ylab("")
  plot_list[[i]]<-p  
  
}


variable1 <- ggarrange(NULL,NULL,plot_list[[1]],NULL,NULL, 
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

variables <- annotate_figure(variable1, top = text_grob("Distribution of covariates", 
                                                        color = "Black", face = "bold", size = 14),
                             left = text_grob("Count", color = "Black", size = 14,  rot = 90))
ggsave(paste0(HisDir, '/', Sys.Date(),  'freqpoly_durce_grouped.pdf'), variables, width=13, height=13)



#________________________________ geospatial coveriates plots______________________________

#make an urban map of all cluster values 

u_df_18_fin <- df_10_18_fin %>% filter(DHSYEAR == 2018) 
u_df_15_fin <- df_10_18_fin %>% filter(DHSYEAR == 2015) 
u_df_10_fin <- df_10_18_fin %>% filter(DHSYEAR == 2010)


#make cluster maps 

clustermap<-function(cluster_shp, title){
  tm_shape(state_sf) + #this is the health district shapfile with DS estimates info
    tm_polygons()+
    tm_shape(cluster_shp)+ #this is the points shape file with LLIN and number of kids info by cluster 
    tm_bubbles(size =0.2, col = "p_test", 
               border.col= "black", palette="seq",textNA = "Missing",
               breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=T)+
    tm_layout(aes.palette = list(seq ="-RdYlBu"), title = title)+
    tm_legend(legend.title.size = 0.8, legend.just="top")
}

map_18<-clustermap(u_df_18_fin, "2018 malaria prevalence by cluster (DHS)")
map_15 <-clustermap(u_df_15_fin, "2015 malaria prevalence by cluster (DHS)")
map_10 <-clustermap(u_df_10_fin, "2010 malaria prevalence by cluster (DHS)")


urban_map<-tmap_arrange(map_18, map_15, map_10)


tmap_save(tm =urban_map, filename = file.path(ResultDir, "maps", "urban_malaria_maps.pdf"), 
          width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)



#make maps for all variable using their cluster values 


for (i in 1:19){ 
  m<-tm_shape(state_sf) + #this is the health district shapfile with DS estimates info
    tm_polygons()+
    tm_shape(df_10_18_fin)+ #this is the points shape file with LLIN and number of kids info by cluster 
    tm_bubbles(size=0.1, col = names(clu_df_cont)[grepl(name_list[[i]], names(clu_df_cont))], 
               border.col= "black", palette="seq",textNA = "Missing",
               breaks=c(0, 20, 30,40, 50, 60, 70, 80, 90, 100), legend.col.show=F)+
    tm_layout(aes.palette = list(seq ="-RdYlBu"), main.title = labels_data[label_list[[i]]], 
              main.title.position = "center", main.title.size =0.8)
  plot_list[[i]]<-m
    

}
grid.newpage()

tt <- tmap_grob(plot_list[[1]])

plot_grid(, tmap_grob(plot_list[[2]]),tmap_grob(plot_list[[3]]))

as.ggplot(plot_list[[2]])                   
                        
map_plpot <-tmap_arrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]],plot_list[[6]],
                         plot_list[[7]],plot_list[[8]],plot_list[[9]],plot_list[[10]],plot_list[[11]], 
                         plot_list[[12]],plot_list[[13]],plot_list[[14]],plot_list[[15]],plot_list[[16]],
                         plot_list[[17]], plot_list[[18]], plot_list[[19]],nrow = 4, ncol= 5)

tmap_save(tm =map_plpot, filename = file.path(ResultDir, "maps", "dependent_dhscov_maps.pdf"), 
          width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)



p<- ggplot() + 
  geom_sf(data =state_sf)+
  geom_point(df_10_18_fin, mapping = aes(x = LONGNUM, y = LATNUM, color = edu_a))+
  theme_minimal()+
  theme(panel.border = element_rect(fill = NA,color = "black", size=0.2, linetype = 'solid'),
        text=element_text(size=7), 
        axis.title.x = element_text(size=7, hjust = 0.5, vjust = +3),
        title = element_text(size=7),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(margin = margin(r = -1.7)),
        axis.text.y = element_text(margin = margin(r = -1.7)),
        legend.position = "none") +
  guides(color=guide_legend("Legend/Buffers")) +
  xlab(xlab_data[label_list[[i]]]) +
  ylab("")
  
p

#______________________________
df_10_18_fin <- df_10_18_fin %>% filter(LONGNUM > 0.000000)
 
for (i in 1:34) { 
  p<- ggplot() + 
    geom_sf(data =state_sf)+
    geom_point(df_10_18_fin, mapping = aes_string(x = "LONGNUM", y = "LATNUM", 
                                                  color = names(clu_df_cont)[var_list[[i]]]), size = 0.4)+
    theme_minimal()+
    theme(legend.position = "none", 
          panel.border = element_rect(fill = NA,color = "black", size=0.2, linetype = 'solid'),
          text=element_text(size=7), 
          axis.title.x = element_text(size=7, hjust = 0.5, vjust = +3),
          title = element_text(size=7),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(margin = margin(r = -1.7)),
          axis.text.y = element_text(margin = margin(r = -1.7))) +
    
    labs (title = labels_data[label_list[[i]]], x = "values") +
    xlab(xlab_data[label_list[[i]]]) +
    ylab("")
  plot_list[[i]]<-p
}


variable1 <- ggarrange(NULL,NULL,plot_list[[1]],NULL,NULL, 
                       NULL,NULL,text_grob("DHS covariates", face = "italic", size = 10, color = "darkorchid"),NULL,NULL,
                       plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]],plot_list[[6]],
                       plot_list[[7]],plot_list[[8]],plot_list[[9]],plot_list[[10]],plot_list[[11]], 
                       plot_list[[12]],plot_list[[13]],plot_list[[14]],plot_list[[15]],plot_list[[16]],
                       plot_list[[17]],plot_list[[18]], plot_list[[19]],NULL,NULL,
                       
                       NULL,NULL,text_grob("Geospatial covariates", face = "italic", size = 10, color = "turquoise"),NULL,NULL,
                       plot_list[[20]],plot_list[[21]],plot_list[[22]],plot_list[[23]],plot_list[[24]],
                       
                       
                       plot_list[[25]],plot_list[[26]],plot_list[[27]],plot_list[[28]],plot_list[[29]],
                       plot_list[[30]],plot_list[[31]],plot_list[[32]],plot_list[[33]],plot_list[[34]],
                       
                       nrow = 11, ncol= 5, heights = c(1,0.2,1,1,1,1,0.2,1,1,1, widths = c(2,2,2,2,2)))

variables <- annotate_figure(variable1, top = text_grob("Distribution of covariates", 
                                                        color = "Black", face = "bold", size = 14),
                             left = text_grob("Count", color = "Black", size = 14,  rot = 90))
ggsave(paste0(MapsDir, '/', Sys.Date(),  'maps_grouped.pdf'), variables, width=13, height=18)






#________________________ plotting goespatials averages per state

state_sf <- state_sf %>% mutate(NAME_1 = case_when(NAME_1 == "Federal Capital Territory" ~ "Fct Abuja",
                                                     NAME_1 == "Nassarawa" ~ "Nasarawa",
                                                     TRUE ~ as.character(NAME_1)))%>% mutate(NAME_1 = tolower(NAME_1))


state_mean <- clu_df_10_18 %>% group_by(names(clu_df_cont[3:38])) %>% summarise(Mean_sales = mean(Sales))

  
sf <- left_join(state_mean, state_sf, by =c("shstate" = "NAME_1"))




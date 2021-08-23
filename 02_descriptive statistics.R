
x <- c("tidyverse","INLA", "ggplot2", "ggpubr",  "rgdal", "sp", "sf", "tmap", 
       'paletteer', 'cowplot', 'gridExtra', 'lme4', 'reshape2', "patchwork", "gdata",'cowplot', 'mmtable2', 'ggsci') #"inlabru","rebus"




lapply(x, library, character.only = TRUE) #applying the library function to packages

options(repr.plot.width = 14, repr.plot.height = 8)

#remotes::install_github("ianmoran11/mmtable2")



#_________________________________Directories

user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
ProjectDir <- file.path(NuDir, 'data', 'nigeria_dhs' , 'data_analysis')
DataDir <- file.path(ProjectDir, "data")
DHSData <- file.path(DataDir, 'DHS')
DataIn <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'DHS_survey_extract')
Rdata <- file.path(DataDir, 'DHS', 'Subset_data', "urban_malaria_rdata")
ResultDir <-file.path(ProjectDir, "results", "research_plots")
HisDir <-file.path(ResultDir, "histograms")
MapsDir <- file.path(ResultDir, "maps")
BinDir <- file.path(ProjectDir, "bin")
SrcDir <- file.path(ProjectDir, 'src', 'Research', 'urban_rural_transmission_analysis')
RastDir <- file.path(DataDir, "Raster_files")
CsvDir <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'cleaned_cluster_covariates_all')


#source functions 

#_______________________________ Load pre-clustered data:
df <- read.csv(file.path(CsvDir, "all_cluster_variables_urban_malaria_0m_geospatial.csv"), 
                         header = T, sep = ',') 

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

#__________________________________Loading Spactial pointd

# urban cluster points

dhs18_sf <- st_read(file.path(DHSData, "Downloads", 
                              "NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp"),) 
mis15_sf <- st_read(file.path(DHSData, "Downloads", "NG_2015_MIS_06192019/NGGE71FL/NGGE71FL.shp"),) 
mis10_sf <- st_read(file.path(DHSData, "Downloads", "NG_2010_MIS_06192019/NGGE61FL/NGGE61FL.shp"),) 
sf_10_18 <- rbind(dhs18_sf, mis15_sf, mis10_sf) %>%filter(URBAN_RURA == "U") %>%  rename(v001 = DHSCLUST)


# join dhs variables to cluster points by year 
#df_10_18_fin <- left_join(sf_10_18, clu_df_10_18, by = 
                            #c("DHSCLUST" = "v001", "DHSYEAR" = "dhs_year"))


#
###################################################################################
##_______________________________________urban barplots ___________________________
###################################################################################
paletteer::paletteer_d("awtools::a_palette")

#figure 1
p_all = ggplot(clu_df_10_18, aes(x=num_child_6_59, y=child_6_59_tested_malaria)) +
  geom_point(aes(size=child_6_59_tested_malaria, fill=as.factor(dhs_year)), alpha=0.7, shape=21, color = 'transparent') + 
  geom_smooth(method=lm)+
  scale_fill_igv()+
  stat_cor(method = "pearson", col='darkred') + 
  scale_size(range = c(2, 25), name="Number of children tested for malaria", guide =FALSE)+
  theme_bw() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.text.x = element_text(size = 16, color = "black"), 
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size =16),
        legend.title=element_text(size=16, colour = 'black'),
        legend.text =element_text(size = 16, colour = 'black'),
        legend.key.height = unit(1, "cm"))+
  guides(fill=guide_legend(title="Survey year", override.aes = list(size =5)))+
  xlab('Number of children 6 - 59 months')+
  ylab('Children 6 - 59 months tested for malaria')

ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_Figure1_number_child_compared_number_tested.pdf'), p_all, width = 13, height = 9)

table(clu_df_10_18$dhs_year)
y = clu_df_10_18$num_child_6_59[clu_df_10_18$dhs_year ==2010]
table(y)
y = clu_df_10_18$num_child_6_59[clu_df_10_18$dhs_year ==2015]
table(y)
y = clu_df_10_18$num_child_6_59[clu_df_10_18$dhs_year ==2018]
table(y)


#figure 2
#examine the number of children tested 
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

#what is their spatial distribution?
#read in state shape file 
stateshp <- readOGR(file.path(DataDir, "shapefiles","gadm36_NGA_shp"), layer ="gadm36_NGA_1",
                    use_iconv=TRUE, encoding= "UTF-8")
state_sf <- st_as_sf(stateshp)



df <- clu_df_10_18 %>%  dplyr::select(v001, positives, child_6_59_tested_malaria, DHSYEAR=dhs_year)
map <- sf_10_18 %>% left_join(df, by=c('v001', 'DHSYEAR'))  %>%  filter(LATNUM != 0) 
map$positives_prop <- round(map$positives/map$child_6_59_tested_malaria, 1)
map$positives_cut <- cut(map$positives_prop, breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), include.lowest = TRUE)
levels(map$positives_cut) <- c(levels(map$positives_cut),"Missing data")
map$positives_cut[is.na(map$positives_cut)] <- "Missing data"

summary(map$positives_prop)

#plot(clu_df_10_18$num_child_6_59, clu_df_10_18$child_6_59_tested_malaria)# make plot and add to methods 


#table data 
df <- map %>% select(positives_cut) %>%  group_by(positives_cut) %>%  summarize(`Count` = n())  
st_geometry(df)<- NULL
colnames(df)[1]<- 'Category'

#map data 
map_plot<-ggplot(state_sf) +
  geom_sf(color='lightgrey')+
  geom_point(data = map,
    aes(color = positives_cut,  geometry = geometry),
    stat = "sf_coordinates"
  ) +
  viridis::scale_color_viridis(option='C', discrete=TRUE, labels=c('0 - 0.2 (604)', '0.3 - 0.4 (98)', '0.5 - 0.6 (47)', '0.7 - 0.8 (17)', '0.9 - 1.0 (5)', 'Missing data')) +
  scale_size_continuous(guide = FALSE)+
  labs(color='Test positivity rate (count)') + 
  ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank(),
                 rect = ggplot2::element_blank(),
                 plot.background = ggplot2::element_rect(fill = "white", colour = NA), 
                 legend.title.align=0.5,
                 legend.title=element_text(size=16, colour = 'black'), 
                 legend.text =element_text(size = 16, colour = 'black'),
                 legend.key.height = unit(0.65, "cm")) +
  guides(color = guide_legend(override.aes = list(size = 5)))+
  xlab("")+
  ylab("")

map_plot<- ggdraw(align_legend(map_plot))

state_lagos = dplyr::filter(state_sf, (NAME_1 %in% c('Lagos')))
map_lagos = dplyr::filter(map, (ADM1NAME %in% c('LAGOS')))
map_lag<-ggplot(state_lagos) +
  geom_sf(color='lightgrey')+
  geom_point(data = map_lagos,
             aes(color = positives_cut, geometry = geometry),
             stat = "sf_coordinates"
  ) +
  viridis::scale_color_viridis(option='C', discrete=TRUE) +
  scale_size_continuous(guide = FALSE)+
  labs(color='Test positivity rate (count)') + 
  ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank(),
                 rect = ggplot2::element_blank(),
                 plot.background = ggplot2::element_rect(fill = "white", colour = NA), 
                 legend.position = 'none',
                 panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  guides(color = guide_legend(override.aes = list(size = 5)))+
  theme(plot.margin = unit(c(0,0,0,0), "pt")) +
  xlab("Lagos")+
  ylab("")


state_anambra = dplyr::filter(state_sf, (NAME_1 %in% c('Anambra')))
map_anambra = dplyr::filter(map, (ADM1NAME %in% c('ANAMBRA')))
map_anam<-ggplot(state_anambra) +
  geom_sf(color='lightgrey')+
  geom_point(data = map_anambra,
             aes(color = positives_cut, geometry = geometry),
             stat = "sf_coordinates"
  ) +
  viridis::scale_color_viridis(option='C', discrete=TRUE) +
  scale_size_continuous(guide = FALSE)+
  labs(color='Test positivity rate (count)') + 
  ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank(),
                 rect = ggplot2::element_blank(),
                 plot.background = ggplot2::element_rect(fill = "white", colour = NA), 
                 legend.position = 'none',
                 panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                 plot.title = element_text(margin=margin(0,0,0,0))) +
  guides(color = guide_legend(override.aes = list(size = 5)))+
  xlab("Anambra")+
  ylab("")


state_rivers = dplyr::filter(state_sf, (NAME_1 %in% c('Rivers')))
map_rivers = dplyr::filter(map, (ADM1NAME %in% c('RIVERS')))
map_river<-ggplot(state_rivers) +
  geom_sf(color='lightgrey')+
  geom_point(data = map_rivers,
             aes(color = positives_cut, geometry = geometry),
             stat = "sf_coordinates"
  ) +
  viridis::scale_color_viridis(option='C', discrete=TRUE) +
  scale_size_continuous(guide = FALSE)+
  labs(color='Test positivity rate (count)') + 
  ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank(),
                 rect = ggplot2::element_blank(),
                 plot.background = ggplot2::element_rect(fill = "white", colour = NA), 
                 legend.position = 'none',
                 panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                 plot.title = element_text(margin=margin(0,0,0,0))) +
  guides(color = guide_legend(override.aes = list(size = 5)))+
  xlab("Rivers")+
  ylab("")

patch1 <- ((map_lag /(map_anam + map_river))| map_plot)+ plot_layout(ncol = 2)
pach2 <- (tests + positives)/ patch1 + plot_layout(nrow = 2)+  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold', size = 16))
combo_figure1 <- (tests + positives) / map_plot+  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold', size = 16))
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_Figure1_malaria_tests_positives.pdf'), pach2, width = 13, height = 9)

map_low_values <- map %>% na.omit(positives) %>%  filter(positives_prop == 0) %>%  group_by(ADM1NAME) %>%  summarise(n())
map_low <- map_low_values %>%  filter(`n()` >15)
cluster <- map %>% na.omit(positives)


#figure 3
#trends by DHS year 
trend_data<- clu_df_10_18 %>%  mutate(positives_prop = positives/child_6_59_tested_malaria)
table(trend_data$first_interview_month)

trend_data$month_year <- paste0(trend_data$first_interview_month, "_", trend_data$dhs_year)
table(trend_data$month_year)

trend_data_10 <- trend_data[trend_data$first_interview_month ==10,]

p_all_10 = ggplot(trend_data_10, aes(x=positives_prop, fill=as.factor(dhs_year))) +
  geom_density(alpha=0.5) + 
  scale_x_continuous(expand = c(0.03, 0)) +
  scale_y_continuous(expand = c(0.03, 0)) +
  theme_bw() + 
  scale_fill_manual(values = c('#0073C2B2', "#EE4C97B2", "#E18727B2"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.text.x = element_text(size = 16, color = "black"), 
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size =16),
      legend.position = 'none') + 
  guides(fill=guide_legend(title="Survey year"))+
  xlab('Test positivity rate for clusters sampled in october')+
  ylab('Density')


trend_data_11 <- trend_data[trend_data$first_interview_month ==11,]

p_all_11 = ggplot(trend_data_11, aes(x=positives_prop, fill=as.factor(dhs_year))) +
  geom_density(alpha=0.5) + 
  scale_x_continuous(expand = c(0.03, 0)) +
  scale_y_continuous(expand = c(0.03, 0)) +
  theme_bw() + 
  scale_fill_manual(values = c('#0073C2B2', "#EE4C97B2", "#E18727B2"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.text.x = element_text(size = 16, color = "black"), 
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size =16),
        legend.title=element_text(size=16, colour = 'black'),
        legend.text =element_text(size = 16, colour = 'black'),
        legend.key.height = unit(1, "cm"))+
  guides(fill=guide_legend(title="Survey year"))+
  xlab('Test positivity rate for clusters sampled in november')+
  ylab('Density')


trend_data_12 <- trend_data[trend_data$first_interview_month ==12,]

p_all_12 = ggplot(trend_data_12, aes(x=positives_prop, fill=as.factor(dhs_year))) +
  geom_density(alpha =0.5) +
  scale_x_continuous(expand = c(0.03, 0)) +
  scale_y_continuous(expand = c(0.03, 0)) +
  theme_bw() + 
  scale_fill_manual(values = c('#0073C2B2', "#EE4C97B2", "#E18727B2"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.text.x = element_text(size = 16, color = "black"), 
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size =16),
     legend.position = 'none')+
  guides(fill=guide_legend(title="Survey year"))+
  xlab('Test positivity rate for clusters sampled in december')+
  ylab('Density')


all_plots <- p_all_10 / p_all_11 / p_all_12 +  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold', size = 16))
ggsave(paste0(ResultDir, '/updated_figures/', Sys.Date(), '_Figure3_malaria_tests_positivity_trends.pdf'), all_plots, width = 13, height = 9)
data <- data.frame(pos =trend_data_12[trend_data_12$dhs_year == 2010,'positives_prop'])
data_ <- data %>%  filter(pos == 0)

#figure 4
#something about the covariates 

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




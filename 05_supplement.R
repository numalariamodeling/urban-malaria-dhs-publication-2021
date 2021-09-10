#___________________________________________ Sup map Plots______________________________________________________
name_list = c("edu_a", "wealth", "roof_type", "median_age", "all_female_sex", "preg_women",  
              "net_use_child", "med_treat_fever", "ACT_use_U5",
              
              "pop_density_0m", "pop_den_U5_FB_4000m", "housing_2015_4000m", 
              "EVI_0m","dist_water_bodies_0m", "temperature_monthly_0m", "soil_wetness_0m", "precipitation_monthly_0m",
              "minutes_nearest_city_1000m", "motorized_travel_healthcare_2019_2000m", "LONGNUM", "LATNUM")


dhs = read.csv(file.path(CsvDir, "New_082321", "final_dataset", "final_dataset.csv"), header = T, sep = ',')
# join dhs variables to cluster points by year 
df_10_18_fin <- left_join(sf_all, dhs, by = c("v001" = "v001", "DHSYEAR" = "dhs_year"))%>% filter(URBAN_RURA == "U")

df_10_18_fin <- df_10_18_fin %>% dplyr::select(name_list)

#function
df_10_18_fin = df_10_18_fin %>% filter(LONGNUM > 0.000000)

map_fun2 <- function(polygon_name, point_data, var_n, title, break1, break2, break3, break4, break5, break6,label1, label2, label3, label4, label5){
  point_data[[var_n]] = cut(point_data[[var_n]], breaks=c(break1,break2, break3, break4, break5, break6, NA), include.lowest = TRUE)
  p= ggplot() + 
    geom_sf(data =polygon_name, color='lightgrey')+
    geom_point(point_data, mapping = aes(x = LONGNUM, y = LATNUM, color = point_data[[var_n]]), size =3, alpha =0.6)+
    theme_bw()+
    theme(axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          rect = ggplot2::element_blank(),
          plot.background = ggplot2::element_rect(fill = "white", colour = NA), 
          legend.title.align=0.5,
          legend.title=element_text(size=16, colour = 'black'), 
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(0.65, "cm"),
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank()) +
    labs (title = title, x = "values", size=25) +
    xlab("") +
    ylab("")+
    guides(color=guide_legend(title = "", override.aes = list(size = 5)))+
    scale_color_manual(values = c("#00AFBB", "burlywood4", "#FC4E07", "#E7B800", "green4", "lightgrey"), 
                       labels = c(label1, label2, label3, label4, label5, "NA"))
  
}

#plot maps
edu_map <- map_fun2(state_sf, df_10_18_fin, 1, "Percentage of reproductive-age women with sendary or higher educational attainment", 
                    0, 20, 40, 60, 80, 100, "0 - 20", "20 - 40","40 - 60", "60 - 80", "80 - 100")
wealth_map <- map_fun2(state_sf, df_10_18_fin, 2, "Percentage of reproductive-age women belonging to fourth or higher wealth quantile", 
                       0, 20, 40, 60, 80, 100, "0 - 20", "20 - 40","40 - 60", "60 - 80", "80 - 100")
roof_map <- map_fun2(state_sf, df_10_18_fin, 3, "Percentage of housing with conversional roof type", 
                     0, 20, 40, 60, 80, 100, "0 - 20", "20 - 40","40 - 60", "60 - 80", "80 - 100")
median_map <- map_fun2(state_sf, df_10_18_fin, 4, "Median age",10, 17, 24, 31, 38, 45, "10 - 17", "17 - 24","24 - 31", "31 - 38", "38 - 45")
female_map <- map_fun2(state_sf, df_10_18_fin, 5, "Percentage of female population", 
                       0, 20, 40, 60, 80, 100, "0 - 20", "20 - 40","40 - 60", "60 - 80", "80 - 100")
preg_map <- map_fun2(state_sf, df_10_18_fin, 6, "Percentage of preganant women", 
                     0, 20, 40, 60, 80, 100, "0 - 20", "20 - 40","40 - 60", "60 - 80", "80 - 100")
child_net_map <- map_fun2(state_sf, df_10_18_fin, 7, "Percentage of under five year olds using bed nets", 
                          0, 20, 40, 60, 80, 100, "0 - 20", "20 - 40","40 - 60", "60 - 80", "80 - 100")
treat_fever_map <- map_fun2(state_sf, df_10_18_fin, 8, "Percantage of medicine treated fevers", 0, 20, 40, 60, 80, 100, "0 - 20", "20 - 40","40 - 60", "60 - 80", "80 - 100")
ACT_map <- map_fun2(state_sf, df_10_18_fin, 9, "Percentage of under five year olds using ACT", 
                    0, 20, 40, 60, 80, 100, "0 - 20", "20 - 40","40 - 60", "60 - 80", "80 - 100")
pop_map <- map_fun2(state_sf, df_10_18_fin, 10, "Population density",0, 110, 220, 330, 440, 550, "0 - 110", "110 - 220","220 - 330", "330 - 440", "440 - 550")
pop_U5_map <- map_fun2(state_sf, df_10_18_fin, 11, "Under five population density", 
                       0, 40, 80, 120, 160, 200, "0 - 40", "40 - 80","80 - 120", "160 - 200", "")
housing_map <- map_fun2(state_sf, df_10_18_fin, 12, "Housing quality (2015)", 
                        0, 0.20, 0.40, 0.60, 0.80, 1, "0 - 0.2", "0.2 - 0.4","0.4 - 0.6", "0.6 - 0.8", "")
EVI_map <- map_fun2(state_sf, df_10_18_fin, 13, " Enhanced vegetation index", 
                    0, 0.20, 0.40, 0.60, 0.80, 1, "0 - 0.2", "0.2 - 0.4","0.4 - 0.6", "0.6 - 0.8", "")
water_map <- map_fun2(state_sf, df_10_18_fin, 14, "Distance to water bodies", 
                      0, 2000, 4000, 6000, 8000, 10000, "0 - 2000", "2000 - 4000","4000 - 6000", "6000 - 8000", "8000 - 10000")
temp_map <- map_fun2(state_sf, df_10_18_fin, 15, "Temperature", 
                     18, 21, 24, 27, 30, 33, "18 - 21", "21 - 24","24 - 27", "27 - 30", "30 - 33")
soil_map <- map_fun2(state_sf, df_10_18_fin, 16, "Soil wetness", 0, 0.20, 0.40, 0.60, 0.80, 1, "0 - 0.2", "0.2 - 0.4","0.4 - 0.6", "0.6 - 0.8", "0.8 - 1")
prec_map <- map_fun2(state_sf, df_10_18_fin, 17, "Precipitation",0, 130, 260, 390, 520, 650, "0 - 130", "130 - 260","260 - 390", "390 - 520", "520 - 650")
city_map <- map_fun2(state_sf, df_10_18_fin, 18, "Travel time to nearest city", 
                     0, 90, 180, 270, 360, 450, "0 - 90", "90 - 180","180 - 270", "360 - 450", "")
travel_healthcare_map <- map_fun2(state_sf, df_10_18_fin, 19, "Travel time to healthcare",
                                  0, 20, 40, 60, 80, 100, "0 - 20", "20 - 40","40 - 60", "60 - 80", "80 - 100")

#page1
variable1 = ggarrange(text_grob("Demographic Health Survey covariates", face = "bold", size = 16, color = "dodgerblue"),
                      edu_map, wealth_map, roof_map,  
                      nrow = 4, ncol= 1, heights = c(0.2,1,1,1 , widths = c(1,1,1)))
variables = annotate_figure(variable1, top = text_grob("Supplement Maps", 
                                                       color = "Black", face = "bold", size = 14))
ggsave(paste0(ResultDir, '/maps/', Sys.Date(),  'sup_map1.pdf'), variables, width=8.5, height=13)

#page2
variable1 = ggarrange(median_map, female_map, preg_map,  
                      nrow = 3, ncol= 1, heights = c(1,1,1 , widths = c(1,1,1)))
ggsave(paste0(ResultDir, '/maps/', Sys.Date(),  'sup_map2.pdf'), variable1, width=8.5, height=13)

#page3
variable1 = ggarrange(child_net_map, treat_fever_map, ACT_map,  
                      nrow = 3, ncol= 1, heights = c(1,1,1 , widths = c(1,1,1)))
ggsave(paste0(ResultDir, '/maps/', Sys.Date(),  'sup_map3.pdf'), variable1, width=8.5, height=13)

#page4
variable1 = ggarrange(text_grob("Geospatial covariates", face = "bold", size = 16, color = "dodgerblue"),
                      pop_map, pop_U5_map, housing_map,  
                      nrow = 4, ncol= 1, heights = c(0.2,1,1,1 , widths = c(1,1,1)))
ggsave(paste0(ResultDir, '/maps/', Sys.Date(),  'sup_map4.pdf'), variable1, width=8.5, height=13)

#page5
variable1 = ggarrange(EVI_map, water_map, soil_map,  
                      nrow = 3, ncol= 1, heights = c(1,1,1 , widths = c(1,1,1)))
ggsave(paste0(ResultDir, '/maps/', Sys.Date(),  'sup_map5.pdf'), variable1, width=8.5, height=13)

#page6
variable1 = ggarrange(temp_map, prec_map, city_map,  
                      nrow = 3, ncol= 1, heights = c(1,1,1 , widths = c(1,1,1)))
ggsave(paste0(ResultDir, '/maps/', Sys.Date(),  'sup_map6.pdf'), variable1, width=8.5, height=13)

#page6
variable1 = ggarrange(travel_healthcare_map, NULL, NULL,  
                      nrow = 3, ncol= 1, heights = c(1,1,1 , widths = c(1,1,1)))
ggsave(paste0(ResultDir, '/maps/', Sys.Date(),  'sup_map6.pdf'), variable1, width=8.5, height=13)

###Combine pdfs

qpdf::pdf_combine(MapsDir = c("sup_map1.pdf", "sup_map2.pdf", "sup_map3.pdf", "sup_map4.pdf",
                              "sup_map5.pdf", "sup_map6.pdf"), MapsDir = "supplement_maps.pdf")


#_______________________________________________________________________________________________________

############Uncleaned, awaiting final list of vars###############################################

#______________________________________________________________________________________________________


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
  geom_point(df_10_18_fin, mapping = aes(x = LONGNUM, y = LATNUM, color = ed_cut), size =8, alpha =0.5)+
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


for (i in 1:34) { 
  df_10_18_fin = df_10_18_fin %>% filter(LONGNUM > 0.000000)
  df_10_18_fin[[1]] = cut(df_10_18_fin[[1]], breaks=5, include.lowest = TRUE)
  p= ggplot() + 
    geom_sf(data =state_sf, color='lightgrey')+
    geom_point(df_10_18_fin, mapping = aes_string(x = "LONGNUM", y = "LATNUM", color = name_list[[1]]), size =8, alpha =0.7)+
    theme_bw()+
    map_theme(panel.grid.major = element_blank()) +
    labs (title = labels_data[[1]], x = "values", size=20) +
    xlab("") +
    ylab("")+
    guides(color=guide_legend(""))+
    scale_color_manual(values = c("#00AFBB", "burlywood4", "#FC4E07", "#E7B800", "#00AFBB", "lightgrey"), 
                       labels = c("0 - 20", "20 - 40", "40 - 60", "60 - 80", "80 - 100", "NA"))
  plot_list[[1]]=p
  
}


map_fun2 <- function(polygon_name, point_data, title, variable, break1, break2, break3, break4, break5, break6){
  point_data = point_data %>% filter(LONGNUM > 0.000000)
  point_data$variable = cut(df_10_18_fin$variable, breaks=c(), include.lowest = TRUE)
  p= ggplot() + 
    geom_sf(data =state_sf, color='lightgrey')+
    geom_point(df_10_18_fin, mapping = aes_string(x = "LONGNUM", y = "LATNUM", color = name_list[[i]]), size =8, alpha =0.7)+
    theme_bw()+
    map_theme() +
    theme(panel.grid.major = element_blank()) +
    labs (title = labels_data[label_list[[i]]], x = "values", size=20) +
    xlab("") +
    ylab("")+
    guides(color=guide_legend(""))+
    scale_color_manual(values = c("#00AFBB", "burlywood4", "#FC4E07", "#E7B800", "#00AFBB", "lightgrey"), 
                       labels = c("0 - 20", "20 - 40", "40 - 60", "60 - 80", "80 - 100", "NA"))
  plot_list[[i]]=p
  
}



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


name_list = c("test", 
              "edu","wealth","housing_q","floor", "roof_type", 
              "wall","household_size","mean_age","median_age","preg_women",
              "net_use_all","net_use_child","fever_cases","med_treat_fever","ACT_use_U5",
              "all_female_sex","U5_pop","female_child_sex",
              
              "pop_density","U5_FB","housing_2000","housing_2015","building", 
              "elev_","minutes_to_city","travel_metre_2015","travel_metre_2019","walking_metre", 
              "travel_healthcare","walking_healthcare","dominant","secondary_vector","temp_all_yrs",
              "v001")

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




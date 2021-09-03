
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

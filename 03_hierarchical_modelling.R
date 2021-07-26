
x <- c("tidyverse","INLA", "ggplot2", "ggpubr", "inlabru", "rgdal", "sp", "sf", "tmap", 'paletteer', 'cowplot', 'gridExtra', 'lme4')




lapply(x, library, character.only = TRUE) #applying the library function to packages

options(repr.plot.width = 14, repr.plot.height = 8)

###################################################################################
####Directories
###################################################################################

Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
DataDir <-file.path(NGDir, "data")
ResultDir <-file.path(NGDir, "results")
BinDir <- file.path(NGDir, "bin")
SrcDir <- file.path(NGDir, "src", "DHS")
VarDir <- file.path(SrcDir, "1_variables_scripts")
ProjectDir <- file.path(NuDir, "projects", "hbhi_nigeria")
PresentDir<-file.path(NuDir, "presentations")
Personal_P_Dir <- file.path(NuDir, "presentations", "team member archive_Ifeoma", "210409_EPI_seminar", "pictures")
Sem_Dir <- file.path(NuDir, "presentations", "team member archive_Ifeoma", "210409_EPI_seminar")
Man_Dir <- file.path(ProjectDir, "project_notes", "publication", "Urban-rural determinants of malaria infection in Nigeria", "Illustrations")


###################################################################################
####Loading data
###################################################################################
# Load pre-clustered data:


# Load pre-clustered data:
clu_variales_10_18 <- read.csv(file.path(DataDir, "Nigeria_2010_2018_clustered_final_dataset.csv"), 
                               header = T, sep = ',')


#Loading cluster points

dhs18_sf <- st_read(file.path(DataDir,"NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp"),) 
mis15_sf <- st_read(file.path(DataDir,"NG_2015_MIS_06192019/NGGE71FL/NGGE71FL.shp"),)
mis10_sf <- st_read(file.path(DataDir,"NG_2010_MIS_06192019/NGGE61FL/NGGE61FL.shp"),)

# join dhs variables to cluster points by year 

df_18 <- clu_variales_10_18 %>% filter(data_source == "dhs2018") 
df_18_fin <- left_join(dhs18_sf, df_18, by = c("DHSCLUST" ="hv001"))
df_15 <- clu_variales_10_18 %>% filter(data_source == "mis2015") 
df_15_fin <- left_join(mis15_sf, df_15, by = c("DHSCLUST" ="hv001"))
df_10 <- clu_variales_10_18 %>% filter(data_source == "mis2010") 
df_10_fin <- left_join(mis10_sf, df_10, by = c("DHSCLUST" = "hv001"))
clu_variales_10_18 <- rbind(df_18_fin, df_15_fin, df_10_fin)


#cleaning precip data
clu_variales_10_18 <- clu_variales_10_18%>% mutate(annual_precipitation = scale(clu_variales_10_18$annual_precipitation, center = T))
#dat2[which(dat2$pop_den<0),]

clu_variales_10_18 <- clu_variales_10_18 %>% mutate(pop_den = na_if(pop_den, -9999))
clu_variales_10_18$annual_precipitation <- replace(clu_variales_10_18$annual_precipitation, which(clu_variales_10_18$annual_precipitation < 0), NA)
clu_variales_10_18 <- clu_variales_10_18 %>% mutate(log_pop_den = log(pop_den))


#filtering by residence type
urbandataset <- clu_variales_10_18 %>% filter(Rural_urban == 1) %>%  
  dplyr::select(p_test, wealth_2, u5_prop, preg,edu_a, hh_size, ACT_use_u5,pop_den,
                hh_members_age, sex_f, data_source, humidindex, Rural_urban, annual_precipitation,housing_qua, net_use, build_count, state, region, pop_count, housing_qua) %>% 
  na.omit()

table(urbandataset$data_source)

# Binarize response:
urbandataset$y <- ifelse(urbandataset$p_test < 0.1, 0,1)


#data cleaning

missing_values <- sapply(urbandataset, function(x) sum(is.na(x)))
summary(urbandataset$pop_count)

urbandataset <- urbandataset %>% filter(annual_precipitation>=0, pop_den != -9999)
table(urbandataset$data_source)


###################################################################################
#regular urban model without random effect 
u_rmod <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                 hh_members_age + sex_f + log(annual_precipitation) +log(build_count) + humidindex, family = 'binomial',
               data = urbandataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE)) 

summary(u_rmod)

#model with random effects in state
u_iid <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                hh_members_age + sex_f + log(annual_precipitation) +log(build_count)+  humidindex +f(state, model = "iid"), family = 'binomial',
              data = urbandataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE))

summary(u_iid)

###################################################################################
####final urban model
###################################################################################

#model with random intercept in state and region
urbandataset$state_2 <- urbandataset$state
u_iid2 <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                 hh_members_age + sex_f + log(annual_precipitation) +log(build_count)
               + humidindex +f(state, model = "iid") + f(region, model = "iid") +  f(state_2, net_use, model = "iid"), family = 'binomial',
               data = urbandataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE),
               control.compute = list(cpo=TRUE, dic = TRUE))

summary(u_iid2) # model with the lowest dic and marginal loglikelihood


#extraction and converting results to dataframe
u_iid2_fixed <- as.data.frame(u_iid2$summary.fixed)

#making column names a column
u_iid2_fixed <- cbind(coefs = rownames(u_iid2_fixed), u_iid2_fixed)
rownames(u_iid2_fixed) <- 1:nrow(u_iid2_fixed)

#changing row names
names(u_iid2_fixed)[names(u_iid2_fixed) == "0.975quant"] <- "logodds975"
names(u_iid2_fixed)[names(u_iid2_fixed) == "0.5quant"] <- "logodds5"
names(u_iid2_fixed)[names(u_iid2_fixed) == "0.025quant"] <- "logodds25"

#
u_iid2_prob <- u_iid2_fixed %>% mutate(probs_0.975 = plogis(logodds975))
u_iid2_prob <- u_iid2_prob %>% mutate(probs_0.5 = plogis(logodds5))
u_iid2_prob <- u_iid2_prob %>% mutate(probs_0.025 = plogis(logodds25))
u_iid2_prob <- u_iid2_prob[,c("coefs", "probs_0.025", "probs_0.5", "probs_0.975")]
u_iid2_prob


#plots of the posterior for the betas 

plot_fun<- function(data, x_label){
  ggplot(data.frame(inla.smarginal(data)), aes(x, y)) +
    geom_line(color = "green") +
    theme_bw()+
    geom_vline(xintercept=0, linetype="dashed", color = "red")+
    xlab(x_label)+
    ylab("")
}


data <- list(u_iid2$marginals.fixed$`(Intercept)`,u_iid2$marginals.fixed$wealth_2, u_iid2$marginals.fixed$edu_a, u_iid2$marginals.fixed$net_use, 
             u_iid2$marginals.fixed$hh_size, u_iid2$marginals.fixed$ACT_use_u5, u_iid2$marginals.fixed$hh_members_age,
             u_iid2$marginals.fixed$sex_f, u_iid2$marginals.fixed$`log(annual_precipitation)`, u_iid2$marginals.fixed$`log(build_count)`,
             u_iid2$marginals.fixed$humidindex)

labels_data <- list("intercept", "Highest wealth quintile", "Education", "Bednet Use", "Average Household size", "ACT_use",
                    "Average Household age", "Proportion of females", "log(annual precipitation)", 
                    "log(build_count)", "humidity index")

plots<-map2(data, labels_data, plot_fun)

figure<-ggarrange(plotlist = plots, nrow =4, ncol=3)
figure<-annotate_figure(figure, left = "Density")
figure

#extracting state random intercept 
u_random_effects_ <- u_iid2$summary.random[[1]]

#extracting factors 
u_random_effects_$ID <-str_to_title(u_random_effects_$ID)
u_random_effects_$ID <- factor(u_random_effects_$ID, levels=rev(u_random_effects_$ID))
u_random_effects_$ID<- trimws(u_random_effects_$ID)


#quick plot of state_random intercept 

u_fp <- ggplot(data=u_random_effects_, aes(x=ID, y=mean, ymin=`0.025quant`, ymax=`0.975quant`)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Label") + ylab("Mean (95% CI)") +
  theme_bw()  # use a white background
print(u_fp)# no significant state variations 


#map 

#read in state shape file 
stateshp <- readOGR(file.path(DataDir,"gadm36_NGA_shp"), layer ="gadm36_NGA_1", use_iconv=TRUE, encoding= "UTF-8")
state_sf <- st_as_sf(stateshp)




state_sf <- state_sf %>% mutate(NAME_1 = case_when(NAME_1 == "Federal Capital Territory" ~ "Fct Abuja",
                                                   NAME_1 == "Nassarawa" ~ "Nasarawa",
                                                   TRUE ~ as.character(NAME_1)
))

u_map_df <- left_join(state_sf, u_random_effects_, by =c("NAME_1" = "ID"))


# we see spatial clustering of state-level random effects, although state effects are not statistically significant 
u_map <- tm_shape(u_map_df)+
  tm_polygons(col = "mean", midpoint =NA, palette = "-RdYlGn")+
  tm_text("NAME_1")

u_map


#extracting region random intercept 
u_random_effects_ <- u_iid2$summary.random[[2]]
re_fp <- ggplot(data=u_random_effects_, aes(x=ID, y=mean, ymin=`0.025quant`, ymax=`0.975quant`)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Label") + ylab("Mean (95% CI)") +
  theme_bw()  # use a white background
print(re_fp)#significant variation in the south west increasing likelihood of transmission and decreased likelihood in the north 


#map 



#############################################################################################
#########regional map
#read in state shape file 
regionshp <-  readOGR(file.path(DataDir,"gadm36_NGA_shp"), layer ="gadm36_NGA_1", use_iconv=TRUE, encoding= "UTF-8")
region_sf <- st_as_sf(regionshp)



region_sf <- region_sf %>% mutate(NAME_1 = case_when(NAME_1 == "Federal Capital Territory" ~ "Fct Abuja",
                                                     NAME_1 == "Nassarawa" ~ "Nasarawa",
                                                     TRUE ~ as.character(NAME_1)
))



regions <- clu_variales_10_18[,c("state", "region")]


region_sf <- region_sf %>% mutate(NAME_1 = tolower(NAME_1))


region_sf <- st_join(region_sf, regions, by =c("NAME_1" = "state"))

#We have missing data in three states below, we assign the region mannually
region_sf$region[which(region_sf$NAME_1 =="kwara")]<-"north central"
region_sf$region[which(region_sf$NAME_1 =="plateau")]<-"north central"
region_sf$region[which(region_sf$NAME_1 =="zamfara")]<-"north west"

#grouping states into regions
region_sf <- region_sf %>% group_by(NAME_1) %>% slice(1L)
region_sf_na<-subset(region_sf,region_sf$region=="NA")

u_map_df <- left_join(region_sf, u_random_effects_, by =c("region" = "ID"))
#u_map_df  <- u_map_df %>% group_by(region) %>% slice(1L)
u_map_df <- st_as_sf(u_map_df)

u_map_df%>% group_by(region) %>% 
  summarise(geometry = sf::st_union(geometry)) %>% ungroup()

# we see spatial clustering of state-level random effects, although state effects are not statistically significant 
u_map <- tm_shape(u_map_df)+
  tm_fill(col = "mean", midpoint =NA, palette = "-RdYlGn") +
  tm_borders() +
  tm_text("NAME_1", size = 0.8, col = "snow4")

u_map




## These scripts are used to compute the fraction of women receiving antenatal care in Nigeria 
rm(list=ls())
memory.limit(size = 50000)

# -----------------------------------------
### Required functions and settings
## -----------------------------------------

# # Reading in the necessary packages 
list.of.packages <- c("tidyverse", "survey", "haven", "ggplot2", "purrr",  "stringr", "sp", "rgdal", "raster",
                      "lubridate", "RColorBrewer","sf",   "labelled", "plotrix", "arules", "foreign",
                      "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "sjlabelled", "raster", "rlist", 'rgeos',  'ggpubr',
                      'cowplot', 'gridExtra', 'lme4', "patchwork", 'ggsci', 'glue', 'ggrepel', 'reshape2', 'viridis')


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


lapply(list.of.packages, library, character.only = TRUE) #applying the library function to packages


#read files function 
read.files <- function(path, general_pattern, specific_pattern, fun) {
  files <- list.files(path = path , pattern = general_pattern, full.names = TRUE, recursive = TRUE)
  files<- files[(grep(specific_pattern, files))]
  sapply(files, fun, simplify = F)
}

#survey estimates generating functions  
result.prop<- function(var, var1, design) {
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, svyciprop, method ='logit', levels=0.95, vartype= "se", na.rm=T, influence = TRUE)
}


#survey design function 
svydesign.fun <- function(filename){
  svydesign(id= ~id,
            strata=~strat,nest=T, 
            weights= ~wt, data=filename)
}

#estimation functions 
estim_prop <- function(df, col, by){
  svy_mal <- svydesign.fun(df)
  clu_est <- result.prop(col, by, design=svy_mal)
}

options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed

## -----------------------------------------
### Paths
## -----------------------------------------

user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
ProjectDir <- file.path(NuDir, "data", 'nigeria','nigeria_dhs' , 'data_analysis')
DataDir <- file.path(ProjectDir, 'data')
DHSData <- file.path(DataDir, 'DHS')
DataIn <- file.path(NuDir, 'projects','urban_malaria','sampling','ANC')
CSVDir <- file.path(DataIn, 'DHS_Extract')
DataDir2 <-file.path(NuDir, "data", "nigeria")
setwd(DataIn)

DataDir <-file.path(Drive, "Downloads")
DataDir2 <-file.path(Drive, "Downloads")
DHSData <-file.path(Drive)
DataIn<-file.path(Drive, "Downloads")
CSVDir <-file.path(Drive, "Downloads", 'DHS_Extract')

#load data IR data ANC varaible 
dhs_ir <- read.files(DataDir2, "*NGIR.*\\.DTA", 'NGIR7AFL', read_dta)  #reads in the IR files


look_for(dhs_ir[[1]], "bidx_02")

#create a variable for skilled antenatal provision, problem accessing healthcare
dhs_ir <- dhs_ir %>% map(~mutate(., sum_anc_prov = (m2a_1 + m2b_1 + m2c_1),
                                 ski_prov = ifelse(sum_anc_prov >= 1,1, 0),
                                 any_anc_sum = (m2a_1 + m2b_1 + m2c_1+ m2d_1 + m2g_1 + m2h_1 + m2k_1 + m2n_1),#remvoved anc vars without data 
                                 any_anc = ifelse(any_anc_sum >= 1, 1, 0),
                                 access_prob = ifelse((v467b== 1)| (v467c== 1)|(v467d== 1)|(v467f==1), 1,0),
                                 wt=v005/1000000,strat=v022,
                                 id=v021, num_p=1)) %>%
  map(~filter(., midx_1 == 1)) %>% #filtering to the last antenatal only
  map(~filter(., v025 == 1)) #filtering to urban areas only 

prob <- dhs_ir[[1]] %>% dplyr::select(midx_2,v467b, v467c, v467d, v467f, access_prob, any_anc_sum,any_anc, ski_prov, 
                                      m2a_1, m2b_1, m2c_1, m2d_1, m2e_1, m2f_1, m2g_1, m2h_1, m2i_1, m2j_1, m2k_1, m2l_1, m2m_1,m2n_1)

vars <- c('ski_prov', 'access_prob')

#vars<- c('age_cat')
for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('v001')
  df <- dhs_ir %>% 
    map(~drop_na(.,vars[i]))
  df <- pmap(list(df,col,by), estim_prop)
  df <- plyr::ldply(df)
  df[, vars[i]]<- df[, vars[i]]
  write.csv(df, file =file.path(DataIn, paste0(vars[i], "_all_DHS_IR_18.csv")))
}




#load data PR data for social, behavioral, intervention coverage, and malaria
dhs_pr <- read.files(DataDir, "*NGPR.*\\.DTA", 'NGPR7AFL', read_dta)  #reads in the IR files

#create a variable for skilled antenatal provision
dhs_pr <- dhs_pr %>% map(~mutate(., wealth = ifelse(hv270 <4, 0, 1),
                                 edu_a = ifelse(hv106 %in% c(0, 1, 2), 0,ifelse(hv106 >= 8, NA, ifelse(hv106 == 2|3, 1, NA))),
                                 p_test = ifelse(hml32 > 1, NA, hml32),
                                 net_use = ifelse(hml12 %in% c(1,2), 1,0),
                                 wt=hv005/1000000,strat=hv022,
                                 id=hv021, num_p=1)) %>%
  map(~filter(., hv025 == 1)) #filtering to urban areas only 


vars <- c('wealth','edu_a', 'p_test', 'net_use')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv001')
  df <- dhs_pr %>% 
    map(~drop_na(.,vars[i]))
  df <- pmap(list(df,col,by), estim_prop)
  df <- plyr::ldply(df)
  df[, vars[i]]<- df[, vars[i]]
  write.csv(df, file =file.path(DataIn, paste0(vars[i], "_all_DHS_IR_18.csv")))
}

dhs <- read.files(DataDir, "*NGHR.*\\.DTA", 'NGHR7AFL', read_dta)  #reads in the HR files


#computes household-level access 
dhs <- dhs %>% map(~filter(., hv025 ==1)) %>% 
  map(~dplyr::select(., hhid, hv001, starts_with('hml10'), hv005, 
                     starts_with('hv103'), hv013, hv021, hv022)) %>% 
  map(~mutate(., potuse = 2 * rowSums(dplyr::select(., contains('hml10')),na.rm=T),
              slept_night = rowSums(dplyr::select(., contains('hv103')), na.rm=T), 
              potuse2 = ifelse(potuse/slept_night > 1, slept_night, potuse),
              access = potuse2/slept_night,
              wt=hv005/1000000,strat=hv022,
              id=hv021)) %>% 
  map(~dplyr::select(., hv001, access, wt, strat, id))


#generate mean access proportion by cluster  
vars <- c('access')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv001')
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <-  pmap(list(df,col,by), estim_prop)
  df_access <- plyr::ldply(df)
  write.csv(df, file =file.path(DataIn, paste0(vars[i], "_all_DHS_PR_10_15_18.csv")))
}


#data merge
#compute ITN use given access 
files <- list.files(path = file.path(CSVDir), pattern = '.csv', full.names = TRUE, recursive = TRUE)
df <-sapply(files, read.csv, simplify = F)
df <- df %>% map_if(~ all(c('X') %in% colnames(.x)),~dplyr::select(., -X)) %>% 
  map_if(~ all(c('se') %in% colnames(.x)),~dplyr::select(., -se)) %>% 
  map_if(~ all(c('.id') %in% colnames(.x)),~dplyr::select(., -.id)) %>% 
  map_if(~ all(c('ci_l') %in% colnames(.x)),~dplyr::select(., -ci_l)) %>% 
  map_if(~ all(c('ci_u') %in% colnames(.x)),~dplyr::select(., -ci_u)) %>% 
  map_if(~ all(c('ID') %in% colnames(.x)), ~rename(., v001 = ID)) %>%  
  map_if(~ all(c('hv001') %in% colnames(.x)), ~rename(., v001 = hv001))


df <- left_join(df[[1]], df[[2]], by = 'v001') %>% left_join(df[[3]], by = 'v001')%>% left_join(df[[4]], by = 'v001')%>% 
  left_join(df[[5]], by = 'v001') %>% left_join(df[[6]], by = 'v001')




#filtering oyo and Kano states only 
states <- dhs_ir[[1]]%>% dplyr::select(v001, sstate) %>% filter(duplicated(v001) == FALSE) 

df_states <- df %>% left_join(states, by="v001") %>% filter(sstate == 100|sstate == 210) %>% mutate(sstate = ifelse(sstate== 100,'kano', 'oyo'))



#state agregates
vars<- c('ski_prov')
for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('sstate')
  df_state <- dhs_ir %>% 
    map(~drop_na(.,vars[i]))
  df_state <- pmap(list(df_state,col,by), estim_prop)
  df_state <- plyr::ldply(df_state)
  df_state[, vars[i]]<- df_state[, vars[i]]
  # write.csv(df, file =file.path(DataIn, paste0(vars[i], "_all_DHS_IR_18.csv")))
}

df_state <- df_state %>% filter(sstate == 100|sstate == 210) %>% mutate(sstate = ifelse(sstate== 100,'kano', 'oyo'))



#_________________________
#Visuals
#_________________________

#metro level maps

ward_pop <- read_csv(file.path(DataDir2, 'nigeria_wardpop', 'GRID3_ward_admin_pop.csv')) %>% dplyr::select(ward_name, mean) %>%
  plyr::rename(c("ward_name" = "WardName", 'mean'='mean_pop'))

Ibadan_metro <- read_sf(file.path(DataDir2, 'kano_ibadan_shape_files', 'Ibadan_metro_wards_shapes', 'ibadan_wards.shp'))
Ibadan_metro <- left_join(Ibadan_metro, ward_pop, by = 'WardName') %>% filter(duplicated(WardName) == FALSE)


kano_metro <-  read_sf(file.path(DataDir2, 'kano_ibadan_shape_files', 'kano_metro_Wards_shapes', 'kano_metro_wards.shp')) 
kano_metro <- left_join(kano_metro, ward_pop, by = 'WardName') %>% filter(duplicated(WardName) == FALSE)

#ward populat


#dhs points


sf18 = st_read(file.path(DHSData, "Downloads", "NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp"),) %>%
  filter(URBAN_RURA == "U") %>%  dplyr::rename(v001 = DHSCLUST)

df_18_fin_kano <- left_join(df_states, sf18, by = "v001") %>% filter(LONGNUM > 0.000000) %>% filter(sstate == 'kano') %>% st_as_sf()
points_within_kano_metro <- df_18_fin_kano %>% st_transform (4326)%>% filter(LONGNUM > 8.387695) %>% filter(LATNUM > 11.86428)


df_18_fin_oyo <- left_join(df_states, sf18, by = "v001") %>% filter(LONGNUM > 0.000000) %>% filter(sstate == 'oyo')
df_18_fin_oyo <- st_as_sf(df_18_fin_oyo)

points_within_oyo_metro <- df_18_fin_oyo %>% st_transform (4326)%>% filter(LONGNUM > 3.540555) %>% filter(LATNUM < 7.676494) 

#plot two histograms in same graph at state level

#define data
x1 = df_states %>% filter(sstate == 'kano') 
x1 <- x1$ski_prov
x2 = df_states %>% filter(sstate == 'oyo')
x2 <- x2$ski_prov

#plot two histograms in same graph; The figure gets saved in  ANC folder
pdf('Skiled ANC in Kano and Oyo states.pdf')
hist(x1, col=rgb(0,0,1,0.2), xlim=c(0, 1), ylim=c(0, 10),
     xlab='Values', ylab='Frequency', main='Skiled ANC in Oyo state')
hist(x2, col=rgb(1,0,0,0.2), add=TRUE)
legend('topleft', c('Kano', 'Oyo'),
       fill=c(rgb(0,0,1,0.2), rgb(1,0,0,0.2)))


dev.off()


Ibadan_kano_metro <- dplyr::bind_rows(points_within_oyo_metro, points_within_kano_metro)

#define data
x1 = Ibadan_kano_metro %>% filter(sstate == 'kano') 
x1 <- x1$ski_prov
x2 = Ibadan_kano_metro %>% filter(sstate == 'oyo')
x2 <- x2$ski_prov
#plot two histograms in same graph; The figure gets saved in  ANC folder
pdf('Ibadan_kano_metro_level_skiled_anc.pdf')
hist(x1, col=rgb(0,0,1,0.2), xlim=c(0, 1), ylim=c(0, 10),
     xlab='Proportion', ylab='Frequency', main='Ibadan and kano metro skiled antenatal care provision')
hist(x2, col=rgb(1,0,0,0.2), add=TRUE)
legend('topleft', c('Ibadan', 'Kano_metro'),
       fill=c(rgb(0,0,1,0.2), rgb(1,0,0,0.2)))


dev.off()


#bar plots
barplt_fun <- function(dataframe, label_column, plot_title, xlab_title, ylab_title){
  ggplot(dataframe, aes_string(x=label_column, y='ski_prov', fill=label_column)) +
    geom_bar(stat="identity")+theme_minimal()+
    geom_text(aes(label = sprintf("%0.2f", round(ski_prov, digits = 2), vjust=1.6, color="white", size = 6))) +
    scale_fill_manual(values=c("#E69F00", "#56B4E9")) + labs(fill = "Lacation")+
    theme(legend.title.align=0.5,
          legend.title=element_text(size=14, colour = 'black'), 
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(0.65, "cm"),
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14))+
    labs (title = plot_title, size=50) +
    xlab(xlab_title) +
    ylab(ylab_title)
}

#state level
state_plot <- barplt_fun(df_state, 'sstate', "State level proportion of skilled antenatal care provision", "State", "Proportion of skilled antenatal care")
state_plot

ggsave(paste0(DataIn, '/Figures/', Sys.Date(), 'State_level_barplot.pdf'), state_p, width = 14, height =9)

#Ibadan and kano metro level


Ibadan_kano_mean <- Ibadan_kano_metro %>% dplyr::group_by(sstate) %>% dplyr::summarize(ski_prov = mean(ski_prov))%>%
  mutate(sstate=str_replace(sstate, 'kano','kano metro')) %>% mutate(sstate=str_replace(sstate, 'oyo','Ibadan'))


metro_plot <- barplt_fun(Ibadan_kano_mean, 'sstate', "Ibadan and kano metro level proportion of skilled antenatal care provision", "Location", "Proportion of skilled antenatal care")

metro_plot



#map fun
map_fun2 <- function(polygon_name,sec_fill, point_data, var_n, aes_names, title, label1, label2, label3, label4, label5){
  point_data[[var_n]] = cut(point_data[[var_n]], breaks=c(0.5, 0.6, 0.7, 0.8, 0.9, 1), include.lowest = TRUE)
  p= ggplot() + 
    geom_sf(data =polygon_name, color=alpha('#8971B3', 0.5))+
    #geom_sf(data =polygon_name2, fill= sec_fill)+
    geom_text_repel(
      data = polygon_name,
      aes(label =  polygon_name[[aes_names]], geometry = geometry),color ='black',
      stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
    geom_point(point_data, mapping = aes(x = LONGNUM, y = LATNUM, color = point_data[[var_n]]), size =4, alpha =0.6)+
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
    scale_color_manual(values = c('darkred', "#E7B800","green4", 'deepskyblue'), 
                       labels = c(label1, label2, label3, label4, label5))
  
}



#ibadan
ibadan_map <- map_fun2(Ibadan_metro, 'slategray2',points_within_oyo_metro, 6, 3, "Prevalence of skilled ANC in Ibadan wards", 
                       "70% - 79%", "80% - 89%", "90% - 100%",  '', '')
ibadan_map

#ggsave(paste0(DataIn, '/Figures/', Sys.Date(), 'ANC_ibadan_map.pdf'), oyo_map_metro, width = 14, height =9)

#kano
oyo_map_metro <- map_fun2(kano_metro, 'slategray2',points_within_kano_metro, 6, 3,"Prevalence of skilled ANC in kano metro", 
                          "60% - 69%", "70% - 79%", "80% - 89%", "90% - 100%","")
oyo_map_metro

#ggsave(paste0(DataIn, '/Figures/', Sys.Date(), 'ANC_kano_map.pdf'), oyo_map_metro, width = 14, height =9)


#map fun ovalaying popilation 

#map fun
map_fun2 <- function(polygon_name,sec_fill, point_data, var_n, aes_names, title, label1, label2, label3, label4, label5){
  point_data[[var_n]] = cut(point_data[[var_n]], breaks=c(0.5, 0.6, 0.7, 0.8, 0.9, 1), include.lowest = TRUE)
  p= ggplot() + 
    geom_sf(data =polygon_name, aes(fill = mean_pop, alpha = 0.1))+
    #geom_sf(data =oyo_sampled_lga, fill= 'blue')+
    geom_text_repel(
      data = polygon_name,
      aes(label =  polygon_name[[aes_names]], geometry = geometry),color ='black',
      stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
    geom_point(point_data, mapping = aes(x = LONGNUM, y = LATNUM, color = point_data[[var_n]]), size =4, alpha =0.6)+
    theme_bw()+
    theme(axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          rect = ggplot2::element_blank(),
          plot.background = ggplot2::element_rect(fill = "white"), 
          legend.title.align=0.5,
          legend.title=element_text(size=16, colour = 'black'), 
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(0.65, "cm"),
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank()) +
    labs (title = title, x = "values", size=25) +
    xlab("") +
    ylab("")+
    guides(color=guide_legend(title = "ANC proportion", override.aes = list(size = 5)))+
    scale_color_manual(values = c('darkred', "#E7B800","green4", 'deepskyblue'), 
                       labels = c(label1, label2, label3, label4, label5))
  
}


#ibadan
ibadan_map <- map_fun2(Ibadan_metro, 'slategray2',points_within_oyo_metro, 6, 3, "Prevalence of skilled ANC and population size in Ibadan wards", 
                       "70% - 79%", "80% - 89%", "90% - 100%",  '', '')
ibadan_map

#ggsave(paste0(DataIn, '/Figures/', Sys.Date(), 'ANC_ibadan_map.pdf'), oyo_map_metro, width = 14, height =9)

#kano
kano_map_metro <- map_fun2(kano_metro, 'slategray2',points_within_kano_metro, 6, 3,"Prevalence of skilled ANC and ward population size in kano metro", 
                           "60% - 69%", "70% - 79%", "80% - 89%", "90% - 100%","")
kano_map_metro 

#ggsave(paste0(DataIn, '/Figures/', Sys.Date(), 'ANC_kano_map.pdf'), oyo_map_metro, width = 14, height =9)


#State level maps

#read in state shape file 
stateshp = readOGR(file.path(DataDir, "shapefiles","gadm36_NGA_shp"), layer ="gadm36_NGA_2",use_iconv=TRUE, encoding= "UTF-8")
state_sf_kano = st_as_sf(stateshp) %>% filter(NAME_1 == 'Kano')
state_sf_oyo = st_as_sf(stateshp) %>% filter(NAME_1 == 'Oyo')

wards <- read_sf(file.path(DataDir2, 'Boundary_VaccWards_Export', 'Boundary_VaccWards_Export.shp'))

#oyo
oyo_map_state <- map_fun2(state_sf_oyo, 'slategray2',df_18_fin_oyo, 3, 2, "Prevalence of skilled ANC in oyo state", 
                          ".7 - .8", ".8 - .9", ".9 - 1",  '', '')
oyo_map_state

ggsave(paste0(DataIn, '/Figures/', Sys.Date(), 'ANC_ibadan_map.pdf'), oyo_map_metro, width = 14, height =9)

#kano
kano_map_state <- map_fun2(wards, 'slategray2',df_18_fin_kano, 3, 3, "Prevalence of skilled ANC in kano state", 
                           '', '', ".7 - .8", ".8 - .9", ".9 - 1")
kano_map_state

ggsave(paste0(DataIn, '/Figures/', Sys.Date(), 'ANC_kano_map.pdf'), oyo_map_metro, width = 14, height =9)

#sampled wards only
oyo_sampled_lga <- state_sf_oyo %>% filter(str_detect(NAME_2, 'Ibadan'))%>% st_transform (4326)
Ibadan_sampled <- left_join(Ibadan_metro, ward_pop, by = 'WardName') %>% filter(duplicated(WardName) == FALSE)

#oyo
points_within_oyo_metro <- df_18_fin_oyo %>% st_transform (4326)%>% filter(LONGNUM > 3.540555) %>% filter(LATNUM < 7.676494) 

oyo_map_state <- map_fun2(oyo_sampled_lga, 'slategray2',points_within_oyo_metro, 3, 7, "Prevalence of skilled ANC in oyo state", 
                          ".7 - .8", ".8 - .9", ".9 - 1",  '', '')
oyo_map_state


###############################
# Sampled LGA level Plots 
##################################


#map fun
map_fun2 <- function(polygon_name,sec_fill, point_data, var_n, aes_names, title, label1, label2, label3, label4, label5){
  point_data[[var_n]] = cut(point_data[[var_n]], breaks=c(0.5, 0.6, 0.7, 0.8, 0.9, 1), include.lowest = TRUE)
  p= ggplot() + 
    geom_sf(data =polygon_name, aes(fill = mean_pop, alpha = 0.2))+
    geom_text_repel(
      data = polygon_name,
      aes(label =  polygon_name[[aes_names]], geometry = geometry),color ='black',
      stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
    geom_point(point_data, mapping = aes(x = LONGNUM, y = LATNUM, color = point_data[[var_n]]), size =4, alpha =0.6)+
    theme_bw()+
    theme(axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          rect = ggplot2::element_blank(),
          plot.background = ggplot2::element_rect(fill = "white"), 
          legend.title.align=0.5,
          legend.title=element_text(size=16, colour = 'black'), 
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(0.65, "cm"),
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank()) +
    labs (title = title, x = "values", size=25) +
    xlab("") +
    ylab("")+
    guides(color=guide_legend(title = "ANC proportion", override.aes = list(size = 5)))+
    scale_color_manual(values = c('darkred', "#E7B800","green4"), 
                       labels = c(label1, label2, label3, label4, label5))
  
}

####
#Plots for Sampled locations 
#Sampled ibadan mpa
Ibadan_sampled_poly <- Ibadan_metro %>% filter(LGACode == 31006 | LGACode == 31007 | 
                                                 LGACode == 31008 | LGACode == 31009 | LGACode == 31010)%>% filter(Urban == 'Yes')
sampled_ibadan_points <- df_18_fin_oyo %>% st_transform (4326)%>% filter(LONGNUM > 3.815983 & LONGNUM < 3.928) %>% filter(LATNUM <7.464584 & LATNUM >7.33007)
ibadan_map <- map_fun2(Ibadan_sampled_poly, 'slategray2',sampled_ibadan_points, 6, 3, "Prevalence of skilled ANC and population size in Ibadan sampled LGAs", 
                       "70% - 79%", "80% - 89%", "90% - 100%",  '', '')
ibadan_map

ggsave(paste0(DataIn, '/Figures/', Sys.Date(), 'ANC_ibadan_sampled_map.pdf'), oyo_map_metro, width = 14, height =9)

#Sampled kano map
kano_sampled_poly <- kano_metro %>% filter(LGACode == 20012 | LGACode == 20030 | LGACode == 20024 | LGACode == 20043 | LGACode == 20037| LGACode == 20001)
sampled_kano_points <- df_18_fin_kano %>% st_transform (4326)%>% filter(LONGNUM > 8.444448 & LONGNUM < 8.619980) %>% filter(LATNUM >11.93313 & LATNUM <12.071351)
kano_map <- map_fun2(kano_sampled_poly, 'slategray2',sampled_kano_points, 6, 3, "Prevalence of skilled ANC and population size in Urban Kano metro wards", 
                     "60% - 69%", "70% - 79%", "80% - 89%", "90% - 100%","")
kano_map

ggsave(paste0(DataIn, '/Figures/', Sys.Date(), 'sampled_ANC_kano_map_sampled.pdf'), oyo_map_metro, width = 14, height =9)


########################################
#histogrms sampled only
##############################################
#define data

x1 <- sampled_kano_points$ski_prov
x2 <- sampled_ibadan_points$ski_prov

#plot two histograms in same graph; The figure gets saved in  ANC folder
pdf('Skiled ANC in sampled LGAs for Kano metro and Ibadan.pdf')
hist(x1, col=rgb(0,0,1,0.2), xlim=c(0, 1), ylim=c(0, 10),
     xlab='Values', ylab='Frequency', main='Skiled ANC in sampled LGAs for Kano metro and Ibadan')
hist(x2, col=rgb(1,0,0,0.2), add=TRUE)
legend('topleft', c('Kano metro clusters', 'Ibandan clusters'),
       fill=c(rgb(0,0,1,0.2), rgb(1,0,0,0.2)))


dev.off()

### Bar plot

#state level
sampled_df <- dplyr::bind_rows(sampled_ibadan_points, sampled_kano_points)%>% dplyr::group_by(ADM1NAME) %>% dplyr::summarize(ski_prov = mean(ski_prov))%>%
  mutate(ADM1NAME=str_replace(ADM1NAME, 'KANO','kano metro')) %>% mutate(ADM1NAME=str_replace(ADM1NAME, 'OYO','Ibadan metro'))
sampled_bar <- barplt_fun(sampled_df, 'ADM1NAME', "sampled LGAs proportion of skilled antenatal care provision", "State", "Proportion of skilled antenatal care")

ggsave(paste0(DataIn, '/Figures/', Sys.Date(), '"sampled_LGAs_barplot.pdf'), sampled_bar, width = 14, height =9)

#########################################
# Density plot - sampled only
#########################################

theme_manuscript <- function(){
  theme_bw() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 12, color = "black"), 
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size =12),
          legend.title=element_text(size=12, colour = 'black'),
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}


den_plot.fun <- function(df, annot, plot_title){
  den_plot <- ggplot(df, aes(x= value, fill = variable, color = variable)) + theme_manuscript() + #theme(legend.position="none")+
    bar(alpha = 0.2)+ 
    scale_fill_viridis(discrete=TRUE) +
    #scale_color_viridis(discrete=TRUE) +
    #geom_text( data=annot, aes(x=x, y=y, label=variable, color= variable), hjust=0, size=4.5) +
    labs (title = plot_title, x = "values", y = 'density', size=25) 
  
}

melt_socioe <- as.data.frame(Ibadan_kano_metro)%>% dplyr::select(ski_prov,  wealth, sstate) %>% tibble::rowid_to_column("Cluster")# %>% gather(variable, value)

#%>% tibble::rowid_to_column("Cluster")


#annot <- data.frame(variable= c("% of skilled ANC", "Malaria prevalance", '% of education attainment', '% of higher wealth quantile'),
#                    x = c(0.78, 0, 0.17, 0.6),
#                    y = c(3.6, 3.2, 2.5, 2))


den_plot_socioe <- den_plot.fun(melt_socioe, annot,'Relationship between skilled ANC provision with socioeconomic variables')
den_plot_socioe
ggsave(paste0(DataIn, '/Figures/', Sys.Date(), 'den_plot_socioe.pdf'), den_plot_socioe, width = 14, height =9)


melt_acc_bhv <- as.data.frame(Ibadan_kano_metro)%>% dplyr::select(ski_prov, p_test, access_prob, net_use) %>% gather(variable, value)


den_plot_acc_bhv <- den_plot.fun(melt_acc_bhv, annot, 'Relationship between skilled ANC provision with access and behavioral variables')
den_plot_acc_bhv
ggsave(paste0(DataIn, '/Figures/', Sys.Date(), 'den_plot_socioe.pdf'), den_plot_acc_bhv, width = 14, height =9)


#######################
#Bar plot
#######################
# Change the colors manually

barplot.fun2 <- function(df, x_axis, value, plot_value, main_tiltle ){
  bar_p <- melt(df, id.vars = x_axis) %>% 
    ggplot(aes(y=value, x=Cluster)) + 
    geom_bar(aes(fill = variable),stat = "identity",position = "dodge", alpha = 0.8) + 
    theme_manuscript()+ 
    scale_fill_viridis(discrete=TRUE) +
    labs (title = main_tiltle, x = 'Cluster points in sampled LGAs', y = "Proportion", size=25) 
}

bar_df_iba <- melt_socioe %>% filter(sstate == "oyo") %>% dplyr::select(Cluster, wealth, ski_prov)%>% 
  transform(Cluster = reorder(Cluster, -wealth, decreasing = T))

bar_ib <- barplot.fun2(bar_df_iba, 'Cluster', value, Cluster,'Ibadan proportion of high wealth quantile and skilled ANC provision in sampled LGAs')
bar_ib 

ggsave(paste0(DataIn, '/Figures/', Sys.Date(), 'ibadan_wealth_anc.pdf'), bar_ib , width = 14, height =9)

bar_df_kan <- melt_socioe %>% filter(sstate == "kano") %>% dplyr::select(Cluster, wealth, ski_prov)%>% 
  transform(Cluster = reorder(Cluster, -wealth, decreasing = T))

bar_kan <- barplot.fun2(bar_df_kan, 'Cluster', value, Cluster,'Ibadan proportion of high wealth quantile and skilled ANC provision in sampled LGAs')
bar_kan 

ggsave(paste0(DataIn, '/Figures/', Sys.Date(), 'kano_wealth_anc.pdf'), bar_kan , width = 14, height =9)
########################################################################
#First birth ANC
#######################################################################
fir_birth_list <-  list(fir_birth <- dhs_ir[[1]] %>% mutate_all(funs(replace_na(.,0)))%>% 
                          mutate(total = rowSums(dplyr::select(., starts_with('bidx_'))))  %>% filter(total ==1)) 

#create a variable for skilled antenatal provision, problem accessing healthcare
dhs_ir_frstb <- fir_birth_list %>% map(~mutate(., sum_anc_prov = (m2a_1 + m2b_1 + m2c_1),
                                               ski_prov = ifelse(sum_anc_prov >= 1,1, 0),
                                               any_anc_sum = (m2a_1 + m2b_1 + m2c_1+ m2d_1 + m2g_1 + m2h_1 + m2k_1 + m2n_1),#remvoved anc vars without data 
                                               any_anc = ifelse(any_anc_sum >= 1, 1, 0),
                                               access_prob = ifelse((v467b== 1)| (v467c== 1)|(v467d== 1)|(v467f==1), 1,0),
                                               wt=v005/1000000,strat=v022,
                                               id=v021, num_p=1)) %>%
  map(~filter(., midx_1 == 1)) %>% #filtering to the last antenatal only
  map(~filter(., v025 == 1)) #filtering to urban areas only 


vars <- c('ski_prov_fbirth', 'access_prob')

#vars<- c('age_cat')
for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('v001')
  df <- dhs_ir_frstb %>% 
    map(~drop_na(.,vars[i]))
  df <- pmap(list(df,col,by), estim_prop)
  df <- plyr::ldply(df)
  df[, vars[i]]<- df[, vars[i]]
  write.csv(df, file =file.path(DataIn, paste0(vars[i], "_all_DHS_IR_18.csv")))
}

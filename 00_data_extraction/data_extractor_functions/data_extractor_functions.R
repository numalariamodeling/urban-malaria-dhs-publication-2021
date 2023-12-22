
# # Reading in the necessary packages 
list.of.packages <- c("tidyverse", "survey", "haven", "ggplot2", "purrr",  "stringr", "sp", "raster",
       "lubridate", "RColorBrewer","sf",   "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "sjlabelled", "raster", "rlist", 'ggpubr',
       'cowplot', 'gridExtra', 'lme4', "patchwork", 'ggsci', 'glue')


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


lapply(list.of.packages, library, character.only = TRUE) #applying the library function to packages


#read files function 
read.files <- function(path, general_pattern, specific_pattern, fun) {
  files <- list.files(path = path , pattern = general_pattern, full.names = TRUE, recursive = TRUE)
  files<- files[(grep(specific_pattern, files))]
  sapply(files, fun, simplify = F)
}



#survey design function 
svydesign.fun <- function(filename){
  svydesign(id= ~id,
            strata=~strat,nest=T, 
            weights= ~wt, data=filename)
}


#survey estimates generating functions  
result.prop<- function(var, var1, design) {
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, svyciprop, method ='logit', levels=0.95, vartype= "se", na.rm=T, influence = TRUE)
}


result.mean<- function(var, var1, design) {
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, vartype= "se", na.rm=T, influence = TRUE)
}


result.median<- function(var, var1, design) {
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svyquantile, design, quantiles=0.5, ci=TRUE, vartype="ci", na.rm=T)
}



result.sum<- function(var, var1, design) {
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svytotal, design, quantiles=0.5, ci=TRUE, vartype="ci", na.rm=T)
}


#estimation functions 
estim_prop <- function(df, col, by){
  svy_mal <- svydesign.fun(df)
  clu_est <- result.prop(col, by, design=svy_mal)
}



estim_mean <- function(df, col, by){
  svy_mal <- svydesign.fun(df)
  clu_est <- result.mean(col, by, design=svy_mal)
}

estim_median <- function(df, col, by){
  svy_mal <- svydesign.fun(df)
  clu_est <- result.median(col, by, design=svy_mal)
}

estim_sum <- function(df, col, by){
  svy_mal <- svydesign.fun(df)
  clu_est <- result.sum(col, by, design=svy_mal)
}


get_crs <- function(df, raster){
  dhs <- spTransform(x = df, CRSobj = crs(raster))
}



extract_fun <- function(raster, dhs, buffer){
  clu_val<-raster::extract(raster,dhs, buffer = buffer, fun = mean, df =TRUE) %>%
    mutate(dhs_year = dhs$DHSYEAR)%>%
    mutate(hv001 = dhs$DHSCLUST) 
}


extract_fun_month <- function(raster, dhs, buffer){
  clu_val<-raster::extract(raster,dhs, buffer = buffer, fun = mean, df =TRUE) %>%
    mutate(dhs_year = dhs$DHSYEAR, hv001 = dhs$DHSCLUST, month = dhs$hv006)
}

map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA), 
        legend.title.align=0.5,
        legend.title=element_text(size=16, colour = 'black'), 
        legend.text =element_text(size = 16, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}


rast_ex_fun <- function(main_dir, pri_dir, end_pattern, start_pattern, output_name_start, shp_file, col_pattern, output_name_start2,output_name_end){
  vars <- c(0, 1000, 2000, 3000, 4000)
  files <- list.files(path = file.path(main_dir, pri_dir) , pattern = end_pattern, full.names = TRUE, recursive = TRUE)
  files<- files[(grep(start_pattern, files))]
  raster<-sapply(files, raster, simplify = F)
  
  for (i in 1:length(vars)) {
    var_name <- paste0(output_name_start, as.character(vars[i]), 'm')
    df <- map2(shp_file, raster, get_crs)
    df <- pmap(list(raster, df, vars[i]), extract_fun)
    df <- df %>%  map(~rename_with(., .fn=~paste0(var_name), .cols = col_pattern))
    df <- plyr::ldply(df) %>% dplyr::select(-c(ID))
    write.csv(df, file =file.path(GeoDir, paste0(output_name_start, as.character(vars[i]), 
                                                 'm_buffer', output_name_end)),row.names = FALSE)
  }
}



rast_ex_fun_time <- function(main_dir, pri_dir, end_pattern, output_name_start, shp_file, col_pattern, output_name_start2,output_name_end){
  vars <- c(0, 1000, 2000, 3000, 4000)
  files <- list.files(path = file.path(main_dir, pri_dir) ,pattern = end_pattern, full.names = TRUE, recursive = TRUE)
  raster <- sapply(files, raster, simplify = F)
  
  for (i in 1:length(vars)) {
    var_name <- paste0(output_name_start, as.character(vars[i]), 'm')
    df <- map2(shp_file, raster, get_crs)
    df <- pmap(list(raster, df, vars[i]), extract_fun_month)
    df <- df %>%  map(~rename_with(., .fn=~paste0(var_name), .cols = col_pattern))
    df <- plyr::ldply(df) %>% dplyr::select(-c(ID))
    df <- df %>% arrange(month) %>%  group_by(dhs_year, hv001) %>%  slice(1)
    write.csv(df, file =file.path(GeoDir, paste0(output_name_start, as.character(vars[i]), 
                                               'm_buffer', output_name_end)),row.names = FALSE)
  }
}


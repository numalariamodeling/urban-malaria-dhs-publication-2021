rm(list=ls())
#memory.limit(size = 50000)

## -----------------------------------------
### Paths
## -----------------------------------------

Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
DataDir <-file.path(NGDir, "data")
SrcDir <- file.path(NGDir, 'src', 'Research', 'urban_rural_transmission_analysis')
ResultDir <-file.path(NGDir, "results")
BinDir <- file.path(NGDir, "bin")
DHSData <- file.path(DataDir, 'DHS')
DataIn <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates')
ProjectDir <- file.path(NuDir, "projects", "urban_malaria")
manuscript_dir <- file.path(ProjectDir,'manuscript')
illustrations_dir <- file.path(manuscript_dir,'illustrations')

ifelse(!dir.exists(file.path(DataIn, "cleaned_cluster_covariates_all")), 
       dir.create(file.path(DataIn, "cleaned_cluster_covariates_all")), FALSE)

cleandatDir <- file.path(DataIn, 'cleaned_cluster_covariates_all')

# ------------------------------------------
### Required functions and settings
## -----------------------------------------
source(file.path(SrcDir, "functions", "model functions.R"))
library(corrplot)
library(Hmisc)
library(ggcorrplot)
library(BAS)

# ------------------------------------------
### Data cleaning  
## -----------------------------------------

#DHS data 

files <- list.files(path = file.path(DataIn, 'DHS_survey_extract') , pattern = '.csv', full.names = TRUE, recursive = TRUE)
files<- files[-grep('_0m_|_1000m_|_3000m_|_4000m_|Temp_covereates|DHS_18.csv|pop_density_|p_test_lagos_|pop_density_2000m_buffer_DHS_10_15_18.csv|pop_density_2km_buffer_DHS_10_15_18_30sec|building_density|elevation_', files)]
df <-sapply(files, read.csv, simplify = F)

df <- df %>% map(~dplyr::select(., -X)) %>%  map_if(~ all(c('X.1') %in% colnames(.x)),~dplyr::select(., -X.1)) %>% 
  map_if(~ all(c('se') %in% colnames(.x)),~dplyr::select(., -se)) %>% 
  map_if(~ all(c('ci_l') %in% colnames(.x)),~dplyr::select(., -ci_l)) %>% 
  map_if(~ all(c('ci_u') %in% colnames(.x)),~dplyr::select(., -ci_u)) %>% 
  map_if(~ all(c('ID') %in% colnames(.x)), ~rename(., v001 = ID)) %>%  
  map_if(~ all(c('hv001') %in% colnames(.x)), ~rename(., v001 = hv001))


df <- df %>%  map(~mutate(., dhs_year = str_split(.id, "_", simplify = T)[, 4]) ) %>%  map(~dplyr::select(., -.id))

df<- df[order(sapply(df,nrow),decreasing = T)]

df <- df %>%  purrr::reduce(left_join, by = c('dhs_year', 'v001'))


#geospatial covariates 
files <- list.files(path = file.path(DataIn, 'geospatial_covariates') , pattern = '.csv', full.names = TRUE, recursive = FALSE)
files<- files[-grep('_0m_|_1000m_|_3000m_|_4000m_|pop_density_FB', files)]
df_geo <-sapply(files, read.csv, simplify = F) %>% map(~dplyr::select(., -.id)) 
 
df_geo <- df_geo %>% map_if(~ all(c('hv001') %in% colnames(.x)), ~rename(., v001 = hv001))


df_geo<- df_geo[order(sapply(df_geo,nrow),decreasing = T)]

df_geo <- df_geo %>%  purrr::reduce(left_join, by = c('dhs_year', 'v001')) %>%  mutate(dhs_year = as.character(dhs_year))


#joining all datasets together 

df <- df %>%  left_join(df_geo, by = c('dhs_year', 'v001')) 

write.csv(df, paste0(cleandatDir, '/all_cluster_variables_urban_malaria.csv'))



# ------------------------------------------
### Check for collinearity 
## -----------------------------------------
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


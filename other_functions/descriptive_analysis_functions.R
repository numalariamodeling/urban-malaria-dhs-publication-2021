# # Reading in the necessary packages 

list.of.packages <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "raster",
                      "lubridate", "sf", "labelled","scales",  "raster", "rlist", 'ggpubr', #, 'rgeos'
                      'cowplot', 'gridExtra', 'lme4', 'ggsci', 'patchwork', 'ggcorrplot', 'pscl', 'visreg', 'viridis', 'splines', 'shades')


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


lapply(list.of.packages, library, character.only = TRUE) #applying the library function to packages

align_legend <- function(p, hjust = 0.5)
{
  # extract legend
  g <- cowplot::plot_to_gtable(p)
  grobs <- g$grobs
  legend_index <- which(sapply(grobs, function(x) x$name) == "guide-box")
  legend <- grobs[[legend_index]]
  
  # extract guides table
  guides_index <- which(sapply(legend$grobs, function(x) x$name) == "layout")
  
  # there can be multiple guides within one legend box  
  for (gi in guides_index) {
    guides <- legend$grobs[[gi]]
    
    # add extra column for spacing
    # guides$width[5] is the extra spacing from the end of the legend text
    # to the end of the legend title. If we instead distribute it by `hjust:(1-hjust)` on
    # both sides, we get an aligned legend
    spacing <- guides$width[5]
    guides <- gtable::gtable_add_cols(guides, hjust*spacing, 1)
    guides$widths[6] <- (1-hjust)*spacing
    title_index <- guides$layout$name == "title"
    guides$layout$l[title_index] <- 2
    
    # reconstruct guides and write back
    legend$grobs[[gi]] <- guides
  }
  
  # reconstruct legend and write back
  g$grobs[[legend_index]] <- legend
  g
}


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

theme_corr <- function(){
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        axis.text.x = element_text(size = 12, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"))
}

igv.lm.point <- function(df, x, y, point_val, legend_title, xlab, ylab){
  plot <- ggplot(df, aes(x=x, y=y)) +
    geom_point(aes(fill=as.factor(point_val)), alpha=0.7, shape=21, size = 10) + 
    scale_fill_igv()+
    stat_cor(method = "pearson", col='darkred') + 
    theme_manuscript()+
    guides(fill=guide_legend(title=legend_title, override.aes = list(size =5)))+
    xlab(xlab)+
    ylab(ylab)
  
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


gmap_fun <- function(polygon_name, point_data, labels, fill, legend_title){
  ggplot(polygon_name) +
    geom_sf(color='lightgrey')+
    geom_point(data = point_data,
               aes(fill=fill,  geometry = geometry),
               stat = "sf_coordinates", alpha = 0.45, size=3, shape=21
    ) +
    viridis::scale_fill_viridis(option='C', discrete=TRUE, labels=labels, na.value ='grey', limits=c('[0,0.2]', '(0.2,0.4]', '(0.4,0.6]', '(0.6,0.8]', '(0.8,1]', NA)) +
    map_theme() + 
    guides(fill = guide_legend(title=legend_title, override.aes = list(size = 5)))+
    xlab("")+
    ylab("")
}


#cluisters over time map plot function
gmap_fun2 <- function(polygon_name, point_data, labels, fill, legend_title){
  ggplot(polygon_name) +
    geom_sf(color='lightgrey')+
    geom_point(data = point_data,
               aes(fill=fill,  geometry = geometry),
               stat = "sf_coordinates", alpha = 0.45, size=3, shape=21) +
    scale_fill_manual(values = c("#5560AB",  "#FAAF43", "#EE3C96", "lightseagreen")) +
    map_theme() + 
    guides(fill = guide_legend(title=legend_title, override.aes = list(size = 4)))+
    xlab("")+
    ylab("")
}


hist_fun <-function(df, x,fill, xlab, ylab,labels){
  ggplot(df_all, aes(x =x, fill =fill))+
    geom_histogram(alpha = 0.4, position="identity") +
    scale_x_continuous(expand = c(0.03, 0)) +
    scale_y_continuous(expand = c(0.03, 0)) +
    labs(x = xlab, y =ylab)+
    scale_fill_discrete(labels = labels)+
    theme_manuscript()+
    guides(fill=guide_legend(title=NULL))
  
}



gdensity_fun <- function(df, x, fill,legend_title, xlab, ylab){
  ggplot(df, aes(x=x, fill=as.factor(fill))) +
    geom_density(alpha = 0.7, stat = "density", trim = F) + 
    scale_x_continuous(expand = c(0.03, 0)) +
    scale_y_continuous(expand = c(0.03, 0)) +
    theme_bw() + 
    scale_fill_manual(values = c("#5560AB",  "#FAAF43", "#EE3C96", "lightseagreen")) +
    theme_manuscript()+
    guides(fill=guide_legend(title=legend_title))+
    xlab(xlab)+
    ylab(ylab)
}




hist_fun2 <-function(df, xmin, xmax){
  p<- ggplot(df, aes_string(x=names(df)[var_list[[2]]])) + 
    geom_histogram(bins = 30, alpha = 0.7, position="identity", color = "violetred4", fill = colr_data[colr_list[[1]]])+
    theme_manuscript()+ 
    labs (title = labels_data[label_list[[2]]], x = "values") +
    xlab(xlab_data[2]) +
    ylab("Count")+
    scale_y_continuous(expand = c(0.03, 0))+
    scale_x_continuous(limits = c(xmin, xmax), expand = c(0.01, 0))
  
}


cdf_hist = function(df, fill,color, x, xlab, bins){
  hist=ggplot(df, aes(x =.data[[x]]))+geom_histogram(alpha = 0.4, position="identity", bins=bins)
  max_y=max(ggplot_build(hist)$data[[1]]$count)
  ggplot(df, aes(.data[[x]]))+
    geom_histogram(fill=fill, color= color, alpha = 0.4, position="identity", bins = bins) +
    stat_ecdf(aes_(y =bquote(..y..* .(max_y)), color =color))+
    scale_y_continuous(name= 'Count', sec.axis=sec_axis(trans = ~./max_y, name = 'Cumulative percent', labels = function(x) format(x *100, digits=2, nsmall=0)))+
    theme_manuscript()+theme(legend.position = 'none')+
    xlab(xlab)
}



forest_fun = function(data, color1, color2, xname, breaks, labels){
  ggplot(data=data, aes(y=index, x= coefficient, xmin=lci, xmax=uci))+ 
    geom_point(shape = 15, color=color1, size = 3)+ 
    geom_errorbarh(height=.1, color =color2)+
    scale_x_continuous(name=xname)+
    scale_y_continuous(name = "", breaks=breaks, labels = labels, trans = 'reverse')+
    geom_vline(xintercept=0, color='black', linetype="dashed", alpha=.5)+
    theme_manuscript()
}




plots_fun <- function(df, y_var, color_point, color_smooth, fill_smooth, reg_fam, y_lab){
  plots = df %>% {map2(., xlab, ~ggplot(.x,aes_string(x='values', y=y_var))+
                                      geom_point(shape=42, size= 3, color = color_point, alpha = 0.5) +
                                      geom_smooth(aes(fill = "Trend"), se = FALSE, color = color_smooth, method = 'glm', method.args = list(family = reg_fam(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))+
                                      geom_smooth(aes(color = "Confidence Interval"), fill = fill_smooth, linetype = 0, method = 'glm', method.args = list(family = reg_fam(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x), length =4)[2:3]))+
                                      theme_manuscript()+
                                      labs(x = .y, y = y_lab)+
                                      guides(fill =FALSE, color =FALSE))}
}




plots_fun2 <- function(df, y_var, color_point, color_smooth, fill_smooth, reg_fam, y_lab){
  plots = df %>% {map2(., xlab, ~ggplot(.x,aes_string(x='values', y=y_var))+
                         geom_point(shape=42, size= 3, color = color_point, alpha = 0.5) +
                         geom_smooth(aes(fill = "Trend"), se = FALSE, color = color_smooth, method = 'glm', method.args = list(family = reg_fam(link = "log")), formula = y ~ ns(x, 3))+
                         geom_smooth(aes(color = "Confidence Interval"), fill = fill_smooth, linetype = 0, method = 'glm', method.args = list(family = reg_fam(link = "log")), formula = y ~ ns(x, 3))+
                         theme_manuscript()+
                         labs(x = .y, y = y_lab)+
                         guides(fill =FALSE, color =FALSE))}
}

plots_fun3 <- function(df, y_var, color_point, color_smooth, fill_smooth, reg_fam, x_lab1, y_lab){
  plots = df %>% {purrr::map2(., xlab, ~ggplot(.x,aes_string(x='values', y=y_var))+
                                geom_point(shape=42, size= 3, color = color_point, alpha = 0.5) +
                                geom_smooth(aes(fill = "Trend"), se = FALSE, color = color_smooth, method = 'glm', method.args = list(family = reg_fam(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))+
                                geom_smooth(aes(color = "Confidence Interval"), fill = fill_smooth, linetype = 0, method = 'glm', method.args = list(family = reg_fam(link = "log")), formula = y ~ ns(x, 3, knots = seq(min(x),max(x),length =4)[2:3]))+
                                theme_manuscript()+
                                labs(x = x_lab1, y =y_lab)+
                                guides(fill =FALSE, color =FALSE))}
}

plots_para <- function(xlist, fill_list, color_list, bin_list){
  x=list(xlist)
  fill = list(fill_list)
  color = list(color_list)
  bins = list(bin_list)
}



state_map <- function(state, STATE_NAME, title){
  df = dplyr::filter(state_sf, (NAME_1 %in% c(state)))
  STATE <- STATE_NAME
  map_name = dplyr::filter(map, (ADM1NAME %in% c(STATE)))
  map_state = gmap_fun(df, map_name, labels=c('0 - 0.2', '0.3 - 0.4', '0.5 - 0.6', '0.7 - 0.8', '0.9 - 1.0', 'Missing data'),
                      map_name$positives_cut, title)
  map_state = map_state + theme(legend.position = 'none', panel.border = element_rect(colour = "black", fill=NA, size=0.5))+ xlab(state)
  
  
}



#x <- c("tidyverse","INLA", "ggplot2", "ggpubr",  "rgdal", "sp", "sf", "tmap", 
#'paletteer', 'cowplot', 'gridExtra', 'lme4', 'reshape2', "patchwork", "gdata",'cowplot', 'mmtable2', 'ggsci') #"inlabru","rebus"




#lapply(x, library, character.only = TRUE) #applying the library function to packages
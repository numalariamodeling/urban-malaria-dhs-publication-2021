# # Reading in the necessary packages 
list.of.packages <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
                      "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
                      "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "sjlabelled", "raster", "rlist", 'rgeos', 'INLA', 'ggpubr',
                      'cowplot', 'gridExtra', 'lme4', 'ggsci', 'patchwork')


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
          axis.text.x = element_text(size = 16, color = "black"), 
          axis.text.y = element_text(size = 16, color = "black"),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size =16),
          legend.title=element_text(size=16, colour = 'black'),
          legend.text =element_text(size = 16, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}

igv.lm.point <- function(x, y, point_val, legend_title, xlab, ylab){
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
    geom_density(alpha=0.5) + 
    scale_x_continuous(expand = c(0.03, 0)) +
    scale_y_continuous(expand = c(0.03, 0)) +
    theme_bw() + 
    scale_fill_manual(values = c('#0073C2B2', "#EE4C97B2", "#E18727B2"))+
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





#x <- c("tidyverse","INLA", "ggplot2", "ggpubr",  "rgdal", "sp", "sf", "tmap", 
#'paletteer', 'cowplot', 'gridExtra', 'lme4', 'reshape2', "patchwork", "gdata",'cowplot', 'mmtable2', 'ggsci') #"inlabru","rebus"




#lapply(x, library, character.only = TRUE) #applying the library function to packages
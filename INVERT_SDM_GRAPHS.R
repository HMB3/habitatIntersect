## ENVIRONMENT SETTINGS =============================================================


# \
# 
# Load the packages ::
# 
# 
# \
# 
# To install, run :


## Set env
rm(list = ls())
options(java.parameters = "-Xmx64000m")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_321')



## Function to load or install packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos = "https://cran.csiro.au/")
  sapply(pkg, require, character.only = TRUE)
}


## Load packages
#devtools::install_github("HMB3/nenswniche")
library(nenswniche)
library(cowplot)
data('sdmgen_packages')
ipak(sdmgen_packages)


## The functions expect these folders,
inv_rs_dir           <- './output/invert_maxent_pbi_ala_site/'
inv_back_dir         <- './output/invert_maxent_pbi_ala_site/back_sel_models/'
inv_results_dir      <- './output/invert_maxent_pbi_ala_site/results/'
inv_fire_dir         <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/FESM_SDM_intersect/'





## 5). INDIVIDUAL TAXA GRAPHS =============================================================


## Species % burnt scatter plots ----
INVERT.FESM.TABLE.TAXA.RANGE <- INVERT.FESM.TABLE %>% 
  
  left_join(., dplyr::select(invert_LUT,
                             Taxa,
                             Aus_records,
                             AOO,
                             EOO,
                             KOP_count), by = c('Taxa')) %>% 
  
  dplyr::select(Percent_burnt, Habitat_km2, Habitat_burnt_km2, AOO, EOO, Aus_records, KOP_count)


INVERT.FESM.TABLE.FAM.RANGE <- INVERT.FESM.TABLE.FAM %>% 
  
  left_join(., dplyr::select(invert_LUT,
                             Taxa,
                             Aus_records,
                             AOO,
                             EOO,
                             KOP_count), by = c('Taxa')) %>% 
  
  dplyr::select(Percent_burnt, Habitat_km2, Habitat_burnt_km2, AOO, EOO, Aus_records, KOP_count)


INVERT.FESM.TABLE.GEN.RANGE <- INVERT.FESM.TABLE.GEN %>% 
  
  left_join(., dplyr::select(invert_LUT,
                             Taxa,
                             Aus_records,
                             AOO,
                             EOO,
                             KOP_count), by = c('Taxa')) %>% 
  
  dplyr::select(Percent_burnt, Habitat_km2, Habitat_burnt_km2, AOO, EOO, Aus_records, KOP_count)


INVERT.FESM.TABLE.SPP.RANGE <- INVERT.FESM.TABLE.SPP %>% 
  
  left_join(., dplyr::select(invert_LUT,
                             Taxa,
                             Aus_records,
                             AOO,
                             EOO,
                             KOP_count), by = c('Taxa')) %>% 
  
  dplyr::select(Percent_burnt, Habitat_km2, Habitat_burnt_km2, AOO, EOO, Aus_records, KOP_count)


INVERT.FESM.TABLE.SPP.LOG <-
  
  INVERT.FESM.TABLE.SPP.RANGE %>% mutate(log_AOO     = log(AOO),
                                         log_EOO     = log(EOO),
                                         log_Habit   = log(Habitat_km2),
                                         log_Burnt   = log(Habitat_burnt_km2),
                                         log_Records = log(Aus_records)) %>% 
  
  dplyr::select(-AOO, -EOO, -Habitat_km2, -Habitat_burnt_km2, -Aus_records) %>% 
  dplyr::select(Percent_burnt, log_Habit, log_Burnt, log_AOO, log_EOO, log_Records) 
  

## Could create a standard graph here :: pairs.panel
png(paste0(inv_fire_dir, 'fesm_inv_taxa_scatter_plots.png'),
    12, 8, units = 'in', res = 500)


psych::pairs.panels(INVERT.FESM.TABLE.TAXA.RANGE,
                    method   = "pearson", # correlation method
                    hist.col = "#00AFBB",
                    density  = TRUE,      # show density plots
                    ellipses = FALSE,
                    cex = 1.2,
                    cex.labels = 1.2,
                    lwd = 2,
                    col = "blue")

dev.off()
gc()


png(paste0(inv_fire_dir, 'fesm_inv_species_scatter_plots.png'),
    12, 8, units = 'in', res = 500)

psych::pairs.panels(INVERT.FESM.TABLE.SPP.LOG,
                    method   = "pearson", # correlation method
                    hist.col = "#00AFBB",
                    density  = TRUE,      # show density plots
                    ellipses = FALSE,
                    cex = 1.2,
                    cex.labels = 0.9,
                    lwd = 2,
                    col = "blue")

dev.off()
gc()


## Species % burnt graphs ----

# for(taxa in INVERT.FESM.TABLE$Taxa) {
#   
#   ## taxa = INVERT.FESM.TABLE$Taxa[1]
#   bar_df  <- INVERT.FESM.TABLE %>% filter(Taxa == taxa) %>% 
#     mutate(Percent_unburnt = 100 - Percent_burnt) 
#   
#   plot <- ggplot(bar_df, aes(fill=Percent_burnt, y=Percent_burnt, x=Taxa)) + 
#     geom_bar(position="stack", stat="identity")
#   
#   print(plot)
#   
# }


## Graphs for each species ----


## Graphs of the % burnt and un-burnt
# for(taxa in INVERT.FESM.TABLE$Taxa) {
#   
#   ## taxa = INVERT.FESM.TABLE$Taxa[1]
#   bar_df  <- INVERT.FESM.TABLE %>% filter(Taxa == taxa) %>% 
#     mutate(Percent_unburnt = 100 - Percent_burnt) 
#   
#   plot <- ggplot(bar_df, aes(fill=Percent_burnt, y=Percent_burnt, x=Taxa)) + 
#     geom_bar(position="stack", stat="identity")
#   
#   print(plot)
#   
# }


## Set variables
tsize     = 30
capt_size = 20
xsize     = 10
ysize     = 30
ycol      = 'black'
lab_size  = 8
mar       = 1.5

ymin      = 0 
axis_multiplier = 0.2
ylab  = '\nPercent (%)\n'
xlab  = ''


spp_run <- nrow(INVERT.MAXENT.SPP.RESULTS)


## Graphs of the % burnt in each Vegetation type
for(taxa in unique(INVERT.FESM.VEG.TABLE$Taxa)) {
  
  ## Update the table
  # taxa <- unique(INVERT.FESM.VEG.TABLE$Taxa)[10]
  message('writing sdm fire vegetation for ', taxa)
  
  save_name     <- gsub(' ', '_', taxa)
  bar_df        <- INVERT.FESM.VEG.TABLE %>% filter(Taxa == taxa) %>% 
    mutate(Habitat_Veg_burnt_perc = round(Habitat_Veg_burnt_perc, 2))
  bar_df$Vegetation <- factor(bar_df$Vegetation, 
                              levels = bar_df$Vegetation[order(bar_df$Habitat_Veg_burnt_perc, 
                                                               decreasing = TRUE)])
  
  overall_burnt <- INVERT.FESM.TABLE %>% filter(Taxa == taxa) %>% 
    .$Percent_burn %>% round(., 2)
  
  area_burnt    <- INVERT.FESM.TABLE %>% filter(Taxa == taxa) %>% 
    .$Habitat_burnt_km2 %>% round(., 0)
  
  ##
  ymax = max(bar_df$Habitat_Veg_burnt_perc) + 
    max(bar_df$Habitat_Veg_burnt_perc * axis_multiplier)
  
  sdm_fire_veg_plot <- ggplot(bar_df, 
                              aes(x = Vegetation, 
                                  y = Habitat_Veg_burnt_perc, 
                                  fill = Vegetation)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    
    scale_fill_manual(values = c('Extremely tall open forest' = 'forestgreen', 
                                 'Low open forest'            = 'lightgoldenrod1',
                                 'Medium Open forest'         = "yellowgreen",
                                 'Very tall open forest'      = "palegreen3",
                                 'Tall open forest'           = "mediumspringgreen",
                                 'Tall closed forest'         = "limegreen",
                                 'very tall closed forest'    = "mediumseagreen"), na.value = "grey") +
    
    geom_text(aes(label   = Habitat_Veg_burnt_perc, hjust = + 0.5), 
              hjust       = -0.5, 
              position    = position_dodge(width = 1),
              inherit.aes = TRUE,
              size        = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab('Percentage (%) Burnt') +
    ggtitle(taxa) +
    xlab('') +
    labs(caption = paste0(overall_burnt, ' % Burnt overall (', 
                          area_burnt, ' km2)')) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size  = xsize),
          axis.title.x    = element_text(size  = xsize, face = "bold"),
          legend.position = 'none',
          
          axis.title.y    = element_text(size = ysize, face = "bold"),
          axis.text.y     = element_text(size = ysize, color = "black"),
          plot.subtitle   = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"),
          plot.caption    = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"))
  
  png(paste0(inv_fire_dir, save_name, '_sdm_veg_intersect_fire_barplots.png'),
      12, 6, units = 'in', res = 500)
  plot(sdm_fire_veg_plot)
  dev.off()
  gc()
  
}


## Graphs of the % burnt in each Burn category 
for(taxa in unique(INVERT.FESM.CLASS.TABLE$Taxa)) {
  
  ## Update the table
  # taxa <- unique(INVERT.FESM.CLASS.TABLE$Taxa)[10]
  message('writing sdm fire classes for ', taxa)
  
  save_name     <- gsub(' ', '_', taxa)
  bar_df        <- INVERT.FESM.CLASS.TABLE %>% filter(Taxa == taxa) 
  
  bar_df$Burn_Category <- factor(bar_df$Burn_Category, 
                                 levels = bar_df$Burn_Category[order(bar_df$Burn_Category_burnt_perc, 
                                                                     decreasing = TRUE)])
  
  area_burnt    <- INVERT.FESM.CLASS.TABLE %>% filter(Taxa == taxa) %>% 
    .$ Burn_Category_burnt_area %>% sum()
  
  ## Update the 
  ymax = max(bar_df$Burn_Category_burnt_perc) + 
    max(bar_df$Burn_Category_burnt_perc * axis_multiplier)
  
  sdm_fire_class_plot <- ggplot(bar_df, 
                                aes(x = Burn_Category, 
                                    y = Burn_Category_burnt_perc, 
                                    fill = Burn_Category)) +
    
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    
    scale_fill_manual(values = c('Low severity'           = "#FEEDDE", 
                                 'Moderate-low severity'  = "#FDBE85",
                                 'Moderate-high severity' = "#FD8D3C",
                                 'High severity'          = "#D94701"), na.value = "grey") +
    
    geom_text(aes(label   = Burn_Category_burnt_perc, hjust = + 0.5), 
              hjust       = -0.5, 
              position    = position_dodge(width = 1),
              inherit.aes = TRUE,
              size        = lab_size) +
    
    theme_classic(base_size = 16) +
    ylab('Percentage (%) Burnt') +
    ggtitle(taxa) +
    xlab('') +
    labs(caption = paste0(area_burnt, ' km2 Burnt across classes ')) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold.italic"),
          axis.text.x     = element_text(size  = xsize),
          axis.title.x    = element_text(size  = xsize, face = "bold"),
          legend.position = 'none',
          
          axis.title.y    = element_text(size = ysize, face  = "bold"),
          axis.text.y     = element_text(size = ysize, color = "black"),
          plot.subtitle   = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"),
          plot.caption    = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"))
  
  png(paste0(inv_fire_dir, save_name, '_sdm_fire_classes_barplots.png'),
      12, 6, units = 'in', res = 500)
  plot(sdm_fire_class_plot)
  dev.off()
  gc()
  
}





## 6). ALL TAXA GRAPHS =============================================================


## 
INVERT.GEN.FESM.VEG.GROUP <- INVERT.FESM.VEG.TABLE.SPP %>% 
  
  ## group by Vegetation
  group_by(Vegetation) %>% 
  summarise(Average_burnt_area    = mean(Habitat_Veg_burnt_area),
            Average_burnt_percent = mean(Habitat_Veg_burnt_perc),
            Average_burnt_area    = round(Average_burnt_area, 2),
            Average_burnt_percent = round(Average_burnt_percent, 2)) %>% 
  arrange(-Average_burnt_percent)


## Factor
INVERT.GEN.FESM.VEG.GROUP$Vegetation <- 
  factor(INVERT.GEN.FESM.VEG.GROUP$Vegetation, 
         levels = INVERT.GEN.FESM.VEG.GROUP$Vegetation
         [order(INVERT.GEN.FESM.VEG.GROUP$Average_burnt_percent, 
                decreasing = TRUE)])

## Vegetation burnt across all species ----
ymax = max(INVERT.GEN.FESM.VEG.GROUP$Average_burnt_percent) + 
  max(INVERT.GEN.FESM.VEG.GROUP$Average_burnt_percent * axis_multiplier)


all_sdm_fire_veg_plot <- ggplot(INVERT.GEN.FESM.VEG.GROUP, 
                                aes(x = Vegetation, 
                                    y = Average_burnt_percent, 
                                    fill = Vegetation)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  
  scale_fill_manual(values = c('Extremely tall open forest' = 'forestgreen', 
                               'Low open forest'            = 'lightgoldenrod1',
                               'Medium Open forest'         = "yellowgreen",
                               'Very tall open forest'      = "palegreen3",
                               'Tall open forest'           = "mediumspringgreen",
                               'Tall closed forest'         = "limegreen",
                               'very tall closed forest'    = "mediumseagreen"), na.value = "grey") +
  
  geom_text(aes(label   = Average_burnt_percent, hjust = + 0.5), 
            hjust       = -0.5, 
            position    = position_dodge(width = 1),
            inherit.aes = TRUE,
            size        = lab_size) +
  
  theme_classic(base_size = 16) +
  ylab('Percentage (%) Habitat Burnt') +
  ggtitle('') +
  xlab('') +
  # labs(caption = caption) +
  
  ylim(c(ymin, ymax)) +
  
  theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
        plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
        axis.text.x     = element_text(size  = xsize),
        axis.title.x    = element_text(size  = xsize, face = "bold"),
        legend.position = 'none',
        
        axis.title.y    = element_text(size = ysize, face = "bold"),
        axis.text.y     = element_text(size = ysize, color = "black"),
        plot.subtitle   = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"),
        plot.caption    = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"))

png(paste0(inv_fire_dir, 'all_sdm_fire_veg_plot.png'),
    12, 8, units = 'in', res = 500)
plot(all_sdm_fire_veg_plot)
dev.off()





## 
INVERT.GEN.FESM.CLASS.GROUP <- INVERT.FESM.CLASS.TABLE.SPP %>% 
  
  ## group by Vegetation
  group_by(Burn_Category) %>% 
  summarise(Average_burnt_area    = mean(Burn_Category_burnt_area),
            Average_burnt_percent = mean(Burn_Category_burnt_perc),
            Average_burnt_area    = round(Average_burnt_area, 2),
            Average_burnt_percent = round(Average_burnt_percent, 2)) %>% 
  arrange(-Average_burnt_percent)


INVERT.GEN.FESM.CLASS.GROUP$Burn_Category <- 
  factor(INVERT.GEN.FESM.CLASS.GROUP$Burn_Category, 
         levels = INVERT.GEN.FESM.CLASS.GROUP$Burn_Category
         [order(INVERT.GEN.FESM.CLASS.GROUP$Average_burnt_percent, 
                decreasing = TRUE)])


## Classes burnt across all species ----
ymax = max(INVERT.GEN.FESM.CLASS.GROUP$Average_burnt_percent) + 
  max(INVERT.GEN.FESM.CLASS.GROUP$Average_burnt_percent * axis_multiplier)


all_sdm_fire_class_plot <- ggplot(INVERT.GEN.FESM.CLASS.GROUP, 
                                  aes(x = Burn_Category, 
                                      y = Average_burnt_percent, 
                                      fill = Burn_Category)) +
  
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  
  scale_fill_manual(values = c('Low severity'           = "#FEEDDE", 
                               'Moderate-low severity'  = "#FDBE85",
                               'Moderate-high severity' = "#FD8D3C",
                               'High severity'          = "#D94701"), na.value = "grey") +
  
  geom_text(aes(label   = Average_burnt_percent, hjust = + 0.5), 
            hjust       = -0.5, 
            position    = position_dodge(width = 1),
            inherit.aes = TRUE,
            size        = lab_size) +
  
  theme_classic(base_size = 16) +
  ylab('Percentage (%) Habitat Burnt') +
  ggtitle('') +
  xlab('') +
  # labs(caption = caption) +
  
  ylim(c(ymin, ymax)) +
  
  theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
        plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
        axis.text.x     = element_text(size  = xsize),
        axis.title.x    = element_text(size  = xsize, face = "bold"),
        legend.position = 'none',
        
        axis.title.y    = element_text(size = ysize, face  = "bold"),
        axis.text.y     = element_text(size = ysize, color = "black"),
        plot.subtitle   = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"),
        plot.caption    = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"))

png(paste0(inv_fire_dir, 'all_sdm_fire_class_plot.png'),
    12, 8, units = 'in', res = 500)
plot(all_sdm_fire_class_plot)
dev.off()
gc()




## Invert Types burnt across all species ---
INVERT.GEN.FESM.TYPE.GROUP <- INVERT.FESM.TABLE.SPP %>% 
  
  ## group by TYPE
  group_by(Type) %>% 
  summarise(Average_burnt_area    = mean(Habitat_burnt_km2),
            Average_burnt_percent = mean(Percent_burnt),
            Average_burnt_area    = round(Average_burnt_area, 2),
            Average_burnt_percent = round(Average_burnt_percent, 2)) %>% 
  arrange(-Average_burnt_percent)


## Factor
INVERT.GEN.FESM.TYPE.GROUP$Type <- 
  factor(INVERT.GEN.FESM.TYPE.GROUP$Type, 
         levels = INVERT.GEN.FESM.TYPE.GROUP$Type
         [order(INVERT.GEN.FESM.TYPE.GROUP$Average_burnt_percent, 
                decreasing = TRUE)])

##
ymax = max(INVERT.GEN.FESM.TYPE.GROUP$Average_burnt_percent) + 
  max(INVERT.GEN.FESM.TYPE.GROUP$Average_burnt_percent * axis_multiplier)

all_sdm_fire_type_plot <- ggplot(INVERT.GEN.FESM.TYPE.GROUP, 
                                 aes(x = Type, 
                                     y = Average_burnt_percent, 
                                     fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  
  scale_fill_manual(values = c('Spiders' = 'forestgreen', 
                               'Beetles' = 'lightgoldenrod1',
                               'Snails'  = "yellowgreen",
                               'Bugs'    = "palegreen3")) +
  
  geom_text(aes(label   = Average_burnt_percent, hjust = + 0.5), 
            hjust       = -0.5, 
            position    = position_dodge(width = 1),
            inherit.aes = TRUE,
            size        = lab_size) +
  
  theme_classic(base_size = 16) +
  ylab('Percentage (%) Habitat Burnt') +
  ggtitle('All Invert Taxa') +
  xlab('') +
  
  ylim(c(ymin, ymax)) +
  
  theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
        plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
        axis.text.x     = element_text(size  = xsize),
        axis.title.x    = element_text(size  = xsize, face = "bold"),
        legend.position = 'none',
        
        axis.title.y    = element_text(size = ysize, face  = "bold"),
        axis.text.y     = element_text(size = ysize, color = "black"),
        plot.subtitle   = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"),
        plot.caption    = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"))

png(paste0(inv_fire_dir, 'all_sdm_fire_type_plot.png'),
    12, 8, units = 'in', res = 500)
plot(all_sdm_fire_type_plot)
dev.off()



## 
INVERT.GEN.FESM.VEG.TYPE.GROUP <- INVERT.FESM.VEG.TABLE.SPP %>% 
  
  ## group by Vegetation
  group_by(Vegetation, Type) %>% 
  summarise(Average_burnt_area    = mean(Habitat_Veg_burnt_area),
            Average_burnt_percent = mean(Habitat_Veg_burnt_perc),
            Average_burnt_area    = round(Average_burnt_area, 2),
            Average_burnt_percent = round(Average_burnt_percent, 2)) %>% 
  arrange(-Average_burnt_percent)


## Create Factors
INVERT.GEN.FESM.VEG.TYPE.GROUP$Type <-
  
  factor(INVERT.GEN.FESM.VEG.TYPE.GROUP$Type, 
         levels = unique(INVERT.GEN.FESM.VEG.TYPE.GROUP$Type)
         [order(INVERT.GEN.FESM.VEG.TYPE.GROUP$Average_burnt_percent, 
                decreasing = TRUE)])

INVERT.GEN.FESM.VEG.TYPE.GROUP$Vegetation <- 
  
  factor(INVERT.GEN.FESM.VEG.TYPE.GROUP$Vegetation, 
         levels = unique(INVERT.GEN.FESM.VEG.TYPE.GROUP$Vegetation)
         [order(INVERT.GEN.FESM.VEG.TYPE.GROUP$Average_burnt_percent, 
                decreasing = TRUE)])



## Veg/Types burnt across all species ----
xvar        = 'Average_burnt_percent'
yvar        = 'Vegetation'
title       = '' #'Habitat burnt by Vegetation'
caption     = paste0(spp_run, ' Invert Species Analysed')
col_palette = "Pastel2"

tsize      = 30
strip_size = 20
capt_size  = 20
xsize      = 10
ysize      = 15
lab_angle  = 45
ycol       = 'black'
lab_size   = 4
mar        = 1.5

ymin       = 0
axis_multiplier = 0.2
ylab       = ''
xlab       = 'Percentage (%) habitat burnt'
facet_var  = 'Type'
col_var    = 'Vegetation'
wrap_scale = "fixed"


##
all_sdm_fire_veg_type <- ggplot(INVERT.GEN.FESM.VEG.TYPE.GROUP, 
                                aes(x    = !!sym(xvar), 
                                    y    = !!sym(yvar), 
                                    fill = !!sym(yvar))) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  
  geom_text(aes(label   = Average_burnt_percent, hjust = + 1.2),
            
            hjust       = 0.7, 
            vjust       = 0.8,
            position    = position_dodge(width = 1),
            inherit.aes = TRUE,
            size        = lab_size) +
  
  scale_fill_manual(values = c('Extremely tall open forest' = 'forestgreen', 
                               'Low open forest'            = 'lightgoldenrod1',
                               'Medium Open forest'         = "yellowgreen",
                               'Very tall open forest'      = "palegreen3",
                               'Tall open forest'           = "mediumspringgreen",
                               'Tall closed forest'         = "limegreen",
                               'very tall closed forest'    = "mediumseagreen"), na.value = "grey") +
  
  theme_classic(base_size = 16) +
  ylab(ylab) +
  # ggtitle(title) +
  xlab(xlab) +
  labs(caption = caption) +
  
  theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
        plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
        strip.text      = element_text(size  = strip_size, face = 'bold'),
        axis.text.x     = element_blank(),
        axis.title.x    = element_text(size = xsize, face = "bold"),
        legend.position = 'bottom',
        legend.title    = element_blank(),
        
        axis.title.y  = element_text(size = ysize, face = "bold"),
        axis.text.y   = element_text(size = ysize, color = ycol),
        plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"),
        plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black")) +
  
  facet_wrap(as.formula(paste("~", facet_var)), scales = wrap_scale)

png(paste0(inv_fire_dir, 'all_habitat_veg_type_barplot.png'),
    12, 8, units = 'in', res = 500)
plot(all_sdm_fire_veg_type)
dev.off()




## Classes/Types burnt across all species ----
xvar       = 'Average_burnt_percent'
yvar       = 'Burn_Category'
title      = '' #'Habitat burnt by Class'
caption    = paste0(spp_run, ' Invert Species Analysed')
ylab       = ''
xlab       = 'Percentage (%) of burnt'
col_var    = 'Burn_Category'


## 
INVERT.GEN.FESM.CLASS.TYPE.GROUP <- INVERT.FESM.CLASS.TABLE.SPP %>% 
  
  ## group by Vegetation
  group_by(Burn_Category, Type) %>% 
  summarise(Average_burnt_area    = mean(Burn_Category_burnt_area),
            Average_burnt_percent = mean(Burn_Category_burnt_perc),
            Average_burnt_area    = round(Average_burnt_area, 2),
            Average_burnt_percent = round(Average_burnt_percent, 2)) %>% 
  arrange(-Average_burnt_percent)


## Create Factors
INVERT.GEN.FESM.CLASS.TYPE.GROUP$Type <-
  
  factor(INVERT.GEN.FESM.CLASS.TYPE.GROUP$Type, 
         levels = unique(INVERT.GEN.FESM.CLASS.TYPE.GROUP$Type)
         [order(INVERT.GEN.FESM.CLASS.TYPE.GROUP$Average_burnt_percent, 
                decreasing = TRUE)])

INVERT.GEN.FESM.CLASS.TYPE.GROUP$Burn_Category <- 
  
  factor(INVERT.GEN.FESM.CLASS.TYPE.GROUP$Burn_Category, 
         levels = unique(INVERT.GEN.FESM.CLASS.TYPE.GROUP$Burn_Category)
         [order(INVERT.GEN.FESM.CLASS.TYPE.GROUP$Average_burnt_percent, 
                decreasing = TRUE)])


## Can't set the maximum
all_sdm_fire_class_type <- ggplot(INVERT.GEN.FESM.CLASS.TYPE.GROUP, 
                                  aes(x    = !!sym(xvar), 
                                      y    = !!sym(yvar), 
                                      fill = !!sym(yvar))) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  
  geom_text(aes(label   = Average_burnt_percent, hjust = + 1.2),
            
            hjust       = 0.7, 
            vjust       = 0.8,
            position    = position_dodge(width = 1),
            inherit.aes = TRUE,
            size        = lab_size) +
  
  scale_fill_manual(values = c('Low severity'           = "#FEEDDE", 
                               'Moderate-low severity'  = "#FDBE85",
                               'Moderate-high severity' = "#FD8D3C",
                               'High severity'          = "#D94701"), na.value = "grey") +
  
  theme_classic(base_size = 16) +
  ylab(ylab) +
  # ggtitle(title) +
  xlab(xlab) +
  labs(caption = caption) +
  
  theme(plot.margin     = unit(c(mar, mar, mar, mar), "cm"),
        plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
        strip.text      = element_text(size  = strip_size, face = 'bold'),
        axis.text.x     = element_blank(),
        axis.title.x    = element_text(size = xsize, face = "bold"),
        legend.position = 'bottom',
        legend.title    = element_blank(),
        
        axis.title.y  = element_text(size = ysize, face = "bold"),
        axis.text.y   = element_text(size = ysize, color = ycol),
        plot.subtitle = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"),
        plot.caption  = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black")) +
  
  facet_wrap(as.formula(paste("~", facet_var)), scales = wrap_scale)

png(paste0(inv_fire_dir, 'all_habitat_fesm_class_type_barplot.png'),
    12, 8, units = 'in', res = 500)
plot(all_sdm_fire_class_type)
dev.off()


## Arrange all plots into one graph


grid_plot <- plot_grid(all_sdm_fire_veg_plot,
                       all_sdm_fire_class_plot,
                       all_sdm_fire_veg_type,
                       all_sdm_fire_class_type,
                       
                       nrow       = 2,
                       label_size = 3, 
                       align      = 'hv')


png(paste0(inv_fire_dir, 'sdm_invert_combined_barplots.png'),
    20, 16, units = 'in', res = 600)
plot(grid_plot)
dev.off()



## END =============================================================
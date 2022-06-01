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
data('sdmgen_packages')
ipak(sdmgen_packages)


## The functions expect these folders,
inv_rs_dir           <- './output/invert_maxent_pbi_ala/'
inv_results_dir      <- './output/invert_maxent_pbi_ala/results/'
inv_fire_dir         <- './output/invert_maxent_pbi_ala/Habitat_suitability/FESM_SDM_intersect/'



## Try and set the raster temp directory to a location not on the partition, to save space
rasterOptions(tmpdir = tempdir)
terraOptions(memfrac = 0.5, 
             tempdir = tempdir) 






## 1). COLLATE % BURNT FOR ALL TAXA =============================================================


## get target taxa
data('insect_data_families')
data('all.insect.families')
data('all.insect.genera')
data('all.insect.spp')

data('target.insect.spp')
data('target.insect.genera')
data('target.insect.families')
data('target.host.plants')
data('all.insect.plant.spp')


analysis_taxa <- str_trim(c(target.insect.spp, 
                            target.insect.genera, 
                            target.insect.families)) %>% unique()


## Update the  
INVERT.MAXENT.RESULTS <- compile_sdm_results(taxa_list    = analysis_taxa,
                                             results_dir  = inv_back_dir,
                                             data_path    = inv_results_dir,
                                             sdm_path     = inv_back_dir,
                                             save_data    = TRUE,
                                             save_run     = 'INVERT_ALL_TAXA_ALA_PBI')



## Get the list of files
INVERT.FESM.list    <- list.files('./output/invert_maxent_pbi_ala/Habitat_suitability/FESM_SDM_intersect/', 
                                  pattern     = '_SDM_intersect_Fire.csv', 
                                  full.names  = TRUE, 
                                  recursive   = TRUE) 

INVERT.FESM.VEG.list <- list.files('./output/invert_maxent_pbi_ala/Habitat_suitability/FESM_SDM_intersect/', 
                                   pattern     = 'VEG_intersect_Fire.csv', 
                                   full.names  = TRUE, 
                                   recursive   = TRUE) 


## Now combine the SUA tables for each species into one table 
INVERT.FESM.TABLE <- INVERT.FESM.list %>%
  
  ## pipe the list into lapply
  lapply(function(x) {
    
    ## create the character string
    f <- paste0(x)
    
    ## load each .RData file
    d <- read.csv(f)
    d
    
  }) %>%
  
  ## bind together
  bind_rows()


## Now combine the SUA tables for each species into one table 
INVERT.FESM.VEG.TABLE <- INVERT.FESM.VEG.list %>%
  
  ## pipe the list into lapply
  lapply(function(x) {
    
    ## create the character string
    f <- paste0(x)
    
    ## load each .RData file
    d <- read.csv(f)
    d
    
  }) %>%
  
  ## bind together
  bind_rows()




## Subset to just the analysis species - some species did not process properly?
INVERT.FESM.TABLE <-  INVERT.FESM.TABLE[INVERT.FESM.TABLE$Taxa %in%
                                          sort(unique(INVERT.MAXENT.RESULTS$searchTaxon)) , ] %>%
  .[complete.cases(.), ]


INVERT.FESM.VEG.TABLE <-  INVERT.FESM.TABLE[INVERT.FESM.VEG.TABLE$Taxa %in%
                                               sort(unique(INVERT.MAXENT.RESULTS$searchTaxon)) , ] %>%
  .[complete.cases(.), ]


## How many taxa have been processed?
length(unique(INVERT.FESM.TABLE$Taxa))
View(INVERT.FESM.TABLE)
View(INVERT.FESM.VEG.TABLE)



## Also save a big table of just the background taxa
INVERT.FESM.TABLE.FAM <- INVERT.FESM.TABLE %>% .[.$Taxa %in% target.insect.families, ]
INVERT.FESM.TABLE.GEN <- INVERT.FESM.TABLE %>% .[.$Taxa %in% target.insect.genera, ]
INVERT.FESM.TABLE.SPP <- INVERT.FESM.TABLE %>% .[.$Taxa %in% target.insect.spp, ]

INVERT.FESM.VEG.TABLE.FAM <- INVERT.FESM.VEG.TABLE %>% .[.$Taxa %in% target.insect.families, ]
INVERT.FESM.VEG.TABLE.GEN <- INVERT.FESM.VEG.TABLE %>% .[.$Taxa %in% target.insect.genera, ]
INVERT.FESM.VEG.TABLE.SPP <- INVERT.FESM.VEG.TABLE %>% .[.$Taxa %in% target.insect.spp, ]



## Save the FESM intersect results to file
write_csv(INVERT.FESM.TABLE,
          paste0(inv_results_dir, '/INVERT_TAXA_SDM_intersect_Fire_ALA_PBI.csv'))

write_csv(INVERT.FESM.VEG.TABLE,
          paste0(inv_results_dir, '/INVERT_TAXA_SDM_VEG_intersect_Fire_ALA_PBI.csv'))





## 2). SAVE TAXA TO DATABASE =============================================================


## Read in the geopackage, so we can save the results
SDM_ALL_INVERT_TAXA_ALA_PBI    <- paste0(inv_results_dir, 'SDM_ALL_INVERT_TAXA_ALA_PBI.gpkg')
ALL_INVERT_TAXA_ALA_PBI_NICHES <- read_csv(paste0(inv_results_dir, 'GLOBAL_NICHES_ALL_INVERT_TAXA_ALA_PBI.csv'))


st_write(ALL_INVERT_TAXA_ALA_PBI_NICHES, 
         
         dsn    = SDM_ALL_INVERT_TAXA_ALA_PBI, 
         layer  = 'INVERT_TAXA_NICHES_ALA_PBI',
         
         quiet  = TRUE,
         append = TRUE)


st_write(INVERT.MAXENT.RESULTS, 
         
         dsn    = SDM_ALL_INVERT_TAXA_ALA_PBI, 
         layer  = 'INVERT_TAXA_MAXENT_RESULTS_ALA_PBI',
         
         quiet  = TRUE,
         append = TRUE)


st_write(INVERT.FESM.TABLE, 
         
         dsn    = SDM_ALL_INVERT_TAXA_ALA_PBI, 
         layer  = 'INVERT_TAXA_SDM_intersect_Fire_ALA_PBI',
         
         quiet  = TRUE,
         append = TRUE)


st_write(INVERT.FESM.VEG.TABLE, 
         
         dsn    = SDM_ALL_INVERT_TAXA_ALA_PBI, 
         layer  = 'INVERT_TAXA_SDM_intersect_Veg_Fire_ALA_PBI',
         
         quiet  = TRUE,
         append = FALSE)



## Check database has the new layers
st_layers(dsn = SDM_ALL_INVERT_TAXA_ALA_PBI)$name






## 2). CREATE HABITAT LOSS GRAPHS =============================================================


## Species % burnt graphs ----

for(taxa in INVERT.FESM.TABLE$Taxa) {
  
  ## taxa = INVERT.FESM.TABLE$Taxa[1]
  bar_df  <- INVERT.FESM.TABLE %>% filter(Taxa == taxa) %>% 
    mutate(Percent_unburnt = 100 - Percent_burnt) 
  
  plot <- ggplot(bar_df, aes(fill=Percent_burnt, y=Percent_burnt, x=Taxa)) + 
    geom_bar(position="stack", stat="identity")
  
  print(plot)
  
}


## Graphs for each species ----

## Graphs of the % burnt and un-burnt
for(taxa in INVERT.FESM.TABLE$Taxa) {
  
  ## taxa = INVERT.FESM.TABLE$Taxa[1]
  bar_df  <- INVERT.FESM.TABLE %>% filter(Taxa == taxa) %>% 
    mutate(Percent_unburnt = 100 - Percent_burnt) 
  
  plot <- ggplot(bar_df, aes(fill=Percent_burnt, y=Percent_burnt, x=Taxa)) + 
    geom_bar(position="stack", stat="identity")
  
  print(plot)
  
}


## Graphs of the % burnt in each Vegetation type
for(taxa in unique(INVERT.FESM.VEG.TABLE$Taxa)) {
  
  ## Update the table
  # taxa <- unique(INVERT.FESM.VEG.TABLE$Taxa)[10]
  message('writing sdm fire vegetation for ', taxa)
  
  save_name     <- gsub(' ', '_', taxa)
  bar_df        <- INVERT.FESM.VEG.TABLE %>% filter(Taxa == taxa) %>% 
    mutate(Habitat_Veg_burnt_perc = round(Habitat_Veg_burnt_perc, 2))
  bar_df$Vegetation <- factor(bar_df$Vegetation, 
                              levels = bar_df$Vegetation[order(bar_df$Habitat_Veg_burnt_perc, decreasing = TRUE)])
  
  
  overall_burnt <- INVERT.FESM.TABLE %>% filter(Taxa == taxa) %>% .$Percent_burn %>% round(., 2)
  
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
    labs(caption = paste0(overall_burnt, ' % Burnt Overall')) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size  = xsize),
          axis.title.x    = element_text(size  = xsize, face = "bold"),
          legend.position = 'none',
          
          axis.title.y    = element_text(size = ysize, face = "bold"),
          axis.text.y     = element_text(size = ysize, color = "black"),
          plot.subtitle   = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"),
          plot.caption    = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"))
  
  png(paste0(inv_fire_dir, save_name, '_SDM_VEG_intersect_Fire_Barplots.png'),
      12, 6, units = 'in', res = 500)
  plot(sdm_fire_veg_plot)
  dev.off()
  gc()
  
}


## Graphs of the % burnt in each Burn category 
for(taxa in unique(INVERT.FESM.VEG.TABLE$Taxa)) {
  
  ## Update the table
  # taxa <- unique(INVERT.FESM.VEG.TABLE$Taxa)[10]
  message('writing sdm fire vegetation for ', taxa)
  
  save_name     <- gsub(' ', '_', taxa)
  bar_df        <- INVERT.FESM.VEG.TABLE %>% filter(Taxa == taxa) %>% 
    mutate(Habitat_Veg_burnt_perc = round(Habitat_Veg_burnt_perc, 2))
  bar_df$Vegetation <- factor(bar_df$Vegetation, 
                              levels = bar_df$Vegetation[order(bar_df$Habitat_Veg_burnt_perc, decreasing = TRUE)])
  
  
  overall_burnt <- INVERT.FESM.TABLE %>% filter(Taxa == taxa) %>% .$Percent_burn %>% round(., 2)
  
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
    labs(caption = paste0(overall_burnt, ' % Burnt Overall')) +
    
    ylim(c(ymin, ymax)) +
    
    theme(plot.margin     = unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold"),
          axis.text.x     = element_text(size  = xsize),
          axis.title.x    = element_text(size  = xsize, face = "bold"),
          legend.position = 'none',
          
          axis.title.y    = element_text(size = ysize, face = "bold"),
          axis.text.y     = element_text(size = ysize, color = "black"),
          plot.subtitle   = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"),
          plot.caption    = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"))
  
  png(paste0(inv_fire_dir, save_name, '_SDM_VEG_intersect_Fire_Barplots.png'),
      12, 6, units = 'in', res = 500)
  plot(sdm_fire_veg_plot)
  dev.off()
  gc()
  
}


## Graphs across all species ----

## Group by burn category, Veg type and Invert type
INVERT.TAXA.FESM.VEG.GROUP <- INVERT.FESM.VEG.TABLE %>% 
  
  ## group by Vegetation
  group_by(Vegetation) %>% 
  summarise(Average_burnt_area    = mean(Habitat_Veg_burnt_area),
            Average_burnt_percent = mean(Habitat_Veg_burnt_perc)) %>% 
  arrange(-Average_burnt_percent)


INVERT.FAM.FESM.VEG.GROUP <- INVERT.FESM.VEG.TABLE.FAM %>% 
  
  ## group by Vegetation
  group_by(Vegetation) %>% 
  summarise(Average_burnt_area    = mean(Habitat_Veg_burnt_area),
            Average_burnt_percent = mean(Habitat_Veg_burnt_perc)) %>% 
  arrange(-Average_burnt_percent)


INVERT.GEN.FESM.VEG.GROUP <- INVERT.FESM.VEG.TABLE.GEN %>% 
  
  ## group by Vegetation
  group_by(Vegetation) %>% 
  summarise(Average_burnt_area    = mean(Habitat_Veg_burnt_area),
            Average_burnt_percent = mean(Habitat_Veg_burnt_perc)) %>% 
  arrange(-Average_burnt_percent)


INVERT.GEN.FESM.VEG.GROUP <- INVERT.FESM.VEG.TABLE.SPP %>% 
  
  ## group by Vegetation
  group_by(Vegetation) %>% 
  summarise(Average_burnt_area    = mean(Habitat_Veg_burnt_area),
            Average_burnt_percent = mean(Habitat_Veg_burnt_perc)) %>% 
  arrange(-Average_burnt_percent)



## END =============================================================
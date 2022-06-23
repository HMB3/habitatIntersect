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
inv_rs_dir           <- './output/invert_maxent_pbi_ala_site/'
inv_back_dir         <- './output/invert_maxent_pbi_ala_site/back_sel_models/'
inv_results_dir      <- './output/invert_maxent_pbi_ala_site/results/'
inv_fire_dir         <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/FESM_SDM_intersect/'





## 1). TAXA LISTS =============================================================


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


## Full list of analysis taxa -
analysis_taxa   <- str_trim(c(target.insect.spp, 
                              target.insect.genera, 
                              target.insect.families)) %>% unique()

site_cols <- c("genus", 
               "species", 
               "family",
               "Host_Genus",
               "Host_species",
               "plantTaxon",
               "lat", 
               "lon", 
               "country", 
               "state",
               "locality",
               "institutionCode", 
               "basisOfRecord")


taxa_qc    <- read_excel(paste0(inv_results_dir, 'SDM_target_species.xlsx'),
                         sheet = 'Invert_QA_check')

sdm_taxa   <- read_excel(paste0(inv_results_dir, 'INVERTS_FIRE_SPATIAL_DATA_LUT_JUNE_2022.xlsm'),
                         sheet = 'Missing_taxa')

invert_LUT <- read_excel(paste0(inv_results_dir, 'INVERTS_FIRE_SPATIAL_DATA_LUT_JUNE_2022.xlsm'),
                         sheet = 'TABLE 3') %>% dplyr::select("searchTaxon",       
                                                              "Aus_records",       
                                                              "AOO",               
                                                              "EOO",               
                                                              "KOP_count",         
                                                              "Type") %>% rename(Taxa = searchTaxon)

table(invert_LUT$Type)

species_remain <- taxa_qc %>% 
  filter(grepl("Missing", Note)) %>%
  filter(is.na(Morpho)) %>%
  dplyr::select(Binomial) %>% 
  .$Binomial %>% sort()


taxa_remain <- sdm_taxa %>% 
  filter(Size == 0) %>%
  dplyr::select(Taxa) %>% 
  .$Taxa %>% sort() %>% gsub('_', ' ', .,)


taxa_done <- sdm_taxa %>% 
  filter(Size > 0) %>%
  dplyr::select(Taxa) %>% 
  .$Taxa %>% sort()


taxa_difference <- c(taxa_remain, species_remain) %>% unique() %>% sort()
intersect(analysis_taxa, taxa_difference) %>% sort()





## 2). TAXA NICHES AND SDM RESULTS =============================================================


## Combine Niches
ALL_INVERT_TAXA_ALA_PBI_NICHES   <- read_csv(paste0(inv_results_dir, 'GLOBAL_NICHES_ALL_INVERT_TAXA_ALA_PBI.csv'))
ALL_INVERT_TAXA_ALA_SITE_NICHES  <- read_csv(paste0(inv_results_dir, 'GLOBAL_NICHES_ALL_INVERT_TAXA_ALA_PBI_SITES.csv'))
ALL_INVERT_TAXA_ALA_SPID_NICHES  <- read_csv(paste0(inv_results_dir, 'GLOBAL_NICHES_ALL_SPIDER_TAXA_ALA_PBI_SITES.csv'))
ALL_INVERT_TAXA_ALA_DIFF_NICHES  <- read_csv(paste0(inv_results_dir, 'GLOBAL_NICHES_DIFFERENCE_INVERT_TAXA_ALA_PBI.csv'))


ALL_INVERT_TAXA_ALA_PBI_NICHES_UNI  <- ALL_INVERT_TAXA_ALA_PBI_NICHES %>% 
  .[.$searchTaxon %in% 
      setdiff(ALL_INVERT_TAXA_ALA_PBI_NICHES$searchTaxon, 
              ALL_INVERT_TAXA_ALA_SITE_NICHES$searchTaxon), ]


ALL_INVERT_TAXA_ALA_SITE_NICHES_UNI <- ALL_INVERT_TAXA_ALA_SITE_NICHES %>% 
  .[.$searchTaxon %in% 
      setdiff(ALL_INVERT_TAXA_ALA_SITE_NICHES$searchTaxon, 
              ALL_INVERT_TAXA_ALA_PBI_NICHES_UNI$searchTaxon), ]


ALL_INVERT_TAXA_ALL_NICHES_UNI      <- bind_rows(ALL_INVERT_TAXA_ALA_PBI_NICHES_UNI, 
                                                 ALL_INVERT_TAXA_ALA_SITE_NICHES_UNI)


ALL_INVERT_TAXA_ALA_SPID_NICHES_UNI <- ALL_INVERT_TAXA_ALA_SPID_NICHES %>% 
  
  .[.$searchTaxon %in% 
      setdiff(ALL_INVERT_TAXA_ALA_SPID_NICHES$searchTaxon, 
              ALL_INVERT_TAXA_ALL_NICHES_UNI$searchTaxon), ]


ALL_INVERT_TAXA_ALL_NICHES_UNI      <- bind_rows(ALL_INVERT_TAXA_ALL_NICHES_UNI,
                                                 ALL_INVERT_TAXA_ALA_SPID_NICHES_UNI,
                                                 ALL_INVERT_TAXA_ALA_DIFF_NICHES) 


write_csv(ALL_INVERT_TAXA_ALL_NICHES_UNI,
          paste0(inv_results_dir, '/AUS_INVERT_TAXA_ALL_NICHES.csv'))


## Update the  
SITES.MAXENT.RESULTS    <- compile_sdm_results(taxa_list    = analysis_taxa,
                                               results_dir  = inv_back_dir,
                                               data_path    = inv_results_dir,
                                               sdm_path     = inv_back_dir,
                                               save_data    = FALSE,
                                               save_run     = 'INVERT_ALL_TAXA_ALA_PBI_SITES')


SPID.MAXENT.RESULTS     <- compile_sdm_results(taxa_list    = taxa_difference,
                                               results_dir  = inv_back_dir,
                                               data_path    = inv_results_dir,
                                               sdm_path     = inv_back_dir,
                                               save_data    = FALSE,
                                               save_run     = 'INVERT_SPIDER_TAXA_ALA_PBI_SITES')


SITES.ALL.MAXENT.RESULTS <- bind_rows(SITES.MAXENT.RESULTS, 
                                      SPID.MAXENT.RESULTS %>% 
                                        
                                        .[.$searchTaxon %in% 
                                            setdiff(SPID.MAXENT.RESULTS$searchTaxon, 
                                                    SITES.MAXENT.RESULTS$searchTaxon), ])


INVERT.MAXENT.FAM.RESULTS <- compile_sdm_results(taxa_list    = target.insect.families,
                                                 results_dir  = inv_back_dir,
                                                 data_path    = inv_habitat_dir,
                                                 sdm_path     = inv_back_dir,
                                                 save_data    = FALSE,
                                                 save_run     = 'INVERT_ANALYSIS_TAXA')


INVERT.MAXENT.GEN.RESULTS <- compile_sdm_results(taxa_list    = target.insect.genera,
                                                 results_dir  = inv_back_dir,
                                                 data_path    = inv_habitat_dir,
                                                 sdm_path     = inv_back_dir,
                                                 save_data    = FALSE,
                                                 save_run     = 'INVERT_ANALYSIS_TAXA')


INVERT.MAXENT.SPP.RESULTS <- compile_sdm_results(taxa_list    = target.insect.spp,
                                                 results_dir  = inv_back_dir,
                                                 data_path    = habitat_dir,
                                                 sdm_path     = inv_back_dir,
                                                 save_data    = FALSE,
                                                 save_run     = 'INVERT_ANALYSIS_TAXA')


write_csv(SITES.ALL.MAXENT.RESULTS,
          paste0(inv_results_dir, '/MAXENT_RESULTS_INSECTS_SPID_TAXA_ALA_PBI_SITES.csv'))




## 3). COMBINE TABLES =============================================================


## Get the list of files
INVERT.FESM.list    <- list.files(inv_fire_dir, 
                                  pattern     = '_SDM_intersect_Fire.csv', 
                                  full.names  = TRUE, 
                                  recursive   = TRUE) 


INVERT.FESM.VEG.list <- list.files(inv_fire_dir, 
                                   pattern     = 'VEG_intersect_Fire.csv', 
                                   full.names  = TRUE, 
                                   recursive   = TRUE)


INVERT.FESM.CLASS.list <- list.files(inv_fire_dir, 
                                     pattern     = 'VEG_intersect_Fire_Classes.csv', 
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



INVERT.FESM.CLASS.TABLE <- INVERT.FESM.CLASS.list %>%
  
  ## pipe the list into lapply
  lapply(function(x) {
    
    ## create the character string
    f <- paste0(x)
    
    ## load each .RData file
    d <- read.csv(f)
    if(nrow(d) < 1)       {
      sdm_fire_class <- data.frame(matrix(NA, 
                                          ncol = 4, 
                                          nrow = 1))
      
      
      colnames(sdm_fire_class) <- c('Taxa', 
                                    'Burn_Category', 
                                    'Burn_Category_burnt_area',  
                                    'Burn_Category_burnt_perc')
      
      sdm_fire_class$Taxa                     <- 'None'
      sdm_fire_class$Burn_Category            <- 'None'
      sdm_fire_class$Burn_Category_burnt_area <- 0
      sdm_fire_class$Burn_Category_burnt_perc <- 0
      d <- sdm_fire_class
    }
    d
    
  }) %>%
  
  ## bind together
  bind_rows() %>% filter(Burn_Category_burnt_area > 0)




## Subset to just the analysis species - some species did not process properly?
INVERT.FESM.TABLE <-  INVERT.FESM.TABLE[INVERT.FESM.TABLE$Taxa %in%
                                          sort(unique(SITES.ALL.MAXENT.RESULTS$searchTaxon)) , ] %>%
  .[complete.cases(.), ] %>% 
  
  mutate(Habitat_km2       = round(Habitat_km2, 2),
         Habitat_burnt_km2 = round(Habitat_burnt_km2, 2),
         Percent_burnt     = round(Percent_burnt, 2)) %>% 
  
  left_join(., invert_LUT %>% select(Taxa, Type), by = "Taxa")


INVERT.FESM.VEG.TABLE <-  INVERT.FESM.VEG.TABLE[INVERT.FESM.VEG.TABLE$Taxa %in%
                                                  sort(unique(SITES.ALL.MAXENT.RESULTS$searchTaxon)) , ] %>%
  .[complete.cases(.), ] %>% 
  
  mutate(Habitat_Veg_burnt_area = round(Habitat_Veg_burnt_area, 2),
         Habitat_Veg_burnt_perc = round(Habitat_Veg_burnt_perc, 2)) %>% 
  
  left_join(., invert_LUT %>% select(Taxa, Type), by = "Taxa")


INVERT.FESM.CLASS.TABLE <-  INVERT.FESM.CLASS.TABLE[INVERT.FESM.CLASS.TABLE$Taxa %in%
                                                      sort(unique(SITES.ALL.MAXENT.RESULTS$searchTaxon)) , ] %>%
  .[complete.cases(.), ] %>% 
  
  mutate(Burn_Category_burnt_area = round(Burn_Category_burnt_area, 2),
         Burn_Category_burnt_perc = round(Burn_Category_burnt_perc, 2)) %>% 
  
  left_join(., invert_LUT %>% select(Taxa, Type), by = "Taxa")



## How many taxa have been processed?
length(unique(INVERT.FESM.TABLE$Taxa))
View(INVERT.FESM.TABLE)
View(INVERT.FESM.VEG.TABLE)
View(INVERT.FESM.CLASS.TABLE)



## Also save a big table of just the background taxa
INVERT.FESM.TABLE.FAM <- INVERT.FESM.TABLE %>% .[.$Taxa %in% target.insect.families, ]
INVERT.FESM.TABLE.GEN <- INVERT.FESM.TABLE %>% .[.$Taxa %in% target.insect.genera, ]
INVERT.FESM.TABLE.SPP <- INVERT.FESM.TABLE %>% .[.$Taxa %in% target.insect.spp, ]


INVERT.FESM.VEG.TABLE.FAM <- INVERT.FESM.VEG.TABLE %>% .[.$Taxa %in% target.insect.families, ]
INVERT.FESM.VEG.TABLE.GEN <- INVERT.FESM.VEG.TABLE %>% .[.$Taxa %in% target.insect.genera, ]
INVERT.FESM.VEG.TABLE.SPP <- INVERT.FESM.VEG.TABLE %>% .[.$Taxa %in% target.insect.spp, ]





## 4). SAVE TABLES =============================================================


## Save to CSV ----
write_csv(INVERT.FESM.TABLE,
          paste0(inv_results_dir, '/INVERT_TAXA_SDM_intersect_Fire_ALA_PBI_SITE.csv'))

write_csv(INVERT.FESM.TABLE.FAM,
          paste0(inv_results_dir, '/INVERT_FAM_SDM_intersect_Fire_ALA_PBI_SITE.csv'))

write_csv(INVERT.FESM.TABLE.GEN,
          paste0(inv_results_dir, '/INVERT_GEN_SDM_intersect_Fire_ALA_PBI_SITE.csv'))

write_csv(INVERT.FESM.TABLE.SPP,
          paste0(inv_results_dir, '/INVERT_SPP_SDM_intersect_Fire_ALA_PBI_SITE.csv'))


write_csv(INVERT.FESM.VEG.TABLE,
          paste0(inv_results_dir, '/INVERT_TAXA_SDM_VEG_intersect_Fire_ALA_PBI_SITE.csv'))

write_csv(INVERT.FESM.VEG.TABLE.FAM,
          paste0(inv_results_dir, '/INVERT_FAM_SDM_VEG_intersect_Fire_ALA_PBI_SITE.csv'))

write_csv(INVERT.FESM.VEG.TABLE.GEN,
          paste0(inv_results_dir, '/INVERT_GEN_SDM_VEG_intersect_Fire_ALA_PBI_SITE.csv'))

write_csv(INVERT.FESM.VEG.TABLE.SPP,
          paste0(inv_results_dir, '/INVERT_SPP_SDM_VEG_intersect_Fire_ALA_PBI_SITE.csv'))



## Save to geo-package ----
SDM_ALL_INVERT_TAXA_ALA_PBI    <- paste0(inv_results_dir, 'SDM_ALL_INVERT_TAXA_ALA_PBI.gpkg')


st_write(ALL_INVERT_TAXA_ALA_PBI_NICHES, 
         
         dsn    = SDM_ALL_INVERT_TAXA_ALA_PBI, 
         layer  = 'INVERT_TAXA_NICHES_ALA_PBI',
         
         quiet  = TRUE,
         append = TRUE)


st_write(SITES.ALL.MAXENT.RESULTS, 
         
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


# png(paste0(inv_fire_dir, 'fesm_inv_family_scatter_plots.png'),
#     12, 8, units = 'in', res = 500)
# 
# psych::pairs.panels(INVERT.FESM.TABLE.FAM.RANGE,
#                     method   = "pearson", # correlation method
#                     hist.col = "#00AFBB",
#                     density  = TRUE,      # show density plots
#                     ellipses = FALSE,
#                     cex = 1.2,
#                     cex.labels = 1.2,
#                     lwd = 2,
#                     col = "blue")
# 
# dev.off()
# gc()


# png(paste0(inv_fire_dir, 'fesm_inv_genus_scatter_plots.png'),
#     12, 8, units = 'in', res = 500)
# 
# psych::pairs.panels(INVERT.FESM.TABLE.GEN.RANGE,
#                     method   = "pearson", # correlation method
#                     hist.col = "#00AFBB",
#                     density  = TRUE,      # show density plots
#                     ellipses = FALSE,
#                     cex = 1.2,
#                     cex.labels = 1.2,
#                     lwd = 2,
#                     col = "blue")
# 
# dev.off()
# gc()


png(paste0(inv_fire_dir, 'fesm_inv_species_scatter_plots.png'),
    12, 8, units = 'in', res = 500)

psych::pairs.panels(INVERT.FESM.TABLE.SPP.RANGE,
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
xsize     = 20
ysize     = 20
ycol      = 'black'
lab_size  = 8

ymin      = 0 
axis_multiplier = 0.2
ylab  = '\nPercent (%)\n'
xlab  = ''


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
for(taxa in unique(INVERT.FESM.CLASS.TABLE$Taxa)) {
  
  ## Update the table
  # taxa <- unique(INVERT.FESM.CLASS.TABLE$Taxa)[10]
  message('writing sdm fire vegetation for ', taxa)
  
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
    
    theme(plot.margin     = unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
          plot.title      = element_text(vjust = 5, size = tsize, face = "bold.italic"),
          axis.text.x     = element_text(size  = xsize),
          axis.title.x    = element_text(size  = xsize, face = "bold"),
          legend.position = 'none',
          
          axis.title.y    = element_text(size = ysize, face  = "bold"),
          axis.text.y     = element_text(size = ysize, color = "black"),
          plot.subtitle   = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"),
          plot.caption    = element_text(size = capt_size, hjust = 0.5, face = "italic", color="black"))
  
  png(paste0(inv_fire_dir, save_name, '_SDM_Fire_Classes_Barplots.png'),
      12, 6, units = 'in', res = 500)
  plot(sdm_fire_class_plot)
  dev.off()
  gc()
  
}









## 6). ALL TAXA GRAPHS =============================================================


## Graphs across all species ----

## Group by burn category, Veg type and Invert type
## Thes will all be the same
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
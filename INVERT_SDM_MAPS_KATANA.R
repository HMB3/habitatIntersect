

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
tempdir              <- './TEMP/'
ALA_dir              <- './data/ALA/Insects/'
check_dir            <- './data/ALA/Insects/check_plots/'
back_dir             <- './output/invert_maxent_raster_update/back_sel_models'
full_dir             <- './output/invert_maxent_raster_update/full_models'
results_dir          <- './output/invert_maxent_raster_update/results/'
plants_dir           <- './output/plant_maxent_raster_update/results/'
veg_dir              <- './data/Remote_sensing/Veg_data/Forest_cover/'
habitat_dir          <- './output/invert_maxent_raster_update/Habitat_suitability/'
intersect_dir        <- './output/invert_maxent_raster_update/Habitat_suitability/SVTM_intersect/'
threshold_dir        <- './output/invert_maxent_raster_update/Habitat_suitability/SDM_thresholds/'
plants_threshold_dir <- './output/plant_maxent_raster_update/Habitat_suitability/SDM_thresholds/'
intersect_dir        <- './output/invert_maxent_raster_update/Habitat_suitability/FESM_SDM_intersect/'


dir_lists   <- c(ALA_dir,  tempdir, check_dir,   back_dir,  habitat_dir, intersect_dir,
                 full_dir, results_dir, habitat_dir, veg_dir,
                 threshold_dir, intersect_dir)


## Create the folders if they don't exist
for(dir in dir_lists) {
  if(!dir.exists(dir)) {
    message('Creating ', dir, ' directory')
    dir.create(dir) 
  } else {
    message(dir, ' directory already exists')}
}



## Try and set the raster temp directory to a location not on the partition, to save space
rasterOptions(tmpdir = tempdir)
terraOptions(memfrac = 0.5, 
             tempdir = tempdir) 





## 1). LOAD VEGETATION RASTER DATA =============================================================

# \
# 
# Load the vegetation rasters at 280m.
# 
# \



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


analysis_taxa <- str_trim(c(target.insect.spp, target.insect.genera, target.insect.families)) %>% unique()


## Read in the SDM data
## This function aggregates the results for models that ran successfully
INVERT.MAXENT.RESULTS     <- compile_sdm_results(taxa_list    = analysis_taxa,
                                                 results_dir  = back_dir,
                                                 data_path    = habitat_dir,
                                                 sdm_path     = back_dir,
                                                 save_data    = FALSE,
                                                 save_run     = 'INVERT_ANALYSIS_TAXA')


INVERT.MAXENT.FAM.RESULTS <- compile_sdm_results(taxa_list    = target.insect.families,
                                                 results_dir  = back_dir,
                                                 data_path    = habitat_dir,
                                                 sdm_path     = back_dir,
                                                 save_data    = FALSE,
                                                 save_run     = 'INVERT_ANALYSIS_TAXA')


INVERT.MAXENT.GEN.RESULTS <- compile_sdm_results(taxa_list    = target.insect.genera,
                                                 results_dir  = back_dir,
                                                 data_path    = habitat_dir,
                                                 sdm_path     = back_dir,
                                                 save_data    = FALSE,
                                                 save_run     = 'INVERT_ANALYSIS_TAXA')


INVERT.MAXENT.SPP.RESULTS <- compile_sdm_results(taxa_list    = target.insect.spp,
                                                 results_dir  = back_dir,
                                                 data_path    = habitat_dir,
                                                 sdm_path     = back_dir,
                                                 save_data    = FALSE,
                                                 save_run     = 'INVERT_ANALYSIS_TAXA')


PLANT.MAXENT.RESULTS      <- compile_sdm_results(taxa_list    = target.host.plants,
                                                 results_dir  = './output/plant_maxent_raster_update/back_sel_models',
                                                 data_path    = './output/plant_maxent_raster_update/Habitat_suitability/',
                                                 sdm_path     = './output/plant_maxent_raster_update/back_sel_models/',
                                                 save_data    = FALSE,
                                                 save_run     = 'INVERT_ANALYSIS_TAXA')


## How many target taxa were modelled?
nrow(INVERT.MAXENT.SPP.RESULTS)/length(target.insect.spp)      *100 
nrow(INVERT.MAXENT.GEN.RESULTS)/length(target.insect.genera)   *100
nrow(INVERT.MAXENT.FAM.RESULTS)/length(target.insect.families) *100


## Get map_taxa from the maxent results table above, change the species column,
## then create a list of logistic thresholds
invert_map_taxa <- INVERT.MAXENT.RESULTS$searchTaxon %>% gsub(" ", "_", .,)
invert_map_spp  <- INVERT.MAXENT.SPP.RESULTS$searchTaxon %>% gsub(" ", "_", .,)
plant_map_taxa  <- PLANT.MAXENT.RESULTS$searchTaxon  %>% gsub(" ", "_", .,)


## SDM output, re-sampled to 100m
study_sdm_binary <- stack(
  list.files(threshold_dir,
             'current_suit_not_novel_above', full.names = TRUE))


sdm_threshold_features <- list.files(path       = threshold_dir,
                                     pattern    = '_current_suit_not_novel_above_', 
                                     recursive  = FALSE,
                                     full.names = FALSE) %>% 
  
  .[grep(".tif", .)] %>% gsub('.tif', '', .)

sdm_threshold_list        <- sdm_threshold_features %>% as.list() 
names(sdm_threshold_list) <- sdm_threshold_features


## FESM   : https://datasets.seed.nsw.gov.au/dataset/fire-extent-and-severity-mapping-fesm
## VALUES : 1-4, burn intensity from 2019-2020 fires, originally @ 10m resolution, re-sampled to 100m
template_raster_250m <- raster('./data/Bushfire_indices/R_outputs/250m/AUS/Extra/Annual_precip_GDA_ALB.tif')
FESM_NSW_10m         <- raster('./data/Remote_sensing/FESM/fesm_20200319_albers.tif')
FESM_AUS_20m         <- raster('./data/Remote_sensing/FESM/NBR_Burn_severity_classed_ALB.tif')


## Read in feature layers for fire that have been repaired in ArcMap
FESM_east_20m <- st_read('./data/Remote_sensing/FESM/Fire_perimeters_for_forests_and_woodlands.shp') %>% 
  st_transform(., st_crs(3577)) %>% filter(!st_is_empty(.)) %>% 
  as_Spatial() %>% repair_geometry() 


## NIAFED data is much coarser and has more empty geometries
Burnt_unburnt <- 
  
  st_read('./data/Remote_sensing/NIAFED/NIAFED_combo_east_Alb.shp') %>%
  st_transform(., st_crs(3577)) %>% 
  filter(!st_is_empty(.)) 


## Read in the SDM data, to intersect with the Veg layers
# SVTM_Veg_Class_GDA          = readRDS('./data/Remote_sensing/aligned_rasters/SVTM_Veg_Class_GDA.rds')
AUS_forest_RS_ras           = raster(paste0(veg_dir, 'alpsbk_aust_y2009_sf1a2_forest.tif'))
AUS_forest_RS_feat          = st_read(paste0(veg_dir, 'Aus_forest_cover_east_coast_classes.shp')) %>% 
  st_transform(., st_crs(3577))


## Read in the reptile points
SDM.SPAT.OCC.BG.GDA = readRDS(paste0(results_dir, 'SDM_SPAT_OCC_BG_ALL_TARGET_INSECT_TAXA.rds'))


## Check projections and resolutions
projection(FESM_NSW_10m);projection(study_sdm_binary[[1]]);projection(SDM.SPAT.OCC.BG.GDA)
raster::xres(FESM_AUS_20m);raster::xres(study_sdm_binary[[1]])



## 2). INTERSECT SDMs WITH VEG RASTER =============================================================

# \
# 
# To use the habitat suitability rasters in area calculations (e.g. comparing the area of suitable habitat
# affected by fire), we need to convert the continuous suitability scores (ranging from 0-1) to binary values
# (either 1, or 0). To do this, we need to pick a threshold of habitat suitability, below which the species 
# is not considered present. Here we have chosen the 10th% Logistic threshold for each taxa (ref).
# 
# \



## Select the Vegetation pixels that intersect with the records of each invertebrate species
taxa_records_habitat_features_intersect(analysis_df    = SDM.SPAT.OCC.BG.GDA,
                                        taxa_list      = target.insect.spp,
                                        taxa_level     = 'species',
                                        habitat_poly   = AUS_forest_RS_feat,
                                        output_path    = intersect_dir,
                                        buffer         = 5000,
                                        poly_path      = 'data/Spatial_data/Study_areas/AUS_2016_AUST.shp',
                                        epsg           = 3577)


## Select the Vegetation pixels that intersect with the records of each invertebrate genus 
taxa_records_habitat_features_intersect(analysis_df    = SDM.SPAT.OCC.BG.GDA,
                                        taxa_list      = target.insect.genera,
                                        taxa_level     = 'genus',
                                        habitat_poly   = SVTM_Veg_Class_GDA,
                                        output_path    = intersect_dir,
                                        buffer         = 5000)


## Select the Vegetation pixels that intersect with the records of each invertebrate family
taxa_records_habitat_features_intersect(analysis_df    = SDM.SPAT.OCC.BG.GDA,
                                        taxa_list      = target.insect.families,
                                        taxa_level     = 'family',
                                        habitat_poly   = SVTM_Veg_Class_GDA,
                                        output_path    = intersect_dir,
                                        buffer         = 5000)





## 3). ESTIMATE % BURNT FOR EACH TAXA =============================================================


# \
# 
# To use the habitat suitability rasters in area calculations (e.g. comparing the area of suitable habitat
# affected by fire), we need to convert the continuous suitability scores (ranging from 0-1) to binary values
# (either 1, or 0). To do this, we need to pick a threshold of habitat suitability, below which the species 
# is not considered present. Here we have chosen the 10th% Logistic threshold for each taxa (ref).
# 
# \



## Add Host Plants to the Maxent LUT 
## Read in the host plant species
host_plants <- read_excel('./output/invert_maxent_raster_update/Habitat_suitability/NENSW_INVERTEBRATES_SPATIAL_DATA_LUT_SEP2021.xlsm',
                          sheet = 'NENSW_INV_TAXA_ASSOCIATIONS') %>% filter(Target_taxon == "Yes") %>%
  dplyr::select(searchTaxon, Host_Plant_taxon)


MAXENT.RESULTS.HOSTS <- INVERT.MAXENT.RESULTS %>% left_join(., host_plants, by = "searchTaxon") %>%
  mutate(host_dir = gsub(' ', '_', Host_Plant_taxon)) %>%
  mutate(host_dir = ifelse(!is.na(Host_Plant_taxon),  paste0(host_back_dir, host_dir, '/full/'), NA))


# For each Invertebrate species, calculate the % of suitable habitat that was burnt by the
# 2019-2020 fires. We can do this by combining pixels in the rasters like this: 
#   
# 
# - [Invert_SDM + Host_plant_SDM + Inv Veg pixels] * Fire_layer 
# 
# 
# This will give us the % of suitable habitat in each burn intensity category(0-5).

## Calculate Insect habitat - fails after this species?
## Code is stalling before or after :: Naranjakotta - it should be the taxa either side of that...
calculate_taxa_habitat(taxa_list          = rev(MAXENT.RESULTS.HOSTS$searchTaxon),
                       targ_maxent_table  = MAXENT.RESULTS.HOSTS,
                       host_maxent_table  = PLANT.MAXENT.RESULTS,
                       target_path        = './output/invert_maxent_raster_update/back_sel_models/',
                       intersect_path     = 'G:/North_east_NSW_fire_recovery/output/invert_maxent_raster_update/Habitat_suitability/SVTM_intersect',
                       raster_pattern     = '_SVTM_intersection_5000m.tif',
                       fire_raster        = FESM_100m_align,
                       cell_size          = 100,
                       output_path        = './output/invert_maxent_raster_update/Habitat_suitability/FESM_SDM_intersect/',
                       country_shp        = 'AUS',
                       country_prj        = CRS("+init=EPSG:3577"),
                       write_rasters      = TRUE)


# For each taxa, we create a table of the area in square kilometers of suitable habitat that intersects with each burn 
# intensity category from the FESM fire intensity layer. Let's combine all those tables together, to create a master 
# table of estimated burnt area.


## Calculate Insect habitat
INVERT.FESM.list <- list.files('./output/invert_maxent_raster_update/Habitat_suitability/FESM_SDM_intersect/', 
                               pattern     = '_intersect_Fire.csv', 
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
  
  ## finally, bind all the rows together
  bind_rows


## Subset to just the analysis species - some species did not process properly?
INVERT.FESM.TABLE <-  INVERT.FESM.TABLE[INVERT.FESM.TABLE$Habitat_taxa %in% 
                                          sort(unique(MAXENT.RESULTS.HOSTS$searchTaxon)) , ] %>%  
  .[complete.cases(.), ]


## How many taxa have been processed?
length(unique(INVERT.FESM.TABLE$Habitat_taxa))
View(INVERT.FESM.TABLE)


## Save the FESM intersect results to file
write_csv(INVERT.FESM.TABLE, 
          './output/invert_maxent_raster_update/Habitat_suitability/FESM_SDM_intersect/INVERT_TAXA_SDM_VEG_intersect_Fire.csv')


message('sdm fire intersect run succsessfully for ', length(INVERT.FESM.list), 'taxa')




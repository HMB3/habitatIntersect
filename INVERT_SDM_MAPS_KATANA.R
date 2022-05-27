

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
main_dir             <- paste0(getwd(), "/")
tempdir              <- './TEMP/'
ALA_dir              <- './data/ALA/'
INV_dir              <- './data/ALA/Insects/'
check_dir            <- './data/ALA/Insects/check_plots/'
out_dir              <- './output/'

inv_rs_dir           <- './output/invert_maxent_pbi_ala/'
inv_back_dir         <- './output/invert_maxent_pbi_ala/back_sel_models/'
inv_full_dir         <- './output/invert_maxent_pbi_ala/full_models/'
inv_results_dir      <- './output/invert_maxent_pbi_ala/results/'

plant_rs_dir         <- './output/plant_maxent_raster_update/'
plant_back_dir       <- './output/plant_maxent_raster_update/back_sel_models/'
plant_full_dir       <- './output/plant_maxent_raster_update/full_models/'
plant_results_dir    <- './output/plant_maxent_raster_update/results/'

veg_dir              <- './data/Remote_sensing/Veg_data/Forest_cover/'
inv_habitat_dir      <- './output/invert_maxent_pbi_ala/Habitat_suitability/'
inv_inters_dir       <- './output/invert_maxent_pbi_ala/Habitat_suitability/SDM_Veg_intersect/'
inv_thresh_dir       <- './output/invert_maxent_pbi_ala/Habitat_suitability/SDM_thresholds/'
inv_fire_dir         <- './output/invert_maxent_pbi_ala/Habitat_suitability/FESM_SDM_intersect/'

plant_habitat_dir    <- './output/plant_maxent_raster_update/Habitat_suitability/'
plant_inters_dir     <- './output/plant_maxent_raster_update/Habitat_suitability/Veg_intersect/'
plant_thresh_dir     <- './output/plant_maxent_raster_update/Habitat_suitability/SDM_thresholds/'
plant_fire_dir       <- './output/plant_maxent_raster_update/Habitat_suitability/FESM_SDM_intersect/'




dir_list <- c(tempdir, ALA_dir, 
              INV_dir, check_dir, out_dir, inv_rs_dir, inv_back_dir, inv_full_dir, inv_results_dir,
              plant_rs_dir, plant_back_dir, plant_full_dir, plant_results_dir, veg_dir,
              inv_habitat_dir, inv_inters_dir, inv_thresh_dir, inv_fire_dir,
              plant_habitat_dir, plant_inters_dir, plant_thresh_dir, plant_fire_dir)


## Create the folders if they don't exist
for(dir in dir_list) {
  
  if(!dir.exists(paste0(main_dir, dir))) {
    message('Creating ', dir, ' directory')
    dir.create(file.path(main_dir, dir), recursive = TRUE) 
    
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


analysis_taxa <- str_trim(c(target.insect.spp, 
                            target.insect.genera, 
                            target.insect.families)) %>% unique()


## Read in the SDM data
## This function aggregates the results for models that ran successfully
INVERT.MAXENT.RESULTS     <- compile_sdm_results(taxa_list    = analysis_taxa,
                                                 results_dir  = inv_back_dir,
                                                 data_path    = inv_habitat_dir,
                                                 sdm_path     = inv_back_dir,
                                                 save_data    = FALSE,
                                                 save_run     = 'INVERT_ANALYSIS_TAXA')


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


PLANT.MAXENT.RESULTS      <- compile_sdm_results(taxa_list    = target.host.plants,
                                                 results_dir  = plant_back_dir,
                                                 data_path    = plant_habitat_dir,
                                                 sdm_path     = plant_back_dir,
                                                 save_data    = FALSE,
                                                 save_run     = 'PLANT_ANALYSIS_TAXA')


## How many target taxa were modelled?
nrow(INVERT.MAXENT.SPP.RESULTS)/length(target.insect.spp)      *100 
nrow(INVERT.MAXENT.GEN.RESULTS)/length(target.insect.genera)   *100
nrow(INVERT.MAXENT.FAM.RESULTS)/length(target.insect.families) *100


## Get map_taxa from the maxent results table above, change the species column,
## then create a list of logistic thresholds
invert_map_taxa <- INVERT.MAXENT.RESULTS$searchTaxon     %>% gsub(" ", "_", .,)
invert_map_spp  <- INVERT.MAXENT.SPP.RESULTS$searchTaxon %>% gsub(" ", "_", .,)
plant_map_taxa  <- PLANT.MAXENT.RESULTS$searchTaxon      %>% gsub(" ", "_", .,)



## FESM   : https://datasets.seed.nsw.gov.au/dataset/fire-extent-and-severity-mapping-fesm
## VALUES : 1-4, burn intensity from 2019-2020 fires, originally @ 10m resolution, re-sampled to 100m
template_raster_250m <- raster('./data/CSIRO_layers/250m/AUS/Extra/Annual_precip_GDA_ALB.tif')


## Read in feature layers for fire that have been repaired in ArcMap
FESM_east_20m_binary <- readRDS('./data/Remote_sensing/FESM/Fire_perimeters_sub.rds')
FESM_east_20m_categ  <- readRDS('./data/Remote_sensing/FESM/NBR_Burn_severity_classes_sub.rds')

FESM_east_20m_binary_sub   <- readRDS('./data/Remote_sensing/FESM/Fire_perimeters_sub.rds')
FESM_east_20m_binary_split <- readRDS('./data/Remote_sensing/FESM/Fire_perimeters_split.rds')
FESM_east_20m_categ_sub    <- readRDS('./data/Remote_sensing/FESM/NBR_Burn_severity_classes_sub.rds')


## Read in the SDM data, to intersect with the Veg layers
AUS_forest_RS_feat       <- readRDS(paste0(veg_dir, 'Aus_forest_cover_east_coast_classes_sub.rds'))
AUS_forest_RS_feat_split <- readRDS(paste0(veg_dir,'Aus_forest_cover_east_coast_classes_split_sub.rds')) 

## Read in the reptile points
SDM.SPAT.OCC.BG.GDA <- readRDS(paste0(inv_results_dir, 'SDM_SPAT_OCC_BG_ALL_INVERT_TAXA_ALA_PBI.rds'))
intersect_cols      <- c("searchTaxon", "species", "genus", "family", "SOURCE", "gridcode", "Vegetation")
million_metres      <- 1000000


## Check projections and resolutions
projection(FESM_east_20m_binary);projection(SDM.SPAT.OCC.BG.GDA)





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
                                        taxa_list      = sort(target.insect.spp),
                                        taxa_level     = 'species',
                                        habitat_poly   = AUS_forest_RS_feat,
                                        int_cols       = intersect_cols,
                                        output_path    = inv_inters_dir,
                                        buffer         = 5000,
                                        raster_convert = FALSE,
                                        save_shp       = FALSE,
                                        save_png       = FALSE,
                                        poly_path      = 'data/Feature_layers/Boundaries/AUS_2016_AUST.shp',
                                        epsg           = 3577)

gc()


## Select the Vegetation pixels that intersect with the records of each invertebrate genus 
taxa_records_habitat_features_intersect(analysis_df    = SDM.SPAT.OCC.BG.GDA,
                                        taxa_list      = target.insect.genera,
                                        taxa_level     = 'genus',
                                        habitat_poly   = AUS_forest_RS_feat,
                                        int_cols       = intersect_cols,
                                        output_path    = inv_inters_dir,
                                        buffer         = 5000,
                                        raster_convert = FALSE,
                                        save_shp       = FALSE,
                                        save_png       = FALSE,
                                        poly_path      = 'data/Feature_layers/Boundaries/AUS_2016_AUST.shp',
                                        epsg           = 3577)

gc()


## Select the Vegetation pixels that intersect with the records of each invertebrate family
taxa_records_habitat_features_intersect(analysis_df    = SDM.SPAT.OCC.BG.GDA,
                                        taxa_list      = rev(target.insect.families),
                                        taxa_level     = 'family',
                                        habitat_poly   = AUS_forest_RS_feat,
                                        int_cols       = intersect_cols,
                                        output_path    = inv_inters_dir,
                                        buffer         = 5000,
                                        raster_convert = FALSE,
                                        save_shp       = FALSE,
                                        poly_path      = 'data/Feature_layers/Boundaries/AUS_2016_AUST.shp',
                                        epsg           = 3577)

gc()



## Now also intersect the whole SDM layer with the Veg layer, creating a cross-tab of habitat
# SDM.TARG.INVERT.POINTS <- SDM.SPAT.OCC.BG.GDA %>% .[.$searchTaxon %in% analysis_taxa, ] %>%
#   st_as_sf() %>%
#   dplyr::select(searchTaxon, lon, lat) %>%
#   st_transform(., st_crs(3577)) %>% st_as_sf() %>% st_subdivide()
# 
# 
# sdm_veg_crosstab          <- st_intersection(SDM.TARG.INVERT.POINTS,
#                                              AUS_forest_RS_feat)
# saveRDS(sdm_veg_crosstab, paste0(veg_dir,'sdm_veg_crosstab.rds'))
# 
# 
# ## Read it back in
# # sdm_veg_crosstab  <- readRDS(paste0(veg_dir,'sdm_veg_crosstab.rds'))
# 
# sdm_veg_crosstab_df <- sdm_veg_crosstab  %>% as_tibble() %>% 
#   dplyr::select(searchTaxon, Vegetation) %>% 
#   group_by(searchTaxon, Vegetation)      %>% 
#   tally() %>% 
#   
#   mutate(Percent = round(n / sum(n) *100, 2))
# 
# write_csv(sdm_veg_crosstab_df, paste0(inv_results_dir, 'sdm_veg_crosstab.csv'))










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
# host_plants <- read_excel(paste0(inv_results_dir, '/INVERTS_FIRE_SPATIAL_DATA_LUT_JUNE_2022.xlsm'),
#                           sheet = 'TABLE 3') %>% filter(!is.na(HostTaxon)) %>%
#   dplyr::select(searchTaxon, HostTaxon) %>% na.omit()
# 
# 
# PLANT.RESULTS.HOSTS <- PLANT.MAXENT.RESULTS %>% 
#   
#   rename(HostTaxon = "searchTaxon") %>% 
#   left_join(., host_plants, by = "HostTaxon") %>% 
#   dplyr::select(searchTaxon, HostTaxon, everything()) %>%
#   rename(host_dir = results_dir) %>% na.omit() %>% distinct()
# 
# 
# INVERT.RESULTS.HOSTS <- INVERT.MAXENT.RESULTS %>% 
#   
#   left_join(., select(PLANT.RESULTS.HOSTS, 
#                       "searchTaxon", 
#                       "HostTaxon", 
#                       "host_dir"), by = "searchTaxon")


# For each Invertebrate species, calculate the % of suitable habitat that was burnt by the
# 2019-2020 fires. We can do this by combining pixels in the rasters like this: 
#   
# 
# - [Invert_SDM + Host_plant_SDM + Inv Veg pixels] * Fire_layer 
# 
# 
# This will give us the % of suitable habitat in each burn intensity category(0-5).


## Calculate Insect habitat within binary fire layer
# calculate_taxa_habitat_host_features(taxa_list          = rev(INVERT.MAXENT.SPP.RESULTS$searchTaxon),
#                                      targ_maxent_table  = INVERT.RESULTS.HOSTS,
#                                      host_maxent_table  = PLANT.RESULTS.HOSTS,
#                                      
#                                      target_path        = inv_back_dir,
#                                      output_path        = inv_fire_dir,
#                                      intersect_path     = inv_inters_dir,
#                                      intersect_patt     = '_SDM_VEG_intersection.gpkg',
#                                      host_path          = plant_thresh_dir,
#                                      thresh_patt        = '_current_suit_not_novel_above_',
#                                      
#                                      int_cols           = intersect_cols,
#                                      main_int_layer     = FESM_east_20m_binary_split,
#                                      second_int_layer   = AUS_forest_RS_feat,
#                                      template_raster    = template_raster_250m,
#                                      poly_path          = 'data/Feature_layers/Boundaries/AUS_2016_AUST.shp',
#                                      epsg               = 3577)
# 
# gc()


# calculate_taxa_habitat_host_features(taxa_list          = rev(INVERT.MAXENT.SPP.RESULTS$searchTaxon),
#                                      targ_maxent_table  = INVERT.RESULTS.HOSTS,
#                                      host_maxent_table  = PLANT.RESULTS.HOSTS,
#                                      
#                                      target_path        = inv_back_dir,
#                                      output_path        = inv_fire_dir,
#                                      intersect_path     = inv_inters_dir,
#                                      intersect_patt     = '_SDM_VEG_intersection.gpkg',
#                                      host_path          = plant_thresh_dir,
#                                      thresh_patt        = '_current_suit_not_novel_above_',
#                                      
#                                      int_cols           = intersect_cols,
#                                      main_int_layer     = FESM_east_20m_categ_sub,
#                                      second_int_layer   = AUS_forest_RS_feat,
#                                      template_raster    = template_raster_250m,
#                                      poly_path          = 'data/Feature_layers/Boundaries/AUS_2016_AUST.shp',
#                                      epsg               = 3577)
# 
# gc()



# For each taxa, we create a table of the area in square kilometers of suitable habitat that intersects with each burn 
# intensity category from the FESM fire intensity layer. Let's combine all those tables together, to create a master 
# table of estimated burnt area.

message('sdm fire intersect run succsessfully for ', length(INVERT.MAXENT.SPP.RESULTS$searchTaxon), 'taxa')





## END =============================================================


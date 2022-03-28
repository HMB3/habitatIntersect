

## ENVIRONMENT SETTINGS =============================================================


# \
# 
# Load the packages ::
# 
# 
# \
# 
# To install, run :


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
library(nenswniche)
data('sdmgen_packages')
ipak(sdmgen_packages)

## Try and set the raster temp directory to a location not on the partition, to save space
rasterOptions(tmpdir = 'E:/Bush_fire_analysis/nenswniche/TEMP')
terraOptions(memfrac = 0.5, 
             tempdir = 'E:/Bush_fire_analysis/nenswniche/TEMP')





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
sp_epsg3577  <- '+proj=aea +lat_0=0 +lon_0=132 +lat_1=-18 +lat_2=-36 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
SDM.SPAT.OCC.BG.GDA       <- readRDS('./output/results/SDM_SPAT_OCC_BG_GDA_ALL_TARGET_INVERT_TAXA.rds')
SDM.PLANT.SPAT.OCC.BG.GDA <- readRDS('./output/results/SDM_SPAT_OCC_BG_TARGET_HOST_PLANTS.rds')


## SVTM Rasters : https://www.environment.nsw.gov.au/vegetation/state-vegetation-type-map.htm
## The State Vegetation Type Map (SVTM)  of Plant Community Types across NSW, originally @ 5m resolution, re-sampled to 100m
GBAM        <- raster('./data/Remote_sensing/aligned_rasters/GBAM_100m.tif')


## SDM output, re-sampled to 100m
study_sdm_binary <- stack(
  list.files('./output/invert_maxent_raster_update/Habitat_suitability/SDM_thresholds',
             'current_suit_not_novel_above', full.names = TRUE))


## FESM   : https://datasets.seed.nsw.gov.au/dataset/fire-extent-and-severity-mapping-fesm
## VALUES : 1-4, burn intensity from 2019-2020 fires, originally @ 10m resolution, re-sampled to 100m
FESM_2_100m     <- raster('./data/Remote_sensing/aligned_rasters/FESM_2_100m_align.tif')
FESM_100m       <- raster('./data/Remote_sensing/aligned_rasters/FESM_100m_align.tif')
FESM_100m_align <- readRDS('./data/Remote_sensing/aligned_rasters/FESM_100m_align.rds')
SVTM_Veg_Class_GDA        <- readRDS('./data/Remote_sensing/aligned_rasters/SVTM_Veg_Class_GDA.rds')



## Check projections and resolutions
projection(FESM_100m);projection(study_sdm_binary[[1]]);projection(GBAM);projection(SDM.SPAT.OCC.BG.GDA)
raster::xres(FESM_100m);raster::xres(study_sdm_binary[[1]]);raster::xres(GBAM)



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
taxa_records_habitat_intersect(analysis_df    = SDM.SPAT.OCC.BG.GDA,
                               taxa_list      = target.insect.spp,
                               taxa_level     = 'species',
                               habitat_poly   = SVTM_Veg_Class_GDA,
                               output_path    = './output/invert_maxent_raster_update/Habitat_suitability/SVTM_intersect/',
                               buffer         = 5000)


## Select the Vegetation pixels that intersect with the records of each invertebrate genus 
taxa_records_habitat_intersect(analysis_df    = SDM.SPAT.OCC.BG.GDA,
                               taxa_list      = target.insect.genera,
                               taxa_level     = 'genus',
                               habitat_poly   = SVTM_Veg_Class_GDA,
                               output_path    = './output/invert_maxent_raster_update/Habitat_suitability/SVTM_intersect/',
                               buffer         = 5000)


## Select the Vegetation pixels that intersect with the records of each invertebrate family
taxa_records_habitat_intersect(analysis_df    = SDM.SPAT.OCC.BG.GDA,
                               taxa_list      = target.insect.families,
                               taxa_level     = 'family',
                               habitat_poly   = SVTM_Veg_Class_GDA,
                               output_path    = './output/invert_maxent_raster_update/Habitat_suitability/SVTM_intersect/',
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




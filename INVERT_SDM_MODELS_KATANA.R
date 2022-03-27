


## ENVIRONMENT SETTINGS =============================================================


# \
# 
# This code prepares all the data and code needed for the analysis of inverts habitat after the 2019-2020 fires ::
#   
#   
#   \


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
library(nenswniche)
data('sdmgen_packages')
ipak(sdmgen_packages)


## Try and set the raster temp directory to a location not on the partition, to save space
rasterOptions(tmpdir = 'E:/Bush_fire_analysis/nenswniche/TEMP')
terraOptions(memfrac = 0.5, 
             tempdir = 'E:/Bush_fire_analysis/nenswniche/TEMP') 


## 
# source('./R/SDM_GEN_PROCESSOR_FUNCTIONS.R')
# source('./R/SDM_GEN_MAXENT_FUNCTIONS.R')
# source('./R/SDM_GEN_MAPPING_FUNCTIONS.R')




## 1). LOAD RASTER DATA =============================================================

# \
# 
# Load raster data at 280m resolution.
# 
# \



## 250m Precip layers
aus_precip_250m <- raster::stack(
  list.files('./data/Bushfire_indices/R_outputs/250m/AUS/Precip',        pattern =".tif", full.names = TRUE))

east_precip_250m <- raster::stack(
  list.files('./data/Bushfire_indices/R_outputs/250m/EAST_COAST/Precip', pattern =".tif", full.names = TRUE))


## 250m temperature layers
aus_temp_250m <- raster::stack(
  list.files('./data/Bushfire_indices/R_outputs/250m/AUS/Temp',          pattern =".tif", full.names = TRUE))

east_temp_250m <- raster::stack(
  list.files('./data/Bushfire_indices/R_outputs/250m/EAST_COAST/Temp',   pattern =".tif", full.names = TRUE))


## 250m Soil layers
aus_soil_250m <- raster::stack(
  list.files('./data/Bushfire_indices/R_outputs/250m/AUS/Soil',          pattern =".tif", full.names = TRUE))

east_soil_250m <- raster::stack(
  list.files('./data/Bushfire_indices/R_outputs/250m/EAST_COAST/Soil',   pattern =".tif", full.names = TRUE))


## 250m Geology Australia
aus_geology_250m <- raster::stack(
  list.files('./data/Bushfire_indices/R_outputs/250m/AUS/Geology',        pattern =".tif", full.names = TRUE))

east_geology_250m <- raster::stack(
  list.files('./data/Bushfire_indices/R_outputs/250m/EAST_COAST/Geology', pattern =".tif", full.names = TRUE))


## 250m topo Australia
aus_topo_250m <- raster::stack(
  list.files('./data/Bushfire_indices/R_outputs/250m/AUS/Topo',           pattern =".tif", full.names = TRUE))

east_topo_250m <- raster::stack(
  list.files('./data/Bushfire_indices/R_outputs/250m/EAST_COAST/Topo',    pattern =".tif", full.names = TRUE))


## 250m terrain indices Australia
aus_terrain_250m <- raster::stack(
  list.files('./data/Bushfire_indices/R_outputs/250m/AUS/Indices',        pattern =".tif", full.names = TRUE))

east_terrain_250m <- raster::stack(
  list.files('./data/Bushfire_indices/R_outputs/250m/EAST_COAST/Indices', pattern =".tif", full.names = TRUE))


## Climate data for Australia, in GDA Albers projection
aus.grids.current.250m <- stack(aus_precip_250m,
                                aus_temp_250m,
                                aus_soil_250m,
                                aus_topo_250m,
                                aus_terrain_250m,
                                aus_geology_250m)

east.grids.current.250m <- stack(east_precip_250m,
                                 east_temp_250m,
                                 east_soil_250m,
                                 east_topo_250m,
                                 east_terrain_250m,
                                 east_geology_250m)


## And a stack of grids for vegetation, in GDA Albers projection
aus.veg.grids.250m <- stack(
  
  list.files('./data/Remote_sensing/Veg_data/height_and_cover/Aus/250m', 
             '_250m.tif', full.names = TRUE))

east.veg.grids.250m <- stack(
  
  list.files('./data/Remote_sensing/Veg_data/height_and_cover/Eastern_Aus/250m', 
             '_east_coast.tif', full.names = TRUE))


names(east.grids.current.250m) <- gsub('_EAST_COAST', '', names(east.grids.current.250m))
names(aus.veg.grids.250m)      <- names(east.veg.grids.250m) <- c("Plant_cover_fraction_0_5m", 
                                                                  "Plant_cover_fraction_5_10m",  
                                                                  "Plant_cover_fraction_10_30m",      
                                                                  "Plant_cover_fraction_30m",
                                                                  "Total_Plant_cover_fraction",  
                                                                  "Tree_canopy_height_25th", 
                                                                  "Tree_canopy_height_50th", 
                                                                  "Tree_canopy_height_75th",   
                                                                  "Tree_canopy_height_95th",   
                                                                  "Tree_canopy_peak_foliage",
                                                                  "Tree_canopy_peak_foliage_total",
                                                                  "mrvbf")


## Combine the grids into raster stacks
aus.climate.veg.grids.250m   <- stack(aus.grids.current.250m, aus.veg.grids.250m)
east.climate.veg.grids.250m  <- stack(east.grids.current.250m, east.veg.grids.250m)
aus_annual_precip            <- raster('./data/Bushfire_indices/R_outputs/250m/AUS/Extra/Annual_precip_WGS84.tif')
aus_annual_precip_alb        <- raster('./data/Bushfire_indices/R_outputs/250m/AUS/Extra/Annual_precip_GDA_ALB.tif')


## Should be 1km*1km, It should havle a value of 1 for land, and NA for the ocean
aus_annual_precip_alb[aus_annual_precip_alb > 0] <- 1
template_raster_250m <- aus_annual_precip_alb
data('AUS')


AUS_shp <- readOGR(dsn              = './data/Spatial_data/AUS_2016_AUST.shp',
                   layer            = 'AUS_2016_AUST',
                   stringsAsFactors = FALSE)





## 2). RUN SDM ANALYSIS =============================================================



# The backbone of the R workflow is a list of (taxonomically Ridgey-Didge!) Taxa names 
# that we supply. The analysis is designed to process data for one taxa at a time, 
# allowing taxa results to be updated as required. Unfortunately, Australia's insects
# are not very well sampled...so we can analyse at the genus and family level.
# 
# \
# 
# The site data comes from the PBI database :
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
data('AUS')
SDM.SPAT.OCC.BG.GDA       <- readRDS('./output/invert_maxent_raster_update/results/SDM_SPAT_OCC_BG_ALL_TARGET_INSECT_TAXA.rds')
SDM.PLANT.SPAT.OCC.BG.GDA <- readRDS('./output/plant_maxent_raster_update/results/SDM_SPAT_OCC_BG_ALL_TARGET_HOST_PLANTS.rds')


## Run family-level models for invertebrates
run_sdm_analysis_no_crop(taxa_list               = sort(target.insect.families),
                         taxa_level              = 'family',
                         maxent_dir              = 'output/invert_maxent_raster_update/full_models',     
                         bs_dir                  = 'output/invert_maxent_raster_update/back_sel_models',
                         sdm_df                  = SDM.SPAT.OCC.BG.GDA,
                         sdm_predictors          = names(aus.climate.veg.grids.250m),
                         sp_country_prj          = sp_epsg3577,
                         
                         backwards_sel           = TRUE,      
                         template_raster         = template_raster_250m,
                         cor_thr                 = 0.8,  
                         pct_thr                 = 5, 
                         k_thr                   = 4, 
                         min_n                   = 10,  
                         max_bg_size             = 100000,
                         background_buffer_width = 100000,
                         shapefiles              = TRUE,
                         features                = 'lpq',
                         replicates              = 5,
                         responsecurves          = TRUE,
                         country_shp             = AUS_shp)

gc()


## Run genus-level models for invertebrates
run_sdm_analysis_no_crop(taxa_list               = rev(sort(target.insect.genera)),
                 taxa_level              = 'genus',
                 maxent_dir              = 'output/invert_maxent_raster_update/full_models',     
                 bs_dir                  = 'output/invert_maxent_raster_update/back_sel_models',
                 sdm_df                  = SDM.SPAT.OCC.BG.GDA,
                 sdm_predictors          = names(aus.climate.veg.grids.250m),
                 sp_country_prj          = sp_epsg3577,
                 
                 backwards_sel           = TRUE,      
                 template_raster         = template_raster_250m,
                 cor_thr                 = 0.8,  
                 pct_thr                 = 5, 
                 k_thr                   = 4, 
                 min_n                   = 10,  
                 max_bg_size             = 100000,
                 background_buffer_width = 100000,
                 shapefiles              = TRUE,
                 features                = 'lpq',
                 replicates              = 5,
                 responsecurves          = TRUE,
                 country_shp             = AUS_shp)

gc()


## Run species-level models for invertebrates
run_sdm_analysis_no_crop(taxa_list               = target.insect.spp,
                 taxa_level              = 'species',
                 maxent_dir              = 'output/invert_maxent_raster_update/full_models',     
                 bs_dir                  = 'output/invert_maxent_raster_update/back_sel_models',
                 sdm_df                  = SDM.SPAT.OCC.BG.GDA,
                 sdm_predictors          = names(aus.climate.veg.grids.250m),
                 sp_country_prj          = sp_epsg3577,
                 
                 backwards_sel           = TRUE,      
                 template_raster         = template_raster_250m,
                 cor_thr                 = 0.8,  
                 pct_thr                 = 5, 
                 k_thr                   = 4, 
                 min_n                   = 10,  
                 max_bg_size             = 100000,
                 background_buffer_width = 100000,
                 shapefiles              = TRUE,
                 features                = 'lpq',
                 replicates              = 5,
                 responsecurves          = TRUE,
                 country_shp             = AUS_shp)

gc()


## Run species-level models for host plants - how many plants are in the dataset
target.host.plants %in% SDM.PLANT.SPAT.OCC.BG.GDA$searchTaxon %>% table()


run_sdm_analysis_no_crop(taxa_list               = sort(target.host.plants),
                 taxa_level              = 'species',
                 maxent_dir              = 'output/plant_maxent_raster_update/full_models',     
                 bs_dir                  = 'output/plant_maxent_raster_update/back_sel_models',
                 sdm_df                  = SDM.PLANT.SPAT.OCC.BG.GDA,
                 sdm_predictors          = names(aus.climate.veg.grids.250m),
                 sp_country_prj          = sp_epsg3577,
                 
                 backwards_sel           = TRUE,      
                 template_raster         = template_raster_250m,
                 cor_thr                 = 0.8,  
                 pct_thr                 = 5, 
                 k_thr                   = 4, 
                 min_n                   = 10,  
                 max_bg_size             = 100000,
                 background_buffer_width = 100000,
                 shapefiles              = TRUE,
                 features                = 'lpq',
                 replicates              = 5,
                 responsecurves          = TRUE,
                 country_shp             = AUS,
                 crop_Koppen             = FALSE)



## 3). PROJECT SDMs =============================================================


# \
# 
# The next step is to project the SDM predictions across geographic space.
# First, we need to extract the SDM results from the models. Each model generates a 'threshold' 
# of probability of occurrence (see ref), which we use to create map of habitat suitability 
# across Australia (). 
# 
# \


## Create a table of maxent results
## This function aggregates the results for models that ran successfully
INVERT.MAXENT.RESULTS     <- compile_sdm_results(taxa_list    = analysis_taxa,
                                                 results_dir  = 'output/invert_maxent_raster_update/back_sel_models',
                                                 data_path    = "./output/invert_maxent_raster_update/Habitat_suitability/",
                                                 sdm_path     = "./output/invert_maxent_raster_update/back_sel_models/",
                                                 save_data    = FALSE,
                                                 save_run     = "INVERT_ANALYSIS_TAXA")


INVERT.MAXENT.FAM.RESULTS <- compile_sdm_results(taxa_list    = target.insect.families,
                                                 results_dir  = 'output/invert_maxent_raster_update/back_sel_models',
                                                 data_path    = "./output/invert_maxent_raster_update/Habitat_suitability/",
                                                 sdm_path     = "./output/invert_maxent_raster_update/back_sel_models/",
                                                 save_data    = FALSE,
                                                 save_run     = "INVERT_ANALYSIS_TAXA")


INVERT.MAXENT.GEN.RESULTS <- compile_sdm_results(taxa_list    = target.insect.genera,
                                                 results_dir  = 'output/invert_maxent_raster_update/back_sel_models',
                                                 data_path    = "./output/invert_maxent_raster_update/Habitat_suitability/",
                                                 sdm_path     = "./output/invert_maxent_raster_update/back_sel_models/",
                                                 save_data    = FALSE,
                                                 save_run     = "INVERT_ANALYSIS_TAXA")


INVERT.MAXENT.SPP.RESULTS <- compile_sdm_results(taxa_list    = target.insect.spp,
                                                 results_dir  = 'output/invert_maxent_raster_update/back_sel_models',
                                                 data_path    = "./output/invert_maxent_raster_update/Habitat_suitability/",
                                                 sdm_path     = "./output/invert_maxent_raster_update/back_sel_models/",
                                                 save_data    = FALSE,
                                                 save_run     = "INVERT_ANALYSIS_TAXA")


PLANT.MAXENT.RESULTS      <- compile_sdm_results(taxa_list    = target.host.plants,
                                                 results_dir  = 'output/plant_maxent/back_sel_models',
                                                 data_path    = "./output/invert_maxent_raster_update/Habitat_suitability/",
                                                 sdm_path     = "./output/plant_maxent/back_sel_models/",
                                                 save_data    = FALSE,
                                                 save_run     = "INVERT_ANALYSIS_TAXA")


## How many target taxa were modelled?
nrow(INVERT.MAXENT.SPP.RESULTS)/length(target.insect.spp)      *100 
nrow(INVERT.MAXENT.GEN.RESULTS)/length(target.insect.genera)   *100
nrow(INVERT.MAXENT.FAM.RESULTS)/length(target.insect.families) *100


## Get map_taxa from the maxent results table above, change the species column,
## then create a list of logistic thresholds
invert_map_taxa <- INVERT.MAXENT.RESULTS$searchTaxon %>% gsub(" ", "_", .,)
invert_map_spp  <- INVERT.MAXENT.SPP.RESULTS$searchTaxon %>% gsub(" ", "_", .,)
plant_map_taxa  <- PLANT.MAXENT.RESULTS$searchTaxon  %>% gsub(" ", "_", .,)


# The projection function takes the maxent models created by the 'fit_maxent_targ_bg_back_sel' function, 
# and projects the models across geographic space - currently just for Australia. It uses the rmaxent 
# package https://github.com/johnbaums/rmaxent. It assumes that the maxent models were generated by the 
# 'fit_maxent_targ_bg_back_sel' function. Note that this step is quite memory heavy, best run with > 64GB of RAM.



## Create a local projection for mapping : Australian Albers
aus_albers <- CRS('+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')


## Project SDMs across the Study area for the invert taxa
tryCatch(
  project_maxent_current_grids_mess(country_shp     = AUS, 
                                    country_prj     = sp_epsg3577,
                                    local_prj       = sp_epsg3577,
                                    
                                    taxa_list       = invert_map_taxa,    
                                    maxent_path     ='./output/invert_maxent_raster_update/back_sel_models/',
                                    
                                    current_grids   = east.climate.veg.grids.250m,         
                                    create_mess     = TRUE,
                                    save_novel_poly = TRUE),
  
  ## If the species fails, write a fail message to file
  error = function(cond) {
    
    ## This will write the error message inside the text file, but it won't include the species
    file.create(file.path("output/invert_maxent_raster_update/back_sel_models/mapping_failed_current.txt"))
    cat(cond$message, file = file.path("output/invert_maxent_raster_update/back_sel_models/inv_mapping_failed_current.txt"))
    warning(cond$message)
    
  })


## Project SDMs across the Study area for the plant taxa
tryCatch(
  project_maxent_current_grids_mess(country_shp     = AUS, 
                                    country_prj     = CRS("+init=EPSG:3577"),
                                    local_prj       = aus_albers,
                                    
                                    taxa_list       = plant_map_taxa,    
                                    maxent_path     = './output/plant_maxent/back_sel_models/',
                                    
                                    current_grids   = study.climate.veg.grids,         
                                    create_mess     = TRUE,
                                    save_novel_poly = FALSE),
  
  ## If the species fails, write a fail message to file
  error = function(cond) {
    
    ## This will write the error message inside the text file, but it won't include the species
    file.create(file.path("output/plant_maxent/back_sel_models/mapping_failed_current.txt"))
    cat(cond$message, file = file.path("output/plant_maxent/back_sel_models/plant_sdm_mapping_failed_current.txt"))
    warning(cond$message)
    
  })




## 4). THRESHOLD SDMs =============================================================


# \
# 
# To use the habitat suitability rasters in area calculations (e.g. comparing the area of suitable habitat
#                                                              affected by fire), we need to convert the continuous suitability scores (ranging from 0-1) to binary values
# (either 1, or 0). To do this, we need to pick a threshold of habitat suitability, below which the species 
# is not considered present. Here we have chosen the 10th% Logistic threshold for each taxa (ref).
# 
# 
# \


## Combine all the site data into one table 
habitat_threshold(taxa_list     = sort(unique(INVERT.MAXENT.SPP.RESULTS$searchTaxon)),
                  maxent_table  = INVERT.MAXENT.RESULTS,
                  maxent_path   = './output/invert_maxent_raster_update/back_sel_models/',
                  cell_factor   = 9,
                  country_shp   = 'AUS',
                  country_prj   = CRS("+init=EPSG:3577"))


## Threshold the invertebrate SDM models to be either 0 or 1
habitat_threshold(taxa_list     = sort(unique(INVERT.MAXENT.RESULTS$searchTaxon)),
                  maxent_table  = INVERT.MAXENT.RESULTS,
                  maxent_path   = './output/invert_maxent_raster_update/back_sel_models/',
                  cell_factor   = 9,
                  country_shp   = 'AUS',
                  country_prj   = CRS("+init=EPSG:3577"))


## Threshold the Plant SDM models to be either 0 or 1
habitat_threshold(taxa_list     = sort(unique(PLANT.MAXENT.RESULTS$searchTaxon)),
                  maxent_table  = PLANT.MAXENT.RESULTS,
                  maxent_path   = './output/plant_maxent/back_sel_models/',
                  cell_factor   = 9,
                  country_shp   = 'AUS',
                  country_prj   = CRS("+init=EPSG:3577"),
                  write_rasters = TRUE)



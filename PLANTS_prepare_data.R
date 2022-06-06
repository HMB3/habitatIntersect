

## ENVIRONMENT SETTINGS =============================================================


# \
# 
# This code prepares all the data and code needed for the analysis of inverts habitat after the 2019-2020 fires ::
#   
#   
#   \


## Set env
rm(list = ls())
#if (!Sys.getenv("JAVA_TOOL_OPTIONS")) {
if (all(Sys.getenv("JAVA_HOME")=="")) {
  Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_321')
}

if (all(Sys.getenv("JAVA_TOOL_OPTIONS")=="")) {
  options(java.parameters = "-Xmx64G")
}

options(warn=0)

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

inv_rs_dir           <- './output/invert_maxent_pbi_ala_site/'
inv_back_dir         <- './output/invert_maxent_pbi_ala_site/back_sel_models/'
inv_full_dir         <- './output/invert_maxent_pbi_ala_site/full_models/'
inv_results_dir      <- './output/invert_maxent_pbi_ala_site/results/'

plant_rs_dir         <- './output/plant_maxent_raster_update/'
plant_back_dir       <- './output/plant_maxent_raster_update/back_sel_models/'
plant_full_dir       <- './output/plant_maxent_raster_update/full_models/'
plant_results_dir    <- './output/plant_maxent_raster_update/results/'

veg_dir              <- './data/Remote_sensing/Veg_data/Forest_cover/'
inv_habitat_dir      <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/'
inv_records_dir      <- './output/invert_maxent_pbi_ala_site_sites/Habitat_suitability/SDM_point_data/'
inv_inters_dir       <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/SDM_Veg_intersect/'
inv_thresh_dir       <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/SDM_thresholds/'
inv_fire_dir         <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/FESM_SDM_intersect/'

plant_habitat_dir    <- './output/plant_maxent_raster_update/Habitat_suitability/'
plant_inters_dir     <- './output/plant_maxent_raster_update/Habitat_suitability/Veg_intersect/'
plant_thresh_dir     <- './output/plant_maxent_raster_update/Habitat_suitability/SDM_thresholds/'
plant_fire_dir       <- './output/plant_maxent_raster_update/Habitat_suitability/FESM_SDM_intersect/'


dir_list <- c(tempdir, ALA_dir, 
              INV_dir, check_dir, out_dir, inv_rs_dir, inv_back_dir, inv_full_dir, inv_results_dir,
              plant_rs_dir, plant_back_dir, plant_full_dir, plant_results_dir, veg_dir,
              inv_habitat_dir, inv_inters_dir, inv_thresh_dir, inv_fire_dir,
              plant_habitat_dir, plant_inters_dir, plant_thresh_dir, plant_fire_dir)


## Create the folders if they don't exist.
for(dir in dir_list) {
  
  if(!dir.exists(paste0(main_dir, dir))) {
    message('Creating ', dir, ' directory')
    dir.create(file.path(main_dir, dir), recursive = TRUE) 
    
  } else {
    message(dir, ' directory already exists')}
}


## Try and set the raster temp directory to a location not on the partition, to save space
rasterOptions(memfrac = 0.9,
              tmpdir  = tempdir)

terraOptions(memfrac = 0.9, 
             tempdir = tempdir) 


coord_clean <- FALSE
save_name   <- 'ALL_PLANT_TAXA_ALA_PBI'


# STEP 1 :: Get spp lists ----


## To Do :
## 1). Check the errors that Finlay may have found
## 2). Clean up the folders


## get target taxa
data('target.host.plants')


host_taxa_updated <- read_excel(paste0(inv_results_dir, 'INVERST_HSM_CHECK.xlsx'),
                                sheet = 'All_plants') %>% select(searchTaxon) %>% .$searchTaxon 

background_plants <- host_taxa_updated %>% 
  c(target.host.plants, .) %>% unique %>% sort()





# STEP 2 :: Combine taxa occurrence data ----


## 250m Climate, Soil and terrain variables
aus.climate.veg.grids.250m <- raster::stack(
  list.files('./data/CSIRO_layers/250m/AUS/',      pattern =".tif", full.names = TRUE))

east.climate.veg.grids.250m  <- raster::stack(
  list.files('./data/CSIRO_layers/250m/FESM_EXT/', pattern =".tif", full.names = TRUE))


names(aus.climate.veg.grids.250m)[1:11] <-
  
  names(east.climate.veg.grids.250m)[1:11] <- 
  c("Tree_canopy_peak_foliage_total",
    "Plant_cover_fraction_0_5m", 
    "Plant_cover_fraction_5_10m",  
    "Plant_cover_fraction_10_30m",      
    "Plant_cover_fraction_30m",
    "Total_Plant_cover_fraction",  
    "Tree_canopy_height_25th", 
    "Tree_canopy_height_50th", 
    "Tree_canopy_height_75th",   
    "Tree_canopy_height_95th",   
    "Tree_canopy_peak_foliage")

identical(names(east.climate.veg.grids.250m),
          names(aus.climate.veg.grids.250m))

## Now remove the individual rasters
rm(list = ls(pattern = 'aus_'))
rm(list = ls(pattern = 'east_'))

aus_annual_precip       <- raster('./data/CSIRO_layers/250m/AUS/Extra/Annual_precip_WGS84.tif')
aus_annual_precip_alb   <- raster('./data/CSIRO_layers/250m/AUS/Extra/Annual_precip_GDA_ALB.tif')


## Should be 1km*1km, It should havle a value of 1 for land, and NA for the ocean
aus_annual_precip_alb[aus_annual_precip_alb > 0] <- 1
template_raster_250m <- aus_annual_precip_alb
gc()





# STEP 3 :: extract environmental values ----


ALA.LAND.SPP <- combine_ala_records(taxa_list         =  background_plants,
                                    records_path      = "./data/ALA/Plants/",
                                    records_extension = "_ALA_records.RData",
                                    record_type       = "ALA",
                                    keep_cols         = ALA_keep,
                                    year_filt         = FALSE,
                                    unique_cells      = FALSE,
                                    world_raster      = aus_annual_precip)


host_taxa_updated %in% unique(ALA.LAND.SPP$searchTaxon) %>% table()
setdiff(host_taxa_updated, unique(ALA.LAND.SPP$searchTaxon))


## The below should really all be one big table
## Where we use different columns for the analysis :: species, genus or family
Plants_ALA_down_1 <- read_csv('./data/ALA/Plants/manual_download/records-2022-06-06.csv')
Plants_ALA_down_2 <- read_csv('./data/ALA/Plants/manual_download/records-2022-06-06_2.csv')
Plants_ALA_down_3 <- read_csv('./data/ALA/Plants/manual_download/records-2022-06-06_3.csv')
Plants_ALA_down_4 <- read_csv('./data/ALA/Plants/manual_download/records-2022-06-06_4.csv')


error_cols <- c('eventTime', 'identificationID', 'taxonID', 
                'occurrenceID', 'eventID', 'verbatimElevation...77', 'datasetID')

Plants_ALA_down <- Plants_ALA_down_1[, !names(Plants_ALA_down_1) %in% error_cols] %>% 
  dplyr::select(., -starts_with("verbatim")) %>% 
  
  bind_rows(., Plants_ALA_down_2 [, !names(Plants_ALA_down_2) %in% error_cols])  %>%
  dplyr::select(., -starts_with("verbatim")) %>% 
  
  bind_rows(., Plants_ALA_down_3 [, !names(Plants_ALA_down_3) %in% error_cols])  %>%
  dplyr::select(., -starts_with("verbatim")) %>% 
  
  bind_rows(., Plants_ALA_down_4 [, !names(Plants_ALA_down_4) %in% error_cols])  %>%
  dplyr::select(., -starts_with("verbatim")) 


# Plants_ALA_1 <- read_csv('./data/ALA/Plants/records-2021-11-25.csv')
# Plants_ALA_2 <- read_csv('./data/ALA/Plants/records-2021-11-25_part2.csv')
# Plants_ALA_3 <- read_csv('./data/ALA/Plants/records-2021-11-25_part3.csv')
# Plants_ALA_4 <- read_csv('./data/ALA/Plants/records-2021-11-25_part4.csv')
# Plants_ALA_5 <- read_csv('./data/ALA/Plants/records-2021-11-25_part5.csv')
# Plants_ALA_6 <- read_csv('./data/ALA/Plants/records-2021-11-25_part6.csv')
# Plants_ALA_7 <- read_csv('./data/ALA/Plants/records-2021-11-25_part7.csv')
# Plants_ALA_8 <- read_csv('./data/ALA/Plants/records-2021-11-25_part8.csv')
# Plants_ALA_9 <- read_csv('./data/ALA/Plants/records-2021-11-25_part9.csv')


# Plants_ALA <- Plants_ALA_1[, !names(Plants_ALA_1) %in% error_cols] %>% 
#   dplyr::select(., -starts_with("verbatim")) %>% 
#   
#   bind_rows(., Plants_ALA_2[, !names(Plants_ALA_2) %in% error_cols])  %>%
#   dplyr::select(., -starts_with("verbatim")) %>% 
#   
#   bind_rows(., Plants_ALA_3[, !names(Plants_ALA_3) %in% error_cols])  %>%
#   dplyr::select(., -starts_with("verbatim")) %>% 
#   
#   bind_rows(., Plants_ALA_4[, !names(Plants_ALA_4) %in% error_cols])  %>%
#   dplyr::select(., -starts_with("verbatim")) %>% 
#   
#   bind_rows(., Plants_ALA_5[, !names(Plants_ALA_5) %in% error_cols])  %>%
#   dplyr::select(., -starts_with("verbatim")) %>% 
#   
#   bind_rows(., Plants_ALA_6[, !names(Plants_ALA_6) %in% error_cols])  %>%
#   dplyr::select(., -starts_with("verbatim")) %>% 
#   
#   bind_rows(., Plants_ALA_7[, !names(Plants_ALA_7) %in% error_cols])  %>%
#   dplyr::select(., -starts_with("verbatim")) 



# saveRDS(Plants_ALA, paste0(ALA_dir, 'ALA_plants.rds'))

ALA.LAND.PLANT.SPP <- format_ala_dump(ALA_table     = Plants_ALA_down,
                                      record_type   = "ALA",
                                      keep_cols     = ALA_keep,
                                      year_filt     = FALSE,
                                      unique_cells  = FALSE,
                                      world_raster  = aus_annual_precip)


# saveRDS(Plants_ALA, paste0(ALA_dir, 'ALA_plants_invert_hosts.rds'))

ALA.LAND.SPP.UPDATE <- ALA.LAND.SPP %>% bind_rows(., ALA.LAND.PLANT.SPP)


ALA_LAND_INV_SPP_sf <- SpatialPointsDataFrame(coords      = ALA.LAND.SPP.UPDATE %>% 
                                                dplyr::select(lon, lat) %>% as.matrix(),
                                              data        = ALA.LAND.SPP.UPDATE,
                                              proj4string = CRS("+init=epsg:4326")) %>%
  
  st_as_sf() %>% 
  st_transform(., st_crs(4326))



COMBO.RASTER.PLANT.SPP <- combine_records_extract(records_df       = ALA_LAND_INV_SPP_sf,
                                                  add_sites        = FALSE,
                                                  filter_taxo      = FALSE,
                                                  
                                                  site_df          = NA,
                                                  thin_records     = FALSE,
                                                  template_raster  = template_raster_250m,
                                                  world_raster     = aus.climate.veg.grids.250m,
                                                  epsg             = 4326,
                                                  taxa_list        = target.host.plants,
                                                  taxa_level       = 'family',   
                                                  
                                                  ## This might need to change too
                                                  raster_divide    = FALSE,
                                                  save_data        = FALSE,
                                                  data_path        = inv_results_dir,
                                                  save_run         = 'ALL_PLANTS')


COMBO_RASTER_PBI_PLANT_sf <- SpatialPointsDataFrame(coords      = COMBO.RASTER.PLANT.SPP %>% 
                                                      dplyr::select(lon, lat) %>% as.matrix(),
                                                    data        = COMBO.RASTER.PLANT.SPP,
                                                    proj4string = CRS("+init=epsg:4326")) %>%
  
  st_as_sf() %>% 
  st_transform(., st_crs(4326))




# Records data

## Update with the full Plants download from ALA ----
## Combine GBIF and ALA data, and extract environmental values
## Some of the site records will be removed here, if they are outside the raster bounds.
## This seems strange, but maybe they are right on the coast
COMBO.RASTER.ALA.SPP = combine_records_extract(records_df       = ALA_LAND_INV_SPP_sf,
                                               add_sites        = FALSE,
                                               filter_taxo      = FALSE,
                                               
                                               site_df          = NA,
                                               thin_records     = FALSE,
                                               template_raster  = template_raster_250m,
                                               world_raster     = aus.climate.veg.grids.250m,
                                               epsg             = 4326,
                                               taxa_list        = target.insect.families,
                                               taxa_level       = NA,   
                                               
                                               ## This might need to change too
                                               raster_divide    = FALSE,
                                               save_data        = FALSE,
                                               data_path        = inv_rs_dir,
                                               save_run         = 'ALL_INV_SPP_ALA')
gc()



## Genera
COMBO.RASTER.ALA.GEN <- COMBO.RASTER.ALA.SPP %>%
  dplyr::select(-searchTaxon) %>% 
  dplyr::rename(searchTaxon = genus) %>% dplyr::select(searchTaxon, everything())


## Families
COMBO.RASTER.ALA.FAM <- COMBO.RASTER.ALA.SPP %>%
  dplyr::select(-searchTaxon)  %>% 
  dplyr::rename(searchTaxon = family) %>% dplyr::select(searchTaxon, everything())


## Now bind the ALA data together
COMBO.SPP.GEN.FAM.ALA <- bind_rows(COMBO.RASTER.ALA.SPP,
                                   COMBO.RASTER.ALA.GEN,
                                   COMBO.RASTER.ALA.FAM)

rm(COMBO.RASTER.ALA.SPP)
rm(COMBO.RASTER.ALA.GEN)
rm(COMBO.RASTER.ALA.FAM)
gc()


## Now bind the ALA and PBI tables together
COMBO.SPP.GEN.FAM.ALA.PBI <- bind_rows(COMBO.SPP.GEN.FAM.ALA, COMBO.SPP.GEN.FAM.PBI)

rm(COMBO.SPP.GEN.FAM.ALA)
rm(COMBO.SPP.GEN.FAM.PBI)
gc()





# Prepare niches ----


##
GLOB.NICHE.ALL = calc_enviro_niches(coord_df     = COMBO.SPP.GEN.FAM.ALA.PBI %>% .[.$searchTaxon %in% analysis_taxa, ],
                                    prj          = CRS("+init=epsg:4326"),
                                    country_shp  = AUS,
                                    world_shp    = LAND,
                                    kop_shp      = Koppen_shp,
                                    taxa_list    = analysis_taxa,
                                    env_vars     = names(aus.climate.veg.grids.250m),
                                    cell_size    = 2,
                                    save_data    = TRUE,
                                    save_run     = save_name,
                                    data_path    = inv_results_dir)


plot_range_histograms(coord_df     = COMBO.SPP.GEN.FAM.ALA.PBI %>% .[.$searchTaxon %in% analysis_taxa, ],
                      taxa_list    = target.insect.genera,
                      range_path   = check_dir)


# STEP 4 :: Prepare SDM table ----


##
if(coord_clean) {
  
  message('clean coordinates')
  COORD.CLEAN = coord_clean_records(records      = COMBO.SPP.GEN.FAM.ALA.PBI,
                                    site_flag    = 'SITE',
                                    occ_flag     = 'ALA',
                                    multi_source = TRUE,
                                    
                                    capitals     = 10000,  
                                    centroids    = 5000,   
                                    save_data    = FALSE,
                                    save_run     = "TARGET_INSECT_SPECIES",
                                    data_path    = "./output/results/")
  
  
  COORD_CLEAN_sf <- SpatialPointsDataFrame(coords      = COORD.CLEAN %>% 
                                             dplyr::select(lon, lat) %>% as.matrix(),
                                           data        = COORD.CLEAN,
                                           proj4string = CRS("+init=epsg:4326")) %>%
    
    st_as_sf() %>% 
    st_transform(., st_crs(4326))
  
} else {
  message('do not clean coordinates')
  COMBO.SPP.GEN.FAM.ALA.PBI$coord_summary <- TRUE
  COORD_CLEAN_sf <- SpatialPointsDataFrame(coords      = COMBO.SPP.GEN.FAM.ALA.PBI %>% 
                                             dplyr::select(lon, lat) %>% as.matrix(),
                                           data        = COMBO.SPP.GEN.FAM.ALA.PBI,
                                           proj4string = CRS("+init=epsg:4326")) %>%
    
    st_as_sf() %>% 
    st_transform(., st_crs(4326))
}


## Combine occ data with the bg data 
SDM.SPAT.OCC.BG.GDA <- prepare_sdm_table(coord_df          = COORD_CLEAN_sf,
                                         taxa_list         = analysis_taxa,
                                         
                                         site_flag         = 'SITE',
                                         occ_flag          = 'ALA',
                                         spat_out_remove   = FALSE,
                                         site_split        = FALSE,
                                         
                                         sdm_table_vars    = c('searchTaxon', 
                                                               'species',  
                                                               'genus', 
                                                               'family',
                                                               'order',
                                                               'class',                         
                                                               'phylum',                        
                                                               'kingdom',
                                                               'lon',      
                                                               'lat',
                                                               'year',
                                                               'SOURCE', 
                                                               'SPAT_OUT',
                                                               names(aus.climate.veg.grids.250m)),
                                         
                                         save_run          = save_name,
                                         read_background   = FALSE,
                                         country_epsg      = 3577,
                                         world_epsg        = "+init=epsg:4326",
                                         save_data         = TRUE,
                                         data_path         = inv_results_dir)


## Create subset of target reptiles
## Save each taxa as an individual shapefile
# SDM.SPAT.OCC.BG.GDA = readRDS('./output/invert_maxent_pbi_ala_site/results/SDM_SPAT_OCC_BG_ALL_TARGET_INSECT_TAXA.rds')
SDM.SPAT.OCC.BG.TARG.PLANT   <- SDM.SPAT.OCC.BG.GDA %>% .[.$searchTaxon %in% analysis_taxa, ]
SDM.SPAT.OCC.BG.TARG.PLANT.SF <- SDM.SPAT.OCC.BG.TARG.PLANT%>% st_as_sf()


st_write(SDM.SPAT.OCC.BG.TARG.PLANT%>% st_as_sf(), 
         dsn   = paste0(inv_results_dir, save_name, '.gpkg'), 
         layer = 'SDM_TARGET_INVERT_TAXA', 
         quiet = TRUE)

gc()


message('sdm data preparation code successfuly run')





# STEP 5 :: Check data ----


## Check the species data - 34 target species are in the final data
## These then drop out due to cross-validation, etc. 
SDM.SPAT.OCC.BG.GDA    <- readRDS(paste0(inv_results_dir,   
                                         'SDM_SPAT_OCC_BG_ALL_INVERT_TAXA_ALA_PBI.rds'))


unique(SDM.SPAT.OCC.BG.GDA$searchTaxon) %in% target.insect.spp      %>% table()
unique(SDM.SPAT.OCC.BG.GDA$searchTaxon) %in% target.insect.genera   %>% table()
unique(SDM.SPAT.OCC.BG.GDA$searchTaxon) %in% target.insect.families %>% table()


## Look for missing taxa 
'Asteron'         %in% unique(SDM.SPAT.OCC.BG.GDA$searchTaxon)
'Graycassis'      %in% unique(SDM.SPAT.OCC.BG.GDA$searchTaxon)
'Venatrix'        %in% unique(SDM.SPAT.OCC.BG.GDA$searchTaxon)
'Mysticarion'     %in% unique(SDM.SPAT.OCC.BG.GDA$searchTaxon)
'Austrochloritis' %in% unique(SDM.SPAT.OCC.BG.GDA$searchTaxon)
'Diphyoropa'      %in% unique(SDM.SPAT.OCC.BG.GDA$searchTaxon)


## Also save a big table of just the background taxa
SDM.SPAT.OCC.BG.TARG.GDA <- SDM.SPAT.OCC.BG.GDA %>% .[.$searchTaxon %in% analysis_taxa, ]
SDM.SPAT.OCC.BG.TARG.FAM <- SDM.SPAT.OCC.BG.GDA %>% .[.$searchTaxon %in% target.insect.families, ]
SDM.SPAT.OCC.BG.TARG.GEN <- SDM.SPAT.OCC.BG.GDA %>% .[.$searchTaxon %in% target.insect.genera, ]
SDM.SPAT.OCC.BG.TARG.SPP <- SDM.SPAT.OCC.BG.GDA %>% .[.$searchTaxon %in% target.insect.spp, ]


st_write(SDM.SPAT.OCC.BG.TARG.FAM %>% st_as_sf(), 
         dsn   = file.path(getwd(), 
                           paste0(inv_results_dir, 'SDM_ALL_INVERT_FAMILIES_ALA_PBI.gpkg')), 
         layer = 'SDM_TARGET_INVERT_FAMILIES', 
         quiet = TRUE)


st_write(SDM.SPAT.OCC.BG.TARG.GEN %>% st_as_sf(), 
         dsn   = file.path(getwd(), paste0(inv_results_dir, 'SDM_ALL_INVERT_GENERA_ALA_PBI.gpkg')), 
         layer = 'SDM_TARGET_INVERT_GENERA', 
         quiet = TRUE)


st_write(SDM.SPAT.OCC.BG.TARG.SPP %>% st_as_sf(), 
         dsn   = file.path(getwd(), paste0(inv_results_dir, 'SDM_ALL_INVERT_SPECIES_ALA_PBI.gpkg')), 
         layer = 'SDM_TARGET_INVERT_SPECIES', 
         quiet = TRUE)


st_write(SDM.SPAT.OCC.BG.TARG.GDA %>% st_as_sf(), 
         dsn   = file.path(getwd(), paste0(inv_results_dir, 'SDM_ALL_INVERT_TAXA_ALA_PBI.gpkg')), 
         layer = 'SDM_TARGET_INVERT_TAXA', 
         quiet = TRUE)


## Loop through families
for(taxa in target.insect.families) {
  
  ## 
  if(taxa %in% unique(SDM.SPAT.OCC.BG.TARG.GDA$searchTaxon)) {
    
    taxa_shp <- paste0(inv_records_dir,
                       taxa, '_SDM_ALA_PBI_points.shp')
    
    if(!file.exists(taxa_shp)) {
      
      message('Subsetting ', taxa, ' shapefile and geo-package layers')
      taxa_occ <- SDM.SPAT.OCC.BG.TARG.GDA %>% .[.$searchTaxon %in% taxa, ]
      
      st_write(taxa_occ %>% st_as_sf(), 
               paste0(inv_records_dir, taxa, '_SDM_ALA_PBI_POINTS.shp'))
      
      st_write(taxa_occ %>% st_as_sf(), 
               dsn = paste0(inv_records_dir, 'SDM_INVERT_TARG_TAXA_ALA_PBI.gpkg'), 
               layer = paste0(taxa, '_SDM_points'), 
               quiet = TRUE)
      
    } else {
      message(taxa, ' SDM .shp already exists')}
    
  } else {
    message(taxa, ' has no data')}
}



## Loop through genera
for(taxa in target.insect.genera) {
  
  ## 
  if(taxa %in% unique(SDM.SPAT.OCC.BG.TARG.GDA$searchTaxon)) {
    
    taxa_shp <- paste0(inv_records_dir,
                       taxa, '_SDM_ALA_PBI_points.shp')
    
    if(!file.exists(taxa_shp)) {
      
      message('Subsetting ', taxa, ' shapefile and geo-package layers')
      taxa_occ <- SDM.SPAT.OCC.BG.TARG.GDA %>% .[.$searchTaxon %in% taxa, ]
      
      st_write(taxa_occ %>% st_as_sf(), 
               paste0(inv_records_dir, taxa, '_SDM_ALA_PBI_points.shp'))
      
      st_write(taxa_occ %>% st_as_sf(), 
               dsn = paste0(inv_records_dir, 'SDM_INVERT_TARG_TAXA__ALA_PBI.gpkg'), 
               layer = paste0(taxa, '_SDM_points'), 
               quiet = TRUE)
      
    } else {
      message(taxa, ' SDM .shp already exists')}
    
  } else {
    message(taxa, ' has no data')}
}





## Loop through species
for(taxa in target.insect.spp) {
  
  ## taxa = target.insect.spp[79] 
  if(taxa %in% unique(SDM.SPAT.OCC.BG.TARG.GDA$searchTaxon)) {
    
    taxa_shp <- paste0(inv_records_dir,
                       taxa, '_SDM_ALA_PBI_points.shp')
    
    if(!file.exists(taxa_shp)) {
      
      message('Subsetting ', taxa, ' shapefile and geo-package layers')
      taxa_occ <- SDM.SPAT.OCC.BG.TARG.GDA %>% .[.$searchTaxon %in% taxa, ]
      
      st_write(taxa_occ %>% st_as_sf(), 
               paste0(inv_records_dir, taxa, '_SDM_ALA_PBI_points.shp'))
      
      st_write(taxa_occ %>% st_as_sf(), 
               dsn = paste0(inv_records_dir, taxa, '_SDM_ALA_PBI_points.gpkg'), 
               layer = paste0(taxa, '_SDM_points'), 
               quiet = TRUE)
      
      st_write(taxa_occ %>% st_as_sf(), 
               dsn = paste0(inv_records_dir, 'SDM_INVERT_TARG_TAXA_ALA_PBI.gpkg'), 
               layer = paste0(taxa, '_SDM_points'), 
               quiet = TRUE)
      
    } else {
      message(taxa, ' SDM .shp already exists')}
    
  } else {
    message(taxa, ' has no data')}
}


## END =============================================================





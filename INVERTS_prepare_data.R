

## ENVIRONMENT SETTINGS =============================================================


# \
# 
# This code prepares all the data and code needed to analyse of inverts habitat after the 2019-2020 fires ::
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

inv_rs_dir           <- './output/invert_maxent_pbi_ala/'
inv_back_dir         <- './output/invert_maxent_pbi_ala/back_sel_models/'
inv_results_dir      <- './output/invert_maxent_pbi_ala/results/'

veg_dir              <- './data/Remote_sensing/Veg_data/Forest_cover/'
inv_habitat_dir      <- './output/invert_maxent_pbi_ala/Habitat_suitability/'
inv_records_dir      <- './output/invert_maxent_pbi_ala/Habitat_suitability/SDM_point_data/'
inv_inters_dir       <- './output/invert_maxent_pbi_ala/Habitat_suitability/SDM_Veg_intersect/'
inv_thresh_dir       <- './output/invert_maxent_pbi_ala/Habitat_suitability/SDM_thresholds/'
inv_fire_dir         <- './output/invert_maxent_pbi_ala/Habitat_suitability/FESM_SDM_intersect/'




dir_list <- c(tempdir, ALA_dir, 
              INV_dir, check_dir, out_dir, inv_rs_dir, inv_back_dir, inv_full_dir, inv_results_dir,
              veg_dir,
              inv_habitat_dir, inv_inters_dir, inv_thresh_dir, inv_fire_dir)


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
save_name   <- 'ALL_INVERT_TAXA_ALA_PBI_SITES'


# STEP 1 :: Get spp lists ----


## To Do :
## 1). Check the errors that Finlay may have found
## 2). Clean up the folders


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


## Full list of analysis taxa
## Full list of analysis taxa, 
analysis_taxa   <- str_trim(c(target.insect.spp, 
                              target.insect.genera, 
                              target.insect.families)) %>% unique()

taxa_qc <- read_excel(paste0(inv_habitat_dir, 'SDM_point_data/SDM_target_species.xlsx'),
                      sheet = 'Invert_QA_check')

taxa_remain <- taxa_qc %>% 
  filter(QC == "X") %>%
  filter(is.na(Morpho)) %>%
  dplyr::select(Binomial) %>% 
  .$Binomial %>% c(., 'Rayieria basifer') %>% sort()



## 
all_niche <- read_csv(paste0(inv_results_dir, '/AUS_INVERT_TAXA_ALL_NICHES.csv'))
taxa_diff <- setdiff(analysis_taxa, all_niche$searchTaxon) %>% sort()




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

## The below shold really all be one big table
## Where we use different columns for the analysis :: species, genus or family
Insects_ALA_1 <- read_csv('./data/ALA/Insects/records-2022-05-12.csv')
Insects_ALA_2 <- read_csv('./data/ALA/Insects/records-2022-05-12_part2.csv')

Insects_ALA <- Insects_ALA_1 %>% dplyr::select(-eventTime, -identificationID, -taxonID) %>%
  bind_rows(., dplyr::select(Insects_ALA_2, -eventTime, -identificationID, -taxonID))


ALA.LAND.INV.SPP <- format_ala_dump(ALA_table     = Insects_ALA,
                                    record_type   = "ALA",
                                    keep_cols     = ALA_keep,
                                    year_filt     = FALSE,
                                    unique_cells  = FALSE,
                                    world_raster  = aus_annual_precip)

rm(Insects_ALA)
rm(Insects_ALA_1)
rm(Insects_ALA_2)


## The below shold really all be one big table
## Where we use different columns for the analysis :: species, genus or family
SPIDERS  <- read_csv('./data/ALA/Spiders/SPIDERS-2022-06-02.csv')
MOLLUSCS <- read_csv('./data/ALA/Snails/MOLLUSCS-2022-06-02.csv')


Spiders_ALA <- SPIDERS %>% dplyr::select(., -'verbatimElevation...81', -'verbatimElevation...82') %>% 
  bind_rows(., dplyr::select(MOLLUSCS, -'verbatimElevation...81', -'verbatimElevation...82'))


ALA.LAND.SPID.SPP <- format_ala_dump(ALA_table     = Spiders_ALA,
                                     record_type   = "ALA",
                                     keep_cols     = ALA_keep,
                                     year_filt     = FALSE,
                                     unique_cells  = FALSE,
                                     world_raster  = aus_annual_precip)

rm(SPIDERS)
rm(MOLLUSCS)


ALA.LAND.INV.SPP <- bind_rows(ALA.LAND.INV.SPP, ALA.LAND.SPID.SPP)


ALA_LAND_INV_SPP_sf <- SpatialPointsDataFrame(coords      = ALA.LAND.INV.SPP %>% 
                                                dplyr::select(lon, lat) %>% as.matrix(),
                                              data        = ALA.LAND.INV.SPP,
                                              proj4string = CRS("+init=epsg:4326")) %>%
  
  st_as_sf() %>% 
  st_transform(., st_crs(4326))


## Prepare site data 
## Combine all the site data into one table
all_insect_pbi <- PBI_AUS %>%
  
  ## Remove the NA coordinates, and add a 'SOURCE' column
  completeFun(., c('lat', 'lon')) %>% 
  filter(lon < 180 & lat > -90) %>% 
  mutate(SOURCE = 'SITE')


## Create a SPDF for the SITE Data
all_insect_pbi_sf <- SpatialPointsDataFrame(coords      = all_insect_pbi %>% 
                                              dplyr::select(lon, lat) %>% as.matrix(),
                                            data        = all_insect_site_df_species,
                                            proj4string = CRS("+init=epsg:4326")) %>%
  
  st_as_sf() %>% 
  st_transform(., st_crs(4326))


COMBO.RASTER.PBI.SPP <- combine_records_extract(records_df       = all_insect_pbi_sf,
                                                add_sites        = FALSE,
                                                filter_taxo      = FALSE,
                                                
                                                site_df          = NA,
                                                thin_records     = FALSE,
                                                template_raster  = template_raster_250m,
                                                world_raster     = aus.climate.veg.grids.250m,
                                                epsg             = 4326,
                                                taxa_list        = taxa_diff,
                                                taxa_level       = 'family',   
                                                
                                                ## This might need to change too
                                                raster_divide    = FALSE,
                                                save_data        = FALSE,
                                                data_path        = inv_results_dir,
                                                save_run         = 'ALL_INV_SPP_PBI')


COMBO_RASTER_PBI_SPP_sf <- SpatialPointsDataFrame(coords      = COMBO.RASTER.PBI.SPP %>% 
                                                    dplyr::select(lon, lat) %>% as.matrix(),
                                                  data        = COMBO.RASTER.PBI.SPP,
                                                  proj4string = CRS("+init=epsg:4326")) %>%
  
  st_as_sf() %>% 
  st_transform(., st_crs(4326))


## Genera
COMBO.RASTER.PBI.GEN <- COMBO.RASTER.PBI.SPP %>%
  dplyr::select(-searchTaxon) %>% 
  dplyr::rename(searchTaxon = genus) %>% dplyr::select(searchTaxon, everything())


## Families
COMBO.RASTER.PBI.FAM <- COMBO.RASTER.PBI.SPP %>%
  dplyr::select(-searchTaxon)  %>% 
  dplyr::rename(searchTaxon = family) %>% dplyr::select(searchTaxon, everything())


COMBO.SPP.GEN.FAM.PBI <- bind_rows(COMBO.RASTER.PBI.SPP,
                                   COMBO.RASTER.PBI.GEN,
                                   COMBO.RASTER.PBI.FAM)

rm(COMBO.RASTER.PBI.SPP)
rm(COMBO.RASTER.PBI.GEN)
rm(COMBO.RASTER.PBI.FAM)
gc()





# Records data

## Update with the full insects download from ALA ----
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
GLOB.NICHE.ALL = calc_enviro_niches(coord_df     = COMBO.SPP.GEN.FAM.ALA.PBI %>% .[.$searchTaxon %in% taxa_diff, ],
                                    prj          = CRS("+init=epsg:4326"),
                                    country_shp  = AUS,
                                    world_shp    = LAND,
                                    kop_shp      = Koppen_shp,
                                    taxa_list    = taxa_diff,
                                    env_vars     = names(aus.climate.veg.grids.250m),
                                    cell_size    = 2,
                                    save_data    = TRUE,
                                    save_run     = save_name,
                                    data_path    = inv_results_dir)


plot_range_histograms(coord_df     = COMBO.SPP.GEN.FAM.ALA.PBI %>% .[.$searchTaxon %in% taxa_diff, ],
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
                                         taxa_list         = taxa_diff,
                                         
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
# SDM.SPAT.OCC.BG.GDA = readRDS('./output/invert_maxent_pbi_ala/results/SDM_SPAT_OCC_BG_ALL_TARGET_INSECT_TAXA.rds')
SDM.SPAT.OCC.BG.TARG.INV    <- SDM.SPAT.OCC.BG.GDA %>% .[.$searchTaxon %in% taxa_diff, ]
SDM.SPAT.OCC.BG.TARG.INV.SF <- SDM.SPAT.OCC.BG.TARG.INV %>% st_as_sf()


st_write(SDM.SPAT.OCC.BG.TARG.INV %>% st_as_sf(), 
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
SDM.SPAT.OCC.BG.TARG.GDA <- SDM.SPAT.OCC.BG.GDA %>% .[.$searchTaxon %in% taxa_diff, ]
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





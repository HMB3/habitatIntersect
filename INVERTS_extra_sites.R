

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

'%!in%' <- function(x,y)!('%in%'(x,y))


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
inv_back_dir_old     <- './output/invert_maxent_pbi_ala/back_sel_models/'
inv_results_dir      <- './output/invert_maxent_pbi_ala_site/results/'

veg_dir              <- './data/Remote_sensing/Veg_data/Forest_cover/'
inv_habitat_dir      <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/'
inv_records_dir      <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/SDM_point_data/'
inv_inters_dir       <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/SDM_Veg_intersect/'
inv_thresh_dir       <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/SDM_thresholds/'
inv_fire_dir         <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/FESM_SDM_intersect/'


dir_list <- c(tempdir, ALA_dir, 
              INV_dir, check_dir, out_dir, inv_rs_dir, inv_back_dir, inv_results_dir,
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

taxa_qc <- read_excel(paste0(inv_results_dir, 'SDM_target_species.xlsx'),
                      sheet = 'Invert_QA_check')

sdm_taxa <- read_excel(paste0(inv_results_dir, 'INVERTS_FIRE_SPATIAL_DATA_LUT_JUNE_2022.xlsm'),
                       sheet = 'Missing_taxa')

species_remain <- taxa_qc %>% 
  filter(grepl("Missing", Note)) %>%
  filter(is.na(Morpho)) %>%
  dplyr::select(Binomial) %>% 
  .$Binomial %>% sort()


taxa_remain <- sdm_taxa %>% 
  filter(Size == 0) %>%
  dplyr::select(Taxa) %>% 
  .$Taxa %>% sort() 


taxa_done <- sdm_taxa %>% 
  filter(Size > 0) %>%
  dplyr::select(Taxa) %>% 
  .$Taxa %>% sort()


taxa_difference <- c(taxa_remain, species_remain) %>% unique() %>% 
  sort() %>% gsub('_', ' ', .)
intersect(analysis_taxa, taxa_difference) %>% sort()


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


PBI_AUS_SITES <- read_tsv(file      = './data/Taxonomy/PBI_updated_dump_sorted.tsv', 
                          col_names = TRUE) %>% 
  
  ## Now clean up the data so it can be combined with the ALA
  mutate(searchTaxon = paste(Genus, species,  sep = " ")) %>%
  dplyr::rename(genus           = Genus,
                locality        = Locality,
                country         = Country,
                family          = Family,
                lat             = Lat,
                lon             = Lon,
                institutionCode = Inst_Code,
                recordedBy      = Collector,
                state           = State_Prov,
                basisOfRecord   = Coll_Method) %>% 
  
  mutate(plantTaxon = paste(Host_Genus, Host_species,  sep = " ")) %>% 
  
  ## Change this to the order of the clean columns   
  dplyr::select(searchTaxon, one_of(site_cols))




## What are the taxa in the PBI sites
SITE_spp    <- PBI_AUS_SITES$searchTaxon %>% unique() %>% sort()
SITE_genus  <- PBI_AUS_SITES$genus       %>% unique() %>% sort()
SITE_family <- PBI_AUS_SITES$family      %>% unique() %>% sort()
SITE_taxa   <- c(SITE_spp, SITE_genus, SITE_family)

re_analyse_spp  <- intersect(analysis_taxa, SITE_spp)
re_analyse_gen  <- intersect(analysis_taxa, SITE_genus)
re_analyse_fam  <- intersect(analysis_taxa, SITE_family)
re_analyse_taxa <- c(re_analyse_spp, re_analyse_gen, re_analyse_fam) %>% sort()


## only get the old site data that doesn't overlap with the new site data
data('PBI_AUS')
PBI_AUS_UNIQUE <- PBI_AUS %>% 
  
  filter(searchTaxon %!in% re_analyse_taxa)


PBI_AUS_SITES_UNIQUE <- bind_rows(PBI_AUS_UNIQUE,
                                  PBI_AUS_SITES)






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



## Prepare site data 
## Combine all the site data into one table
all_insect_pbi_sites <- PBI_AUS_SITES_UNIQUE %>%
  
  ## Remove the NA coordinates, and add a 'SOURCE' column
  completeFun(., c('lat', 'lon')) %>% 
  filter(lon < 180 & lat > -90) %>% 
  mutate(SOURCE = 'SITE')


## Create a SPDF for the SITE Data
all_insect_pbi_sites_sf <- SpatialPointsDataFrame(coords      = all_insect_pbi_sites %>% 
                                                    dplyr::select(lon, lat) %>% as.matrix(),
                                                  data        = all_insect_pbi_sites,
                                                  proj4string = CRS("+init=epsg:4326")) %>%
  
  st_as_sf() %>% 
  st_transform(., st_crs(4326))


COMBO.RASTER.PBI.SPP <- combine_records_extract(records_df       = all_insect_pbi_sites_sf,
                                                add_sites        = FALSE,
                                                filter_taxo      = FALSE,
                                                
                                                site_df          = NA,
                                                thin_records     = FALSE,
                                                template_raster  = template_raster_250m,
                                                world_raster     = aus.climate.veg.grids.250m,
                                                epsg             = 4326,
                                                taxa_list        = re_analyse_taxa,
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





# STEP 4 :: Prepare SDM table ----


##
if(coord_clean) {
  
  message('clean coordinates')
  COORD.CLEAN = coord_clean_records(records      = COMBO.SPP.GEN.FAM.PBI,
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
  COMBO.SPP.GEN.FAM.PBI$coord_summary <- TRUE
  
  
  # Calc updated niches with new data ----
  
  
  ##
  GLOB.NICHE.ALL = calc_enviro_niches(coord_df     = COMBO.SPP.GEN.FAM.PBI %>% .[.$searchTaxon %in% re_analyse_taxa, ],
                                      prj          = CRS("+init=epsg:4326"),
                                      country_shp  = AUS,
                                      world_shp    = LAND,
                                      kop_shp      = Koppen_shp,
                                      taxa_list    = re_analyse_taxa,
                                      env_vars     = names(aus.climate.veg.grids.250m),
                                      cell_size    = 2,
                                      save_data    = TRUE,
                                      save_run     = save_name,
                                      data_path    = inv_results_dir)
  
  
  plot_range_histograms(coord_df     = COMBO.SPP.GEN.FAM.PBI %>% .[.$searchTaxon %in% re_analyse_taxa, ],
                        taxa_list    = target.insect.genera,
                        range_path   = check_dir)
  
  
  COORD_CLEAN_sf <- SpatialPointsDataFrame(coords      = COMBO.SPP.GEN.FAM.PBI %>% 
                                             dplyr::select(lon, lat) %>% as.matrix(),
                                           data        = COMBO.SPP.GEN.FAM.PBI,
                                           proj4string = CRS("+init=epsg:4326")) %>%
    
    st_as_sf() %>% 
    st_transform(., st_crs(4326))
}


## Combine occ data with the bg data 
SDM.DATA.ALL <- COORD_CLEAN_sf %>%
  
  dplyr::mutate(SPAT_OUT = 'TRUE',
                order    = '',
                class    = '',
                phylum   = '',
                kingdom  = '',
                year     = 2021) %>% 
  
  dplyr::select(one_of(c('searchTaxon', 
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
                         names(aus.climate.veg.grids.250m)))) %>% 
  
  st_transform(., st_crs(3577)) %>% 
  dplyr::select(-lat, -lon)


## Convert lat/lon to eastings and northings for projected coordinate system
SDM.COORDS     <- st_coordinates(SDM.DATA.ALL)
SDM.DATA.ALL$X <- SDM.COORDS[,"X"]
SDM.DATA.ALL$Y <- SDM.COORDS[,"Y"]





## Create subset of target reptiles
## Save each taxa as an individual shapefile
saveRDS(SDM.DATA.ALL,   paste0(inv_results_dir, 'SDM_SPAT_OCC_BG_',  save_run, '.rds'))
write_csv(SDM.DATA.ALL, paste0(inv_results_dir, 'SDM_SPAT_OCC_BG_',  save_run, '.csv'))


st_write(SDM.DATA.ALL %>% st_as_sf(), 
         dsn   = paste0(inv_results_dir, save_name, '.gpkg'), 
         layer = 'SDM_TARGET_INVERT_TAXA_PBI_SITES', 
         quiet = TRUE)

gc()


message('sdm data preparation code successfuly run')





# STEP 5 :: Check data ----


## Make a list of the taxa that were not found at the sites. Move this to the new directory
## Make a list of the taxa that were not found at the sites
non_site_taxa_folders <- setdiff(analysis_taxa, SITE_taxa) %>% 
  setdiff(., taxa_difference) %>% 
  sort() %>% gsub(' ', '_', .) 


inv_thresh_sdms_ras <- list.dirs(path       = inv_back_dir_old, 
                                 recursive  = FALSE,
                                 full.names = TRUE) %>% 
  .[grep(non_site_taxa_folders, .)]

file.copy(from      = inv_thresh_sdms_ras, 
          to        = inv_thresh_dir, 
          overwrite = TRUE, 
          recursive = TRUE, 
          copy.mode = TRUE)



## Check the species data - 34 target species are in the final data
## These then drop out due to cross-validation, etc. 
SDM.SPAT.OCC.BG.GDA    <- readRDS(paste0(inv_results_dir,   
                                         'SDM_SPAT_OCC_BG_ALL_INVERT_TAXA_ALA_PBI_SITES.rds'))


unique(SDM.SPAT.OCC.BG.GDA$searchTaxon) %in% target.insect.spp      %>% table()
unique(SDM.SPAT.OCC.BG.GDA$searchTaxon) %in% target.insect.genera   %>% table()
unique(SDM.SPAT.OCC.BG.GDA$searchTaxon) %in% target.insect.families %>% table()



## Also save a big table of just the background taxa
SDM.SPAT.OCC.BG.TARG.GDA <- SDM.SPAT.OCC.BG.GDA %>% .[.$searchTaxon %in% analysis_taxa, ]
SDM.SPAT.OCC.BG.TARG.FAM <- SDM.SPAT.OCC.BG.GDA %>% .[.$searchTaxon %in% target.insect.families, ]
SDM.SPAT.OCC.BG.TARG.GEN <- SDM.SPAT.OCC.BG.GDA %>% .[.$searchTaxon %in% target.insect.genera, ]
SDM.SPAT.OCC.BG.TARG.SPP <- SDM.SPAT.OCC.BG.GDA %>% .[.$searchTaxon %in% target.insect.spp, ]



## Loop through species
for(taxa in re_analyse_spp) {
  
  ## taxa = target.insect.spp[79] 
  if(taxa %in% unique(SDM.SPAT.OCC.BG.TARG.GDA$searchTaxon)) {
    
    taxa_shp <- paste0(inv_records_dir,
                       taxa, '_SDM_ALA_PBI_SITES_points.shp')
    
    if(!file.exists(taxa_shp)) {
      
      message('Subsetting ', taxa, ' shapefile and geo-package layers')
      taxa_occ <- SDM.SPAT.OCC.BG.TARG.GDA %>% .[.$searchTaxon %in% taxa, ]
      
      st_write(taxa_occ %>% st_as_sf(), 
               paste0(inv_records_dir, taxa, '_SDM_ALA_PBI_SITES_points.shp'))
      
      st_write(taxa_occ %>% st_as_sf(), 
               dsn = paste0(inv_records_dir, taxa, '_SDM_ALA_PBI_SITES_points.gpkg'), 
               layer = paste0(taxa, '_SDM_points'), 
               quiet = TRUE)
      
      st_write(taxa_occ %>% st_as_sf(), 
               dsn = paste0(inv_records_dir, 'SDM_INVERT_TARG_TAXA_ALA_PBI_SITES.gpkg'), 
               layer = paste0(taxa, '_SDM_points'), 
               quiet = TRUE)
      
    } else {
      message(taxa, ' SDM .shp already exists')}
    
  } else {
    message(taxa, ' has no data')}
}


## END =============================================================





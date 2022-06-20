

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
inv_results_dir      <- './output/invert_maxent_pbi_ala_site/results/'

plant_rs_dir         <- './output/plant_maxent_raster_update/'
plant_back_dir       <- './output/plant_maxent_raster_update/back_sel_models/'
plant_results_dir    <- './output/plant_maxent_raster_update/results/'
plant_thresh_dir     <- './output/plant_maxent_raster_update/Habitat_suitability/SDM_thresholds/'

veg_dir              <- './data/Remote_sensing/Veg_data/Forest_cover/'
inv_habitat_dir      <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/'
inv_inters_dir       <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/SDM_Veg_intersect/'
inv_thresh_dir       <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/SDM_thresholds/'
inv_fire_dir         <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/FESM_SDM_intersect/'

intersect_veg        <- FALSE
cross_tab_veg        <- FALSE


dir_list <- c(tempdir, ALA_dir, 
              INV_dir, check_dir, out_dir, inv_rs_dir, inv_back_dir, inv_results_dir,
              plant_rs_dir, plant_back_dir, plant_results_dir, veg_dir,
              inv_habitat_dir, inv_inters_dir, inv_thresh_dir, inv_fire_dir,
              plant_thresh_dir)


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





## 1). CREATE SPECIES LISTS =============================================================

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

taxa_qc       <- read_excel(paste0(inv_results_dir, 'SDM_target_species.xlsx'),
                            sheet = 'Invert_QA_check')

sdm_taxa      <- read_excel(paste0(inv_results_dir, 'INVERTS_FIRE_SPATIAL_DATA_LUT_JUNE_2022.xlsm'),
                            sheet = 'Missing_taxa')

species_remain <- taxa_qc %>% 
  filter(grepl("Missing", Note)) %>%
  filter(is.na(Morpho)) %>%
  dplyr::select(Binomial) %>% 
  .$Binomial %>% sort()


taxa_remain <- sdm_taxa %>% 
  filter(Size == 0) %>%
  dplyr::select(Taxa) %>% 
  .$Taxa %>% sort() %>% gsub('_', ' ', .,)


taxa_difference <- c(taxa_remain, species_remain) %>% unique() %>% sort() %>% gsub('_', ' ', .)
intersect(analysis_taxa, taxa_difference) %>% sort()
setdiff(analysis_taxa, taxa_difference)   %>% sort()


host_taxa_updated <- read_excel(paste0(inv_results_dir, 'INVERST_HSM_CHECK.xlsx'),
                                sheet = 'All_host_plants') %>% 
  select(searchTaxon) %>% .$searchTaxon %>% 
  sort()


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


re_analyse_spp  <- intersect(analysis_taxa, SITE_spp)
re_analyse_gen  <- intersect(analysis_taxa, SITE_genus)
re_analyse_fam  <- intersect(analysis_taxa, SITE_family)
re_analyse_taxa <- c(re_analyse_spp, re_analyse_gen, re_analyse_fam) %>% sort()





## 2). LOAD VEGETATION RASTER DATA =============================================================


## Read in the SDM data
## This function aggregates the results for models that ran successfully
## Update the  
SITES.MAXENT.RESULTS     <- compile_sdm_results(taxa_list    = analysis_taxa,
                                                results_dir  = inv_back_dir,
                                                data_path    = inv_results_dir,
                                                sdm_path     = inv_back_dir,
                                                save_data    = FALSE,
                                                save_run     = 'INVERT_ALL_TAXA_ALA_PBI_SITES')


SPID.MAXENT.RESULTS      <- compile_sdm_results(taxa_list    = taxa_difference,
                                                results_dir  = inv_back_dir,
                                                data_path    = inv_results_dir,
                                                sdm_path     = inv_back_dir,
                                                save_data    = FALSE,
                                                save_run     = 'INVERT_SPIDER_TAXA_ALA_PBI_SITES')


SITES.ALL.MAXENT.RESULTS <- bind_rows(SITES.MAXENT.RESULTS, 
                                      SPID.MAXENT.RESULTS %>% .[.$searchTaxon %in% 
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


PLANT.MAXENT.RESULTS      <- compile_sdm_results(taxa_list    = host_taxa_updated,
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
invert_map_taxa <- SITES.ALL.MAXENT.RESULTS$searchTaxon  %>% gsub(" ", "_", .,)
invert_map_spp  <- INVERT.MAXENT.SPP.RESULTS$searchTaxon %>% gsub(" ", "_", .,)
plant_map_taxa  <- PLANT.MAXENT.RESULTS$searchTaxon      %>% gsub(" ", "_", .,)



## FESM   : https://datasets.seed.nsw.gov.au/dataset/fire-extent-and-severity-mapping-fesm
## VALUES : 1-4, burn intensity from 2019-2020 fires, originally @ 10m resolution, re-sampled to 100m
template_raster_250m <- raster('./data/CSIRO_layers/250m/AUS/Extra/Annual_precip_GDA_ALB.tif')


## Read in feature layers for fire that have been repaired in ArcMap
SDM.SPAT.OCC.BG.GDA        <- readRDS(paste0(inv_results_dir,   
                                             'SDM_COMBINED_ALL_INVERT_SPIDERS_ALA_PBI_SITES.rds'))

FESM_east_20m_categ        <- st_read('./data/Remote_sensing/FESM/NBR_Burn_severity_classes.shp')      %>% st_cast(., "POLYGON")
FESM_east_20m_binary_split <- readRDS('./data/Remote_sensing/FESM/Fire_perimeters_split.rds')          %>% st_cast(., "POLYGON")
AUS_forest_RS_feat_split   <- readRDS(paste0(veg_dir,'Aus_forest_cover_east_coast_classes_split.rds')) %>% st_cast(., "POLYGON")


## Now make a 200km grid of the FESM layer
FESM_200km_grid <- st_make_grid(FESM_east_20m_categ, 200000)





## 3). INTERSECT SDMs WITH VEG RASTER =============================================================


# \
# 
# To use the habitat suitability rasters in area calculations (e.g. comparing the area of suitable habitat
# affected by fire), we need to convert the continuous suitability scores (ranging from 0-1) to binary values
# (either 1, or 0). To do this, we need to pick a threshold of habitat suitability, below which the species 
# is not considered present. Here we have chosen the 10th% Logistic threshold for each taxa (ref).
# 
# \


intersect_cols      <- c("searchTaxon", "species", "genus", "family", "SOURCE", "gridcode", "Vegetation")
million_metres      <- 1000000


## Select the Vegetation pixels that intersect with the records of each invertebrate species
if(intersect_veg) {
  
  taxa_records_habitat_features_intersect(analysis_df    = SDM.SPAT.OCC.BG.GDA,
                                          taxa_list      = rev(taxa_difference),
                                          taxa_level     = 'species',
                                          habitat_poly   = AUS_forest_RS_feat_split,
                                          int_cols       = intersect_cols,
                                          output_path    = inv_inters_dir,
                                          buffer         = 5000,
                                          raster_convert = FALSE,
                                          save_shp       = FALSE,
                                          save_png       = FALSE,
                                          poly_path      = 'data/Feature_layers/Boundaries/AUS_2016_AUST.shp',
                                          epsg           = 3577)
  
  gc()
  
  
  
  taxa_records_habitat_features_intersect(analysis_df    = SDM.SPAT.OCC.BG.GDA,
                                          taxa_list      = re_analyse_taxa,
                                          taxa_level     = 'species',
                                          habitat_poly   = AUS_forest_RS_feat_split,
                                          int_cols       = intersect_cols,
                                          output_path    = inv_inters_dir,
                                          buffer         = 5000,
                                          raster_convert = FALSE,
                                          save_shp       = FALSE,
                                          save_png       = FALSE,
                                          poly_path      = 'data/Feature_layers/Boundaries/AUS_2016_AUST.shp',
                                          epsg           = 3577)
  
  gc()
  
  
  taxa_records_habitat_features_intersect(analysis_df    = SDM.SPAT.OCC.BG.GDA,
                                          taxa_list      = analysis_taxa,
                                          taxa_level     = 'species',
                                          habitat_poly   = AUS_forest_RS_feat_split,
                                          int_cols       = intersect_cols,
                                          output_path    = inv_inters_dir,
                                          buffer         = 5000,
                                          raster_convert = FALSE,
                                          save_shp       = FALSE,
                                          save_png       = FALSE,
                                          poly_path      = 'data/Feature_layers/Boundaries/AUS_2016_AUST.shp',
                                          epsg           = 3577)
  
  gc()
  
}

## Now also intersect the whole SDM layer with the Veg layer, creating a cross-tab of habitat
if(cross_tab_veg) {
  
  SDM.TARG.INVERT.POINTS <- SDM.SPAT.OCC.BG.GDA %>% .[.$searchTaxon %in% analysis_taxa, ] %>%
    dplyr::select(searchTaxon, X, Y) %>%
    st_transform(., st_crs(3577)) %>% st_as_sf() %>% st_subdivide()
  
  
  sdm_veg_crosstab          <- st_intersection(SDM.TARG.INVERT.POINTS,
                                               AUS_forest_RS_feat_split)
  
  saveRDS(sdm_veg_crosstab, paste0(veg_dir,'SDM_POINTS_VEG_CROSSTAB.rds'))
  
  
  ## Read it back in
  # sdm_veg_crosstab  <- readRDS(paste0(veg_dir,'sdm_veg_crosstab.rds'))
  
  sdm_veg_crosstab_df <- sdm_veg_crosstab  %>% as_tibble() %>%
    dplyr::select(searchTaxon, Vegetation) %>%
    group_by(searchTaxon, Vegetation)      %>%
    tally() %>%
    
    mutate(Percent = round(n / sum(n) *100, 2))
  
  write_csv(sdm_veg_crosstab_df, paste0(inv_results_dir, 'SDM_POINTS_VEG_CROSSTAB.csv'))
  
}





## 4). ESTIMATE % BURNT OVERALL FOR EACH TAXA =============================================================


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
host_plants <- read_excel(paste0(inv_results_dir, '/INVERST_HSM_CHECK.xlsx'),
                          sheet = 'HSM_check') %>% filter(!is.na(HostTaxon)) %>%
  dplyr::select(searchTaxon, HostTaxon, HostTaxon2, HostTaxon3, HostTaxon4)


PLANT.RESULTS.HOSTS <- PLANT.MAXENT.RESULTS %>%
  
  rename(HostTaxon = "searchTaxon") %>%
  full_join(., host_plants, by = "HostTaxon") %>%
  dplyr::select(searchTaxon, HostTaxon, HostTaxon2, HostTaxon3, HostTaxon4, everything()) %>%
  rename(host_dir = results_dir) %>% distinct() 


INVERT.RESULTS.HOSTS <- SITES.ALL.MAXENT.RESULTS %>%
  
  left_join(., select(PLANT.RESULTS.HOSTS,
                      "searchTaxon",
                      "HostTaxon",
                      "HostTaxon2",
                      "HostTaxon3",
                      "HostTaxon4",
                      "host_dir"), 
            by = "searchTaxon")


# INVERT.RESULTS.HOSTS.ALL <- INVERT.RESULTS.HOSTS %>% 
#   
#   bind_rows(., PLANT.RESULTS.HOSTS %>% .[.$searchTaxon %in% 
#                                            setdiff(PLANT.RESULTS.HOSTS$searchTaxon, 
#                                                    INVERT.RESULTS.HOSTS$searchTaxon), ])


# For each Invertebrate species, calculate the % of suitable habitat that was burnt by the
# 2019-2020 fires. We can do this by combining pixels in the rasters like this: 
#   
# 
# - [Invert_SDM + Host_plant_SDM + Inv Veg pixels] * Fire_layer 
# 
# 
# This will give us the % of suitable habitat in each burn intensity category(0-5).


## Calculate Insect habitat within binary fire layer
# calculate_taxa_habitat_host_features(taxa_list          = sort(INVERT.MAXENT.SPP.RESULTS$searchTaxon),
#                                      targ_maxent_table  = INVERT.RESULTS.HOSTS,
#                                      host_maxent_table  = PLANT.RESULTS.HOSTS,
#                                      
#                                      target_path        = inv_back_dir,
#                                      output_path        = inv_fire_dir,
#                                      thresh_path        = inv_thresh_dir,
#                                      intersect_path     = inv_inters_dir,
#                                      intersect_patt     = '_SDM_VEG_intersection.gpkg',
#                                      host_path          = plant_thresh_dir,
#                                      thresh_patt        = '_current_suit_not_novel_above_',
#                                      
#                                      int_cols           = intersect_cols,
#                                      main_int_layer     = FESM_east_20m_binary_split,
#                                      second_int_layer   = AUS_forest_RS_feat_split,
#                                      template_raster    = template_raster_250m,
#                                      poly_path          = 'data/Feature_layers/Boundaries/AUS_2016_AUST.shp',
#                                      epsg               = 3577)
# 
# gc()


# calculate_taxa_habitat_host_features(taxa_list          = rev(INVERT.MAXENT.GEN.RESULTS$searchTaxon),
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
#                                      second_int_layer   = AUS_forest_RS_feat_split,
#                                      template_raster    = template_raster_250m,
#                                      poly_path          = 'data/Feature_layers/Boundaries/AUS_2016_AUST.shp',
#                                      epsg               = 3577)
# 
# gc()


# calculate_taxa_habitat_host_features(taxa_list          = rev(INVERT.MAXENT.FAM.RESULTS$searchTaxon),
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
#                                      second_int_layer   = AUS_forest_RS_feat_split,
#                                      template_raster    = template_raster_250m,
#                                      poly_path          = 'data/Feature_layers/Boundaries/AUS_2016_AUST.shp',
#                                      epsg               = 3577)
# 
# gc()


# For each taxa, we create a table of the area in square kilometers of suitable habitat that intersects with each burn 
# intensity category from the FESM fire intensity layer. Let's combine all those tables together, to create a master 
# table of estimated burnt area.


## 5). ESTIMATE % BURNT OVERALL FOR TAXA WITHOUT RECORDS =============================================================


## How do we add the plant SDMs together for those inverts without records, but with host plants?





## 6). ESTIMATE % BURNT OVERALL FOR EACH TAXA =============================================================


## Try individual reads

## file <- sort(fire_intersect_feat_path)[3] 
poly <- st_read('data/Feature_layers/Boundaries/AUS_2016_AUST.shp') %>% 
  st_transform(., st_crs(3577)) %>% as_Spatial()

message('Use GEOS geometry for sf operations to speed up intersections')
sf_use_s2(FALSE)



for(taxa in rev(INVERT.MAXENT.SPP.RESULTS$searchTaxon)[1:5]) {
  
  ## taxa <- rev(INVERT.MAXENT.SPP.RESULTS$searchTaxon)[1]
  save_name     <- gsub(' ', '_', taxa)
  target_table  <- INVERT.MAXENT.SPP.RESULTS %>%
    filter(searchTaxon == taxa)
  
  ## First do the straight intersect of SDM with VEG
  ## Get the sdm threshold for each inv taxa
  target_thresh <- INVERT.MAXENT.SPP.RESULTS %>%
    filter(searchTaxon == taxa)       %>%
    dplyr::select(Logistic_threshold) %>%
    distinct() %>% .[1, ] %>% .[[1]]
  
  current_thresh_feat_path <- list.files(path       = inv_thresh_dir,
                                         pattern    = '_current_suit_not_novel_above_', 
                                         recursive  = TRUE,
                                         full.names = TRUE) %>% 
    .[grep(".gpkg", .)] %>% .[grep(save_name, .)]
  
  occ                <- readRDS(sprintf('%s/%s/%s_occ.rds', 
                                        inv_back_dir, save_name, save_name))
  
  sdm_fire_geo       <- paste0(inv_fire_dir, save_name, '_sdm_fire_intersect.gpkg')
  sdm_fire_png       <- paste0(inv_fire_dir, save_name, '_SDM_VEG_intersect_Fire_Categories.png')
  
  if(!file.exists(sdm_fire_png)) {
    
    ## Read in the SDM threshold
    sdm_threshold <- st_read(sdm_fire_geo) %>% st_cast(., "POLYGON") %>% repair_geometry()
    extent_dim    <- extent(sdm_threshold)[1]
    
    if(!is.na(extent_dim)) {
      
      ## Calculate area of the SDM - don't need the fire area
      sdm_areas     <- st_area(sdm_threshold)/million_metres
      sdm_areas_km2 <- drop_units(sdm_areas) 
      sdm_area_km2  <- drop_units(sdm_areas) %>% sum()
      gc()
      
      ## read in the intersect
      layers <- st_layers(dsn = sdm_fire_geo)$name
      
      message('Intersecting SDM with Grid of categorical Fire layers for ', taxa)
      ## 5.10pm
      FESM_200km_grid <- st_make_grid(FESM_east_20m_categ, 200000)
      FESM_grid_sdm   <- st_intersection(FESM_200km_grid, sdm_threshold)
      
      message('Intersecting SDM with categorical Fire layers for ', taxa)
      sdm_fire_classes_int    <- st_intersection(FESM_grid_sdm, FESM_east_20m_categ)
      gc()
      
      sdm_fire_classes_areas_m2  <- st_area(sdm_fire_classes_int)/million_metres
      sdm_fire_classes_areas_km2 <- drop_units(sdm_fire_classes_areas_m2)
      sdm_fire_classes_area_km2  <- drop_units(sdm_fire_classes_areas_m2) %>% sum()
      gc()
      
      ## create sf attributes for each intersecting polygon
      sdm_fire_classes_int_att <- sdm_fire_classes_int %>% 
        
        mutate(Taxa                = taxa,
               Area_poly           = st_area(geom)/million_metres,
               Area_poly           = drop_units(Area_poly),
               Percent_burnt_class = (Area_poly/sdm_area_km2 * 100 %>% round(., 1)))
      
      ## Aggregate the sdm * fire * classes areas into veg classes
      sdm_fire_classes_int <- sdm_fire_classes_int_att %>%
        
        st_set_geometry(NULL) %>% 
        dplyr::select(Taxa, Burn_Categ, Area_poly, Percent_burnt_class) %>% 
        group_by(Taxa, Burn_Categ) %>% 
        summarise(Area_poly           = sum(Area_poly),
                  Percent_burnt_class = sum(Percent_burnt_class))
      
      ## calc % burnt within classes classes
      percent_burnt_classes_overall <- sum(sdm_fire_classes_int$Percent_burnt_class)
      percent_burnt_classes_class   <- sdm_fire_classes_int$Area_poly/
        sdm_fire_classes_area_km2 * 100 %>% round(., 1)
      
      ## Create a tibble of overall areas for each taxon
      ## Include the SDM area in each fire classs here
      class_length <- unique(sdm_fire_classes_int$Burn_Categ) %>% length()
      sdm_fire_classes_areas <- data.frame(matrix(NA, 
                                                  ncol = 4, 
                                                  nrow = class_length))
      
      colnames(sdm_fire_classes_areas) <- c('Taxa', 
                                            'Burn_Category', 
                                            'Burn_Category_burnt_area',  
                                            'Burn_Category_burnt_perc')
      
      sdm_fire_classes_areas <- sdm_fire_classes_areas %>% 
        
        mutate(Taxa                     = taxa,
               Burn_Category            = unique(sdm_fire_classes_int$Burn_Categ),
               Burn_Category_burnt_area = sdm_fire_classes_int$Area_poly,
               Burn_Category_burnt_perc = percent_burnt_classes_class)
      
      ## Save the % burnt layers
      write.csv(sdm_fire_classes_areas,  
                paste0(inv_fire_dir, save_name, '_SDM_VEG_intersect_Fire.csv'), row.names = FALSE)
      gc()
      
      ## Now save the thresh-holded rasters as shapefiles
      message('Saving SDM Fire intersect polygons for ', taxa)
      
      st_write(sdm_fire_classes_int_att, 
               
               dsn    = sdm_fire_geo, 
               layer  = paste0(save_name, '_sdm_fire_classes_intersect_sub'),
               
               quiet  = TRUE,
               append = FALSE)
      gc()
      
      ## Create rasters for plotting
      t <- raster::raster(template_raster_250m) %>% 
        raster::crop(., extent(FESM_east_20m_categ))
      
      current_thresh_ras <- fasterize(sdm_threshold,   t) %>% 
        raster::crop(., extent(FESM_east_20m_categ))
      
      fire_layer_ras <- fasterize(FESM_east_20m_categ, 
                                  field = 'gridcode', t) %>% 
        raster::crop(., extent(FESM_east_20m_categ))
      
      message('writing threshold png for ', taxa)
      png(paste0(inv_fire_dir, save_name, '_SDM_VEG_intersect_Fire_Categories.png'),
          6, 12, units = 'in', res = 400)
      
      plot(fire_layer_ras,                                legend = TRUE)
      plot(current_thresh_ras, add = TRUE, col = 'green', legend = FALSE)
      plot(poly, add = TRUE)
      
      title(main = taxa, 
            sub  = paste0(round(percent_burnt_classes_overall, 2), 
                          " % Suitable habitat Burnt"))
      dev.off()
      gc()
      
    } else {
      message('SDM threshold has no east coast data ', taxa, ' skip')
      cat(taxa)
    }
    
  } else {
    message('SDM fire threshold already done for ', taxa, ' skip')
    cat(taxa)
  }
} 

## Calculate Insect habitat within categorical fire layer
# calculate_habitat_categories_intersect(taxa_list          = rev(INVERT.MAXENT.SPP.RESULTS$searchTaxon),
#                                        targ_maxent_table  = INVERT.RESULTS.HOSTS,
#                                        
#                                        target_path        = inv_back_dir,
#                                        output_path        = inv_fire_dir,
#                                        
#                                        habitat_layer      = AUS_forest_RS_feat_split,
#                                        category_layer     = FESM_east_20m_categ,
#                                        habitat_col        = "Vegetation",
#                                        category_col       = "Burn_Categ",
#                                        intersect_habitat  = FALSE,
#                                        
#                                        template_raster    = template_raster_250m,
#                                        poly_path          = 'data/Feature_layers/Boundaries/AUS_2016_AUST.shp',
#                                        epsg               = 3577)
# 
# gc()




message('sdm fire intersect run succsessfully for ', length(INVERT.MAXENT.SPP.RESULTS$searchTaxon), 'taxa')









## END =============================================================


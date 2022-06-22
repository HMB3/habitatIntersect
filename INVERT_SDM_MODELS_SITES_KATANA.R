


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
inv_full_dir         <- './output/invert_maxent_pbi_ala_site/full_models/'
inv_results_dir      <- './output/invert_maxent_pbi_ala_site/results/'


veg_dir              <- './data/Remote_sensing/Veg_data/Forest_cover/'
inv_habitat_dir      <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/'
inv_inters_dir       <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/SDM_Veg_intersect/'
inv_thresh_dir       <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/SDM_thresholds/'
inv_fire_dir         <- './output/invert_maxent_raster_update/Habitat_suitability/FESM_SDM_intersect/'


dir_list <- c(tempdir, ALA_dir, 
              INV_dir, check_dir, out_dir, inv_rs_dir, inv_back_dir, inv_full_dir, inv_results_dir,
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






## 1). LOAD RASTER DATA =============================================================

# \
# 
# Load raster data at 280m resolution.
# 
# \

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





## 2). TAXA =============================================================



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


## Full list of analysis taxa -
analysis_taxa   <- str_trim(c(target.insect.spp, 
                              target.insect.genera, 
                              target.insect.families)) %>% unique() %>% sort()

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
  .$Taxa %>% sort() %>% gsub('_', ' ', .,)


taxa_done <- sdm_taxa %>% 
  filter(Size > 0) %>%
  dplyr::select(Taxa) %>% 
  .$Taxa %>% sort()


taxa_difference <- c(taxa_remain, species_remain) %>% unique() %>% sort()
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


re_analyse_spp  <- intersect(analysis_taxa, SITE_spp)
re_analyse_gen  <- intersect(analysis_taxa, SITE_genus)
re_analyse_fam  <- intersect(analysis_taxa, SITE_family)
re_analyse_taxa <- c(re_analyse_spp, re_analyse_gen, re_analyse_fam) %>% sort()


## only get the old site data that doesn't overlap with the new site data
PBI_AUS_UNIQUE <- PBI_AUS %>% 
  
  filter(searchTaxon %!in% re_analyse_taxa)


PBI_AUS_SITES_UNIQUE <- bind_rows(PBI_AUS_UNIQUE,
                                  PBI_AUS_SITES)





## 3). RUN SDM ANALYSIS =============================================================


## Read in the SDM data
SDM.SPAT.OCC.BG.GDA      <- readRDS(paste0(inv_results_dir,   
                                           'SDM_SPAT_OCC_BG_ALL_INVERT_TAXA_ALA_PBI.rds'))

drops <- c("SPOUT.OBS") # list of col names
SDM.SPAT.OCC.BG.GDA      <- SDM.SPAT.OCC.BG.GDA[,!(names(SDM.SPAT.OCC.BG.GDA) %in% drops)]


SDM.SPAT.OCC.BG.SITE.GDA <- readRDS(paste0(inv_results_dir,   
                                           'SDM_SPAT_OCC_BG_ALL_INVERT_TAXA_ALA_PBI_SITES.rds')) %>% 
  as_Spatial()


SDM.SPAT.OCC.BG.SPID.GDA <- readRDS(paste0(inv_results_dir,   
                                           'SDM_SPAT_OCC_BG_ALL_SPIDER_TAXA_ALA_PBI_SITES.rds')) 
SDM.SPAT.OCC.BG.SPID.GDA <- SDM.SPAT.OCC.BG.SPID.GDA[,!(names(SDM.SPAT.OCC.BG.SPID.GDA) %in% drops)]


## Bind them together
SDM.SPAT.OCC.BG.PBI.SITE.GDA <- rbind(SDM.SPAT.OCC.BG.GDA,
                                      SDM.SPAT.OCC.BG.SITE.GDA,
                                      SDM.SPAT.OCC.BG.SPID.GDA)


## What is the breakdown of spp?
table(target.insect.spp      %in% SDM.SPAT.OCC.BG.PBI.SITE.GDA$searchTaxon)
table(target.insect.genera   %in% SDM.SPAT.OCC.BG.PBI.SITE.GDA$searchTaxon)
table(target.insect.families %in% SDM.SPAT.OCC.BG.PBI.SITE.GDA$searchTaxon)


## Run family-level models for invertebrates.
run_sdm_analysis_no_crop(taxa_list               = sort(taxa_difference),
                         taxa_level              = 'species',
                         maxent_dir              = inv_back_dir,
                         bs_dir                  = inv_back_dir,
                         sdm_df                  = SDM.SPAT.OCC.BG.PBI.SITE.GDA,
                         sdm_predictors          = names(aus.climate.veg.grids.250m),
                         
                         backwards_sel           = TRUE,
                         template_raster         = template_raster_250m,
                         cor_thr                 = 0.8,
                         pct_thr                 = 5,
                         k_thr                   = 4,
                         min_n                   = 10,
                         max_bg_size             = 100000,
                         background_buffer_width = 100000,
                         feat_save               = TRUE,
                         features                = 'lpq',
                         replicates              = 5,
                         responsecurves          = TRUE,
                         poly_path               = 'data/Feature_layers/Boundaries/AUS_2016_AUST.shp',
                         epsg                    = 3577)


## Run family-level models for invertebrates.
run_sdm_analysis_no_crop(taxa_list               = sort(taxa_difference),
                         taxa_level              = 'family',
                         maxent_dir              = inv_back_dir,
                         bs_dir                  = inv_back_dir,
                         sdm_df                  = SDM.SPAT.OCC.BG.PBI.SITE.GDA,
                         sdm_predictors          = names(aus.climate.veg.grids.250m),
                         
                         backwards_sel           = TRUE,
                         template_raster         = template_raster_250m,
                         cor_thr                 = 0.8,
                         pct_thr                 = 5,
                         k_thr                   = 4,
                         min_n                   = 10,
                         max_bg_size             = 100000,
                         background_buffer_width = 100000,
                         feat_save               = TRUE,
                         features                = 'lpq',
                         replicates              = 5,
                         responsecurves          = TRUE,
                         poly_path               = 'data/Feature_layers/Boundaries/AUS_2016_AUST.shp',
                         epsg                    = 3577)


gc()


## Run genus-level models for invertebrates.
run_sdm_analysis_no_crop(taxa_list               = rev(sort(target.insect.genera)),
                         taxa_level              = 'genus',
                         maxent_dir              = inv_back_dir,
                         bs_dir                  = inv_back_dir,
                         sdm_df                  = SDM.SPAT.OCC.BG.PBI.SITE.GDA,
                         sdm_predictors          = names(aus.climate.veg.grids.250m),
                         
                         backwards_sel           = TRUE,
                         template_raster         = template_raster_250m,
                         cor_thr                 = 0.8,
                         pct_thr                 = 5,
                         k_thr                   = 4,
                         min_n                   = 10,
                         max_bg_size             = 100000,
                         background_buffer_width = 100000,
                         feat_save               = TRUE,
                         features                = 'lpq',
                         replicates              = 5,
                         responsecurves          = TRUE,
                         poly_path               = 'data/Feature_layers/Boundaries/AUS_2016_AUST.shp',
                         epsg                    = 3577)


gc()


## Run species-level models for invertebrates
run_sdm_analysis_no_crop(taxa_list               = rev(sort(re_analyse_spp)),
                         taxa_level              = 'species',
                         maxent_dir              = inv_back_dir,     
                         bs_dir                  = inv_back_dir,
                         sdm_df                  = SDM.SPAT.OCC.BG.PBI.SITE.GDA,
                         sdm_predictors          = names(aus.climate.veg.grids.250m),
                         
                         backwards_sel           = TRUE,      
                         template_raster         = template_raster_250m,
                         cor_thr                 = 0.8,  
                         pct_thr                 = 5, 
                         k_thr                   = 4, 
                         min_n                   = 10,  
                         max_bg_size             = 100000,
                         background_buffer_width = 100000,
                         feat_save               = TRUE,
                         features                = 'lpq',
                         replicates              = 5,
                         responsecurves          = TRUE,
                         poly_path               = 'data/Feature_layers/Boundaries/AUS_2016_AUST.shp',
                         epsg                    = 3577)

gc()





## 4). PROJECT SDMs =============================================================


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
                                                 results_dir  = inv_back_dir,
                                                 data_path    = inv_results_dir,
                                                 sdm_path     = inv_back_dir,
                                                 save_data    = TRUE,
                                                 save_run     = "INVERT_ALL_TAXA_ALA_PBI")


INVERT.MAXENT.FAM.RESULTS <- compile_sdm_results(taxa_list    = target.insect.families,
                                                 results_dir  = inv_back_dir,
                                                 data_path    = inv_habitat_dir,
                                                 sdm_path     = inv_back_dir,
                                                 save_data    = FALSE,
                                                 save_run     = "INVERT_ANALYSIS_TAXA")


INVERT.MAXENT.GEN.RESULTS <- compile_sdm_results(taxa_list    = target.insect.genera,
                                                 results_dir  = inv_back_dir,
                                                 data_path    = inv_habitat_dir,
                                                 sdm_path     = inv_back_dir,
                                                 save_data    = FALSE,
                                                 save_run     = "INVERT_ANALYSIS_TAXA")


INVERT.MAXENT.SPP.RESULTS <- compile_sdm_results(taxa_list    = target.insect.spp,
                                                 results_dir  = inv_back_dir,
                                                 data_path    = inv_habitat_dir,
                                                 sdm_path     = inv_back_dir,
                                                 save_data    = FALSE,
                                                 save_run     = "INVERT_ANALYSIS_TAXA")


## How many target taxa were modelled?
nrow(INVERT.MAXENT.SPP.RESULTS)/length(target.insect.spp)      *100 
nrow(INVERT.MAXENT.GEN.RESULTS)/length(target.insect.genera)   *100
nrow(INVERT.MAXENT.FAM.RESULTS)/length(target.insect.families) *100


## Get map_taxa from the maxent results table above, change the species column,
## then create a list of logistic thresholds
invert_map_taxa <- INVERT.MAXENT.RESULTS$searchTaxon     %>% gsub(" ", "_", .,)
invert_map_spp  <- INVERT.MAXENT.SPP.RESULTS$searchTaxon %>% gsub(" ", "_", .,)



# The projection function takes the maxent models created by the 'fit_maxent_targ_bg_back_sel' function, 
# and projects the models across geographic space - currently just for Australia. It uses the rmaxent 
# package https://github.com/johnbaums/rmaxent. It assumes that the maxent models were generated by the 
# 'fit_maxent_targ_bg_back_sel' function. Note that this step is quite memory heavy, best run with > 64GB of RAM.
# setdiff(host_plant_taxa, PLANT.MAXENT.RESULTS$searchTaxon)

## Project SDMs across the Study area for the invert taxa
tryCatch(
  
  project_maxent_current_grids_mess(taxa_list       = rev(invert_map_taxa),    
                                    maxent_path     = inv_back_dir,
                                    current_grids   = east.climate.veg.grids.250m,         
                                    create_mess     = TRUE,
                                    mess_layers     = FALSE,
                                    save_novel_poly = TRUE,
                                    maxent_table    = INVERT.MAXENT.RESULTS,
                                    output_path     = paste0(inv_thresh_dir, 'inverts_sdm_novel_combo.gpkg'),
                                    poly_path       = 'data/Feature_layers/Boundaries/AUS_2016_AUST.shp',
                                    epsg            = 3577),
  
  ## If the species fails, write a fail message to file
  error = function(cond) {
    
    ## This will write the error message inside the text file, but it won't include the species
    file.create(file.path(inv_back_dir, "mapping_failed_current.txt"))
    cat(cond$message, file = file.path(inv_back_dir, "inv_mapping_failed_current.txt"))
    warning(cond$message)
    
  })


tryCatch(
  
  project_maxent_current_grids_mess(taxa_list       = invert_map_spp,    
                                    maxent_path     = inv_back_dir,
                                    current_grids   = east.climate.veg.grids.250m,         
                                    create_mess     = TRUE,
                                    mess_layers     = FALSE,
                                    save_novel_poly = TRUE,
                                    output_path     = paste0(inv_thresh_dir, 'inverts_sdm_novel_combo.gpkg'),
                                    poly_path       = 'data/Feature_layers/Boundaries/AUS_2016_AUST.shp',
                                    epsg            = 3577),
  
  ## If the species fails, write a fail message to file
  error = function(cond) {
    
    ## This will write the error message inside the text file, but it won't include the species
    file.create(file.path(inv_back_dir, "mapping_failed_current.txt"))
    cat(cond$message, file = file.path(inv_back_dir, "inv_mapping_failed_current.txt"))
    warning(cond$message)
    
  })




## 5). THRESHOLD SDMs =============================================================


# \
# 
# To use the habitat suitability rasters in area calculations (e.g. comparing the area of suitable habitat
#                                                              affected by fire), we need to convert the continuous suitability scores (ranging from 0-1) to binary values
# (either 1, or 0). To do this, we need to pick a threshold of habitat suitability, below which the species 
# is not considered present. Here we have chosen the 10th% Logistic threshold for each taxa (ref).
# 
# 
# \


## Now copy the thresh-holded SDM rasters to stand alone folder (i.e. all taxa in one folder)
inv_thresh_sdms_ras <- list.files(path       = inv_back_dir,
                                  pattern    = '_current_suit_not_novel_above_', 
                                  recursive  = TRUE,
                                  full.names = TRUE) %>% .[grep(".tif", .)]


inv_thresh_sdms_feat <- list.files(path       = inv_back_dir,
                                   pattern    = '_current_suit_not_novel_above_', 
                                   recursive  = TRUE,
                                   full.names = TRUE) %>% .[grep(".gpkg", .)] 



file.copy(from      = inv_thresh_sdms_ras, 
          to        = inv_thresh_dir, 
          overwrite = TRUE, 
          recursive = TRUE, 
          copy.mode = TRUE)


file.copy(from      = inv_thresh_sdms_feat, 
          to        = inv_thresh_dir, 
          overwrite = TRUE, 
          recursive = TRUE, 
          copy.mode = TRUE)



message('sdm code successfuly run')





## END =============================================================

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






## 1). COLLATE % BURNT FOR ALL TAXA =============================================================


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


## Update the  
INVERT.MAXENT.RESULTS <- compile_sdm_results(taxa_list    = analysis_taxa,
                                             results_dir  = inv_back_dir,
                                             data_path    = inv_results_dir,
                                             sdm_path     = inv_back_dir,
                                             save_data    = TRUE,
                                             save_run     = 'INVERT_ALL_TAXA_ALA_PBI')



## Get the list of files
INVERT.FESM.list    <- list.files('./output/invert_maxent_pbi_ala/Habitat_suitability/FESM_SDM_intersect/', 
                                  pattern     = '_SDM_intersect_Fire.csv', 
                                  full.names  = TRUE, 
                                  recursive   = TRUE) 

INVERT.FESM.VEG.list <- list.files('./output/invert_maxent_pbi_ala/Habitat_suitability/FESM_SDM_intersect/', 
                                   pattern     = 'VEG_intersect_Fire.csv', 
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




## Subset to just the analysis species - some species did not process properly?
INVERT.FESM.TABLE <-  INVERT.FESM.TABLE[INVERT.FESM.TABLE$Taxa %in%
                                          sort(unique(INVERT.MAXENT.RESULTS$searchTaxon)) , ] %>%
  .[complete.cases(.), ]


INVERT.FESM..VEG.TABLE <-  INVERT.FESM.TABLE[INVERT.FESM.VEG.TABLE$Taxa %in%
                                          sort(unique(INVERT.MAXENT.RESULTS$searchTaxon)) , ] %>%
  .[complete.cases(.), ]


## How many taxa have been processed?
length(unique(INVERT.FESM.TABLE$Taxa))
View(INVERT.FESM.TABLE)
View(INVERT.FESM.VEG.TABLE)



## Also save a big table of just the background taxa
INVERT.FESM.TABLE.FAM <- INVERT.FESM.TABLE %>% .[.$Taxa %in% target.insect.families, ]
INVERT.FESM.TABLE.GEN <- INVERT.FESM.TABLE %>% .[.$Taxa %in% target.insect.genera, ]
INVERT.FESM.TABLE.SPP <- INVERT.FESM.TABLE %>% .[.$Taxa %in% target.insect.spp, ]



## Save the FESM intersect results to file
write_csv(INVERT.FESM.TABLE,
          paste0(inv_results_dir, '/INVERT_TAXA_SDM_intersect_Fire_ALA_PBI.csv'))

write_csv(INVERT.FESM.VEG.TABLE,
          paste0(inv_results_dir, '/INVERT_TAXA_SDM_VEG_intersect_Fire_ALA_PBI.csv'))





## 2). SAVE TAXA TO DATABASE =============================================================


## Read in the geopackage, so we can save the results
SDM_ALL_INVERT_TAXA_ALA_PBI <- paste0(inv_results_dir, 'SDM_ALL_INVERT_TAXA_ALA_PBI.gpkg')


st_write(INVERT.FESM.TABLE, 
         
         dsn    = SDM_ALL_INVERT_TAXA_ALA_PBI, 
         layer  = 'INVERT_TAXA_SDM_intersect_Fire_ALA_PBI',
         
         quiet  = TRUE,
         append = FALSE)


st_write(INVERT.FESM.VEG.TABLE, 
         
         dsn    = SDM_ALL_INVERT_TAXA_ALA_PBI, 
         layer  = 'INVERT_TAXA_SDM_intersect_Fire_ALA_PBI',
         
         quiet  = TRUE,
         append = FALSE)



## 2). CREATE HABITAT LOSS GRAPHS OF =============================================================


## 



## END =============================================================
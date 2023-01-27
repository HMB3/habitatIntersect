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
library(cowplot)
data('sdmgen_packages')
ipak(sdmgen_packages)


## The functions expect these folders,
inv_rs_dir           <- './output/invert_maxent_pbi_ala_site/'
inv_back_dir         <- './output/invert_maxent_pbi_ala_site/back_sel_models/'
inv_results_dir      <- './output/invert_maxent_pbi_ala_site/results/'
inv_fire_dir         <- './output/invert_maxent_pbi_ala_site/Habitat_suitability/FThanESM_SDM_intersect/'





## 1). TAXA LISTS =============================================================


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
                              target.insect.families)) %>% unique()

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

taxa_qc    <- read_excel(paste0(inv_results_dir, 'SDM_target_species.xlsx'),
                         sheet = 'Invert_QA_check')

sdm_taxa   <- read_excel(paste0(inv_results_dir, 'INVERTS_FIRE_SPATIAL_DATA_LUT_JUNE_2022.xlsm'),
                         sheet = 'Outstanding_taxa')

invert_LUT <- read_excel(paste0(inv_results_dir, 'INVERTS_FIRE_SPATIAL_DATA_LUT_JUNE_2022.xlsm'),
                         sheet = 'TABLE 3') %>% dplyr::select("searchTaxon",       
                                                              "Aus_records",       
                                                              "AOO",               
                                                              "EOO",               
                                                              "KOP_count",         
                                                              "Type") %>% rename(Taxa = searchTaxon)

table(invert_LUT$Type)

species_remain <- taxa_qc %>% 
  filter(grepl("Missing", Note)) %>%
  filter(is.na(Morpho)) %>%
  dplyr::select(Binomial) %>% 
  .$Binomial %>% sort()


taxa_remain <- sdm_taxa %>% 
  dplyr::select(Taxa) %>% 
  .$Taxa %>% sort() %>% gsub('_', ' ', .,)

taxa_difference <- c(taxa_remain, species_remain) %>% unique() %>% sort()
intersect(analysis_taxa, taxa_difference) %>% sort()



invert_LUT <- read_excel(paste0(inv_results_dir, 'INVERTS_FIRE_SPATIAL_DATA_LUT_JUNE_2022.xlsm'),
                         sheet = 'TABLE 3') %>% dplyr::select("searchTaxon",       
                                                              "Aus_records",       
                                                              "AOO",               
                                                              "EOO",               
                                                              "KOP_count",         
                                                              "Type") %>% rename(Taxa = searchTaxon)

table(invert_LUT$Type)







## 2). TAXA NICHES AND SDM RESULTS =============================================================


## Combine Niches
ALL_INVERT_TAXA_ALA_PBI_NICHES   <- read_csv(paste0(inv_results_dir, 'GLOBAL_NICHES_ALL_INVERT_TAXA_ALA_PBI.csv'))
ALL_INVERT_TAXA_ALA_SITE_NICHES  <- read_csv(paste0(inv_results_dir, 'GLOBAL_NICHES_ALL_INVERT_TAXA_ALA_PBI_SITES.csv'))
ALL_INVERT_TAXA_ALA_SPID_NICHES  <- read_csv(paste0(inv_results_dir, 'GLOBAL_NICHES_ALL_SPIDER_TAXA_ALA_PBI_SITES.csv'))
ALL_INVERT_TAXA_ALA_DIFF_NICHES  <- read_csv(paste0(inv_results_dir, 'GLOBAL_NICHES_DIFFERENCE_INVERT_TAXA_ALA_PBI.csv'))


ALL_INVERT_TAXA_ALA_PBI_NICHES_UNI  <- ALL_INVERT_TAXA_ALA_PBI_NICHES %>% 
  .[.$searchTaxon %in% 
      setdiff(ALL_INVERT_TAXA_ALA_PBI_NICHES$searchTaxon, 
              ALL_INVERT_TAXA_ALA_SITE_NICHES$searchTaxon), ]


ALL_INVERT_TAXA_ALA_SITE_NICHES_UNI <- ALL_INVERT_TAXA_ALA_SITE_NICHES %>% 
  .[.$searchTaxon %in% 
      setdiff(ALL_INVERT_TAXA_ALA_SITE_NICHES$searchTaxon, 
              ALL_INVERT_TAXA_ALA_PBI_NICHES_UNI$searchTaxon), ]


ALL_INVERT_TAXA_ALL_NICHES_UNI      <- bind_rows(ALL_INVERT_TAXA_ALA_PBI_NICHES_UNI, 
                                                 ALL_INVERT_TAXA_ALA_SITE_NICHES_UNI)


ALL_INVERT_TAXA_ALA_SPID_NICHES_UNI <- ALL_INVERT_TAXA_ALA_SPID_NICHES %>% 
  
  .[.$searchTaxon %in% 
      setdiff(ALL_INVERT_TAXA_ALA_SPID_NICHES$searchTaxon, 
              ALL_INVERT_TAXA_ALL_NICHES_UNI$searchTaxon), ]


ALL_INVERT_TAXA_ALL_NICHES_UNI      <- bind_rows(ALL_INVERT_TAXA_ALL_NICHES_UNI,
                                                 ALL_INVERT_TAXA_ALA_SPID_NICHES_UNI,
                                                 ALL_INVERT_TAXA_ALA_DIFF_NICHES) 


write_csv(ALL_INVERT_TAXA_ALL_NICHES_UNI,
          paste0(inv_results_dir, '/AUS_INVERT_TAXA_ALL_NICHES.csv'))


## Combine the SDM results  
SITES.MAXENT.RESULTS    <- compile_sdm_results(taxa_list    = analysis_taxa,
                                               results_dir  = inv_back_dir,
                                               data_path    = inv_results_dir,
                                               sdm_path     = inv_back_dir,
                                               save_data    = FALSE,
                                               save_run     = 'INVERT_ALL_TAXA_ALA_PBI_SITES')


SPID.MAXENT.RESULTS     <- compile_sdm_results(taxa_list    = taxa_difference,
                                               results_dir  = inv_back_dir,
                                               data_path    = inv_results_dir,
                                               sdm_path     = inv_back_dir,
                                               save_data    = FALSE,
                                               save_run     = 'INVERT_SPIDER_TAXA_ALA_PBI_SITES')


SITES.ALL.MAXENT.RESULTS <- bind_rows(SITES.MAXENT.RESULTS, 
                                      SPID.MAXENT.RESULTS %>% 
                                        
                                        .[.$searchTaxon %in% 
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


## Save combined maxent results
write_csv(SITES.ALL.MAXENT.RESULTS,
          paste0(inv_results_dir, '/MAXENT_RESULTS_INSECTS_SPID_TAXA_ALA_PBI_SITES.csv'))


## How many target taxa completed?
nrow(INVERT.MAXENT.SPP.RESULTS)/length(target.insect.spp)      *100 
nrow(INVERT.MAXENT.GEN.RESULTS)/length(target.insect.genera)   *100
nrow(INVERT.MAXENT.FAM.RESULTS)/length(target.insect.families) *100


## What are the missing species?
setdiff(target.insect.spp,    INVERT.MAXENT.SPP.RESULTS$searchTaxon)
setdiff(target.insect.genera, INVERT.MAXENT.GEN.RESULTS$searchTaxon)




## 3). COMBINE TABLES =============================================================


## Get the list of files
INVERT.FESM.list    <- list.files(inv_fire_dir, 
                                  pattern     = '_SDM_intersect_Fire.csv', 
                                  full.names  = TRUE, 
                                  recursive   = TRUE) 


INVERT.FESM.VEG.list <- list.files(inv_fire_dir, 
                                   pattern     = 'VEG_intersect_Fire.csv', 
                                   full.names  = TRUE, 
                                   recursive   = TRUE)


INVERT.FESM.CLASS.list <- list.files(inv_fire_dir, 
                                     pattern     = 'VEG_intersect_Fire_Classes.csv', 
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



INVERT.FESM.CLASS.TABLE <- INVERT.FESM.CLASS.list %>%
  
  ## pipe the list into lapply
  lapply(function(x) {
    
    ## create the character string
    f <- paste0(x)
    
    ## load each .RData file
    d <- read.csv(f)
    if(nrow(d) < 1)       {
      sdm_fire_class <- data.frame(matrix(NA, 
                                          ncol = 4, 
                                          nrow = 1))
      
      
      colnames(sdm_fire_class) <- c('Taxa', 
                                    'Burn_Category', 
                                    'Burn_Category_burnt_area',  
                                    'Burn_Category_burnt_perc')
      
      sdm_fire_class$Taxa                     <- 'None'
      sdm_fire_class$Burn_Category            <- 'None'
      sdm_fire_class$Burn_Category_burnt_area <- 0
      sdm_fire_class$Burn_Category_burnt_perc <- 0
      d <- sdm_fire_class
    }
    d
    
  }) %>%
  
  ## bind together
  bind_rows() %>% filter(Burn_Category_burnt_area > 0)




## Subset to just the analysis species - some species did not process properly?
INVERT.FESM.TABLE <-  INVERT.FESM.TABLE[INVERT.FESM.TABLE$Taxa %in%
                                          sort(unique(SITES.ALL.MAXENT.RESULTS$searchTaxon)) , ] %>%
  .[complete.cases(.), ] %>% 
  
  mutate(Habitat_km2       = round(Habitat_km2, 2),
         Habitat_burnt_km2 = round(Habitat_burnt_km2, 2),
         Percent_burnt     = round(Percent_burnt, 2)) %>% 
  
  left_join(., invert_LUT %>% select(Taxa, Type), by = "Taxa")


INVERT.FESM.VEG.TABLE <-  INVERT.FESM.VEG.TABLE[INVERT.FESM.VEG.TABLE$Taxa %in%
                                                  sort(unique(SITES.ALL.MAXENT.RESULTS$searchTaxon)) , ] %>%
  .[complete.cases(.), ] %>% 
  
  mutate(Habitat_Veg_burnt_area = round(Habitat_Veg_burnt_area, 2),
         Habitat_Veg_burnt_perc = round(Habitat_Veg_burnt_perc, 2)) %>% 
  
  left_join(., invert_LUT %>% select(Taxa, Type), by = "Taxa")


INVERT.FESM.CLASS.TABLE <-  INVERT.FESM.CLASS.TABLE[INVERT.FESM.CLASS.TABLE$Taxa %in%
                                                      sort(unique(SITES.ALL.MAXENT.RESULTS$searchTaxon)) , ] %>%
  .[complete.cases(.), ] %>% 
  
  mutate(Burn_Category_burnt_area = round(Burn_Category_burnt_area, 2),
         Burn_Category_burnt_perc = round(Burn_Category_burnt_perc, 2)) %>% 
  
  left_join(., invert_LUT %>% select(Taxa, Type), by = "Taxa")



## How many taxa have been processed?
length(unique(INVERT.FESM.TABLE$Taxa))
View(INVERT.FESM.TABLE)
View(INVERT.FESM.VEG.TABLE)
View(INVERT.FESM.CLASS.TABLE)



## Also save a big table of just the background taxa
INVERT.FESM.TABLE.FAM <- INVERT.FESM.TABLE %>% .[.$Taxa %in% target.insect.families, ]
INVERT.FESM.TABLE.GEN <- INVERT.FESM.TABLE %>% .[.$Taxa %in% target.insect.genera, ]
INVERT.FESM.TABLE.SPP <- INVERT.FESM.TABLE %>% .[.$Taxa %in% target.insect.spp, ]


INVERT.FESM.VEG.TABLE.FAM <- INVERT.FESM.VEG.TABLE %>% .[.$Taxa %in% target.insect.families, ]
INVERT.FESM.VEG.TABLE.GEN <- INVERT.FESM.VEG.TABLE %>% .[.$Taxa %in% target.insect.genera, ]
INVERT.FESM.VEG.TABLE.SPP <- INVERT.FESM.VEG.TABLE %>% .[.$Taxa %in% target.insect.spp, ]

INVERT.FESM.CLASS.TABLE.FAM <- INVERT.FESM.CLASS.TABLE %>% .[.$Taxa %in% target.insect.families, ]
INVERT.FESM.CLASS.TABLE.GEN <- INVERT.FESM.CLASS.TABLE %>% .[.$Taxa %in% target.insect.genera, ]
INVERT.FESM.CLASS.TABLE.SPP <- INVERT.FESM.CLASS.TABLE %>% .[.$Taxa %in% target.insect.spp, ]



## 4). SAVE TABLES =============================================================


## Save to CSV ----
write_csv(INVERT.FESM.TABLE,
          paste0(inv_results_dir, '/INVERT_TAXA_SDM_intersect_Fire_ALA_PBI_SITE.csv'))

write_csv(INVERT.FESM.TABLE.FAM,
          paste0(inv_results_dir, '/INVERT_FAM_SDM_intersect_Fire_ALA_PBI_SITE.csv'))

write_csv(INVERT.FESM.TABLE.GEN,
          paste0(inv_results_dir, '/INVERT_GEN_SDM_intersect_Fire_ALA_PBI_SITE.csv'))

write_csv(INVERT.FESM.TABLE.SPP,
          paste0(inv_results_dir, '/INVERT_SPP_SDM_intersect_Fire_ALA_PBI_SITE.csv'))


write_csv(INVERT.FESM.VEG.TABLE,
          paste0(inv_results_dir, '/INVERT_TAXA_SDM_VEG_intersect_Fire_ALA_PBI_SITE.csv'))

write_csv(INVERT.FESM.VEG.TABLE.FAM,
          paste0(inv_results_dir, '/INVERT_FAM_SDM_VEG_intersect_Fire_ALA_PBI_SITE.csv'))

write_csv(INVERT.FESM.VEG.TABLE.GEN,
          paste0(inv_results_dir, '/INVERT_GEN_SDM_VEG_intersect_Fire_ALA_PBI_SITE.csv'))

write_csv(INVERT.FESM.VEG.TABLE.SPP,
          paste0(inv_results_dir, '/INVERT_SPP_SDM_VEG_intersect_Fire_ALA_PBI_SITE.csv'))


write_csv(INVERT.FESM.CLASS.TABLE,
          paste0(inv_results_dir, '/INVERT_TAXA_SDM_CLASS_intersect_Fire_ALA_PBI_SITE.csv'))

write_csv(INVERT.FESM.CLASS.TABLE.FAM,
          paste0(inv_results_dir, '/INVERT_FAM_SDM_CLASS_intersect_Fire_ALA_PBI_SITE.csv'))

write_csv(INVERT.FESM.CLASS.TABLE.GEN,
          paste0(inv_results_dir, '/INVERT_GEN_SDM_CLASS_intersect_Fire_ALA_PBI_SITE.csv'))

write_csv(INVERT.FESM.CLASS.TABLE.SPP,
          paste0(inv_results_dir, '/INVERT_SPP_SDM_CLASS_intersect_Fire_ALA_PBI_SITE.csv'))



## Save to geo-package ----
SDM_ALL_INVERT_TAXA_ALA_PBI    <- paste0(inv_results_dir, 'SDM_ALL_INVERT_TAXA_ALA_PBI.gpkg')


st_write(ALL_INVERT_TAXA_ALA_PBI_NICHES, 
         
         dsn    = SDM_ALL_INVERT_TAXA_ALA_PBI, 
         layer  = 'INVERT_TAXA_NICHES_ALA_PBI_SITES',
         
         quiet  = TRUE,
         append = TRUE)


st_write(SITES.ALL.MAXENT.RESULTS, 
         
         dsn    = SDM_ALL_INVERT_TAXA_ALA_PBI, 
         layer  = 'INVERT_TAXA_MAXENT_RESULTS_ALA_PBI_SITES',
         
         quiet  = TRUE,
         append = TRUE)


st_write(INVERT.FESM.TABLE, 
         
         dsn    = SDM_ALL_INVERT_TAXA_ALA_PBI, 
         layer  = 'INVERT_TAXA_SDM_intersect_Fire_ALA_PBI_SITES',
         
         quiet  = TRUE,
         append = TRUE)


st_write(INVERT.FESM.VEG.TABLE, 
         
         dsn    = SDM_ALL_INVERT_TAXA_ALA_PBI, 
         layer  = 'INVERT_TAXA_SDM_intersect_Veg_Fire_ALA_PBI_SITES',
         
         quiet  = TRUE,
         append = FALSE)



## Check database has the new layers
st_layers(dsn = SDM_ALL_INVERT_TAXA_ALA_PBI)$name





## END =============================================================
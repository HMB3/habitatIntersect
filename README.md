nenswniche : rapidly estimate environmental ranges for invertebrates and
plants in NE-NSW
================
March 2021

  

The text and code below summarises a workflow in R that can be used to
relatively rapidly assess the environmental range of a species within
Australia, from downloading occurrence records, through to creating maps
of predicted climatic suitability across Australia at 1km\*1km
resolution. An example of this work is published in the journal Science
of the Total Environment ::

  

Burley, H., Beaumont, L.J., Ossola, A., et al. (2019) Substantial
declines in urban tree habitat predicted under climate change. Science
of The Total Environment, 685, 451-462.

<https://www.sciencedirect.com/science/article/pii/S0048969719323289#f0030>

  

To install, run :

``` r
## Function to load or install packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos="https://cran.csiro.au/")
  sapply(pkg, require, character.only = TRUE)
}

## These packages are not on cran, but are needed
# library(devtools)
# install_github('johnbaums/things')
# install_github('johnbaums/rmaxent')
# install_github('traitecoevo/taxonlookup')

## Main package 
# devtools::install_github("HMB3/nenswniche")

## Load packages 
library(nenswniche)
data('sdmgen_packages')
ipak(sdmgen_packages)
```

  
  
  

# Background

This code is being developed at UNSW, to help investigate the impacts of
the 2019/2020 bush fires on Insects in the North East Forests of New
South Wales. The aim is to create a pipeline that rapidly assesses the
habitat suitability of the threatened insect species under current
environmental conditions.

  

# Run SDMs Aross Australia

  

Start the wiki from here, using the pre-prepared data

The next process is to run species distribution models using global
records of each species. The sdm function runs two maxent models: a full
model using all variables, and backwards selection. Given a candidate
set of predictor variables, the backwards selection function identifies
a subset of variables that meets specified multi-collinearity criteria.
Subsequently, backward step-wise variable selection is used to
iteratively drop the variable that contributes least to the model, until
the contribution of each variable meets a specified minimum, or until a
predetermined minimum number of predictors remains.

  

``` r
## SDM.ALL.PLA.SPP.BG = readRDS('./output/results/SDM_SPAT_OCC_BG_ALL_PLANT_SPP.rds')
## SDM.ALL.INS.FAM.BG = readRDS('./output/results/SDM_SPAT_OCC_BG_ALL_INSECT_FAMILIES.rds') %>% rbind(., SDM.ALL.PLA.SPP.BG)
## SDM.ALL.INS.GEN.BG = readRDS('./output/results/SDM_SPAT_OCC_BG_ALL_INSECT_GENERA.rds') %>% rbind(., SDM.ALL.PLA.SPP.BG)
## SDM.ALL.INS.SPP.BG = readRDS('./output/results/SDM_SPAT_OCC_BG_ALL_INSECT_SPP.rds') %>% rbind(., SDM.ALL.PLA.SPP.BG)


## To Do :
## 1). Search Errors - which ones are real?
## 3). Delete error folders
## 4). Re-Run
## 5). Get Gerry and Ryan to check the taxonomy


## Clean up the folders which have failed
run_sdm_analysis(species_list            = rev(analysis_taxa),
                 maxent_dir              = 'output/plant_maxent/full_models',     
                 bs_dir                  = 'output/plant_maxent/back_sel_models',
                 sdm_df                  = SDM.ALL.PLA.SPP.BG,
                 sdm_predictors          = names(aus.climate.veg.grids),
                 
                 backwards_sel           = TRUE,      
                 template_raster         = template_raster_1km_mol,
                 cor_thr                 = 0.8,  
                 pct_thr                 = 5, 
                 k_thr                   = 4, 
                 min_n                   = 20,  
                 max_bg_size             = 70000,
                 background_buffer_width = 200000,
                 shapefiles              = TRUE,
                 features                = 'lpq',
                 replicates              = 5,
                 responsecurves          = TRUE,
                 country_shp             = AUS,
                 koppen_crop             = TRUE,
                 Koppen_zones            = Koppen_zones,
                 Koppen_raster           = Koppen_1975_1km)
```

  

# Project SDMs across Australia

  

The next stage of the process is to project the SDM predictions across
geographic space. First, we need to extract the SDM results from the
models. Each model generates a ‘threshold’ of probability of occurrence
(see ref), which we use to create map of habitat suitability across
Australia ().

  

``` r
## Create a table of maxent results
## This function aggregates the results for models that ran successfully
MAXENT.RESULTS = compile_sdm_results(species_list = analysis_taxa,
                                     results_dir  = 'output/plant_maxent/back_sel_models',
                                     save_data    = FALSE,
                                     data_path    = "./output/plant_maxent/summary_results/",
                                     sdm_path     = "./output/plant_maxent/back_sel_models/",
                                     save_run     = "ALL_INSECTS")

## Get map_taxa from the maxent results table above, change the species column,
## then create a list of logistic thresholds
map_taxa        <- MAXENT.RESULTS$searchTaxon %>% gsub(" ", "_", .,)
percent.10.log  <- MAXENT.RESULTS$Logistic_threshold
sdm.results.dir <- MAXENT.RESULTS$results_dir
```

  

The projection function takes the maxent models created by the
‘fit\_maxent\_targ\_bg\_back\_sel’ function, and projects the models
across geographic space - currently just for Australia. It uses the
rmaxent package <https://github.com/johnbaums/rmaxent>. It assumes that
the maxent models were generated by the
‘fit\_maxent\_targ\_bg\_back\_sel’ function. Note that this step is
quite memory heavy, best run with &gt; 32GB of RAM.

  

``` r
## Create a local projection for mapping : Australian Albers
aus_albers <- CRS('+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 
                   +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')

## Try and set the raster temp directory to a location not on the partition, to save space
rasterOptions(tmpdir = 'G:/Raster_temp/')

## Create current sdm map projections
tryCatch(
  project_maxent_current_grids_mess(country_shp     = AUS, 
                                    country_prj     = CRS("+init=EPSG:3577"),
                                    local_prj       = aus_albers,
                                    
                                    species_list    = map_taxa,    
                                    maxent_path     = './output/plant_maxent/back_sel_models/',
                                    
                                    current_grids   = study.climate.veg.grids,         
                                    create_mess     = TRUE,
                                    save_novel_poly = FALSE),
  
  ## If the species fails, write a fail message to file
  error = function(cond) {
    
    ## This will write the error message inside the text file, but it won't include the species
    file.create(file.path("output/plant_maxent/back_sel_models/mapping_failed_current.txt"))
    cat(cond$message, file = file.path("output/plant_maxent/back_sel_models/mapping_failed_current.txt"))
    warning(cond$message)
    
  })
```

  

![fig1](https://github.com/HMB3/sdmgen/blob/master/output/Acacia_dealbata_mess_panel.png?raw=true)

**Figure 2.** Example of a continuous climatic suitability map for one
plant species under current conditions. Species occurrence points are
plotted in red on the left panel. The cells in the right panel are coded
from 0 : no to low suitability, to 1 : highly suitable. The shaded areas
on the right panel indicate where the maxent model is extrapolating
beyond the training data (i.e. the result of a MESS map).

  

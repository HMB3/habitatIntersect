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
library(devtools)
install_github('johnbaums/things')
install_github('johnbaums/rmaxent')
install_github('traitecoevo/taxonlookup')

## Main package 
devtools::install_github("HMB3/nenswniche")

## Load packages 
library(nenswniche)
data('sdmgen_packages')
ipak(sdmgen_packages)
```

  
  
  

# Background

This code is being developed at UNSW, to help investigate the impacts of
the 2019/2020 bush fires on Insects in the Forests of North Eastern New
South Wales. The aim is to create a pipeline that rapidly assesses the
habitat suitability of the threatened insect species under current
environmental conditions. There are three ways to estimate habitat
suitability :

  

-   Habitat Suitability Models using geographic records for each
    invertebrate taxon
-   Habitat Suitability Models using geographic records of host plants
    for each invertebrate taxon
-   Intersecting geographic records of each invertebrate taxon with
    vegetation maps (e.g. remote sensed vegetation layers)

  

# Run SDMs Across Australia

  

Download example data here :

  

<https://cloudstor.aarnet.edu.au/plus/s/6z88YnumBkKVEir>

  

Once the geographic data for all taxa has been processed and cleaned, we
can habitat suitability models. The function below runs two habitat
suitability models: a full maxent model using all variables, and a
backwards selection maxent. Given a candidate set of predictor
variables, the backwards selection function identifies a subset of
variables that meets specified multi-collinearity criteria.
Subsequently, backward step-wise variable selection is used to
iteratively drop the variable that contributes least to the model, until
the contribution of each variable meets a specified minimum, or until a
predetermined minimum number of predictors remains (maxent models are
run using the dismo package <https://github.com/rspatial/dismo>). Note
that this step is quite memory heavy, best run with &gt; 32GB of RAM.

  

Note that we are modelling at multiple taxonomic levels : species, genus
and family

``` r
## Read in spatial points data frames of the occurrence data
SDM.SPAT.OCC.BG.GDA       = readRDS('./output/results/SDM_SPAT_OCC_BG_GDA_ALL_TARGET_INVERT_TAXA.rds')
SDM.PLANT.SPAT.OCC.BG.GDA = readRDS('./output/results/SDM_SPAT_OCC_BG_TARGET_HOST_PLANTS.rds')


## Run SDMs for a list of taxa
run_sdm_analysis(species_list            = rev(analysis_taxa),    ## taxa list
                 maxent_dir              = 'output/plant_maxent/full_models',     ## full model output folder    
                 bs_dir                  = 'output/plant_maxent/back_sel_models', ## bs output folder 
                 sdm_df                  = SDM.ALL.PLA.SPP.BG,                    ## SPDF of occurrence data
                 sdm_predictors          = names(aus.climate.veg.grids),          ## variables to include into the models 
                 
                 backwards_sel           = TRUE,      
                 template_raster         = template_raster_1km_mol,               ## Template raster
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

  

# Project SDMs across a study area : North-Eastern NSW

  

Next, we take the habitat suitability models, and project them across
geographic space. First, we need to extract the SDM results from the
models. Each model generates a ‘threshold’ of probability of occurrence,
which we use to create map of habitat suitability across Australia.

  

``` r
## Create a table of maxent results
MAXENT.RESULTS = compile_sdm_results(taxa_list    = analysis_taxa,
                                     results_dir  = 'output/veg_climate_topo_maxent/back_sel_models',
                                     data_path    = "./output/veg_climate_topo_maxent/Habitat_suitability/",
                                     sdm_path     = "./output/veg_climate_topo_maxent/back_sel_models/",
                                     save_data    = FALSE,
                                     save_run     = "TARG_INSECT_SPP")
```

  

The sdm projection function below takes the maxent models created by the
sdm function, and projects the models across geographic space -
currently just for Australia. It uses the rmaxent package
<https://github.com/johnbaums/rmaxent>. This step is also best with &gt;
32GB of RAM.

  

``` r
## Create a local projection for mapping : Australian Albers
aus_albers <- CRS('+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 
                   +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')

## Try and set the raster temp directory to a location not on the partition, to save space
terraOptions(memfrac = 0.5, 
             tempdir = 'G:/North_east_NSW_fire_recovery/TEMP')

## Create current sdm map projections
tryCatch(
  project_maxent_current_grids_mess(country_shp     = AUS, 
                                    country_prj     = CRS("+init=EPSG:3577"),
                                    local_prj       = aus_albers,
                                    
                                    taxa_list       = map_taxa,    
                                    maxent_path     = './output/plant_maxent/back_sel_models/',
                                    
                                    current_grids   = study.climate.veg.grids,         
                                    create_mess     = TRUE,
                                    save_novel_poly = FALSE),
  
  ## If the species fails, save the error message
  error = function(cond) {
    
    ## This will write the error message inside the text file, but it won't include the species
    file.create(file.path("output/plant_maxent/back_sel_models/mapping_failed_current.txt"))
    cat(cond$message, file = file.path("output/veg_climate_topo_maxent/back_sel_models/mapping_failed_current.txt"))
    warning(cond$message)
    
  })
```

  

![fig1](https://github.com/HMB3/sdmgen/blob/master/output/Acacia_dealbata_mess_panel.png?raw=true)

**Figure 1.** Example of a continuous climatic suitability map for one
plant species under current conditions. Species occurrence points are
plotted in red on the left panel. The cells in the right panel are coded
from 0 : no to low suitability, to 1 : highly suitable. The shaded areas
on the right panel indicate where the maxent model is extrapolating
beyond the training data (i.e. the result of a MESS map).

  

To use the habitat suitability rasters in area calculations
(e.g. comparing the area of suitable habitat affected by fire), we need
to convert the continuous suitability scores (ranging from 0-1) to
binary values (either 1, or 0). To do this, we need to pick a threshold
of habitat suitability, below which the taxa is not considered present.
Here we’ve chosen the 10th% Logistic threshold for each taxa (ref).

  

``` r
## Threshold the invertebrate SDM models to be either 0 or 1
habitat_threshold(taxa_list     = sort(unique(MAXENT.RESULTS$searchTaxon)),
                  maxent_table  = MAXENT.RESULTS,
                  maxent_path   = './output/veg_climate_topo_maxent/back_sel_models/',
                  cell_factor   = 9,
                  country_shp   = 'AUS',
                  country_prj   = CRS("+init=EPSG:3577"),
                  write_rasters = TRUE)
```

nenswniche : estimate burnt habitat for invertebrates, plants and
reptiles in Eastern Australia
================
June 2021

  

The text and code below summarises an R package that can be used to
rapidly assess the environmental range of a species within
Australia, from downloading occurrence records, through to creating maps
of predicted habitat suitability across Australia. An example of this 
pipeline applied to plants is published in Science of the Total Environment -

  

Burley, H., Beaumont, L.J., Ossola, A., et al. (2019) Substantial
declines in urban tree habitat predicted under climate change. Science
of The Total Environment, 685, 451-462.

<https://www.sciencedirect.com/science/article/pii/S0048969719323289#f0030>

  

To install the package, run :

``` r
## Function to load or install packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos="https://cran.csiro.au/")
  sapply(pkg, require, character.only = TRUE)
}


## Main package 
devtools::install_github("HMB3/nenswniche")

## Load packages 
library(nenswniche)
data('sdmgen_packages')
ipak(sdmgen_packages)
```

  
  
  

# Background

This code was developed at UNSW to investigate the impacts of
the 2019/2020 bush fires on rare Invertebrates in the Forests of Eastern Australia.
We created a list of 85 priority invertebrate species using NSW State government 
listings, expert knowledge and estimates of geographic extent.  We then 
sampled sites affected by the bush fires in November 2021 for these priority 
taxa. Site data was combined with records from Australian and global databases 
to estimate the environmental ranges of all species. Habitat suitability Models 
(HSMs) were calibrated for all taxa, and projected onto baseline environments 
(1976-2005), including soils and vegetation. HSMs were calculated at the species, 
genus and family level. We then calculated the % of suitable habitat burnt for all 
taxa a). across eastern Australia overall b). within each burn intensity category,
and c). within each major vegetation class. 


# Vegetation and Fire data

To assess habitat across eastern Australia, we used a Structural Classification of 
Australian Vegetation [based on Radar and multi-spectral data, (Scarth et al., 2019)], 
which gives height and cover estimates matching the Australian National Vegetation Information 
System (NVIS) at ~30m spatial resolution. We also obtained remotely-sensed estimates 
for the extent and severity of the 2019-2020 fires [derived from Sentinel imagery, 
Mackey, Lindenmayer, Norman, Taylor, and Gould (2021)], which gives estimates of 
burn intensity at ~20m resolution (Fig 1). Given the difference in resolution between 
our environmental (280m), vegetation (30m) and fire layers (20m), we used feature 
layers (i.e., polygons) to combine the spatial data across sources, which avoids 
resampling and loss of information. Our study area is that within a 100km 
buffer of the Fire extent, from Victoria to south east Queensland (i.e., the habitat 
analyses do not consider the whole east coast, see Fig. 1).



Fig 1: Remotely sensed Vegetation classification for eastern Australia [Left panel, 
Scarth et al. (2019)], and fire severity data for the 2019-2020 fires across eastern 
Australia [right panel, Mackey et al. (2021)]. 
  

# Habtiat Suitability Modelling

  

Download example data here :

  

<https://cloudstor.aarnet.edu.au/plus/s/6z88YnumBkKVEir>

  

Once the geographic data for all taxa has been processed and cleaned, we
can habitat suitability models. The function below runs two habitat
suitability models: a full maxent model using all variables, and a
backwards selection maxent. Given a candidate set of predictor
variables (e.g. Fig 2, climate, terrain, soils, remotely sensed vegetation), 
the backwards selection function identifies a subset of
variables that meets specified multi-collinearity criteria.
Subsequently, backward step-wise variable selection is used to
iteratively drop the variable that contributes least to the model, until
the contribution of each variable meets a specified minimum, or until a
predetermined minimum number of predictors remains (maxent models are
run using the dismo package <https://github.com/rspatial/dismo>). Note
that this step is quite memory heavy, best run with > 64 GB RAM.


Fig 2: Geographic Records for Nysisus vinitor (Left Panels), and realized environmental 
niches for Nysisus vinitor (right panels). Note that the habitat suitably models are 
calibrated using environmental data from the whole of Australia (small inset left panel), 
but they are only projected into the extent of the fires (main left panel).  



``` r
## Read in spatial points data frames of the occurrence data
SDM.SPAT.OCC.BG.GDA       = readRDS('./output/results/SDM_SPAT_OCC_BG_GDA_ALL_TARGET_INVERT_TAXA.rds')
SDM.PLANT.SPAT.OCC.BG.GDA = readRDS('./output/results/SDM_SPAT_OCC_BG_TARGET_HOST_PLANTS.rds')


## Run SDMs for a list of taxa
run_sdm_analysis(taxa_list               = sort(target.insect.families),
                 taxa_level              = 'family',
                 maxent_dir              = 'output/veg_climate_topo_maxent/full_models',     
                 bs_dir                  = 'output/veg_climate_topo_maxent/back_sel_models',
                 sdm_df                  = SDM.SPAT.OCC.BG.GDA,
                 sdm_predictors          = names(aus.climate.veg.grids),
                 
                 backwards_sel           = TRUE,      
                 template_raster         = template_raster_1km_mol,
                 cor_thr                 = 0.8,  
                 pct_thr                 = 5, 
                 k_thr                   = 4, 
                 min_n                   = 10,  
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

  

![fig1](https://github.com/HMB3/nenswniche/blob/master/output/Amphistomus_buffer_occ.png?raw=true)
![fig2](https://github.com/HMB3/nenswniche/blob/master/output/Amphistomus_bs_predictor_correlation.png?raw=true)

<!-- ```{r message=TRUE, echo=TRUE, warning=FALSE, eval=FALSE} -->
<!-- ## Create a table of maxent results -->
<!-- plot_grid(https://github.com/HMB3/nenswniche/blob/master/output/Amphistomus_buffer_occ.png?raw=true, -->
<!--           https://github.com/HMB3/nenswniche/blob/master/Amphistomus_bs_predictor_correlation.png?raw=true) -->
<!-- ```  -->

  

**Figure 1.** Top : occurrence data used in the habitat suitability
model for the invertebrate Genus Amphistomus (blue points are occurrence
data, red zone is the 200km buffer around the points, from where
background points are selected). Bottom : Correlations between the final
variables in the backwards selection maxent model.

  

# Project SDMs across North-Eastern NSW

  

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
sdm function, and projects the models across geographic space - here
just for North Eastern NSW. It uses the rmaxent package
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

  

![fig3](https://github.com/HMB3/nenswniche/blob/master/output/Amphistomus_mess_panel.png?raw=true)

**Figure 2.** Example of a continuous habitat suitability map for for
the invertebrate Genus Amphistomus under current conditions. Species
occurrence points are plotted in red on the left panel. The cells in the
right panel are coded from 0 : no to low suitability, to 1 : highly
suitable.

  

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

  

![fig4](https://github.com/HMB3/nenswniche/blob/master/output/Amphistomus_current_suit_not_novel_above_0.1401.png?raw=true)

**Figure 3.** Example of a thresholded continuous habitat suitability
map for for the invertebrate Genus Amphistomus under current conditions.
The Logistic threshold is used here.

  

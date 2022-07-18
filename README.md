habitatIntersect : a pacakge for rapidly estimating burnt habitat
================
June 2022



The text and code below summarises an R package that can be used to
rapidly assess how much of the suitable habitat for multiple species 
has been burnt across Eastern Australia by the 2020 bushfires.

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
devtools::install_github("HMB3/habitatIntersect")

## Load packages 
library(nenswniche)
data('sdmgen_packages')
ipak(sdmgen_packages)
```



Download example data here :


<https://cloudstor.aarnet.edu.au/plus/s/6z88YnumBkKVEir>



# Background <br>



This code was developed at UNSW to investigate the impacts of the 2019/2020 bush fires on 
rare Invertebrates in the Forests of Eastern Australia (it could also be applied to any 
taxonomic group with spatial data). We created a list of 85 priority invertebrate species of
bugs, beetles, snails and spiders using NSW State government listings, expert knowledge and 
estimates of geographic extent. We then sampled sites affected by the bush fires in November 
2021 for these priority taxa. Site data was combined with records from Australian and global 
databases to estimate the environmental ranges of all species. Habitat suitability Models 
(HSMs) were calibrated for all taxa, and projected onto baseline environments (1976-2005), 
including soils and vegetation. HSMs were calculated at the species, genus and family level. 
We then calculated the % of suitable habitat burnt for all taxa a). across eastern Australia 
overall b). within each burn intensity category, and c). within each major vegetation class. <br>



# Vegetation and Fire data <br>



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
analyses do not consider the whole east coast, see Fig. 1). <br>



![](https://github.com/HMB3/nenswniche/blob/master/output/Figures/Fig_Veg_Fire.png?raw=true)

**Figure 1.** Remotely sensed Vegetation classification for eastern Australia [Left panel, 
Scarth et al. (2019)], and fire severity data for the 2019-2020 fires across eastern 
Australia [right panel, Mackey et al. (2021)]. <br>

  

# Habtiat Suitability Modelling



Once the geographic data for all taxa has been processed and cleaned, we can run habitat 
suitability models (HSMs). The function below runs two habitat suitability models: a full 
maxent model using all variables, and a backwards selection maxent. Given a candidate set of 
predictor variables (e.g. Fig 2, climate, terrain, soils, remotely sensed vegetation), the backwards 
selection function identifies a subset of variables that meets specified multi-collinearity criteria. 
Subsequently, backward step-wise variable selection is used to iteratively drop the variable that 
contributes least to the model, until the contribution of each variable meets a specified minimum, 
or until a predetermined minimum number of predictors remains (maxent models are run using the 
dismo package <https://github.com/rspatial/dismo>). Note this step is needs > 64 GB RAM.



![](https://github.com/HMB3/nenswniche/blob/master/output/Figures/Fig_1_niches.png?raw=true)

**Figure 2.** Geographic Records for _Nysisus vinitor_ (Left Panels), and realized environmental 
niches (right panels). Note that the habitat suitably models are calibrated using environmental 
data from the whole of Australia (small inset left panel), but they are only projected into the 
extent of the fires (main left panel).  



``` r
## Read in spatial points data frames of the occurrence data
SDM.SPAT.OCC.BG.GDA       = readRDS('./output/results/SDM_SPAT_OCC_BG_GDA_ALL_TARGET_INVERT_TAXA.rds')


## Run SDMs for a list of taxa - EG 80 Invertebrate species
run_sdm_analysis_no_crop(taxa_list               = sort(target.insect.spp),
                         taxa_level              = 'species',
                         maxent_dir              = inv_back_dir,     
                         bs_dir                  = inv_back_dir,
                         sdm_df                  = SDM.SPAT.OCC.BG.GDA,
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
```



![](https://github.com/HMB3/nenswniche/blob/master/output/Figures/Mutusca_brevicornis_buffer_occ.png?raw=true)
![](https://github.com/HMB3/nenswniche/blob/master/output/Figures/Mutusca_brevicornis_bs_predictor_correlation.png?raw=true)

**Figure 3.** Top : Occurrence points used in the HSM for _Mutusca brevicornis_. Bottom : correlations between the 
final variables in the used in the backward selected HSM.



# Project SDMs across Eastern Australia

  
Next we take the HSMs for each taxa, and use the statistical model to predict habitat suitability 
for all locations across the study area (i.e. all 280m grid cells of the raster layers used for modelling). 
The sdm 'projection' function below uses the rmaxent package <https://github.com/johnbaums/rmaxent>, and 
needs > 64 GB RAM. The resulting surface of continous habitat suitability (0-1) is converted to a binary 
layer (0, 1), Using a probabilistic threshold – the 10th percentile training presence logistic threshold – 
based on the weighting of different model errors (‘commission’ versus ‘omission’ errors, Fig 4). 


``` r

## Create habitat suitability map under current conditions
tryCatch(
  
  project_maxent_current_grids_mess(taxa_list       = invert_map_spp,    
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
  
```



![](https://github.com/HMB3/nenswniche/blob/master/output/Figures/Fig_2_SDM_thresh.png?raw=true)
  
**Figure 4.** Continuous habitat suitability model for _Nysisus vinitor_ (Left, probability of occurrence 0-1), 
and binary (i.e., thresh holded) HSM for N. vinitor (right, 0,1), where cells > 0.254 (the logistic threshold 
for this species) are 1, and cells < 0.254 are 0. The binary HSM layers are used for this analysis of habitat loss.

 

# Estimates of suitable habitat burnt for Invertebrates

To estimate the total area of suitable habitat for our target invert taxa across Eastern Australia that was burnt by 
the 2019-2020 fires, we intersected feature layers of the HSM models for each taxon with an aggregated feature 
layer of the Fire extent [see Mackey et al. (2021) Fig 1]. This fire layer has both binary extent (burnt and unbunrt),
as well as class of burn intensity.Of 80 target species, 38 (~45%) had sufficient data to run HSMs. In general, the 
most widespread species experienced the least burning of habitat, while the most restricted species experienced larger 
proportional habitat burns (Fig 5). In particular, _Amphistomus primonactus_, _Diorygopyx incrassatus_ and _Aulacopris maximus_ 
had > 50% of their suitable habitat burnt by the fires (EG taxa, Table 2). Conversely,  _Nysius vinitor_ and 
_Onthophagus compositus_ had < 10% of their habitat burnt (Fig. 6).



![](https://github.com/HMB3/nenswniche/blob/master/output/Figures/fesm_inv_species_scatter_plots.png?raw=true) 

**Figure 5.** Scatterplot matrix of the relationship between the % of suitable habitat that was estimated to have
burnt in the fires, and different measures of the species geographic range (EOO = Extent of Occurrence, AOO = 
Area of Occupancy).


  
![](https://github.com/HMB3/nenswniche/blob/master/output/Figures/A_primonactus_Fire.png?raw=true)  
![](https://github.com/HMB3/nenswniche/blob/master/output/Figures/N_vinitor_Fire.png?raw=true) 

**Figure 6.** Suitable habitat for invertebrate species (orange), with the fires extent overlaid 
(hatched area) and HSM occurrence points in blue. Top panels: _A. primonactus_ lost among the largest 
proportion (65.4%) of suitable habitat in the fires of all taxa analysed. Bottom panels : _N. vinitor_ 
lost among the smallest proportion (7.2%) of habitat in the fires.  



On average across all Invert species analysed, the area of habitat burnt was greatest within the _low_ 
and _moderate_ severity burn intensity categories, and within the _low_ and _extremely tall_ open forest 
vegetation types (Fig 5). When considering the different types of invertebrates, Snails were estimated
to have had the greatest proportion of their suitable burnt in the low and extremely tall open forest 
types, while Beetles are estimated to have had the smallest proportion of habitat burnt in tall open 
forest and very tall closed forest (Fig 7). Similar patterns were observed in the burn severity classes,
with Bugs having the most suitable habitat burnt in the high burn severity class.



![](https://github.com/HMB3/nenswniche/blob/master/output/Figures/sdm_invert_combined_barplots.png?raw=true) 
**Figure 7.** Summary of the percentage of suitable habitat that was burnt across all 38 Invertebrate species
analysed, within : Vegetation types (top left), burn classes (top right), Vegetation and invert type (bottom left
burn class and invert type (bottom right).



# Invertebrate spatial sampling 



The methods used here have largely been developed for macro-organisms (e.g. trees, mammals, birds), and 
are difficult to apply to invertebrates. To say the least, it is challenging to assess the environmental 
distributions of taxa which have such low levels of spatial sampling – are they actually as rare as the 
data suggests, or are they just poorly sampled? Could some of these taxa be abundant in patches across 
relatively broad geographic areas, but simply very difficult to find? It seems biologically 
implausible that invert taxa – together with plants, the basis of most terrestrial life – are indeed so rare. 
Rather invertebrates suffer from receiving less overall financial, taxonomic and conservation 
effort compared to other groups, particularly mammals (the respective states of invert vs. mammal taxonomies 
reflecting this disparity). Despite the data deficiencies we have highlighted, contemporary data 
and methods advances (e.g. the ALA, spatial analyses software and Remote sensing products) provide unprecedented 
opportunities for spatially targeted sampling of priority taxa for future analyses and conservation assessment. 
We can now develop clear maps of which invert taxa have the best and worst sampling in which regions of relatively 
data rich countries such as Australia, and begin to plug the data and analysis gaps between bugs and the rest of 
the tree of life.



# TBC...



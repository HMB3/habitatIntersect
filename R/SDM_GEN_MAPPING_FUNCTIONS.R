#########################################################################################################################
######################################  MAPPING FUNCTIONS FOR SDM ANALYSIS ---- #########################################
#########################################################################################################################


## Below are the functions used to project SDM models across geographic areas.


#' @title Project current maxent models across geographic space
#' @description This function takes the maxent models created by the 'fit_maxent_targ_bg_back_sel' function,
#' and projects the model across geographic space - currently just for Australia.
#' @param country_shp        SpatialPolygonsDataFrame - Spdf of the country for mapping maxent results (e.g. Australia)
#' @param country_prj        CRS object  - Local projection for mapping maxent results
#' @param local_prj          CRS object  - Local projection for mapping maxent results
#' @param taxa_list          Character string - The species to run maxent predictions for
#' @param maxent_path        Character string - The file path containin the existing maxent models
#' @param climate_path       Character string - The file path where the climate data is saved
#' @param grid_names         Character string - Vector of enviro conditions that you want to include
#' @param current_grids      Character string - Vector of current enviro conditions that you want to include
#' @param save_novel_poly    Character string - Save the novel areas as shapefiles?
#' @param create_mess        Logical - Create mess maps of the predictions (T/F)?
#' @details It uses the rmaxent package https://github.com/johnbaums/rmaxent
#' @export
project_maxent_current_grids_mess = function(country_shp, 
                                             country_prj,   save_novel_poly,
                                             local_prj,     taxa_list,
                                             maxent_path,   
                                             current_grids, 
                                             create_mess) {
  
  ## Read in the Aus and world shapefile and re-rpoject
  country_poly <- country_shp %>%
    spTransform(country_prj)
  
  ## Rename the raster grids
  ## Note this step is only needed if the current grids used in the 
  ## their original form, rather than being renamed
  # names(current_grids) <- grid_names
  
  ## First, run a loop over each scenario:
  lapply(taxa_list, function(x) {
    
    ## Then apply each GCM to each species.
    ## First, check if the maxent model exists
    ## Then apply each GCM to each species
    
    ## Define function to then send to one or multiple cores
    maxent_predict_fun <- function(species) {
      
      ## Create species name
      ## species = taxa_list[1]
      save_name = gsub(' ', '_', species)
      
      ## First check if the species exists
      current_mess_png = sprintf('%s/%s/full/%s_%s.png', maxent_path, species, species, "mess_panel")
      if(!file.exists(current_mess_png)) {
        
        ## Create a path for the current prediction of the species
        f_current  <- sprintf('%s%s/full/%s_current.tif', maxent_path, species, species)
        
        ## Check the file exists
        if(file.exists(sprintf('%s/%s/full/maxent_fitted.rds', maxent_path, species))) {
          message('Then run current maxent projections for ', species)
          
          ## Then, check if the species projection has already been run...
          if(!file.exists(sprintf('%s/%s/full/%s_future_not_novel.tif',
                                  maxent_path, species, species))) {
            
            ## Now read in the SDM model, calibrated on current conditions
            ## if it was run with backwards selection, just use the full model
            if (grepl("back", maxent_path)) {
              
              message('Read in the backwards selected model')
              m <- readRDS(sprintf('%s/%s/full/maxent_fitted.rds', maxent_path, save_name))
              
            } else {
              
              ## Otherwise, index the full model
              message('Read in the full model')
              m <- readRDS(sprintf('%s/%s/full/maxent_fitted.rds', maxent_path, save_name))$me_full
            }
            
            ## Read in species with data and occurrence files
            message('Read in the swd and occ data')
            swd <- as.data.frame(readRDS(sprintf('%s%s/swd.rds', maxent_path, species)))
            occ <- readRDS(sprintf('%s%s/%s_occ.rds', maxent_path, species, species)) %>%
              spTransform(country_prj)
            
            ## If the current raster prediction has not been run, run it.
            if(!file.exists(f_current) == TRUE) {
              
              ## Report which prediction is in progress :: m$me_full, m$me_full@presence
              message('Running current maxent prediction for ', species)
              
              ## Set the names of the rasters to match the occ data, and subset both
              sdm_vars             = names(m@presence)
              current_grids        = subset(current_grids, sdm_vars)
              swd                  = swd [,sdm_vars]
              
              pred.current <- rmaxent::project(
                m, current_grids[[colnames(m@presence)]])$prediction_logistic
              raster::writeRaster(pred.current, f_current, overwrite = TRUE)
              
              gc()
              
            } else {
              message('Use existing prediction for ', species)
              pred.current = raster::raster(sprintf('%s/%s/full/%s_current.tif',
                                                    maxent_path, species, species))
            }
            
            ## Report current mess map in progress
            ## Could work out how to the static mess once, before looping through scenarios
            MESS_dir = sprintf('%s%s/full/%s', maxent_path, species, 'MESS_output')
            
            ## If the current novel layer doesn't exist, create it
            if(!file.exists(sprintf('%s/%s%s.tif', MESS_dir, species, "_current_novel"))) {
              
              ##
              message('Are the environmental variables identical? ',
                      identical(names(swd), names(current_grids)))
              
              ## Create a map of novel environments for current conditions.
              ## This similarity function only uses variables (e.g. n bioclim), not features
              message('Run similarity function for current condtions for ', species)
              mess_current  <- similarity(current_grids, swd, full = TRUE)
              novel_current <- mess_current$similarity_min < 0    ## All novel environments are < 0
              novel_current[novel_current==0]              <- NA  ## 0 values are NA
              
              gc()
              
              
            } else {
              ## Otherwise, read in the current novel layer
              message(species, ' Current similarity analysis already run')
              novel_current = raster::raster(sprintf('%s/%s%s.tif', MESS_dir, species, "_current_novel"))
            }
            
            ## Write out the current mess maps -
            ## create a new folder for the mess output - we are going to print it to the maps
            if(!dir.exists(MESS_dir)) {
              message('Creating MESS directory for ', species)
              dir.create(MESS_dir)
              
              ## Create a PNG file of MESS maps for each maxent variable
              ## raster_list  = unstack(mess_current$similarity) :: list of environmental rasters
              ## raster_names = names(mess_current$similarity)   :: names of the rasters
              message('Creating mess maps of each current environmental predictor for ', species)
              
              ## raster_name <- raster_names[1]
              mapply(function(raster, raster_name) {
                
                ## Create a level plot of MESS output for each predictor variable, for each species
                p <- levelplot(raster, margin = FALSE, scales = list(draw = FALSE),
                               at = seq(minValue(raster), maxValue(raster), len = 100),
                               colorkey = list(height = 0.6),
                               main = gsub('_', ' ', sprintf(' Current_mess_for_%s (%s)', raster_name, species))) +
                  
                  latticeExtra::layer(sp.polygons(country_poly), 
                                      data = list(country_poly = country_poly)) ## need list() for polygon
                
                p <- diverge0(p, 'RdBu')
                f <- sprintf('%s/%s%s%s.png', MESS_dir, species, "_current_mess_", raster_name)
                
                png(f, 8, 8, units = 'in', res = 300, type = 'cairo')
                print(p)
                dev.off()
                
              }, unstack(mess_current$similarity), names(mess_current$similarity))
              
            } else {
              message(species, ' MESS directory already created')
            }
            
            ## Write the raster of novel environments to the MESS sub-directory
            if(!file.exists(sprintf('%s/%s%s.tif', MESS_dir, species, "_current_novel"))) {
              
              message('Writing currently novel environments to file for ', species)
              raster::writeRaster(novel_current, sprintf('%s/%s%s.tif', MESS_dir, species, "_current_novel"),
                                  overwrite = TRUE)
              
            } else {
              message(species, ' Current MESS file already saved')
            }
            
            ## Now mask out novel environments
            ## is.na(novel_current) is a binary layer showing
            ## not novel [=1] vs novel [=0],
            ## so multiplying this with hs_current will mask out novel
            hs_current_not_novel <- pred.current * is.na(novel_current)
            
            ## Write out not-novel raster :: this can go to the main directory
            ## Write the raster of novel environments to the MESS sub-directory
            if(!file.exists(sprintf('%s%s/full/%s%s.tif', maxent_path, species, species, "_current_not_novel"))) {
              
              message('Writing currently un-novel environments to file for ', species)
              raster::writeRaster(hs_current_not_novel, sprintf('%s%s/full/%s%s.tif', maxent_path,
                                                                species, species, 
                                                                "_current_not_novel"), overwrite = TRUE)
              
            } else {
              message(species, ' Current un-novel environments file already saved')
            }
            
            ## Create the MESS path and save shapefiles
            MESS_shp_path <- sprintf('%s%s/full/%s', maxent_path, species, 'MESS_output')
            if(save_novel_poly == TRUE) {
              
              ## Create shape files 
              current_novel_raster <- rast(sprintf('%s/%s%s.tif', MESS_dir, species, "_current_novel"))
              values <- values(current_novel_raster)
              unique(values[1])
              
              if(!is.na(unique(values[1]))) {
                
                current_novel_poly   <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(current_novel_raster), 
                                                                    as_points = FALSE, 
                                                                    merge     = TRUE,
                                                                    fill      = FALSE, 
                                                                    group     = FALSE,
                                                                    agr       = FALSE))
                gc()
                
                ## Now save the novel areas as shapefiles
                ## There is a problem with accessing the files at the same time
                message('Saving current MESS maps to polygons for ', species)
                writeOGR(obj    = current_novel_poly,
                         dsn    = sprintf('%s',  MESS_shp_path),
                         layer  = paste0(species, "_current_novel_polygon"),
                         driver = "ESRI Shapefile", overwrite_layer = TRUE)
                
              } else {
                message('Do not save current MESS maps to shapefile for ', species, ' no cells are novel')
              }
              
            } else {
              message('Do not save current MESS maps to shapefile for ', species)
            }
            
            ## Below, we create a dummy polygon as the first list element (which is the extent
            ## of the raster, expanded by 10%), to plot on panel 1). 50 = approx 50 lines across the polygon
            
            ## Now create a panel of PNG files for maxent projections and MESS maps
            ## All the projections and extents need to match
            empty_ras <- raster::init(current_grids, function(x) NA)
            
            ## Use the 'levelplot' function to make a multipanel output:
            ## occurrence points, current raster and future raster
            current_mess_png = sprintf('%s/%s/full/%s_%s.png', maxent_path, species, species, "mess_panel")
            if(!file.exists(current_mess_png)) {
              
              ## Create level plot of current conditions including MESS
              message('Create current MESS panel maps for ', species)
              
              png(sprintf('%s/%s/full/%s_%s.png', maxent_path, species, species, "mess_panel"),
                  11, 4, units = 'in', res = 300)
              
              print(levelplot(raster::stack(empty_ras,
                                            hs_current_not_novel, 
                                            quick = TRUE), margin = FALSE,
                              
                              ## Create a colour scheme using colbrewer: 100 is to make it continuos
                              ## Also, make it a one-directional colour scheme
                              scales      = list(draw = FALSE),
                              at = seq(0, 1, length = 100),
                              col.regions = colorRampPalette(rev(brewer.pal(11, 'Spectral'))),
                              
                              ## Give each plot a name: the third panel is the GCM
                              names.attr = c('Maxent records', ' Current'),
                              colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                              main       = list(gsub('_', ' ', species), font = 4, cex = 2)) +
                      
                      ## Plot the Aus shapefile with the occurrence points for reference
                      ## Can the current layer be plotted on it's own?
                      ## Add the novel maps as vectors.
                      latticeExtra::layer(sp.polygons(country_poly), data = list(country_poly = country_poly)) +
                      latticeExtra::layer(sp.points(occ, pch = 19, cex = 0.15,
                                                    col = c('red', 'transparent', 'transparent')[panel.number()]),
                                          data = list(occ = occ)))
              dev.off()
              
              gc()
              
              ## Now delete the temporary directory?
              
            } else {
              message(' Current MESS panel maps already created for ', species)
            }
          }
        } else {
          message(species, ' ', ' skipped - SDM not yet run')
        }
        
      } else {
        message(' Current MESS panel maps already created for ', species)
      }
    }
    
    ## Check this is the best way to run parallel
    lapply(taxa_list, maxent_predict_fun)
  })
}






#' @title Threshold current habitat suitability rasters.
#' @description Takes a habitat suitability layer, and creates a binary suitability layer (0, 1) using a threshold value.
#' @param taxa_list          Character string - The species to run maxent predictions for
#' @param maxent_path        Character string - The file path containing the existing maxent models
#' @param maxent_table       Data frame       - A table of maxent results to be used for mapping 
#' @param cell_factor        Numeric          - Cell size to resample output
#' @param country_shp        Character string - Shapefile name that has already been read into R (e.g. in the Package)
#' @param country_prj        Character string - Name of projection
#' @param write_rasters      Logical          - Save rasters (T/F)?
#' @export
habitat_threshold = function(taxa_list,
                             maxent_table,
                             maxent_path,
                             cell_factor,
                             country_shp,
                             country_prj) {
  
  ## Get the AUS shapefile
  country_poly <- get(country_shp) %>%
    spTransform(country_prj)
  
  ## Pipe the list into Lapply
  taxa_list %>%
    
    ## Loop over just the species
    ## taxa = taxa_list[4]
    lapply(function(taxa) {
      
      ## Get the directory
      DIR = maxent_table %>%
        filter(searchTaxon == taxa) %>%
        dplyr::select(results_dir) %>%
        distinct() %>% .[1, ] %>% .[[1]]
      
      thresh = maxent_table %>%
        filter(searchTaxon == taxa) %>%
        dplyr::select(Logistic_threshold) %>%
        distinct() %>% .[1, ] %>% .[[1]]
      
      taxa_name <- gsub(' ', '_', taxa)
      
      ## Check the threshold data exists
      current_file  = sprintf('%s/%s/full/%s_current_not_novel.tif',
                              maxent_path, taxa_name, taxa_name)
      
      current_thresh =  sprintf('%s/%s/full/%s_%s%s.tif', maxent_path,
                                taxa_name, taxa_name, "current_suit_not_novel_above_", thresh)
      
      ## If the threshold raster data doesn't exist :
      if(file.exists(current_file)) {
        
        if(!file.exists(current_thresh)) {
          
          ## Print the taxa being analysed
          message('doing ', taxa, ' | Logistic > ', thresh)
          
          ## Read in the current suitability raster :: get the current_not_novel raster
          f_current <- raster(sprintf('%s/%s/full/%s_current_not_novel.tif',
                                      maxent_path, taxa_name, taxa_name))
          
          ## First, create a simple function to threshold each of the rasters in raster.list,
          ## Then apply this to just the current suitability raster.
          thresh_greater       = function (x) {x > thresh}
          current_suit_thresh  = thresh_greater(f_current)
          current_suit_rast    = terra::rast(current_suit_thresh)
          
          ## Resample rasters
          message('Resample ', taxa, ' to ', cell_factor)
          current_suit_thresh_resample <- terra::disagg(current_suit_rast, fact = cell_factor)
          
          ## Now write the rasters
          
          ## Write the current suitability raster, thresholded using the Maximum training
          ## sensitivity plus specificity Logistic threshold
          message('Writing ', taxa, ' current', ' max train > ', thresh)
          
          ## Save in two places, in the taxa folder, 
          ## and in the habitat suitability folder
          writeRaster(current_suit_thresh_resample, 
                      sprintf('%s/%s/full/%s_%s%s.tif', maxent_path,
                              taxa_name, taxa_name, "current_suit_not_novel_above_", thresh),
                      overwrite = TRUE)
          
          message('writing threshold png for ', taxa)
          png(sprintf('%s/%s/full/%s_%s%s.png', maxent_path,
                      taxa_name, taxa_name, "current_suit_not_novel_above_", thresh),
              16, 10, units = 'in', res = 500)
          
          ##
          raster::plot(current_suit_thresh_resample, main = paste0(taxa, ' > ', thresh), legend = FALSE)
          raster::plot(country_poly, add = TRUE, legend = FALSE)
          dev.off()
          
        } else {
          message('Habitat Suitability threshold raster already exists for', taxa, ' skip')
          cat(taxa)
        }
        
      } else {
        message('No Habitat Suitability raster for', taxa, ' skip')
        cat(taxa)
      }
    }) 
}




#' @title Taxa records intersect 
#' @description Take a table of taxa records, and intersect the records for each taxa with a shapefile  
#' of habitat (e.g. Vegetation).

#' @param analysis_df     SpatialPolygonsDataFrame - Spdf of all the taxa analysed
#' @param taxa_list       Character string - The species to run maxent predictions for
#' @param taxa_level      Character string - the taxnomic level to run maxent models for
#' @param habitat_raster  Character string - The habitat raster which has already been read in
#' @param country_shp     Character string - Shapefile name that has already been read into R (e.g. in the Package)
#' @param buffer          Numeric          - Distance by which to buffer the points (metres using a projected system)
#' @param write_rasters   Logical          - Save rasters (T/F)?
#' @export
taxa_records_habitat_intersect = function(analysis_df,
                                          taxa_list,
                                          taxa_level,
                                          habitat_poly,
                                          output_path,
                                          buffer) {
  
  ## Loop over each directory
  ## taxa = taxa_list[7]
  lapply(taxa_list, function(taxa) {
    
    ## Check if the taxa exists
    if(taxa %in%  unique(analysis_df$searchTaxon)) {
      
      taxa_name  <- gsub(' ', '_', taxa)
      raster_int <- paste0(output_path, taxa_name, '_VEG_intersection_', buffer, 'm.tif')
      
      if(!file.exists(raster_int)) {
        
        ## For each taxa, get the same records that were used in the SDM analysis 
        taxa_df   <- subset(analysis_df, searchTaxon == taxa | !!sym(taxa_level) == taxa)
        
        ## Buffer the points by 50km
        taxa_buffer <- gBuffer(taxa_df, width = buffer)
        
        ## If the taxa don't intersect with the veg layer, we need an exception there
        
        ## Clip the habitat polygon by the 50km buffer
        message('Clip habitat layer to the taxa df for ', taxa)
        habitat_subset <- habitat_poly[taxa_buffer, ]
        
        if(nrow(habitat_subset@data) >0 ) {
          
          ## Intersect clipped habitat with buffer
          ## do we need another exception here?
          message('Intersect taxa df with SVTM for ', taxa)
          taxa_intersects           <- gIntersects(habitat_subset, taxa_buffer, byid = TRUE) 
          taxa_VEG_intersects      <- habitat_subset[as.vector(taxa_intersects), ]
          taxa_VEG_intersects_clip <- raster::crop(taxa_VEG_intersects, taxa_buffer)
          
          gc()
          
          ## Save intersection as a raster
          ## Set the ncol/nrow to match 100m resolutions
          message('convert shapefile to raster for ', taxa)
          extent   <- extent(taxa_VEG_intersects_clip)
          x_length <- (extent[2] - extent[1])/100
          x_length <- round(x_length)
          y_length <- (extent[4] - extent[3])/100
          y_length <- round(y_length)
          
          ## Set the values to 1 : any veg within xkm is considered decent habitat
          r          <- raster(ncol = x_length, nrow = y_length)
          extent(r)  <- extent
          taxa_VEG_intersects_raster <- terra::rasterize(taxa_VEG_intersects_clip, r)
          taxa_VEG_intersects_raster[taxa_VEG_intersects_raster > 0] <- 1
          taxa_VEG_intersects_raster[taxa_VEG_intersects_raster < 0] <- 1
          
          gc()
          
          ## Raster intersect :: doesn't work because the LUT is not working
          ## Get the cells from the raster at those points
          # habitat_id      = cellFromXY(habitat_raster, taxa_df[c("lon", "lat")])  ## Index
          # taxa_rs_habitat = habitat_raster[habitat_id, drop = FALSE]              ## Values
          writeOGR(obj    = taxa_VEG_intersects_clip,
                   dsn    = 'G:/North_east_NSW_fire_recovery/output/veg_climate_topo_maxent/Habitat_suitability/SVTM_intersect',
                   layer  = paste0(taxa_name, '_VEG_intersect'), 
                   driver = 'ESRI Shapefile', 
                   overwrite_layer = TRUE)
          
          ## Save the taxa * habitat intersection as a raster
          message('writing threshold png for ', taxa)
          png(paste0(output_path, taxa_name, "_VEG_intersection.png"),
              16, 10, units = 'in', res = 500)
          
          ##
          plot(taxa_VEG_intersects_raster, main = paste0(taxa, ' SVTM Intersection'))
          plot(taxa_df, add = TRUE, col = "red", lwd = 3)
          dev.off()
          
          ## Save in two places, in the taxa folder, 
          ## and in the habitat suitability folder
          writeRaster(taxa_VEG_intersects_raster, 
                      paste0(output_path, taxa_name, '_VEG_intersection_', buffer, 'm.tif'),
                      overwrite = TRUE)
          
          gc()
          
        } else {
          message('Habitat does not intersect with ', taxa, ' skip')
        }
        
      } else {
        message('Habitat intersect already done for ', taxa, ' skip')
      }
      
    } else {
      message('Skip habitat intersect for ', taxa, ' no data')
    }
    
  })
  
}





#' @title Intersect habitat suitability rasters with Fire layers.
#' @description Takes a habitat suitability layer, and intersects it with a fire suitability layer.
#' @param taxa_list          Character string - The species to run maxent predictions for
#' @param target_path        Character string - The file path containing the existing maxent models
#' @param intersect_path     Character string - The file path containing the intersecting rasters
#' @param raster_pattern     Character string - The pattern to look for of Invertebrate rasters
#' @param targ_maxent_table  Data frame       - A table of maxent results to be used for mapping 
#' @param cell_size          Numeric          - Cell size to resample output
#' @param country_shp        Character string - Shapefile name that has already been read into R (e.g. in the Package)
#' @param country_prj        Character string - Name of projection
#' @param write_rasters      Logical          - Save rasters (T/F)?
#' @export
calculate_taxa_habitat = function(taxa_list,
                                  targ_maxent_table,
                                  host_maxent_table,
                                  target_path,
                                  output_path,
                                  intersect_path,
                                  raster_pattern,
                                  fire_raster,
                                  cell_size,
                                  fire_thresh,
                                  write_rasters,
                                  country_shp,
                                  country_prj) {
  
  ## Get the AUS shapefile
  country_poly <- get(country_shp) %>%
    spTransform(country_prj)
  
  ## Pipe the list into Lapply
  taxa_list %>%
    
    ## Loop over just the species
    ## taxa = taxa_list[74]
    lapply(function(taxa) {
      
      ## Get the directory of the host plants
      host_dir <- targ_maxent_table %>%
        filter(searchTaxon == taxa) %>%
        dplyr::select(host_dir)     %>%
        distinct() %>% .[1, ] %>% .[[1]]
      
      ## Get the directory of the host plants
      host_taxa <- targ_maxent_table    %>%
        filter(searchTaxon == taxa)     %>%
        dplyr::select(Host_Plant_taxon) %>%
        distinct() %>% .[1, ] %>% .[[1]]
      
      ## Get the sdm threshold for each inv taxa
      target_thresh <- targ_maxent_table  %>%
        filter(searchTaxon == taxa)       %>%
        dplyr::select(Logistic_threshold) %>%
        distinct() %>% .[1, ] %>% .[[1]]
      
      ## Get the sdm threshold for each host taxa
      host_thresh <- host_maxent_table    %>%
        filter(searchTaxon == host_taxa)  %>%
        dplyr::select(Logistic_threshold) %>%
        distinct() %>% .[1, ] %>% .[[1]]
      
      ## Get the taxa directory name
      taxa_name <- gsub(' ', '_', taxa)
      host_name <- gsub(' ', '_', host_taxa)
      
      current_thresh = sprintf('%s/%s/full/%s_%s%s.tif', target_path,
                               taxa_name, taxa_name, "current_suit_not_novel_above_", target_thresh)
      
      ## If the invert taxa has a host plant, use the SDM from the host plant
      if(is.na(host_dir)) {
        
        ## If the threshold raster data doesn't exist :
        if(file.exists(current_thresh)) {
          
          ## Print the taxa being analysed
          message('Intersecting SDM with Fire for', taxa, ' | Logistic > ', target_thresh)
          
          ## Read in the current suitability raster :: get the current_not_novel raster
          sdm_threshold    <- raster(current_thresh)
          
          ## Read the SVTM intersect file in
          intersect_file <- list.files(intersect_path, pattern = raster_pattern, full.names = TRUE) %>% 
            .[grep(paste0(taxa_name, collapse = '|'), ., ignore.case = TRUE)]
          
          if(length(intersect_file) == 1) {
            
            message('SDM and Veg rasters intersect for ', taxa, ' but it does not have a host taxa')
            intersect_raster <- raster(intersect_file)
            
            ## Re-sample
            message('resampling Veg intersect raster for ', taxa)
            intersect_sdm <- raster::resample(intersect_raster, sdm_threshold, "bilinear", exent = extent(sdm_threshold))
            
            ##
            message('mosaic Veg and SDM rasters for ', taxa)
            sdm_plus_veg <- raster::mosaic(sdm_threshold, intersect_sdm, fun = max)
            
            ## Multiply the SDM raster by the Fire Raster
            message('multiply habitat raster by the fire raster')
            sdm_plus_veg_intersect_fire <- sdm_plus_veg * fire_raster
            
            ## Then do the Cell stats ::
            ## estimated x % of each taxa's habitat in each fire intensity category (Severe, moderate, low, etc).
            habitat_fire_crosstab <- raster::crosstab(sdm_plus_veg, fire_raster, useNA = TRUE, long = TRUE)
            colnames(habitat_fire_crosstab) <- c('Habitat_taxa', 'FESM_intensity', 'km2')
            
            ## Filter out values we don't want - where habitat = 1, but KEEP where FIRE is NA
            ## If FIRE is NA, that means that....
            sdm_fire_crosstab <- dplyr::filter(habitat_fire_crosstab, Habitat_taxa == 1)
            sdm_fire_crosstab <- sdm_fire_crosstab %>%  
              
              ## Calculate the % burnt in each category, and also the km2
              mutate(km2   = km2/cell_size)             %>% 
              mutate(Percent      = km2/sum(km2) * 100) %>% 
              mutate(Percent      = round(Percent, 2))                %>% 
              mutate(Habitat_taxa = taxa) %>%
              
              ## FESM scores - there
              mutate(
                FESM_intensity = case_when(
                  
                  FESM_intensity == 0 ~ "Unburnt",
                  FESM_intensity == 1 ~ "Non-FESM Burnt Area",
                  FESM_intensity == 2 ~ "Low severity",
                  FESM_intensity == 3 ~ "Moderate severity",
                  FESM_intensity == 4 ~ "High severity",
                  FESM_intensity == 5 ~ "Extreme severity",
                  TRUE                ~ "Outside FESM extent")
              )
            
            ## Save the % burnt layers
            write.csv(sdm_fire_crosstab, paste0(output_path, taxa_name, '_SDM_VEG_intersect_Fire.csv'), row.names = FALSE)
            
            ## Now write the rasters
            ## If the rasters don't exist, write them for each taxa/threshold
            writeRaster(sdm_plus_veg, 
                        paste0(output_path, taxa_name, '_SDM_VEG_intersect.tif'),
                        overwrite = TRUE)
            
            writeRaster(sdm_plus_veg_intersect_fire, 
                        paste0(output_path, taxa_name, '_SDM_VEG_intersect_Fire.tif'),
                        overwrite = TRUE)
            
            message('writing threshold png for ', taxa)
            png(paste0(output_path, taxa_name, '_SDM_VEG_intersect_Fire.png'),
                11, 4, units = 'in', res = 300)
            
            print(levelplot(stack(sdm_plus_veg,
                                  fire_raster,
                                  sdm_plus_intersect_fire, 
                                  quick = TRUE), margin = FALSE,
                            
                            ## Create a colour scheme using colbrewer: 100 is to make it continuos
                            ## Also, make it a one-directional colour scheme
                            scales      = list(draw = FALSE),
                            at = seq(0, 4, length = 8),
                            col.regions = colorRampPalette(rev(brewer.pal(5, 'Spectral'))),
                            
                            ## Give each plot a name: the third panel is the GCM
                            names.attr = c('SDM + Veg', 'Fire', ' [SDM + Veg] * Fire'),
                            colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                            main       = list(gsub('_', ' ', taxa), font = 4, cex = 2)) +
                    
                    ## Plot the Aus shapefile with the occurrence points for reference
                    ## Can the current layer be plotted on it's own?
                    ## Add the novel maps as vectors.
                    latticeExtra::layer(sp.polygons(country_poly), data = list(country_poly = country_poly))) 
            dev.off()
            
            gc()
            
          } else {
            message('SDM and Veg rasters do not intersect for ', taxa, ' and it does not have a host taxa')
            
            ## Multiply the SDM raster by the Fire Raster
            message('multiply habitat raster by the fire raster')
            sdm_intersect_fire <- sdm_threshold * fire_raster
            
            ## Then do the Cell stats ::
            ## estimated x % of each taxa's habitat in each fire intensity category (Severe, moderate, low, etc).
            habitat_fire_crosstab <- raster::crosstab(sdm_threshold, fire_raster, useNA = TRUE, long = TRUE)
            colnames(habitat_fire_crosstab) <- c('Habitat_taxa', 'FESM_intensity', 'km2')
            
            ## Filter out values we don't want - where habitat = 1, but KEEP where FIRE is NA
            ## If FIRE is NA, that means that....
            sdm_fire_crosstab <- dplyr::filter(habitat_fire_crosstab, Habitat_taxa == 1)
            sdm_fire_crosstab <- sdm_fire_crosstab %>% 
              
              ## Calculate the % burnt in each category, and also the km2
              mutate(km2   = km2/cell_size)             %>% 
              mutate(Percent      = km2/sum(km2) * 100) %>% 
              mutate(Percent      = round(Percent, 2))                %>% 
              mutate(Habitat_taxa = taxa) %>%
              
              ## FESM scores - there
              mutate(
                FESM_intensity = case_when(
                  
                  FESM_intensity == 0 ~ "Unburnt",
                  FESM_intensity == 1 ~ "Non-FESM Burnt Area",
                  FESM_intensity == 2 ~ "Low severity",
                  FESM_intensity == 3 ~ "Moderate severity",
                  FESM_intensity == 4 ~ "High severity",
                  FESM_intensity == 5 ~ "Extreme severity",
                  TRUE                ~ "Outside FESM extent")
              )
            
            ## Save the % burnt layers
            write.csv(sdm_fire_crosstab, paste0(output_path, taxa_name, '_SDM_VEG_intersect_Fire.csv'), row.names = FALSE)
            
            writeRaster(sdm_intersect_fire, 
                        paste0(output_path, taxa_name, '_SDM_VEG_intersect_Fire.tif'),
                        overwrite = TRUE)
            
            message('writing threshold png for ', taxa)
            png(paste0(output_path, taxa_name, '_SDM_VEG_intersect_Fire.png'),
                11, 4, units = 'in', res = 300)
            
            print(levelplot(stack(sdm_threshold,
                                  fire_raster,
                                  sdm_intersect_fire,
                                  quick = TRUE), margin = FALSE,
                            
                            ## Create a colour scheme using colbrewer: 100 is to make it continuos
                            ## Also, make it a one-directional colour scheme
                            scales      = list(draw = FALSE),
                            at = seq(0, 4, length = 8),
                            col.regions = colorRampPalette(rev(brewer.pal(5, 'Spectral'))),
                            
                            ## Give each plot a name: the third panel is the GCM
                            names.attr = c('SDM', 'Fire', 'SDM * Fire'),
                            colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                            main       = list(gsub('_', ' ', taxa), font = 4, cex = 2)) +
                    
                    ## Plot the Aus shapefile with the occurrence points for reference
                    ## Can the current layer be plotted on it's own?
                    ## Add the novel maps as vectors.
                    latticeExtra::layer(sp.polygons(country_poly), data = list(country_poly = country_poly))) 
            dev.off()
            
            gc()
            
          }
          
        } else {
          message('Habitat Suitability threshold raster does not exist for', taxa, ' skip')
          cat(taxa)
        }
        
        
      } else {
        message(taxa, ' has a host plant, use both SDMs')
        
        ## Print the taxa being analysed
        message('calculating habitat * fire for ', taxa, ' | Logistic > ', target_thresh)
        host_threshold <- paste0(host_dir, host_name, "_current_suit_not_novel_above_", host_thresh, '.tif')
        
        if(file.exists(host_threshold)) {
          
          ## Read in host and target rasters
          host_threshold <- raster(host_threshold)
          sdm_threshold  <- raster(current_thresh)
          
          ## Read the SVTM intersect file in
          intersect_file <- list.files(intersect_path, pattern = raster_pattern, full.names = TRUE) %>% 
            .[grep(paste0(taxa_name, collapse = '|'), ., ignore.case = TRUE)]
          
          if(length(intersect_file) == 1) {
            
            message('SDM and Veg rasters intersect for ', taxa, ' and it has a host taxa')
            intersect_raster <- raster(intersect_file)
            
            ## Re-sample
            message('resampling Veg intersect raster for ', taxa)
            intersect_sdm <- raster::resample(intersect_raster, sdm_threshold, "bilinear", exent = extent(sdm_threshold))
            
            ##
            message('mosaicing Veg, host and target SDM rasters for ', taxa)
            sdm_plus_host_veg <- raster::mosaic(sdm_threshold, host_threshold, intersect_sdm, fun = max)
            
            ## Multiply the SDM raster by the Fire Raster
            message('Multiply Combo habitat raster by the fire raster')
            sdm_plus_veg_intersect_fire <- sdm_plus_host_veg * fire_raster
            
            ## Then do the Cell stats ::
            ## estimated x % of each taxa's habitat in each fire intensity category (Severe, moderate, low, etc).
            habitat_fire_crosstab <- raster::crosstab(sdm_plus_host_veg, fire_raster, useNA = TRUE, long = TRUE)
            colnames(habitat_fire_crosstab) <- c('Habitat_taxa', 'FESM_intensity', 'km2')
            
            ## Filter out values we don't want - where habitat = 1, but KEEP where FIRE is NA
            ## If FIRE is NA, that means that....
            sdm_fire_crosstab <- dplyr::filter(habitat_fire_crosstab, Habitat_taxa == 1)
            sdm_fire_crosstab <- sdm_fire_crosstab %>%  
              
              ## Calculate the % burnt in each category, and also the km2
              mutate(km2   = km2/cell_size)             %>% 
              mutate(Percent      = km2/sum(km2) * 100) %>% 
              mutate(Percent      = round(Percent, 2))                %>% 
              mutate(Habitat_taxa = taxa) %>%
              
              ## FESM scores - there
              mutate(
                FESM_intensity = case_when(
                  
                  FESM_intensity == 0 ~ "Unburnt",
                  FESM_intensity == 1 ~ "Non-FESM Burnt Area",
                  FESM_intensity == 2 ~ "Low severity",
                  FESM_intensity == 3 ~ "Moderate severity",
                  FESM_intensity == 4 ~ "High severity",
                  FESM_intensity == 5 ~ "Extreme severity",
                  TRUE                ~ "Outside FESM extent")
              )
            ## Save the % burnt layers
            write.csv(sdm_fire_crosstab, paste0(output_path, taxa_name, '_SDM_Host_VEG_intersect_Fire.csv'), row.names = FALSE)
            
            ## Now write the rasters
            ## If the rasters don't exist, write them for each taxa/threshold
            writeRaster(sdm_plus_host_veg, 
                        paste0(output_path, taxa_name, '_SDM_Host_intersect.tif'),
                        overwrite = TRUE)
            
            writeRaster(sdm_plus_veg_intersect_fire, 
                        paste0(output_path, taxa_name, '_SDM_Host_VEG_intersect_Fire.tif'),
                        overwrite = TRUE)
            
            message('writing threshold png for ', taxa)
            png(paste0(output_path, taxa_name, '_SDM_Host_VEG_intersect_Fire.png'),
                11, 4, units = 'in', res = 300)
            
            print(levelplot(stack(sdm_plus_veg,
                                  fire_raster,
                                  sdm_plus_veg_intersect_fire, 
                                  quick = TRUE), margin = FALSE,
                            
                            ## Create a colour scheme using colbrewer: 100 is to make it continuous
                            ## Also, make it a one-directional colour scheme
                            scales      = list(draw = FALSE),
                            at = seq(0, 4, length = 8),
                            col.regions = colorRampPalette(rev(brewer.pal(5, 'Spectral'))),
                            
                            ## Give each plot a name: the third panel is the GCM
                            names.attr = c('SDMs + Veg', 'Fire', ' [SDMs + Veg] * Fire'),
                            colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                            main       = list(gsub('_', ' ', taxa), font = 4, cex = 2)) +
                    
                    ## Plot the Aus shapefile with the occurrence points for reference
                    ## Can the current layer be plotted on it's own?
                    ## Add the novel maps as vectors.
                    latticeExtra::layer(sp.polygons(country_poly), data = list(country_poly = country_poly))) 
            dev.off()
            
            gc()
            
          } else {
            message('SDM and Veg rasters do not intersect for ', taxa, 'but it has a host taxa')
            
            message('mosaicing host and target SDM rasters for ', taxa)
            sdm_plus_host <- raster::mosaic(sdm_threshold, host_threshold, fun = max)
            
            ## Multiply the SDM raster by the Fire Raster
            message('multiply habitat raster by the fire raster')
            sdm_plus_host_intersect_fire <- sdm_plus_host * fire_raster
            
            ## Then do the Cell stats ::
            ## estimated x % of each taxa's habitat in each fire intensity category (Severe, moderate, low, etc).
            habitat_fire_crosstab <- raster::crosstab(sdm_plus_host, fire_raster, useNA = TRUE, long = TRUE)
            colnames(habitat_fire_crosstab) <- c('Habitat_taxa', 'FESM_intensity', 'km2')
            
            ## Filter out values we don't want - where habitat = 1, but KEEP where FIRE is NA
            ## If FIRE is NA, that means that....
            sdm_fire_crosstab <- dplyr::filter(habitat_fire_crosstab, Habitat_taxa == 1)
            sdm_fire_crosstab <- sdm_fire_crosstab %>%  
              
              ## Calculate the % burnt in each category, and also the km2
              mutate(km2          = km2/cell_size)             %>% 
              mutate(Percent      = km2/sum(km2) * 100) %>% 
              mutate(Percent      = round(Percent, 2))                %>% 
              mutate(Habitat_taxa = taxa) %>%
              
              ## FESM scores - there
              mutate(
                FESM_intensity = case_when(
                  
                  FESM_intensity == 0 ~ "Unburnt",
                  FESM_intensity == 1 ~ "Non-FESM Burnt Area",
                  FESM_intensity == 2 ~ "Low severity",
                  FESM_intensity == 3 ~ "Moderate severity",
                  FESM_intensity == 4 ~ "High severity",
                  FESM_intensity == 5 ~ "Extreme severity",
                  TRUE                ~ "Outside FESM extent")
              )
            
            ## Save the % burnt layers
            write.csv(sdm_fire_crosstab, paste0(output_path, taxa_name, '_SDM_Host_intersect_Fire.csv'), row.names = FALSE)
            
            ## Write the current suitability raster, thresholded using the Maximum training
            ## sensitivity plus specificity Logistic threshold
            message('Writing ', taxa, ' SDM * Fire rasdters', ' max train > ', target_thresh)
            
            ## Now write the rasters
            ## If the rasters don't exist, write them for each taxa/threshold
            writeRaster(sdm_plus_host, 
                        paste0(output_path, taxa_name, '_SDM_Host_intersect.tif'),
                        overwrite = TRUE)
            
            ## Save in two places, in the taxa folder, 
            ## and in the habitat suitability folder
            writeRaster(sdm_plus_host_intersect_fire, 
                        paste0(output_path, taxa_name, '_SDM_Host_intersect_Fire.tif'),
                        overwrite = TRUE)
            
            message('writing SDM * FIRE png for ', taxa)
            png(paste0(output_path, taxa_name, '_SDM_Host_intersect_Fire.png'),
                11, 4, units = 'in', res = 300)
            
            print(levelplot(stack(sdm_plus_veg,
                                  sdm_plus_intersect_fire, 
                                  quick = TRUE), margin = FALSE,
                            
                            ## Create a colour scheme using colbrewer: 100 is to make it continuos
                            ## Also, make it a one-directional colour scheme
                            scales      = list(draw = FALSE),
                            at = seq(0, 4, length = 8),
                            col.regions = colorRampPalette(rev(brewer.pal(5, 'Spectral'))),
                            
                            ## Give each plot a name: the third panel is the GCM
                            names.attr = c('SDMs', ' [SDMs * Fire]'),
                            colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                            main       = list(gsub('_', ' ', taxa), font = 4, cex = 2)) +
                    
                    ## Plot the Aus shapefile with the occurrence points for reference...
                    ## Can the current layer be plotted on it's own?
                    ## Add the novel maps as vectors.
                    latticeExtra::layer(sp.polygons(country_poly), data = list(country_poly = country_poly)))
            
            dev.off()
            
            gc()
            
          }
          
        } else {
          message('Habitat Suitability threshold raster does not exist for', taxa, ' skip')
          cat(taxa)
        }
        
      }
    }) 
}





#' @title Project Current Habitat Suitability models into Grids.
#' @description takes the maxent models created by the 'fit_maxent_targ_bg_back_sel' function,
#' and projects the model across geographic space  


#' @param country_shp        SpatialPolygonsDataFrame - Spdf of the country for mapping maxent results (e.g. Australia)
#' @param world_shp          SpatialPolygonsDataFrame - Spdf of the world for mapping maxent results
#' @param country_prj        CRS object  - Local projection for mapping maxent results
#' @param world_prj          CRS object  - Global projection for mapping maxent results
#' @param local_prj          CRS object  - Local projection for mapping maxent results
#' @param scen_list          Character string - The list of global circulation models to create predictions for
#' @param taxa_list       Character string - The species to run maxent predictions for
#' @param maxent_path        Character string - The file path containin the existing maxent models
#' @param climate_path       Character string - The file path where the climate data is saved
#' @param grid_names         Character string - Vector of enviro conditions that you want to include
#' @param time_slice         Character string - The time period to create predictions for (e.g. '2050', or '2070')
#' @param current_grids      Character string - Vector of current enviro conditions that you want to include
#' @param create_mess        Logical - Create mess maps of the preditions (T/F)?
#' @param nclust             Numeric - How many clusters to use for parallel processing (e.g. 1)
#' @param OSGeo_path         Character string - file path of .bat file for converting rasters to polygons (
#' you need to download the OSGeo4W64 setup, see https://www.osgeo.org/)
#' @details It uses the rmaxent package https://github.com/johnbaums/rmaxent
#' @export
project_maxent_grids_mess = function(country_shp,   world_shp,
                                     country_prj,   world_prj, local_prj,
                                     scen_list,     taxa_list,
                                     maxent_path,   climate_path,
                                     grid_names,    time_slice,
                                     current_grids, create_mess,
                                     nclust,        OSGeo_path) {
  
  ## Read in the Aus and world shapefile and re-rpoject
  country_poly   <- country_shp %>%
    spTransform(country_prj)
  
  world_poly <- world_shp %>%
    spTransform(world_prj)
  
  ## First, run a loop over each scenario:
  lapply(scen_list, function(x) {
    
    ## Create a raster stack for each of the 6 GCMs, not for each species
    ## They need to have exactly the same extent.
    ## Could stack all the rasters, or, keep them separate
    ## x = scen_list[1]
    s <- stack(c(sprintf('%s/20%s/%s/%s%s.tif', climate_path, time_slice, x, x, 1:19)))
    identical(projection(s), projection(country_poly))
    
    ## Rename both the current and future environmental stack...
    ## critically important that the order of the name
    ## So this needs to include all the predicor names :: climate, soil, etc
    
    ## Note this step is only needed if the current grids used in the their original form, rather than being renamed
    names(s) <- names(current_grids) <- grid_names
    
    ## Divide the 11 temperature rasters by 10: NA values are the ocean
    message('First, divide the raster stack for ', x, ' by 10 ')
    for(i in 1:11) {
      ## Simple loop
      message(i)
      s[[i]] <- s[[ i]]/10
    }
    
    ## Then apply each GCM to each species.
    ## First, check if the maxent model exists
    ## Then apply each GCM to each species
    
    ## Define function to then send to one or multiple cores
    maxent_predict_fun <- function(species) {
      
      ## Create species name
      ## species = map_spp[1]
      save_name = gsub(' ', '_', species)
      
      ## Create a path for the current prediction of the species
      f_current  <- sprintf('%s%s/full/%s_current.tif', maxent_path, species, species)
      
      ## Check the file exists
      if(file.exists(sprintf('%s/%s/full/maxent_fitted.rds', maxent_path, species))) {
        message('Then run maxent projections for ', species, ' under ', x, ' scenario')
        
        ## Then, check if the species projection has already been run...
        if(!file.exists(sprintf('%s/%s/full/%s_future_not_novel_%s.tif',
                                maxent_path, species, species, x))) {
          
          ## Now read in the SDM model, calibrated on current conditions
          ## if it was run with backwards selection, just use the full model
          if (grepl("back", maxent_path)) {
            
            message('Read in the BS model')
            m   <- readRDS(sprintf('%s/%s/full/maxent_fitted.rds', maxent_path, save_name))
            
          } else {
            ## Otherwise, index the full model
            message('Read in the full model')
            m   <- readRDS(sprintf('%s/%s/full/maxent_fitted.rds', maxent_path, save_name))$me_full
          }
          
          ## Read in species with data and occurrence files
          swd <- as.data.frame(readRDS(sprintf('%s%s/swd.rds',    maxent_path, species, species)))
          occ <- readRDS(sprintf('%s%s/%s_occ.rds', maxent_path, species, species)) %>%
            spTransform(country_prj)
          
          ## If the current raster prediction has not been run, run it
          if(!file.exists(f_current) == TRUE) {
            
            ## Report which prediction is in progress :: m$me_full, m$me_full@presence
            message('Running current prediction for ', species)
            
            pred.current <- rmaxent::project(
              m, current_grids[[colnames(m@presence)]])$prediction_logistic
            writeRaster(pred.current, f_current, overwrite = TRUE)
            
          } else {
            message('Use existing prediction for ', species)
            pred.current = raster(sprintf('%s/%s/full/%s_current.tif',
                                          maxent_path, species, species))
          }
          
          ## Report current mess map in progress
          ## Could work out how to the static mess once, before looping through scenarios
          MESS_dir = sprintf('%s%s/full/%s',
                             maxent_path, species, 'MESS_output')
          
          ## If the current novel layer doesn't exist, create it
          if(!file.exists(sprintf('%s/%s%s.tif', MESS_dir, species, "_current_novel")))  {
            
            ## Set the names of the rasters to match the occ data, and subset both
            sdm_vars             = names(m@presence)
            current_grids        = subset(current_grids, sdm_vars)
            swd                  = swd [,sdm_vars]
            
            ##
            message('Are the environmental variables identical? ',
                    identical(names(swd), names(current_grids)))
            
            ## Create a map of novel environments for current conditions.
            ## This similarity function only uses variables (e.g. n bioclim), not features
            message('Run similarity function for current condtions for ', species)
            mess_current  <- similarity(current_grids, swd, full = TRUE)
            novel_current <- mess_current$similarity_min < 0  ##   All novel environments are < 0
            novel_current[novel_current==0] <- NA             ##   0 values are NA
            
            
          } else {
            ## Otherwise, read in the current novel layer
            message(species, ' Current similarity analysis already run')
            novel_current = raster(sprintf('%s/%s%s.tif', MESS_dir, species, "_current_novel"))
          }
          
          ## Write out the current mess maps -
          ## create a new folder for the mess output - we are going to print it to the maps
          if(!dir.exists(MESS_dir)) {
            message('Creating MESS directory for ', species)
            dir.create(MESS_dir)
            
            ## Create a PNG file of MESS maps for each maxent variable
            ## raster_list  = unstack(mess_current$similarity) :: list of environmental rasters
            ## raster_names = names(mess_current$similarity)   :: names of the rasters
            message('Creating mess maps of each current environmental predictor for ', species)
            mapply(function(raster, raster_name) {
              
              ## Create a level plot of MESS output for each predictor variable, for each species
              p <- levelplot(raster, margin = FALSE, scales = list(draw = FALSE),
                             at = seq(minValue(raster), maxValue(raster), len = 100),
                             colorkey = list(height = 0.6),
                             main = gsub('_', ' ', sprintf(' Current_mess_for_%s (%s)', raster_name, species))) +
                
                latticeExtra::layer(sp.polygons(country_poly), data = list(country_poly = country_poly))  ## need list() for polygon
              
              p <- diverge0(p, 'RdBu')
              f <- sprintf('%s/%s%s%s.png', MESS_dir, species, "_current_mess_", raster_name)
              
              png(f, 8, 8, units = 'in', res = 300, type = 'cairo')
              print(p)
              dev.off()
              
            }, unstack(mess_current$similarity), names(mess_current$similarity))
            
          } else {
            message(species, ' MESS directory already created')
          }
          
          ## Write the raster of novel environments to the MESS sub-directory
          if(!file.exists(sprintf('%s/%s%s.tif', MESS_dir, species, "_current_novel")))  {
            
            message('Writing currently novel environments to file for ', species)
            writeRaster(novel_current, sprintf('%s/%s%s.tif', MESS_dir, species, "_current_novel"),
                        overwrite = TRUE)
            
          } else {
            message(species, ' Current MESS file already saved')
          }
          
          ## Now mask out novel environments
          ## is.na(novel_current) is a binary layer showing
          ## not novel [=1] vs novel [=0],
          ## so multiplying this with hs_current will mask out novel
          hs_current_not_novel <- pred.current * is.na(novel_current)
          
          ## Write out not-novel raster :: this can go to the main directory
          message('Writing currently un-novel environments to file for ', species)
          writeRaster(hs_current_not_novel, sprintf('%s%s/full/%s%s.tif', maxent_path,
                                                    species, species, "_current_not_novel"),
                      overwrite = TRUE)
          
          ## Create file path for future raster doesn't exist, create it
          f_future <- sprintf('%s/%s/full/%s_future_not_novel_%s.tif',
                              maxent_path, species, species, x)
          
          if(!file.exists(f_future)) {
            
            ## Report which prediction is in progress
            message('Running future maxent prediction for ', species, ' under ', x)
            
            ## Create the future raster
            pred.future <- rmaxent::project(
              m, s[[colnames(m@presence)]])$prediction_logistic
            writeRaster(pred.future, f_future, overwrite = TRUE)
            
            ## Report future mess map in progress
            if(create_mess == TRUE) {
              message('Running future mess map for ', species, ' under ', x)
              
              ## Check if the future environments have been created
              hs_future_not_novel = sprintf('%s%s/full/%s%s%s.tif', maxent_path,
                                            species, species, "_future_not_novel_", x)
              
              ## Set the names of the rasters to match the occ data, and subset both
              ## Watch the creation of objects in each run
              sdm_vars             = names(m@presence)
              future_grids         = s
              future_grids         = subset(future_grids, sdm_vars)
              swd                  = swd [,sdm_vars]
              identical(names(swd), names(future_grids))
              
              ## Create a map of novel environments for future conditions
              ## This similarity function only uses variables (e.g. n bioclim), not features.
              ## We don't need to repeat the static layer MESS each time - just
              ## include their results here
              mess_future  <- similarity(future_grids, swd, full = TRUE)
              novel_future <- mess_future$similarity_min < 0  ##   All novel environments are < 0
              novel_future[novel_future==0] <- NA             ##   0 values are NA
              
              ## Write out the future mess maps, for all variables
              writeRaster(mess_future$similarity_min, sprintf('%s/%s%s%s.tif', MESS_dir, species, "_future_mess_", x),
                          overwrite = TRUE)
              
              ## Create a PNG file of all the future MESS output:
              ## raster_list  = unstack(mess_current$similarity) :: list of environmental rasters
              ## raster_names = names(mess_current$similarity)   :: names of the rasters
              message('Creating mess maps of each future environmental predictor for ',
                      species, ' under scenario ', x)
              mapply(function(raster, raster_name) {
                
                p <- levelplot(raster, margin = FALSE, scales = list(draw = FALSE),
                               at = seq(minValue(raster), maxValue(raster), len = 100),
                               colorkey = list(height = 0.6),
                               main = gsub('_', ' ', sprintf('Future_mess_for_%s_%s (%s)',
                                                             raster_name, x, species, x))) +
                  
                  latticeExtra::layer(sp.polygons(country_poly), data = list(country_poly = country_poly))
                
                p <- diverge0(p, 'RdBu')
                f <- sprintf('%s/%s%s%s%s%s.png', MESS_dir, species, "_future_mess_", raster_name, "_", x)
                
                png(f, 8, 8, units = 'in', res = 300, type = 'cairo')
                print(p)
                dev.off()
                
              }, unstack(mess_future$similarity), names(mess_future$similarity))
              
              ## Write the raster of novel environments to the MESS maps sub-directory
              message('Writing future novel environments to file for ',    species, ' under scenario ', x)
              writeRaster(novel_future, sprintf('%s/%s%s%s.tif', MESS_dir, species, "_future_novel_",  x),
                          overwrite = TRUE)
              
              ## mask out future novel environments
              ## is.na(novel_future) is a binary layer showing
              ## not novel [=1] vs novel [=0],
              ## so multiplying this with hs_future will mask out novel
              hs_future_not_novel <- pred.future * is.na(novel_future)
              
              ## This layer of future un-novel environments can be used
              ## for the next algorithm step, where we combine the models
              summary(pred.future,         maxsamp = 100000);
              summary(hs_future_not_novel, maxsamp = 100000)
              
              ## Write out not-novel raster
              ## Try to set up loops so different cores aren't accessing the same files
              message('Writing un-novel environments to file under ', x, ' scenario for ', species)
              writeRaster(hs_future_not_novel, sprintf('%s%s/full/%s%s%s.tif', maxent_path,
                                                       species, species, "_future_not_novel_", x),
                          overwrite = TRUE)
              
            } else {
              message('Dont run future MESS maps for ', species, ' under scenario ',  x )
            }
            
            ## Try writing the current raster out here - this was causing issues....
            #writeRaster(pred.current, f_current, overwrite = TRUE)
            
            ## If we're on windows, use the GDAL .bat file
            current_novel_poly <- polygonizer_windows(sprintf('%s/%s%s.tif',   MESS_dir, species, "_current_novel"),
                                                      OSGeo_path = OSGeo_path)
            novel_future_poly  <- polygonizer_windows(sprintf('%s/%s%s%s.tif', MESS_dir, species, "_future_novel_", x),
                                                      OSGeo_path = OSGeo_path)
            
            ## Create the MESS path and save shapefiles
            MESS_shp_path   = sprintf('%s%s/full/%s',
                                      maxent_path, species, 'MESS_output')
            
            ## Check if the current MESS shapefile exists?
            novel_current_shp <- sprintf('%s/%s%s.shp',   MESS_dir, species, "_current_novel_polygon")
            if(!file.exists(novel_current_shp)) {
              
              ## Re-project the shapefiles
              current_novel_poly = current_novel_poly %>%
                spTransform(country_prj)
              
              ## Now save the novel areas as shapefiles
              ## There is a problem with accessing the files at the same time
              message('Saving current MESS maps to polygons for ', species)
              writeOGR(obj    = current_novel_poly,
                       dsn    = sprintf('%s',  MESS_shp_path),
                       layer  = paste0(species, "_current_novel_polygon"),
                       driver = "ESRI Shapefile", overwrite_layer = TRUE)
            } else {
              message(' Current MESS maps already saved to polygons for ', species)
            }
            
            ## Check if the future MESS shapefile exists?
            novel_future_shp <- sprintf('%s/%s%s%s.shp',   MESS_dir, species, "_future_novel_polygon_", x)
            if(!file.exists(novel_future_shp)) {
              
              novel_future_poly = novel_future_poly %>%
                spTransform(local_prj)
              
              message('Saving current MESS maps to polygons for ', species)
              writeOGR(obj    = novel_future_poly,
                       dsn    = sprintf('%s',  MESS_shp_path),
                       layer  = paste0(species, "_future_novel_polygon_", x),
                       driver = "ESRI Shapefile", overwrite_layer = TRUE)
            } else {
              message('Future MESS maps already saved to polygons for ', species)
            }
            
            ## Create a SpatialLines object that indicates novel areas (this will be overlaid)
            ## Below, we create a dummy polygon as the first list element (which is the extent
            ## of the raster, expanded by 10%), to plot on panel 1). 50 = approx 50 lines across the polygon
            message('Creating polygon list under ', x, ' scenario for ', species)
            
            ## Cast the objects into the sf class so we avoid issues with wrong methods being called in hatch()
            novel_hatch <- list(as(extent(pred.current)*1.1, 'SpatialLines'),
                                hatch(current_novel_poly, 50),
                                hatch(novel_future_poly, 50))
            
            ## Now create a panel of PNG files for maxent projections and MESS maps
            ## All the projections and extents need to match
            empty_ras <- init(pred.current, function(x) NA)
            projection(current_novel_poly);projection(occ);projection(empty_ras);projection(poly)
            projection(pred.current);projection(pred.future)
            identical(extent(pred.current), extent(pred.future))
            
            ## Assign the scenario name (to use in the plot below)
            scen_name = eval(parse(text = sprintf('gcms.%s$GCM[gcms.%s$id == x]', time_slice, time_slice)))
            
            ## Use the 'levelplot' function to make a multipanel output:
            ## occurrence points, current raster and future raster
            current_mess_png = sprintf('%s/%s/full/%s_%s.png', maxent_path, species, species, "mess_panel")
            if(!file.exists(current_mess_png)) {
              
              ## Create level plot of current conditions including MESS
              message('Create current MESS panel maps for ', species)
              
              png(sprintf('%s/%s/full/%s_%s.png', maxent_path, species, species, "mess_panel"),
                  11, 4, units = 'in', res = 300)
              
              print(levelplot(stack(empty_ras,
                                    pred.current, quick = TRUE), margin = FALSE,
                              
                              ## Create a colour scheme using colbrewer: 100 is to make it continuos
                              ## Also, make it a one-directional colour scheme
                              scales      = list(draw = FALSE),
                              at = seq(0, 1, length = 100),
                              col.regions = colorRampPalette(rev(brewer.pal(11, 'Spectral'))),
                              
                              ## Give each plot a name: the third panel is the GCM
                              names.attr = c('Australian records', ' Current'),
                              colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                              main       = list(gsub('_', ' ', species), font = 4, cex = 2)) +
                      
                      ## Plot the Aus shapefile with the occurrence points for reference
                      ## Can the current layer be plotted on it's own?
                      ## Add the novel maps as vectors.
                      latticeExtra::layer(sp.polygons(country_poly), data = list(country_poly = country_poly)) +
                      latticeExtra::layer(sp.points(occ, pch = 19, cex = 0.15,
                                                    col = c('red', 'transparent', 'transparent')[panel.number()]),
                                          data = list(occ = occ)) +
                      latticeExtra::layer(sp.lines(h[[panel.number()]]), data = list(h = novel_hatch)))
              dev.off()
              
            } else {
              message(' Current MESS panel maps already created for ', species)
            }
            
          }
          
          ## Save the global records to PNG :: try to code the colors for ALA/GBIF/INVENTORY
          occ.world <- readRDS(sprintf('%s/%s/%s_occ.rds', maxent_path, species, species)) %>%
            spTransform(world_prj)
          
          ## If the global map of occurrence points hasn't been created, create it
          global_occ_map = sprintf('%s/%s/full/%s_%s.png', maxent_path, species, species, "global_occ_records")
          if(!file.exists(global_occ_map)) {
            
            message('writing map of global records for ', species)
            png(sprintf('%s/%s/full/%s_%s.png', maxent_path, species, species, "global_occ_records"),
                16, 10, units = 'in', res = 500)
            
            ## Add land
            plot(world_poly, #add = TRUE,
                 lwd = 0.01, asp = 1, col = 'grey', bg = 'sky blue')
            
            ## Add points
            points(subset(occ.world, SOURCE == "GBIF"),
                   pch = ".", cex = 3.3, cex.lab = 3, cex.main = 4, cex.axis = 2,
                   xlab = "", ylab = "", asp = 1,
                   col = "orange",
                   legend(7,4.3, unique(occ.world$SOURCE), col = "orange", pch = 1))
            
            points(subset(occ.world, SOURCE == "ALA"),
                   pch = ".", cex = 3.3, cex.lab = 3, cex.main = 4, cex.axis = 2,
                   xlab = "", ylab = "", asp = 1,
                   col = "blue",
                   legend(7,4.3, unique(occ.world$SOURCE), col = "blue", pch = 1))
            
            points(subset(occ.world, SOURCE == "INVENTORY"),
                   pch = ".", cex = 3.3, cex.lab = 3, cex.main = 4, cex.axis = 2,
                   xlab = "", ylab = "", asp = 1,
                   col = "red",
                   legend(7,4.3, unique(occ.world$SOURCE), col = "red", pch = 1))
            
            title(main = list(paste0(gsub('_', ' ', species), ' global SDM records'), font = 4, cex = 2),
                  cex.main = 4,   font.main = 4, col.main = "black")
            
            dev.off()
            
          } else {
            message('Global occurrence maps already created for ', species)
          }
          
          ## Create level plot of scenario x, including MESS
          future_mess_png = sprintf('%s/%s/full/%s_%s.png', maxent_path, species, species, x)
          
          if(!file.exists(future_mess_png)) {
            png(sprintf('%s/%s/full/%s_%s.png', maxent_path, species, species, x),
                11, 4, units = 'in', res = 300)
            
            ## Create a panel of the Australian occurrences, the current layer and the future layer
            print(levelplot(stack(empty_ras,
                                  pred.current,
                                  pred.future, quick = TRUE), margin = FALSE,
                            
                            ## Create a colour scheme using colbrewer: 100 is to make it continuos
                            ## Also, make it a one-directional colour scheme
                            scales      = list(draw = FALSE),
                            at = seq(0, 1, length = 100),
                            col.regions = colorRampPalette(rev(brewer.pal(11, 'Spectral'))),
                            
                            ## Give each plot a name: the third panel is the GCM
                            names.attr = c('Australian records', ' Current',
                                           sprintf('%s, 20%s, RCP8.5', scen_name, time_slice)),
                            colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                            main       = list(gsub('_', ' ', species), font = 4, cex = 2)) +
                    
                    ## Plot the Aus shapefile with the occurrence points for reference
                    ## Can the current layer be plotted on it's own?
                    ## Add the novel maps as vectors.
                    latticeExtra::layer(sp.polygons(country_poly), data = list(country_poly = country_poly)) +
                    latticeExtra::layer(sp.points(occ, pch = 19, cex = 0.15,
                                                  col = c('red', 'transparent', 'transparent')[panel.number()]),
                                        data = list(occ = occ)) +
                    latticeExtra::layer(sp.lines(h[[panel.number()]]), data = list(h = novel_hatch)))
            dev.off()
            
          } else {
            message('Future MESS maps already created for ', species)
          }
          
        } else {
          message(species, ' ', x, ' skipped - prediction already run')
        }
        
      } else {
        message(species, ' ', x, ' skipped - SDM not yet run')
      }
    }
    
    ## Check this is the best way to run parallel
    if (nclust == 1) {
      
      lapply(taxa_list, maxent_predict_fun)
      
    } else {
      ## Export all objects from the function call
      message('Running project_maxent_grids_mess for ', length(taxa_list),
              ' species on ', nclust, ' cores for GCM ', x)
      parLapply(cl, taxa_list, maxent_predict_fun)
    }
  })
}



#' @title       Plot a rasterVis::levelplot
#' @description Plot a rasterVis::levelplot with a colour ramp diverging around zero
#' Adapted from a gist by John Baumgartner for the (https://gist.github.com/johnbaums?direction=desc&sort=updated).

#' @param p    A trellis object resulting from rasterVis::levelplot
#' @param ramp Character string - The name of an RColorBrewer palette (as character), a character
#' @export
diverge0 <- function(p, ramp) {
  
  ## p: a trellis object resulting from rasterVis::levelplot
  ## ramp: the name of an RColorBrewer palette (as character), a character
  ##       vector of colour names to interpolate, or a colorRampPalette.
  if(length(ramp)==1 && is.character(ramp) && ramp %in%
     row.names(brewer.pal.info)) {
    ramp <- suppressWarnings(colorRampPalette(brewer.pal(11, ramp)))
  } else if(length(ramp) > 1 && is.character(ramp) && all(ramp %in% colors())) {
    ramp <- colorRampPalette(ramp)
  } else if(!is.function(ramp))
    stop('ramp should be either the name of a RColorBrewer palette, ',
         'a vector of colours to be interpolated, or a colorRampPalette.')
  
  ##
  rng <- range(p$legend[[1]]$args$key$at)
  s   <- seq(-max(abs(rng)), max(abs(rng)), len=1001)
  i   <- findInterval(rng[which.min(abs(rng))], s)
  
  ##
  zlim <- switch(which.min(abs(rng)), `1`=i:(1000+1), `2`=1:(i+1))
  p$legend[[1]]$args$key$at <- s[zlim]
  p[[grep('^legend', names(p))]][[1]]$args$key$col <- ramp(1000)[zlim[-length(zlim)]]
  p$panel.args.common$col.regions <- ramp(1000)[zlim[-length(zlim)]]
  p
}






#' @title Create vector from a raster. 


#' @description This function takes a raster of a shapefile (for example the urban areas of Australia),
#' and creates a shapefile (i.e. a vector).
#' It uses the rmaxent package https://github.com/johnbaums/rmaxent
#' It assumes that the input df is that returned by the prepare_sdm_table function
#' 
#' @param shp_file           SpatialPolygonsDataFrame - Spdf of spatial units used to aggregate the SDMs (e.g. urban areas of Australia)
#' @param prj                CRS object - Local projection for mapping the shapefile (e.g. Australian Albers)
#' @param sort_var           Character string - The field name in the shapefile to use for sorting (e.g. Urban area names)
#' @param agg_var            Character string - The field name in the shapefile to use for aggregating SDM results (e.g. Urban area codes)
#' @param temp_ras           Raster - An existing raster with the same extent, resolution and projection as the maxent models (e.g. Australia)
#' @param targ_ras           Raster - An existing raster of the shapefile with the same extent, resolution and projection as the maxent models (e.g. Australia)
#' @export
shapefile_vector_from_raster = function (shp_file,
                                         prj,
                                         agg_var,
                                         temp_ras,
                                         targ_ras) {
  
  ## Read in shapefile
  areal_unit <- shp_file %>%
    spTransform(prj)
  
  ## Rasterize shapefile, insert 'sort_var'
  message('Rasterizing shapefile')
  #areal_unit = areal_unit[order(areal_unit$SUA_NAME16),]
  f <- tempfile()
  
  ## Write a temporary raster
  writeOGR(areal_unit, tempdir(), basename(f), 'ESRI Shapefile')
  template <- raster(temp_ras)
  
  ## Rasterize the shapefile
  areal_unit_rast <- gdalUtils::gdal_rasterize(
    normalizePath(paste0(f, '.shp')),
    
    ## The missing step is the .tiff, it was created somewhere else, in R or a GIS
    ## so 'a' needs to match in this step, and the next
    targ_ras, tr = res(template),
    te = c(bbox(template)), a = agg_var, a_nodata = 0, init = 0, ot = 'UInt16', output_Raster = TRUE)
  
  areal_unit_vec <- c(areal_unit_rast[])
  summary(areal_unit_vec)
  
  ## return the vector
  return(areal_unit_vec)
  
}





#########################################################################################################################
######################################  MAPPING FUNCTIONS FOR SDM ANALYSIS ---- #########################################
#########################################################################################################################

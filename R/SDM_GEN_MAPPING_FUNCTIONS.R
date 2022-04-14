#########################################################################################################################
######################################  MAPPING FUNCTIONS FOR SDM ANALYSIS ---- #########################################
#########################################################################################################################


## Below are the functions used to project SDM models across geographic areas.


#' @title Project current maxent models across geographic space
#' @description This function takes the maxent models created by the 'fit_maxent_targ_bg_back_sel' function,
#' and projects the model across geographic space - currently just for Australia.
#' @param taxa_list          Character string - The taxa to run maxent predictions for
#' @param maxent_path        Character string - The file path containing the existing maxent models
#' @param output_path        Character string - The file path where the polygon data will be saved
#' @param current_grids      Character string - Vector of current enviro conditions that you want to include
#' @param save_novel_poly    Character string - Save the novel areas as polygons?
#' @param create_mess        Logical - Create mess maps of the predictions (T/F)?
#' @param poly_path          Character string - file path to feature polygon layer.
#' @param epsg               Numeric - ERSP code of coord ref system to be translated into WKT format
#' @details It uses the rmaxent package https://github.com/johnbaums/rmaxent
#' @export project_maxent_current_grids_mess
project_maxent_current_grids_mess = function(taxa_list,
                                             maxent_path,   
                                             current_grids, 
                                             create_mess,
                                             save_novel_poly,
                                             output_path,
                                             poly_path,
                                             epsg) {
  
  
  ## Create feature polygon for plotting
  poly <- st_read(poly_path) %>% 
    st_transform(., st_crs(epsg)) %>% as_Spatial()
  
  ## Rename the raster grids
  ## Note this step is only needed if the current grids used in the 
  ## their original form, rather than being renamed
  # names(current_grids) <- grid_names
  
  ## First, run a loop over each scenario:
  lapply(taxa_list, function(x) {
    
    ## Then apply each GCM to each taxa.
    ## First, check if the maxent model exists
    ## Then apply each GCM to each taxa
    
    ## Define function to then send to one or multiple cores
    maxent_predict_fun <- function(taxa) {
      
      ## Create taxa name
      ## taxa = taxa_list[4]
      save_name = gsub(' ', '_', taxa)
      
      ## First check if the taxa exists
      current_mess_png = sprintf('%s/%s/full/%s_%s.png', maxent_path, save_name, save_name, "mess_panel")
      if(!file.exists(current_mess_png)) {
        
        ## Create a path for the current prediction of the taxa
        f_current  <- sprintf('%s%s/full/%s_current.tif', maxent_path, save_name, save_name)
        
        ## Check the file exists
        if(file.exists(sprintf('%s/%s/full/maxent_fitted.rds', maxent_path, save_name))) {
          message('Then run current maxent projections for ', taxa)
          
          ## Then, check if the taxa projection has already been run...
          if(!file.exists(sprintf('%s/%s/full/%s_future_not_novel.tif',
                                  maxent_path, save_name, save_name))) {
            
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
            
            ## Read in taxa with data and occurrence files
            message('Read in the swd and occ data')
            swd <- as.data.frame(readRDS(sprintf('%s%s/swd.rds', maxent_path, save_name)))
            occ <- readRDS(sprintf('%s%s/%s_occ.rds', maxent_path, save_name, save_name)) 
            
            ## If the current raster prediction has not been run, run it.
            if(!file.exists(f_current) == TRUE) {
              
              ## Report which prediction is in progress :: m$me_full, m$me_full@presence
              message('Running current maxent prediction for ', taxa)
              
              ## Set the names of the rasters to match the occ data, and subset both
              sdm_vars             = names(m@presence)
              current_grids        = raster::subset(current_grids, sdm_vars)
              swd                  = swd[,sdm_vars]
              
              pred.current <- rmaxent::project(
                m, current_grids[[colnames(m@presence)]])$prediction_logistic
              raster::writeRaster(pred.current, f_current, overwrite = TRUE)
              
              gc()
              
            } else {
              message('Use existing prediction for ', taxa)
              pred.current = raster::raster(sprintf('%s/%s/full/%s_current.tif',
                                                    maxent_path, save_name, save_name))
            }
            
            ## Report current mess map in progress
            ## Could work out how to the static mess once, before looping through scenarios
            MESS_dir = sprintf('%s%s/full/%s', maxent_path, save_name, 'MESS_output')
            
            ## If the current novel layer doesn't exist, create it
            if(!file.exists(sprintf('%s/%s%s.tif', MESS_dir, save_name, "_current_novel"))) {
              
              ##
              message('Are the environmental variables identical? ',
                      identical(names(swd), names(current_grids)))
              
              ## Create a map of novel environments for current conditions.
              ## This similarity function only uses variables (e.g. n bioclim), not features
              message('Run similarity function for current condtions for ', taxa)
              mess_current  <- similarity(current_grids, swd, full = TRUE)
              novel_current <- mess_current$similarity_min < 0    ## All novel environments are < 0
              novel_current[novel_current==0]              <- NA  ## 0 values are NA
              
              gc()
              
            } else {
              ## Otherwise, read in the current novel layer
              message(taxa, ' Current similarity analysis already run')
              novel_current = raster::raster(sprintf('%s/%s%s.tif', MESS_dir, save_name, "_current_novel"))
            }
            
            ## Write out the current mess maps -
            ## create a new folder for the mess output - we are going to print it to the maps
            if(!dir.exists(MESS_dir)) {
              message('Creating MESS directory for ', taxa)
              dir.create(MESS_dir)
              
              ## Create a PNG file of MESS maps for each maxent variable
              ## raster_list  = unstack(mess_current$similarity) :: list of environmental rasters
              ## raster_names = names(mess_current$similarity)   :: names of the rasters
              message('Creating mess maps of each current environmental predictor for ', taxa)
              
              ## raster_name <- raster_names[1]
              mapply(function(raster, raster_name) {
                
                ## Create a level plot of MESS output for each predictor variable, for each taxa
                p <- levelplot(raster, margin = FALSE, scales = list(draw = FALSE),
                               at = seq(minValue(raster), maxValue(raster), len = 100),
                               colorkey = list(height = 0.6),
                               main = gsub('_', ' ', sprintf(' Current_mess_for_%s (%s)', raster_name, save_name))) +
                  
                  latticeExtra::layer(sp.polygons(poly), 
                                      data = list(poly = poly)) ## need list() for polygon
                
                p <- diverge0(p, 'RdBu')
                f <- sprintf('%s/%s%s%s.png', MESS_dir, save_name, "_current_mess_", raster_name)
                
                png(f, 8, 8, units = 'in', res = 300, type = 'cairo')
                print(p)
                dev.off()
                
              }, unstack(mess_current$similarity), names(mess_current$similarity))
              
            } else {
              message(taxa, ' MESS directory already created')
            }
            
            ## Write the raster of novel environments to the MESS sub-directory
            if(!file.exists(sprintf('%s/%s%s.tif', MESS_dir, save_name, "_current_novel"))) {
              
              message('Writing currently novel environments to file for ', taxa)
              raster::writeRaster(novel_current, sprintf('%s/%s%s.tif', MESS_dir, save_name, "_current_novel"),
                                  overwrite = TRUE)
              
            } else {
              message(taxa, ' Current MESS file already saved')
            }
            
            ## Now mask out novel environments
            ## is.na(novel_current) is a binary layer showing
            ## not novel [=1] vs novel [=0],
            ## so multiplying this with hs_current will mask out novel
            hs_current_not_novel <- pred.current * is.na(novel_current)
            
            ## Write out not-novel raster :: this can go to the main directory
            ## Write the raster of novel environments to the MESS sub-directory
            if(!file.exists(sprintf('%s%s/full/%s%s.tif', maxent_path, 
                                    save_name, save_name, "_current_not_novel"))) {
              
              message('Writing currently un-novel environments to file for ', taxa)
              raster::writeRaster(hs_current_not_novel, sprintf('%s%s/full/%s%s.tif', maxent_path,
                                                                save_name, save_name, 
                                                                "_current_not_novel"), overwrite = TRUE)
              
            } else {
              message(taxa, ' Current un-novel environments file already saved')
            }
            
            gc()
            
            ## Create the MESS path and save shapefiles
            MESS_shp_path <- sprintf('%s%s/full/%s', maxent_path, save_name, 'MESS_output')
            if(save_novel_poly) {
              
              ## Create shape files 
              current_novel_raster <- terra::rast(sprintf('%s/%s%s.tif', MESS_dir, save_name, "_current_novel"))
              vals       <- terra::unique(current_novel_raster)
              uniue_vals <- is.na(vals[[1]]) %>% unique()
              
              if(!uniue_vals) {
                
                message('Converting ', taxa, ' raster to polygon')
                current_thresh_poly <- terra::as.polygons(current_novel_raster) 
                gc()
                
                ## Now save the novel areas as shapefiles
                ## There is a problem with accessing the files at the same time
                message('Saving current MESS maps to polygons for ', taxa)
                st_write(current_thresh_poly %>% st_as_sf(),
                         
                         dsn    = sprintf('%s/%s%s.gpkg', MESS_dir, save_name, "_current_novel_poly"),
                         layer  = paste0(save_name, "_current_novel_polygon"),
                         
                         quiet  = TRUE,
                         append = FALSE)
                
                st_write(current_thresh_poly %>% st_as_sf(), 
                         
                         dsn    = file.path(getwd(), output_path), 
                         layer  = paste0(save_name, 
                                         '_current_novel_above'),
                         
                         quiet  = TRUE,
                         append = FALSE)
                
                gc()
                
              } else {
                message('Do not save current MESS maps to shapefile for ', taxa, ' no cells are novel')
              }
              
            } else {
              message('Do not save current MESS maps to shapefile for ', taxa)
            }
            
            ## Below, we create a dummy polygon as the first list element (which is the extent
            ## of the raster, expanded by 10%), to plot on panel 1). 50 = approx 50 lines across the polygon
            
            ## Now create a panel of PNG files for maxent projections and MESS maps
            ## All the projections and extents need to match
            empty_ras <- raster::init(current_grids, function(x) NA)
            
            ## Use the 'levelplot' function to make a multipanel output:
            ## occurrence points, current raster and future raster
            current_mess_png = sprintf('%s/%s/full/%s_%s.png', maxent_path, save_name, save_name, "mess_panel")
            if(!file.exists(current_mess_png)) {
              
              ## Create level plot of current conditions including MESS
              message('Create current MESS panel maps for ', taxa)
              
              png(sprintf('%s/%s/full/%s_%s.png', maxent_path, save_name, save_name, "mess_panel"),
                  8, 16, units = 'in', res = 600)
              
              print(levelplot(raster::stack(empty_ras,
                                            hs_current_not_novel, 
                                            quick = TRUE), margin = FALSE,
                              
                              ## Create a colour scheme using colbrewer: 100 is to make it continuos
                              ## Also, make it a one-directional colour scheme
                              scales      = list(draw = FALSE,  x = list(cex = 1.8), y = list(cex = 1.8),
                                                 xlab = list(cex = 1.8),
                                                 ylab = list(cex = 1.8)),
                              
                              at = seq(0, 1, length = 100),
                              col.regions = colorRampPalette(rev(brewer.pal(9, 'YlOrRd'))),
                              
                              ## Give each plot a name: the third panel is the GCM
                              names.attr = c('HSM records', ' Current'),
                              colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                              main       = list(gsub('_', ' ', taxa), font = 4, cex = 2)) +
                      
                      ## Plot the Aus shapefile with the occurrence points for reference
                      ## Can the current layer be plotted on it's own?
                      ## Add the novel maps as vectors.
                      latticeExtra::layer(sp.polygons(poly), data = list(poly = poly)) +
                      latticeExtra::layer(sp.points(occ, pch = 19, cex = 0.15,
                                                    col = c('red', 'transparent', 'transparent')[panel.number()]),
                                          data = list(occ = occ)))
              dev.off()
              gc()
              
            } else {
              message(' Current MESS panel maps already created for ', taxa)
            }
          }
        } else {
          message(taxa, ' ', ' skipped - SDM not yet run')
        }
        
      } else {
        message(' Current MESS panel maps already created for ', taxa)
      }
    }
    
    ## Check this is the best way to run parallel
    lapply(taxa_list, maxent_predict_fun)
  })
}




#' Resampling a Raster* object via rGDAL.
#' 
#' @param rast Raster* object to be resampled
#' @param rast_base Raster* object with parameters that r
#' should be resampled to.
#' @param method Character. GDAL resampling_method
#' ("near"|"bilinear"|"cubic"|"cubicspline"|
#'  "lanczos"|"average"|"mode"|"max"|"min"|
#'  "med"|"q1"|"q3")
#' @param temp_dir Character. 
#' @param write_rasters Character. 
#' @param output_path Character. 
#' @param output_file Character. 
#' @export gdal_resample
gdal_resample <- function(rast, 
                          rast_base, 
                          method, 
                          temp_dir,
                          write_rasters,
                          output_path,
                          output_file) {
  
  ## Geometry attributes
  t1 <- c(xmin(rast_base), ymin(rast_base), 
          xmax(rast_base), ymax(rast_base))
  res <- res(rast_base)
  
  ## Temporary files
  tmp_outname <- sprintf('%sout.tif', temp_dir)
  tmp_inname  <- sprintf('%sin.tif',  temp_dir)
  
  message('saving temporary raster')
  writeRaster(rast, 
              tmp_inname, 
              datatype  = "INT2U", 
              options   = "COMPRESS=LZW",
              overwrite = TRUE)
  
  ## GDAL time!
  message('resampling raster using gdalwarp')
  gdalwarp(tmp_inname, tmp_outname, 
           tr = res, te = t1, r = method)
  resample_raster = raster(tmp_outname)
  
  if(write_rasters) {
    
    message('saving resampled raster to file')
    writeRaster(resample_raster, 
                paste0(output_path, output_file),
                datatype  = "INT2U", 
                options   = "COMPRESS=LZW",
                overwrite = TRUE)
    
    return(resample_raster)
    
  } else  {
    return(resample_raster)  
  }
}  





#' @title Threshold current habitat suitability rasters.
#' @description Takes a habitat suitability layer, and creates a binary suitability layer (0, 1) using a threshold value.
#' @param taxa_list          Character string - The taxa to run maxent predictions for
#' @param maxent_path        Character string - The file path containing the existing maxent models
#' @param maxent_table       Data frame       - A table of maxent results to be used for mapping 
#' @param cell_factor        Numeric          - Cell size to resample output
#' @param country_shp        Character string - Shapefile name that has already been read into R (e.g. in the Package)
#' @param country_prj        Character string - Name of projection
#' @param write_rasters      Logical          - Save rasters (T/F)?
#' @param poly_path          Character string - file path to feature polygon layer
#' @param epsg               Numeric - ERSP code of coord ref system to be translated into WKT format
#' @export habitat_threshold
habitat_threshold = function(taxa_list,
                             maxent_table,
                             maxent_path,
                             output_path,
                             poly_path,
                             epsg) {
  
  
  ## Convert to SF object for selection - inefficient
  poly <- st_read(poly_path) %>% 
    st_transform(., st_crs(epsg)) %>% as_Spatial()
  
  ## Pipe the list into Lapply
  taxa_list %>%
    
    ## Loop over just the taxa
    ## taxa = taxa_list[1]
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
      
      save_name <- gsub(' ', '_', taxa)
      
      ## Check the threshold data exists
      current_file  = sprintf('%s/%s/full/%s_current_not_novel.tif',
                              maxent_path, save_name, save_name)
      
      current_thresh =  sprintf('%s/%s/full/%s_%s%s.tif', maxent_path,
                                save_name, save_name, "current_suit_not_novel_above_", thresh)
      
      ## If the threshold raster data doesn't exist :
      if(file.exists(current_file)) {
        
        if(!file.exists(current_thresh)) {
          
          ## Print the taxa being analysed
          message('doing ', taxa, ' | Logistic > ', thresh)
          
          ## Read in the current suitability raster :: get the current_not_novel raster
          f_current <- raster(sprintf('%s/%s/full/%s_current_not_novel.tif',
                                      maxent_path, save_name, save_name))
          
          ## First, create a simple function to threshold each of the rasters in raster.list,
          ## Then apply this to just the current suitability raster.
          thresh_greater       = function (x) {x > thresh}
          current_suit_thresh  = thresh_greater(f_current)
          current_suit_rast    = terra::rast(current_suit_thresh)
          
          ## Re-sample rasters
          # message('Resampling Model for ', taxa, ' to ', xres(Ref_raster), 'm')
          # current_suit_thresh_resample <- terra::disagg(current_suit_rast, fact = cell_factor)
          
          ## Write the current suitability raster, threshold-ed using the Maximum training
          ## sensitivity plus specificity Logistic threshold
          message('Writing ', taxa, ' current', ' max train > ', thresh)
          
          ## Save in two places, in the taxa folder, 
          ## and in the habitat suitability folder
          writeRaster(current_suit_rast, 
                      sprintf('%s/%s/full/%s_%s%s.tif', maxent_path,
                              save_name, save_name, "current_suit_not_novel_above_", thresh),
                      overwrite = TRUE)
          
          vals       <- terra::unique(current_suit_rast)
          uniue_vals <- is.na(vals[[1]]) %>% unique()
          
          gc()
          
          if(!uniue_vals) {
            
            message('Converting ', taxa, ' raster to polygon')
            
            # current_thresh_poly <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(current_suit_rast), 
            #                                                    as_points = FALSE, 
            #                                                    merge     = TRUE,
            #                                                    fill      = FALSE, 
            #                                                    group     = FALSE,
            #                                                    agr       = FALSE))
            # gc()
            
            current_thresh_poly     <- terra::as.polygons(current_suit_rast) 
            current_thresh_poly_dat <- terra::subset(current_thresh_poly, current_thresh_poly$layer == 1)
            
            ## Now save the thresh-holded rasters as shapefiles
            message('Saving current threshold SDM rasters to polygons for ', taxa)
            st_write(current_thresh_poly_dat %>% st_as_sf(),
                     
                     dsn    = sprintf('%s/%s/full/%s_%s%s.gpkg', 
                                      maxent_path,
                                      save_name, 
                                      save_name, 
                                      'current_suit_not_novel_above_', 
                                      thresh),
                     
                     layer  = paste0(save_name, 
                                     '_current_suit_not_novel_above_', 
                                     thresh),
                     
                     quiet  = TRUE,
                     append = FALSE)
            
            st_write(current_thresh_poly_dat %>% st_as_sf(), 
                     
                     dsn    = file.path(getwd(), output_path), 
                     
                     layer  = paste0(save_name, 
                                     '_current_suit_not_novel_above_', 
                                     thresh),
                     
                     quiet  = TRUE,
                     append = FALSE)
            
            gc()
            
          } else {
            message('Do not save current MESS maps to shapefile for ', taxa, ' no cells are novel')
          }
          
          
          message('writing threshold png for ', taxa)
          png(sprintf('%s/%s/full/%s_%s%s.png', maxent_path,
                      save_name, save_name, "current_suit_not_novel_above_", thresh),
              16, 10, units = 'in', res = 500)
          
          ##
          raster::plot(current_suit_thresh, main = paste0(taxa, ' > ', thresh), legend = FALSE)
          raster::plot(poly, add = TRUE, legend = FALSE)
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
#' @param taxa_list       Character string - The taxa to run maxent predictions for
#' @param taxa_level      Character string - the taxnomic level to run maxent models for
#' @param habitat_raster  Character string - The habitat raster which has already been read in
#' @param country_shp     Character string - Shapefile name that has already been read into R (e.g. in the Package)
#' @param buffer          Numeric          - Distance by which to buffer the points (metres using a projected system)
#' @param write_rasters   Logical          - Save rasters (T/F)?
#' @export taxa_records_habitat_features_intersect
taxa_records_habitat_features_intersect = function(analysis_df,
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
      
      save_name  <- gsub(' ', '_', taxa)
      raster_int <- paste0(output_path, save_name, '_VEG_intersection_', buffer, 'm.tif')
      
      if(!file.exists(raster_int)) {
        
        ## For each taxa, get the same records that were used in the SDM analysis 
        taxa_df   <- subset(analysis_df, searchTaxon == taxa | !!sym(taxa_level) == taxa)
        
        ## Buffer the points by 50km
        taxa_buffer <- gBuffer(taxa_df, width = buffer)
        
        ## If the taxa don't intersect with the veg layer, we need an exception there
        
        ## Clip the habitat polygon by the 50km buffer
        message('Clip habitat layer to the taxa df for ', taxa)
        habitat_subset <- habitat_poly[taxa_buffer, ]
        
        gc()
        
        if(nrow(habitat_subset@data) >0 ) {
          
          ## Intersect clipped habitat with buffer
          ## do we need another exception here?
          message('Intersect taxa df with SVTM for ', taxa)
          taxa_intersects          <- gIntersects(habitat_subset, taxa_buffer, byid = TRUE) 
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
                   layer  = paste0(save_name, '_VEG_intersect'), 
                   driver = 'ESRI Shapefile', 
                   overwrite_layer = TRUE)
          
          ## Save the taxa * habitat intersection as a raster
          message('writing threshold png for ', taxa)
          png(paste0(output_path, save_name, "_VEG_intersection.png"),
              16, 10, units = 'in', res = 500)
          
          ##
          plot(taxa_VEG_intersects_raster, main = paste0(taxa, ' SVTM Intersection'))
          plot(taxa_df, add = TRUE, col = "red", lwd = 3)
          dev.off()
          
          ## Save in two places, in the taxa folder, 
          ## and in the habitat suitability folder
          writeRaster(taxa_VEG_intersects_raster, 
                      paste0(output_path, save_name, '_VEG_intersection_', buffer, 'm.tif'),
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





#' @title Intersect habitat suitability raster layers with other raster layer (e.g. Fire).
#' @description Takes a habitat suitability layer, and intersects it with a fire suitability layer.
#' @param taxa_list          Character string - The taxa to run maxent predictions for
#' @param targ_maxent_table  data frame - table of maxent results for target taxa
#' @param host_maxent_table  data frame - table of maxent results for host taxa
#' @param target_path        Character string - The file path containing the existing maxent models
#' @param intersect_path     Character string - The file path containing the intersecting rasters
#' @param raster_pattern     Character string - The pattern to look for of Invertebrate rasters
#' @param targ_maxent_table  Data frame       - A table of maxent results to be used for mapping 
#' @param cell_size          Numeric          - Cell size to resample output
#' @param poly_path          Character string - file path to feature polygon layer
#' @param epsg               Numeric - ERSP code of coord ref system to be translated into WKT format
#' @param write_rasters      Logical          - Save rasters (T/F)?
#' @export calculate_taxa_habitat_host_rasters
calculate_taxa_habitat_host_rasters = function(taxa_list,
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
                                               poly_path,
                                               epsg) {
  
  ## Get the AUS shapefile
  poly <- st_read(poly_path) %>% 
    st_transform(., st_crs(epsg)) %>% as_Spatial()
  
  ## Pipe the list into Lapply
  taxa_list %>%
    
    ## Loop over just the taxa
    ## taxa = taxa_list[74]
    lapply(function(taxa) {
      
      
      if(host_maxent_table != 'NONE') {
        
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
        
        ## Get the sdm threshold for each host taxa
        host_thresh <- host_maxent_table    %>%
          filter(searchTaxon == host_taxa)  %>%
          dplyr::select(Logistic_threshold) %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        ## Get the taxa directory name
        save_name <- gsub(' ', '_', taxa)
        host_name <- gsub(' ', '_', host_taxa)
        
      }
      
      ## Get the sdm threshold for each inv taxa
      target_thresh <- targ_maxent_table  %>%
        filter(searchTaxon == taxa)       %>%
        dplyr::select(Logistic_threshold) %>%
        distinct() %>% .[1, ] %>% .[[1]]
      
      host_dir <- NA
      
      ## Get the taxa directory name
      save_name <- gsub(' ', '_', taxa)
      
      current_thresh = sprintf('%s/%s/full/%s_%s%s.tif', target_path,
                               save_name, save_name, 
                               "current_suit_not_novel_above_", target_thresh)
      
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
            .[grep(paste0(save_name, collapse = '|'), ., ignore.case = TRUE)]
          
          if(length(intersect_file) == 1) {
            
            message('SDM and Veg rasters intersect for ', taxa, ' but it does not have a host taxa')
            intersect_raster <- raster(intersect_file)
            
            ## Re-sample
            message('resampling Veg intersect raster for ', taxa)
            intersect_sdm <- raster::resample(intersect_raster, sdm_threshold, "bilinear", 
                                              exent = extent(sdm_threshold))
            
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
            
            gc()
            
            ## Filter out values we don't want - where habitat = 1, but KEEP where FIRE is NA
            ## If FIRE is NA, that means that....
            sdm_fire_crosstab <- dplyr::filter(habitat_fire_crosstab, Habitat_taxa == 1)
            sdm_fire_crosstab <- sdm_fire_crosstab %>%  
              
              ## Calculate the % burnt in each category, and also the km2
              mutate(km2   = km2/cell_size)             %>% 
              mutate(Percent      = km2/sum(km2) * 100) %>% 
              mutate(Percent      = round(Percent, 2))  %>% 
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
            
            gc()
            
            ## Save the % burnt layers
            write.csv(sdm_fire_crosstab, paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.csv'), row.names = FALSE)
            
            ## Now write the rasters
            ## If the rasters don't exist, write them for each taxa/threshold
            writeRaster(sdm_plus_veg, 
                        paste0(output_path, save_name, '_SDM_VEG_intersect.tif'),
                        overwrite = TRUE)
            
            writeRaster(sdm_plus_veg_intersect_fire, 
                        paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.tif'),
                        overwrite = TRUE)
            
            message('writing threshold png for ', taxa)
            png(paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.png'),
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
                    latticeExtra::layer(sp.polygons(poly), data = list(poly = poly))) 
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
            write.csv(sdm_fire_crosstab, paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.csv'), row.names = FALSE)
            
            writeRaster(sdm_intersect_fire, 
                        paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.tif'),
                        overwrite = TRUE)
            
            message('writing threshold png for ', taxa)
            png(paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.png'),
                11, 4, units = 'in', res = 300)
            
            print(levelplot(stack(sdm_threshold,
                                  fire_raster,
                                  sdm_intersect_fire,
                                  quick = TRUE), margin = FALSE,
                            
                            ## Create a colour scheme using colbrewer: 100 is to make it continuos
                            ## Also, make it a one-directional colour scheme
                            scales      = list(draw = FALSE),
                            at = seq(0, 4, length = 8),
                            col.regions = colorRampPalette(rev(brewer.pal(5, 'YlOrRd'))),
                            
                            ## Give each plot a name: the third panel is the GCM
                            names.attr = c('SDM', 'Fire', 'SDM * Fire'),
                            colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                            main       = list(gsub('_', ' ', taxa), font = 4, cex = 2)) +
                    
                    ## Plot the Aus shapefile with the occurrence points for reference
                    ## Can the current layer be plotted on it's own?
                    ## Add the novel maps as vectors.
                    latticeExtra::layer(sp.polygons(poly), data = list(poly = poly))) 
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
            .[grep(paste0(save_name, collapse = '|'), ., ignore.case = TRUE)]
          
          if(length(intersect_file) == 1) {
            
            message('SDM and Veg rasters intersect for ', taxa, ' and it has a host taxa')
            intersect_raster <- raster(intersect_file)
            
            ## Re-sample
            message('resampling Veg intersect raster for ', taxa)
            intersect_sdm <- raster::resample(intersect_raster, sdm_threshold, "bilinear", exent = extent(sdm_threshold))
            gc()
            
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
            write.csv(sdm_fire_crosstab, paste0(output_path, save_name, '_SDM_Host_VEG_intersect_Fire.csv'), row.names = FALSE)
            
            ## Now write the rasters
            ## If the rasters don't exist, write them for each taxa/threshold
            writeRaster(sdm_plus_host_veg, 
                        paste0(output_path, save_name, '_SDM_Host_intersect.tif'),
                        overwrite = TRUE)
            
            writeRaster(sdm_plus_veg_intersect_fire, 
                        paste0(output_path, save_name, '_SDM_Host_VEG_intersect_Fire.tif'),
                        overwrite = TRUE)
            
            message('writing threshold png for ', taxa)
            png(paste0(output_path, save_name, '_SDM_Host_VEG_intersect_Fire.png'),
                11, 4, units = 'in', res = 300)
            
            print(levelplot(stack(sdm_plus_veg,
                                  fire_raster,
                                  sdm_plus_veg_intersect_fire, 
                                  quick = TRUE), margin = FALSE,
                            
                            ## Create a colour scheme using colbrewer: 100 is to make it continuous
                            ## Also, make it a one-directional colour scheme
                            scales      = list(draw = FALSE),
                            at = seq(0, 4, length = 8),
                            col.regions = colorRampPalette(rev(brewer.pal(5, 'YlOrRd'))),
                            
                            ## Give each plot a name: the third panel is the GCM
                            names.attr = c('SDMs + Veg', 'Fire', ' [SDMs + Veg] * Fire'),
                            colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                            main       = list(gsub('_', ' ', taxa), font = 4, cex = 2)) +
                    
                    ## Plot the Aus shapefile with the occurrence points for reference
                    ## Can the current layer be plotted on it's own?
                    ## Add the novel maps as vectors.
                    latticeExtra::layer(sp.polygons(poly), data = list(poly = poly))) 
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
            
            gc()
            
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
            write.csv(sdm_fire_crosstab, paste0(output_path, save_name, '_SDM_Host_intersect_Fire.csv'), row.names = FALSE)
            
            ## Write the current suitability raster, thresholded using the Maximum training
            ## sensitivity plus specificity Logistic threshold
            message('Writing ', taxa, ' SDM * Fire rasdters', ' max train > ', target_thresh)
            
            ## Now write the rasters
            ## If the rasters don't exist, write them for each taxa/threshold
            writeRaster(sdm_plus_host, 
                        paste0(output_path, save_name, '_SDM_Host_intersect.tif'),
                        overwrite = TRUE)
            
            ## Save in two places, in the taxa folder, 
            ## and in the habitat suitability folder
            writeRaster(sdm_plus_host_intersect_fire, 
                        paste0(output_path, save_name, '_SDM_Host_intersect_Fire.tif'),
                        overwrite = TRUE)
            
            message('writing SDM * FIRE png for ', taxa)
            png(paste0(output_path, save_name, '_SDM_Host_intersect_Fire.png'),
                11, 4, units = 'in', res = 300)
            
            print(levelplot(stack(sdm_plus_veg,
                                  sdm_plus_intersect_fire, 
                                  quick = TRUE), margin = FALSE,
                            
                            ## Create a colour scheme using colbrewer: 100 is to make it continuos
                            ## Also, make it a one-directional colour scheme
                            scales      = list(draw = FALSE),
                            at = seq(0, 4, length = 8),
                            col.regions = colorRampPalette(rev(brewer.pal(5, 'YlOrRd'))),
                            
                            ## Give each plot a name: the third panel is the GCM
                            names.attr = c('SDMs', ' [SDMs * Fire]'),
                            colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                            main       = list(gsub('_', ' ', taxa), font = 4, cex = 2)) +
                    
                    ## Plot the Aus shapefile with the occurrence points for reference...
                    ## Can the current layer be plotted on it's own?
                    ## Add the novel maps as vectors.
                    latticeExtra::layer(sp.polygons(poly), data = list(poly = poly)))
            
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





#' @title Intersect habitat suitability feature layers with other feature layer (e.g. Fire).
#' @description Takes a habitat suitability layer, and intersects it with a fire suitability layer.
#' @param taxa_list          Character string - The taxa to run maxent predictions for
#' @param targ_maxent_table  data frame - table of maxent results for target taxa
#' @param host_maxent_table  data frame - table of maxent results for host taxa
#' @param target_path        Character string - The file path containing the existing maxent models
#' @param intersect_path     Character string - The file path containing the intersecting rasters
#' @param raster_pattern     Character string - The pattern to look for of Invertebrate rasters
#' @param cell_size          Numeric          - Cell size to resample output
#' @param poly_path          Character string - file path to feature polygon layer
#' @param epsg               Numeric - ERSP code of coord ref system to be translated into WKT format
#' @param write_rasters      Logical          - Save rasters (T/F)?
#' @export calculate_taxa_habitat_host_features
calculate_taxa_habitat_host_features = function(taxa_list,
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
                                                poly_path,
                                                epsg) {
  
  ## Get the AUS shapefile
  poly <- st_read(poly_path) %>% 
    st_transform(., st_crs(epsg)) %>% as_Spatial()
  
  ## Pipe the list into Lapply
  taxa_list %>%
    
    ## Loop over just the taxa
    ## taxa = taxa_list[74]
    lapply(function(taxa) {
      
      
      if(host_maxent_table != 'NONE') {
        
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
        
        ## Get the sdm threshold for each host taxa
        host_thresh <- host_maxent_table    %>%
          filter(searchTaxon == host_taxa)  %>%
          dplyr::select(Logistic_threshold) %>%
          distinct() %>% .[1, ] %>% .[[1]]
        
        ## Get the taxa directory name
        save_name <- gsub(' ', '_', taxa)
        host_name <- gsub(' ', '_', host_taxa)
        
      }
      
      ## Get the sdm threshold for each inv taxa
      target_thresh <- targ_maxent_table  %>%
        filter(searchTaxon == taxa)       %>%
        dplyr::select(Logistic_threshold) %>%
        distinct() %>% .[1, ] %>% .[[1]]
      
      host_dir <- NA
      
      ## Get the taxa directory name
      save_name <- gsub(' ', '_', taxa)
      
      current_thresh = sprintf('%s/%s/full/%s_%s%s.tif', target_path,
                               save_name, save_name, "current_suit_not_novel_above_", target_thresh)
      
      ## If the invert taxa has a host plant, use the SDM from the host plant
      if(is.na(host_dir)) {
        
        ## If the threshold raster data doesn't exist :
        if(file.exists(current_thresh)) {
          
          ## Print the taxa being analysed
          message('Intersecting SDM with Fire for', taxa, ' | Logistic > ', target_thresh)
          
          ## Read in the current suitability raster :: get the current_not_novel raster
          sdm_threshold <- raster(current_thresh)
          
          ## Read the SVTM intersect file in
          intersect_file <- list.files(intersect_path, pattern = raster_pattern, full.names = TRUE) %>% 
            .[grep(paste0(save_name, collapse = '|'), ., ignore.case = TRUE)]
          
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
            
            gc()
            
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
              mutate(Percent      = round(Percent, 2))  %>% 
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
            write.csv(sdm_fire_crosstab, paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.csv'), row.names = FALSE)
            
            ## Now write the rasters
            ## If the rasters don't exist, write them for each taxa/threshold
            writeRaster(sdm_plus_veg, 
                        paste0(output_path, save_name, '_SDM_VEG_intersect.tif'),
                        overwrite = TRUE)
            
            writeRaster(sdm_plus_veg_intersect_fire, 
                        paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.tif'),
                        overwrite = TRUE)
            
            message('writing threshold png for ', taxa)
            png(paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.png'),
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
                    latticeExtra::layer(sp.polygons(poly), data = list(poly = poly))) 
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
            write.csv(sdm_fire_crosstab, paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.csv'), row.names = FALSE)
            
            writeRaster(sdm_intersect_fire, 
                        paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.tif'),
                        overwrite = TRUE)
            
            message('writing threshold png for ', taxa)
            png(paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.png'),
                11, 4, units = 'in', res = 300)
            
            print(levelplot(stack(sdm_threshold,
                                  fire_raster,
                                  sdm_intersect_fire,
                                  quick = TRUE), margin = FALSE,
                            
                            ## Create a colour scheme using colbrewer: 100 is to make it continuos
                            ## Also, make it a one-directional colour scheme
                            scales      = list(draw = FALSE),
                            at = seq(0, 4, length = 8),
                            col.regions = colorRampPalette(rev(brewer.pal(5, 'YlOrRd'))),
                            
                            ## Give each plot a name: the third panel is the GCM
                            names.attr = c('SDM', 'Fire', 'SDM * Fire'),
                            colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                            main       = list(gsub('_', ' ', taxa), font = 4, cex = 2)) +
                    
                    ## Plot the Aus shapefile with the occurrence points for reference
                    ## Can the current layer be plotted on it's own?
                    ## Add the novel maps as vectors.
                    latticeExtra::layer(sp.polygons(poly), data = list(poly = poly))) 
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
            .[grep(paste0(save_name, collapse = '|'), ., ignore.case = TRUE)]
          
          if(length(intersect_file) == 1) {
            
            message('SDM and Veg rasters intersect for ', taxa, ' and it has a host taxa')
            intersect_raster <- raster(intersect_file)
            
            ## Re-sample
            message('resampling Veg intersect raster for ', taxa)
            intersect_sdm <- raster::resample(intersect_raster, sdm_threshold, "bilinear", exent = extent(sdm_threshold))
            gc()
            
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
            write.csv(sdm_fire_crosstab, paste0(output_path, save_name, '_SDM_Host_VEG_intersect_Fire.csv'), row.names = FALSE)
            
            ## Now write the rasters
            ## If the rasters don't exist, write them for each taxa/threshold
            writeRaster(sdm_plus_host_veg, 
                        paste0(output_path, save_name, '_SDM_Host_intersect.tif'),
                        overwrite = TRUE)
            
            writeRaster(sdm_plus_veg_intersect_fire, 
                        paste0(output_path, save_name, '_SDM_Host_VEG_intersect_Fire.tif'),
                        overwrite = TRUE)
            
            message('writing threshold png for ', taxa)
            png(paste0(output_path, save_name, '_SDM_Host_VEG_intersect_Fire.png'),
                11, 4, units = 'in', res = 300)
            
            print(levelplot(stack(sdm_plus_veg,
                                  fire_raster,
                                  sdm_plus_veg_intersect_fire, 
                                  quick = TRUE), margin = FALSE,
                            
                            ## Create a colour scheme using colbrewer: 100 is to make it continuous
                            ## Also, make it a one-directional colour scheme
                            scales      = list(draw = FALSE),
                            at = seq(0, 4, length = 8),
                            col.regions = colorRampPalette(rev(brewer.pal(5, 'YlOrRd'))),
                            
                            ## Give each plot a name: the third panel is the GCM
                            names.attr = c('SDMs + Veg', 'Fire', ' [SDMs + Veg] * Fire'),
                            colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                            main       = list(gsub('_', ' ', taxa), font = 4, cex = 2)) +
                    
                    ## Plot the Aus shapefile with the occurrence points for reference
                    ## Can the current layer be plotted on it's own?
                    ## Add the novel maps as vectors.
                    latticeExtra::layer(sp.polygons(poly), data = list(poly = poly))) 
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
            
            gc()
            
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
            write.csv(sdm_fire_crosstab, paste0(output_path, save_name, '_SDM_Host_intersect_Fire.csv'), row.names = FALSE)
            
            ## Write the current suitability raster, thresholded using the Maximum training
            ## sensitivity plus specificity Logistic threshold
            message('Writing ', taxa, ' SDM * Fire rasdters', ' max train > ', target_thresh)
            
            ## Now write the rasters
            ## If the rasters don't exist, write them for each taxa/threshold
            writeRaster(sdm_plus_host, 
                        paste0(output_path, save_name, '_SDM_Host_intersect.tif'),
                        overwrite = TRUE)
            
            ## Save in two places, in the taxa folder, 
            ## and in the habitat suitability folder
            writeRaster(sdm_plus_host_intersect_fire, 
                        paste0(output_path, save_name, '_SDM_Host_intersect_Fire.tif'),
                        overwrite = TRUE)
            
            message('writing SDM * FIRE png for ', taxa)
            png(paste0(output_path, save_name, '_SDM_Host_intersect_Fire.png'),
                11, 4, units = 'in', res = 300)
            
            print(levelplot(stack(sdm_plus_veg,
                                  sdm_plus_intersect_fire, 
                                  quick = TRUE), margin = FALSE,
                            
                            ## Create a colour scheme using colbrewer: 100 is to make it continuos
                            ## Also, make it a one-directional colour scheme
                            scales      = list(draw = FALSE),
                            at = seq(0, 4, length = 8),
                            col.regions = colorRampPalette(rev(brewer.pal(5, 'YlOrRd'))),
                            
                            ## Give each plot a name: the third panel is the GCM
                            names.attr = c('SDMs', ' [SDMs * Fire]'),
                            colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                            main       = list(gsub('_', ' ', taxa), font = 4, cex = 2)) +
                    
                    ## Plot the Aus shapefile with the occurrence points for reference...
                    ## Can the current layer be plotted on it's own?
                    ## Add the novel maps as vectors.
                    latticeExtra::layer(sp.polygons(poly), data = list(poly = poly)))
            
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




#' @title Intersect habitat suitability features with Fire layers.
#' @description Takes a habitat suitability layer, and intersects it with a fire suitability layer.
#' @param taxa_list          Character string - The taxa to run maxent predictions for
#' @param layer_list         Character string - list of feature layers to look up
#' @param targ_maxent_table  data frame - table of maxent results for target taxa
#' @param target_path        Character string - The file path containing the existing maxent models
#' @param intersect_path     Character string - The file path containing the intersecting rasters
#' @param poly_path          Character string - file path to feature polygon layer
#' @param epsg               Numeric - ERSP code of coord ref system to be translated into WKT format
#' @param write_rasters      Logical          - Save rasters (T/F)?
#' @export calculate_taxa_habitat_features
calculate_taxa_habitat_features = function(taxa_list,
                                           layer_list,
                                           targ_maxent_table,
                                           target_path,
                                           threshold_path,
                                           output_path,
                                           intersect_path,
                                           intersect_layer,
                                           cell_size,
                                           fire_thresh,
                                           write_rasters,
                                           poly_path,
                                           epsg) {
  
  ## Get the AUS shapefile
  poly <- st_read(poly_path) %>% 
    st_transform(., st_crs(epsg)) %>% as_Spatial()
  
  ## Pipe the list into Lapply
  taxa_list %>%
    
    ## Loop over just the taxa
    ## taxa = taxa_list[1]
    lapply(function(taxa) {
      
      ## Get the sdm threshold for each inv taxa
      target_thresh <- targ_maxent_table  %>%
        filter(searchTaxon == taxa)       %>%
        dplyr::select(Logistic_threshold) %>%
        distinct() %>% .[1, ] %>% .[[1]]
      
      ## Get the taxa directory name
      save_name  <- gsub(' ', '_', taxa)
      layer_name <- layer_list[grep(save_name, layer_list)][[1]]
      
      ## If the invert taxa has a host plant, use the SDM from the host plant
      
      ## If the threshold raster data doesn't exist :
      if(file.exists(current_thresh)) {
        
        ## Print the taxa being analysed
        message('Intersecting SDM with Fire for', taxa, ' | Logistic > ', target_thresh)
        
        ## Read in the current suitability raster :: get the current_not_novel raster
        sdm_threshold <- st_read(dsn   = threshold_path, 
                                 layer = layer_name) %>%
          
          filter(!st_is_empty(.)) %>% 
          repair_geometry() 

        ## Then do the Cell stats ::
        ## estimated x % of each taxa's habitat in each fire intensity category (Severe, moderate, low, etc).
        habitat_fire_intersect <- st_intersection(sdm_threshold, main_int_layer)
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
        write.csv(sdm_fire_crosstab, paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.csv'), row.names = FALSE)
        
        writeRaster(sdm_intersect_fire, 
                    paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.tif'),
                    overwrite = TRUE)
        
        message('writing threshold png for ', taxa)
        png(paste0(output_path, save_name, '_SDM_VEG_intersect_Fire.png'),
            11, 4, units = 'in', res = 300)
        
        print(levelplot(stack(sdm_threshold,
                              intersect_layer,
                              sdm_intersect_fire,
                              quick = TRUE), margin = FALSE,
                        
                        ## Create a colour scheme using colbrewer: 100 is to make it continuos
                        ## Also, make it a one-directional colour scheme
                        scales      = list(draw = FALSE),
                        at = seq(0, 4, length = 8),
                        col.regions = colorRampPalette(rev(brewer.pal(5, 'YlOrRd'))),
                        
                        ## Give each plot a name: the third panel is the GCM
                        names.attr = c('SDM', 'Fire', 'SDM * Fire'),
                        colorkey   = list(height = 0.5, width = 3), xlab = '', ylab = '',
                        main       = list(gsub('_', ' ', taxa), font = 4, cex = 2)) +
                
                ## Plot the Aus shapefile with the occurrence points for reference
                ## Can the current layer be plotted on it's own?
                ## Add the novel maps as vectors.
                latticeExtra::layer(sp.polygons(poly), data = list(poly = poly))) 
        
        dev.off()
        gc()
        
      } else {
        message('Habitat Suitability threshold raster does not exist for', taxa, ' skip')
        cat(taxa)
      }
      
    }) 
}





#' @title       Plot a rasterVis::levelplot
#' @description Plot a rasterVis::levelplot with a colour ramp diverging around zero
#' Adapted from a gist by John Baumgartner for the (https://gist.github.com/johnbaums?direction=desc&sort=updated).

#' @param p    A trellis object resulting from rasterVis::levelplot
#' @param ramp Character string - The name of an RColorBrewer palette (as character), a character
#' @export diverge0
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
#' @export shapefile_vector_from_raster
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
  
  gc()
  
  areal_unit_vec <- c(areal_unit_rast[])
  summary(areal_unit_vec)
  
  ## return the vector
  return(areal_unit_vec)
  
}





#########################################################################################################################
#################################################  ------ TBC ---- ######################################################
#########################################################################################################################

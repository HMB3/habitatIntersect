#########################################################################################################################
###################################  FUNCTIONS FOR PREPARING SDM DATA ---- ##############################################
#########################################################################################################################


## Below are the functions used to estimate species ranges and prepare occurrence data for SDM analysis


'%!in%' <- function(x,y)!('%in%'(x,y))




#' @title Download GBIF occurrences.
#' @description This function downloads taxa occurrence files from GBIF (https://www.gbif.org/).
#' It assumes that the taxa list supplied is taxonomically correct (haha!).
#' It downloads the taxa to the specified folders without returning anything
#' @param species_list   Character vector - List of species binomials to download
#' @param download_path  Character string - File path for species downloads
#' @param download_limit Numeric - How many records can be downloaded at one time? Set by server
#' @export download_GBIF_all_species
download_GBIF_all_species = function(species_list, 
                                     download_path, 
                                     download_limit) {
  
  ## create variables
  GBIF.download.limit = download_limit
  
  ## for every species in the list
  ## sp.n = species_list[1]
  for(sp.n in species_list){
    
    ## First, check if the f*&%$*# file exists
    file_name = paste0(download_path, sp.n, "_GBIF_records.RData")
    
    ## If it's already downloaded, skip
    if (file.exists(file_name)) {
      
      print(paste ("file exists for species", sp.n, "skipping"))
      next
      
    }
    
    ## create a dummy file
    dummy = data.frame()
    save (dummy, file = file_name)
    
    ## Then check the spelling...incorrect nomenclature will return NULL result
    if (is.null(occ_search(scientificName = sp.n, limit = 1)$meta$count) == TRUE) {
      
      ## now append the species which had incorrect nomenclature to the skipped list
      ## this is slow, but it works for now
      print (paste ("Possible incorrect nomenclature", sp.n, "skipping"))
      nomenclature = paste ("Possible incorrect nomenclature |", sp.n)
      next
      
    }
    
    ## Skip species with no records
    if (occ_search(scientificName = sp.n)$meta$count <= 2) {
      
      ## now append the species which had no records to the skipped list
      print (paste ("No GBIF records for", sp.n, "skipping"))
      records = paste ("No GBIF records |", sp.n)
      next
      
    }
    
    ## Check how many records there are, and skip if there are over 200k
    if (occ_search(scientificName = sp.n, limit = 1)$meta$count > GBIF.download.limit) {
      
      ## now append the species which had > 200k records to the skipped list
      print (paste ("Number of records > max for GBIF download via R (200,000)", sp.n))
      max =  paste ("Number of records > 200,000 |", sp.n)
      
    } else {
      
      ## Download ALL records from GBIF
      message("Downloading GBIF records for ", sp.n, " using rgbif :: occ_data")
      key <- name_backbone(name = sp.n, rank = 'species')$usageKey
      
      GBIF <- occ_data(taxonKey = key, limit = GBIF.download.limit)
      GBIF <- as.data.frame(GBIF$data)
      
      # cat("Synonyms returned for :: ",  sp.n, unique(GBIF$scientificName), sep = "\n")
      # cat("Names returned for :: ",     sp.n, unique(GBIF$name),           sep = "\n")
      # cat("Takonkeys returned for :: ", sp.n, unique(GBIF$taxonKey),       sep = "\n")
      
      ## Could also only use the key searched, but that could knock out a lot of species
      message(nrow(GBIF), " Records returned for ", sp.n)
      
      ## Save records to .Rdata file
      save(GBIF, file = file_name)
      
    }
  }
}





#' @title Download ALA occurrences.
#' @description This function downloads species occurrence files from ALA (https://www.ala.org.au/).
#' It assumes that the species list supplied is taxonomically correct.
#' It downloads the species without returning anything. Add Galah!
#'
#' @param species_list   Character vector - List of species binomials to download
#' @param download_path  Character string - File path for species downloads
#' @param download_limit Numeric - How many records can be downloaded at one time? Set by server
#' @export download_ALA_all_species
download_ALA_all_species = function (species_list, 
                                     your_email, 
                                     download_path, 
                                     ala_temp_dir, 
                                     download_limit,
                                     extra_cols,
                                     quality_cols) {
  
  ## create variables
  download_limit  = 200000
  
  ## for every species in the list
  ## sp.n = species_list[2]
  for(sp.n in species_list) {
    
    ## First, check if the f*&%$*# file exists
    message('Searching for records from ', sp.n)
    file_name = paste0(download_path, sp.n, "_ALA_records.RData")
    
    ## If it's already downloaded, skip
    if (!file.exists (file_name)) {
      
      ## If the temp directory doesn't exist, create it
      if(!dir.exists(ala_temp_dir)) {
        message('Creating ', ala_temp_dir)
        dir.create(ala_temp_dir) } else {
          message('temp ALA directory already exists')}
      
      # lsid <- ALA4R::specieslist(sp.n)$taxonConceptLsid
      
      ## create a dummy file
      dummy = data.frame()
      save (dummy, file = file_name)
      
      ## Then check the spelling...incorrect nomenclature will return NULL result
      dir.create(ala_temp_dir)
      if (is.null(ALA4R::occurrences(taxon              = paste0("species:", sp.n), 
                                     download_reason_id = 7, 
                                     email              = your_email,
                                     qa                 = "all")$data) == TRUE) {
        
        ## Now, append the species which had incorrect nomenclature to the skipped list
        print (paste ("Possible incorrect nomenclature", sp.n, "skipping"))
        next
      }
      
      ## Skip species with no records
      if (nrow(ALA4R::occurrences(taxon              = paste0("species:", sp.n), 
                                  download_reason_id = 7, 
                                  email              = your_email,
                                  qa                 = "all")$data) <= 2) {
        
        ## now append the species which had no records to the skipped list
        print (paste ("No ALA records for", sp.n, "skipping"))
        records = paste ("No ALA records |", sp.n)
        next
      }
      
      ## Download ALL records from ALA - also include extra columns and quality columns
      message("Downloading ALA records for ", sp.n, " using ALA4R :: occurrences")
      ALA = ALA4R::occurrences(taxon              = paste0("species:", sp.n), 
                               download_reason_id = 7, 
                               email              = your_email,
                               extra              = extra_cols,
                               qa                 = quality_cols) %>% .[["data"]]
      
      ## Save records to .Rdata file
      message(nrow(ALA), " Records returned for ", sp.n)
      save(ALA, file = file_name)
      gc()
      
    } else {
      message('ALA records for ', sp.n, ' Already downloaded')
    }
  }
}



#' @title Download ALA genus occurrences.
#' @description  This function downloads genus occurrence files from ALA (https://www.ala.org.au/).
#' It assumes that the genus list supplied is taxonomically correct.
#' It downloads the genus to fiel without returning anything to the global environment
#'
#' @param species_list   Character vector - List of genus binomials to download
#' @param download_path  Character string - File path for genus downloads
#' @param extra_cols     Character - extra ALA columns, eg environmental vatriables
#' @param quality_cols   Character - quality ALA columns, eg spatial accuracy
#' @param download_limit Numeric - How many records can be downloaded at one time? Set by server
#' @return               Data frame of all site records, with global enviro conditions for each record location (i.e. lat/lon)
#' @export download_ALA_all_genera
download_ALA_all_genera = function (genera_list, 
                                    your_email, 
                                    download_path, 
                                    ala_temp_dir, 
                                    download_limit,
                                    quality_cols) {
  
  ## create variables
  download_limit  = 200000
  
  ## for every genus in the list
  ## sp.n = species_list[1]
  for(sp.n in genera_list) {
    
    ## First, check if the f*&%$*# file exists
    message('Searching for records from ', sp.n)
    file_name = paste0(download_path, sp.n, "_ALA_records.RData")
    
    ## If it's already downloaded, skip
    if (!file.exists (file_name)) {
      
      ## If the temp directory doesn't exist, create it
      if(!dir.exists(ala_temp_dir)) {
        message('Creating ', ala_temp_dir)
        dir.create(ala_temp_dir) } else {
          message('temp ALA directory already exists')}
      
      # lsid <- ALA4R::specieslist(sp.n)$taxonConceptLsid
      
      ## create a dummy file
      dummy = data.frame()
      save (dummy, file = file_name)
      
      ## Then check the spelling...incorrect nomenclature will return NULL result
      dir.create(ala_temp_dir)
      if (is.null(ALA4R::occurrences(taxon              = paste0("genus:", sp.n), 
                                     download_reason_id = 7, 
                                     email              = your_email,
                                     qa                 = "all")$data) == TRUE) {
        
        ## Now, append the species which had incorrect nomenclature to the skipped list
        print (paste ("Possible incorrect nomenclature", sp.n, "skipping"))
        next
      }
      
      ## Skip species with no records
      if (nrow(ALA4R::occurrences(taxon              = paste0("genus:", sp.n), 
                                  download_reason_id = 7, 
                                  email              = your_email,
                                  qa                 = "all")$data) <= 2) {
        
        ## now append the species which had no records to the skipped list
        print (paste ("No ALA records for", sp.n, "skipping"))
        records = paste ("No ALA records |", sp.n)
        next
      }
      
      ## Download ALL records from ALA - also include extra columns and quality columns
      message("Downloading ALA records for ", sp.n, " using ALA4R :: occurrences")
      ALA = ALA4R::occurrences(taxon              = paste0("genus:", sp.n), 
                               download_reason_id = 7, 
                               email              = your_email,
                               qa                 = quality_cols) %>% .[["data"]]
      
      ## Save records to .Rdata file
      message(nrow(ALA), " Records returned for ", sp.n)
      save(ALA, file = file_name)
      gc()
      
    } else {
      message('ALA records for ', sp.n, ' Already downloaded')
    }
  }
} 





#' @title Download ALA family occurrences
#' @description Download species occurrence files from the Atlas of Living Australia (ALA)
#' This function downloads family occurrence files from ALA (https://www.ala.org.au/).
#' It assumes that the species list supplied is taxonomically correct.
#' It downloads the species to fiel without returning anything to the global environment
#'
#' @param species_list   Character vector - List of species binomials to download
#' @param download_path  Character string - File path for species downloads
#' @param extra_cols     Character - extra ALA columns, eg environmental vatriables 
#' @param quality_cols   Character - quality ALA columns, eg spatial accuracy
#' @param download_limit Numeric - How many records can be downloaded at one time? Set by server
#' @return               Data frame of all site records, with global enviro conditions for each record location (i.e. lat/lon)
#' @export download_ALA_all_families
download_ALA_all_families = function (species_list, 
                                      your_email, 
                                      download_path, 
                                      ala_temp_dir, 
                                      download_limit,
                                      extra_cols,
                                      quality_cols) {
  
  ## Set download limit
  download_limit  = 200000
  
  ## for every species in the list
  ## sp.n = species_list[1]
  for(sp.n in species_list) {
    
    ## First, check if the f*&%$*# file exists
    message('Searching for records from ', sp.n)
    file_name = paste0(download_path, sp.n, "_ALA_records.RData")
    
    ## If it's already downloaded, skip
    if (!file.exists (file_name)) {
      
      ## If the temp directory doesn't exist, create it
      if(!dir.exists(ala_temp_dir)) {
        message('Creating ', ala_temp_dir)
        dir.create(ala_temp_dir) } else {
          message('temp ALA directory already exists')}
      
      # lsid <- ALA4R::specieslist(sp.n)$taxonConceptLsid
      
      ## create a dummy file
      dummy = data.frame()
      save (dummy, file = file_name)
      
      ## Then check the spelling...incorrect nomenclature will return NULL result
      dir.create(ala_temp_dir)
      if (is.null(ALA4R::occurrences(taxon              = paste0("family:", sp.n), 
                                     download_reason_id = 7, 
                                     email              = your_email,
                                     qa                 = "all")$data) == TRUE) {
        
        ## Now, append the species which had incorrect nomenclature to the skipped list
        print (paste ("Possible incorrect nomenclature", sp.n, "skipping"))
        next
      }
      
      ## Skip species with no records
      if (nrow(ALA4R::occurrences(taxon              = paste0("family:", sp.n), 
                                  download_reason_id = 7, 
                                  email              = your_email,
                                  qa                 = "all")$data) <= 2) {
        
        ## now append the species which had no records to the skipped list
        print (paste ("No ALA records for", sp.n, "skipping"))
        records = paste ("No ALA records |", sp.n)
        next
      }
      
      ## Download ALL records from ALA - also include extra columns and quality columns
      message("Downloading ALA records for ", sp.n, " using ALA4R :: occurrences")
      ALA = ALA4R::occurrences(taxon              = paste0("family:", sp.n), 
                               download_reason_id = 7, 
                               email              = your_email,
                               extra              = extra_cols,
                               qa                 = quality_cols) %>% .[["data"]]
      
      ## Save records to .Rdata file
      message(nrow(ALA), " Records returned for ", sp.n)
      save(ALA, file = file_name)
      gc()
      
    } else {
      message('ALA records for ', sp.n, ' Already downloaded')
    }
  }
}



#' @title Download ALA tribe occurrences.
#' @description This function downloads family occurrence files from ALA (https://www.ala.org.au/).
#' It assumes that the species list supplied is taxonomically correct.
#' It downloads the species to field without returning anything to the global environment
#'
#' @param species_list   Character vector - List of species binomials to download
#' @param download_path  Character string - File path for species downloads
#' @param extra_cols     Character - extra ALA columns, eg environmental vatriables
#' @param quality_cols   Character - quality ALA columns, eg spatial accuracy
#' @param download_limit Numeric - How many records can be downloaded at one time? Set by server
#' @return               Data frame of all site records, with global enviro conditions for each record location (i.e. lat/lon)
#' @export download_ALA_all_tribes
download_ALA_all_tribes = function (species_list, 
                                    your_email, 
                                    download_path, 
                                    ala_temp_dir, 
                                    download_limit,
                                    extra_cols,
                                    quality_cols) {
  
  ## create variables
  download_limit  = 200000
  
  ## for every species in the list
  ## sp.n = species_list[1]
  for(sp.n in species_list) {
    
    ## First, check if the f*&%$*# file exists
    message('Searching for records from ', sp.n)
    file_name = paste0(download_path, sp.n, "_ALA_records.RData")
    
    ## If it's already downloaded, skip
    if (!file.exists (file_name)) {
      
      ## If the temp directory doesn't exist, create it
      if(!dir.exists(ala_temp_dir)) {
        message('Creating ', ala_temp_dir)
        dir.create(ala_temp_dir) } else {
          message('temp ALA directory already exists')}
      
      # lsid <- ALA4R::specieslist(sp.n)$taxonConceptLsid
      
      ## create a dummy file
      dummy = data.frame()
      save (dummy, file = file_name)
      
      ## Then check the spelling...incorrect nomenclature will return NULL result
      dir.create(ala_temp_dir)
      if (is.null(ALA4R::occurrences(taxon              = paste0("tribe:", sp.n), 
                                     download_reason_id = 7, 
                                     email              = your_email,
                                     qa                 = "all")$data) == TRUE) {
        
        ## Now, append the species which had incorrect nomenclature to the skipped list
        print (paste ("Possible incorrect nomenclature", sp.n, "skipping"))
        next
      }
      
      ## Skip species with no records
      if (nrow(ALA4R::occurrences(taxon              = paste0("tribe:", sp.n), 
                                  download_reason_id = 7, 
                                  email              = your_email,
                                  qa                 = "all")$data) <= 2) {
        
        ## now append the species which had no records to the skipped list
        print (paste ("No ALA records for", sp.n, "skipping"))
        records = paste ("No ALA records |", sp.n)
        next
      }
      
      ## Download ALL records from ALA - also include extra columns and quality columns
      message("Downloading ALA records for ", sp.n, " using ALA4R :: occurrences")
      ALA = ALA4R::occurrences(taxon              = paste0("tribe:", sp.n), 
                               download_reason_id = 7, 
                               email              = your_email,
                               extra              = extra_cols,
                               qa                 = quality_cols) %>% .[["data"]]
      
      ## Save records to .Rdata file
      message(nrow(ALA), " Records returned for ", sp.n)
      save(ALA, file = file_name)
      gc()
      
    } else {
      message('ALA records for ', sp.n, ' Already downloaded')
    }
  }
} 





#' @title Combine ALA Records for different taxa!
#' @description Combines all occurrence files from ALA into one table.
#' It assumes that all the files come from the previous downloads function.
#' Although you can download all the records for in one go, this is better for
#' Doing small runs of species, or where you want to re-run them constantly
#' @param taxa_list       Character Vector - List of taxa already downloaded
#' @param records_path       File path for downloaded taxa
#' @param records_extension  Which R file type? RDS or RDA
#' @param record_type        Adds a column to the data frame for the data source, EG ALA
#' @param keep_cols          The columns we want to keep - a character list created by you
#' @param year_filt          Character - filter out records based on a year?
#' @param year_lim           Numeric - Records to remove
#' @param unique_cells       Logical - take only one record per cell?
#' @param world_raster       An Raster file of the enviro conditions used (assumed to be global)
#' @export combine_ala_records
combine_ala_records = function(taxa_list, 
                               records_path, 
                               records_extension,
                               record_type, 
                               keep_cols,
                               year_filt,
                               year,
                               year_lim,
                               unique_cells,
                               world_raster) {
  
  ## Should work outside the loop
  download = list.files(records_path, pattern = ".RData")
  length(download)
  
  ## Now these lists are getting too long for the combine step.
  ## Restrict them to just the strings that partially match the  taxa list for each run
  spp.download <- paste(taxa_list, records_extension, sep = "")
  download     = download[download %in% spp.download ]
  message('downloaded taxa ', length(download), ' analyzed taxa ', length(taxa_list))
  
  
  ## Combine all the taxa into a single dataframe at once
  ALL <- download %>%
    
    ## Pipe the list into lapply
    ## x = download [1]
    lapply(function(x) {
      
      ## Create a character string of each .RData file
      message("Reading records data for ", x)
      f <- sprintf(paste0(records_path, "%s"), x)
      
      ## Load each file - check if some are already dataframes
      d <- get(load(f)) %>% as.data.frame()
      
      ## Check if the dataframes have data
      if (nrow(d) >= 2) {
        
        ## If the taxa has < 2 records, escape the loop
        print (paste ("Sufficient occurrence records for ", x, " processing "))
        
        ##  type standardisation
        if("latitude" %in% colnames(d)) {
          names(d)[names(d) == 'latitude']  <- 'lat'
          names(d)[names(d) == 'longitude'] <- 'lon'
        }
        
        if("decimalLatitude" %in% colnames(d)) {
          names(d)[names(d) == 'decimalLatitude']  <- 'lat'
          names(d)[names(d) == 'decimalLongitude'] <- 'lon'
        }
        
        ##  standardi[sz]e catnum colname
        if("catalogueNumber" %in% colnames(d)) {
          d <- d %>% dplyr::select(-catalogueNumber)
        }
        
        if("eventDate" %in% colnames(d)) {
          d <- d %>% dplyr::select(-eventDate)
        }
        
        ## standardi[sz]e catnum colname
        if('coordinateUncertaintyinMetres' %in% colnames(d)) {
          message ("Renaming recordID column to id")
          names(d)[names(d) == 'coordinateUncertaintyinMetres'] <- 'coordinateUncertaintyInMetres'
          d[,"coordinateUncertaintyInMetres"] = as.numeric(unlist(d["coordinateUncertaintyInMetres"]))}
        
        ## standardi[sz]e catnum colname
        if('recordID' %in% colnames(d)) {
          message ("Renaming recordID column to id")
          names(d)[names(d) == 'recordID'] <- 'id'}
        
        ## Create the searchTaxon column - check how to put the data in here
        message ('Formatting occurrence data for ', x)
        d[,"searchTaxon"] = x
        d[,"searchTaxon"] = gsub(records_extension, "", d[,"searchTaxon"])
        
        if(!is.character(d["id"])) {
          d["id"] <- as.character(d["id"])}
        
        ## Choose only the desired columns
        d = d %>%
          dplyr::select(one_of(keep_cols))
        
        ## Then print warnings
        warnings()
        
        ## This is a list of columns in different ALA files which have weird characters
        message ('Formatting numeric occurrence data for ', x)
        # d[,"coordinateUncertaintyInMetres"] = as.numeric(unlist(d["coordinateUncertaintyInMetres"]))
        d["year"]  = as.numeric(unlist(d["year"]))
        d["id"]    = as.character(unlist(d["id"]))
        
      } else {
        message('No ALA dat for ', x, ' skipping')
      }
      return(d)
    }) %>%
    
    ## Finally, bind all the rows together
    bind_rows()
  
  ## Clear the garbage
  gc()
  
  ## Just get the newly downloaded taxa
  ALL = ALL[ALL$searchTaxon %in% taxa_list, ]
  length(unique(ALL$searchTaxon))
  
  if (nrow(ALL) > 0) {
    
    ## What names get returned?
    TRIM <- ALL
    (sum(is.na(TRIM$scientificName)) + nrow(subset(TRIM, scientificName == "")))/nrow(TRIM)*100
    
    
    ## 3). FILTER RECORDS TO THOSE WITH COORDINATES, AND AFTER 1950
    ## Now filter the ALA records using conditions which are not too restrictive
    CLEAN <- TRIM %>%
      
      ## Note that these filters are very forgiving...
      ## Unless we include the NAs, very few records are returned!
      filter(!is.na(lon) & !is.na(lat)) %>%
      filter(lon <= 180 & lat >= -90) %>%
      filter(lon <= 180 & lat >= -90) %>%
      filter(!is.na(year))
    
    if(year_filt) {
      
      CLEAN <- CLEAN %>% filter(year >= year_lim) 
      
    }
    
    ## How many records were removed by filtering?
    message(nrow(TRIM) - nrow(CLEAN), " records removed")
    message(round((nrow(CLEAN))/nrow(TRIM)*100, 2),
            " % records retained using records with valid coordinates and year")
    
    if(unique_cells) {
      
      ## Can use WORLDCIM rasters to get only records where wordlclim data is.
      message('Removing ALA points outside raster bounds for ', length(taxa_list), ' taxa')
      
      ## Now get the XY centroids of the unique 1km * 1km WORLDCLIM blocks where ALA records are found
      ## Get cell number(s) of WORLDCLIM raster from row and/or column numbers. Cell numbers start at 1 in the upper left corner,
      ## and increase from left to right, and then from top to bottom. The last cell number equals the number of raster cell
      world_raster_spat <- terra::rast(world_raster)
      mat <- cbind(lon = CLEAN$lon, lat = CLEAN$lat) 
      xy  <- terra::cellFromXY(world_raster_spat, mat) %>% 
        
        ## get the unique raster cells
        unique %>%
        
        ## Get coordinates of the center of raster cells for a row, column, or cell number of WORLDCLIM raster
        xyFromCell(world_raster_spat, .) %>%
        na.omit()
      
      ## For some reason, we need to convert the xy coords to a spatial points data frame, in order to avoid this error:
      ## 'NAs introduced by coercion to integer range'
      xy <- SpatialPointsDataFrame(coords = xy, data = as.data.frame(xy),
                                   proj4string = CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
      
      ## Now extract the temperature values for the unique 1km centroids which contain ALA data
      ## getValues is much faster than extract
      class(xy)
      z   = terra::extract(world_raster, xy)
      
      ## Then track which values of Z are on land or not
      onland = z %>% is.na %>%  `!` # %>% xy[.,]  cells on land or not
      
      ## Finally, filter the cleaned ALA data to only those points on land.
      ## This is achieved with the final [onland]
      LAND.POINTS = filter(CLEAN, terra::cellFromXY(world_raster_spat, mat) %in%
                             unique(terra::cellFromXY(world_raster_spat, mat))[onland])
      
      ## how many records were on land?
      records.ocean = nrow(CLEAN) - nrow(LAND.POINTS)
      nrow(LAND.POINTS)
      length(unique(LAND.POINTS$searchTaxon))
      
      ## Add a source column
      LAND.POINTS$SOURCE = record_type
      message(round((nrow(LAND.POINTS))/nrow(CLEAN)*100, 2),
              " % records retained using records inside raster bounds")
      
      ## save data
      nrow(LAND.POINTS)
      length(unique(LAND.POINTS$searchTaxon))
      
      ## get rid of some memory
      gc()
      
    } else {
      CLEAN$SOURCE = record_type
      return(CLEAN)
    }
    
  } else {
    message('No ALA dat for this set of taxa, creating empty datframe to other data')
    LAND.POINTS = setNames(data.frame(matrix(ncol = length(keep), nrow = 0)), keep)
    LAND.POINTS$SOURCE = record_type
    return(LAND.POINTS)
  }
}





#' @title Format ALA Records from a big data download
#' @description Combines all occurrence files from ALA into one table.
#' It assumes that all the input table comes from the ALA data dump.
#' @param ALA_table       Character Vector - List of taxa already downloaded
#' @param record_type        Adds a column to the data frame for the data source, EG ALA
#' @param keep_cols          The columns we want to keep - a character list created by you
#' @param year_filt          Logical - filter out records based on a year?
#' @param year_lim           Numeric - Records to remove
#' @param unique_cells       Logical - take only one record per cell?
#' @param world_raster       An Raster file of the enviro conditions used (assumed to be global)
#' @export format_ala_dump
format_ala_dump = function(ALA_table, 
                           record_type, 
                           keep_cols,
                           year_filt,
                           year_lim,
                           unique_cells,
                           world_raster) {
  
  message ("formatting ALA data dump to niche analysis format")
  
  ## Combine all the taxa into a single dataframe at once
  ALL <- ALA_table %>% as.data.frame()
  
  ##  type standardisation
  if("latitude" %in% colnames(ALL)) {
    names(ALL)[names(ALL) == 'latitude']  <- 'lat'
    names(ALL)[names(ALL) == 'longitude'] <- 'lon'
  }
  
  if("decimalLatitude" %in% colnames(ALL)) {
    names(ALL)[names(ALL) == 'decimalLatitude']  <- 'lat'
    names(ALL)[names(ALL) == 'decimalLongitude'] <- 'lon'
  }
  
  ##  standardi[sz]e catnum colname
  if("catalogueNumber" %in% colnames(ALL)) {
    ALL <- ALL %>% dplyr::select(-catalogueNumber)
  }
  
  if("eventDate" %in% colnames(ALL)) {
    ALL <- ALL %>% dplyr::select(-eventDate)
  }
  
  ## standardi[sz]e catnum colname
  if('coordinateUncertaintyinMetres' %in% colnames(ALL)) {
    message ("Renaming recordID column to id")
    names(ALL)[names(ALL) == 'coordinateUncertaintyinMetres'] <- 'coordinateUncertaintyInMetres'
    ALL[,"coordinateUncertaintyInMetres"] = as.numeric(unlist(ALL["coordinateUncertaintyInMetres"]))}
  
  ## standardi[sz]e catnum colname
  if('recordID' %in% colnames(ALL)) {
    message ("Renaming recordID column to id")
    names(ALL)[names(ALL) == 'recordID'] <- 'id'}
  
  ## Create the searchTaxon column - check how to put the data in here
  ALL$searchTaxon = ALL$scientificName
  
  if(!is.character(ALL["id"])) {
    ALL["id"] <- as.character(ALL["id"])}
  
  ## Choose only the desired columns
  ALL = ALL %>%
    dplyr::select(one_of(keep_cols))
  
  ## Then print warnings
  warnings()
  
  ## This is a list of columns in different ALA files which have weird characters
  message ('Formatting numeric occurrence data for all ALA records')
  ALL["year"]  = as.numeric(unlist(ALL["year"]))
  ALL["id"]    = as.character(unlist(ALL["id"]))
  
  ## Clear the garbage
  gc()
  
  ## What names get returned
  TRIM <- ALL
  (sum(is.na(TRIM$scientificName)) + nrow(subset(TRIM, scientificName == "")))/nrow(TRIM)*100
  
  ## 3). FILTER RECORDS TO THOSE WITH COORDINATES, AND AFTER 1950
  ## Now filter the ALA records using conditions which are not too restrictive
  CLEAN <- TRIM %>%
    
    ## Note that these filters are very forgiving...
    ## Unless we include the NAs, very few records are returned!
    filter(!is.na(lon) & !is.na(lat)) %>%
    filter(lon <= 180 & lat >= -90) %>%
    filter(lon <= 180 & lat >= -90) %>%
    filter(!is.na(year))
  
  if(year_filt) {
    
    CLEAN <- CLEAN %>% filter(year >= year_lim) 
    
  }
  
  ## How many records were removed by filtering?
  message(nrow(TRIM) - nrow(CLEAN), " records removed")
  message(round((nrow(CLEAN))/nrow(TRIM)*100, 2),
          " % records retained using records with valid coordinates and year")
  
  if(unique_cells) {
    
    ## Now get the XY centroids of the unique 1km * 1km WORLDCLIM blocks where ALA records are found
    ## Get cell number(s) of WORLDCLIM raster from row and/or column numbers. Cell numbers start at 1 in the upper left corner,
    ## and increase from left to right, and then from top to bottom. The last cell number equals the number of raster cell
    world_raster_spat <- rast(world_raster)
    mat <- cbind(lon = CLEAN$lon, lat = CLEAN$lat) 
    xy  <- terra::cellFromXY(world_raster_spat, mat) %>% 
      
      ## get the unique raster cells
      unique %>%
      
      ## Get coordinates of the center of raster cells for a row, column, or cell number of WORLDCLIM raster
      xyFromCell(world_raster_spat, .) %>%
      na.omit()
    
    ## For some reason, we need to convert the xy coords to a spatial points data frame, in order to avoid this error:
    ## 'NAs introduced by coercion to integer range'
    xy <- SpatialPointsDataFrame(coords = xy, data = as.data.frame(xy),
                                 proj4string = CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
    
    ## Now extract the temperature values for the unique 1km centroids which contain ALA data
    class(xy)
    z   = terra::extract(world_raster, xy)
    
    ## Then track which values of Z are on land or not
    onland = z %>% is.na %>%  `!` # %>% xy[.,]  cells on land or not
    
    ## Finally, filter the cleaned ALA data to only those points on land.
    ## This is achieved with the final [onland]
    LAND.POINTS = filter(CLEAN, terra::cellFromXY(world_raster_spat, mat) %in%
                           unique(terra::cellFromXY(world_raster_spat, mat))[onland])
    
    ## how many records were on land?
    records.ocean = nrow(CLEAN) - nrow(LAND.POINTS)
    nrow(LAND.POINTS)
    length(unique(LAND.POINTS$searchTaxon))
    
    ## get rid of some memory
    gc()
    
    ## Add a source column
    LAND.POINTS$SOURCE = record_type
    message(round((nrow(LAND.POINTS))/nrow(CLEAN)*100, 2),
            " % records retained using records inside raster bounds")
    
    ## save data
    nrow(LAND.POINTS)
    length(unique(LAND.POINTS$searchTaxon))
    
    return(LAND.POINTS)
    
  } else {
    
    ## Add a source column
    CLEAN$SOURCE = record_type
    
    ## save data
    return(CLEAN)
  }
}





#' @title Combine ALA Records
#' @description Combines all occurrence files from GBIF into one table.
#' There are slight differences between ALA and GBIF, so separate functions are useful.
#' Although you can download all the records for in one go, this is better for
#' Doing small runs of taxa, or where you want to re-run them constantly
#' @param taxa_list       List of taxa already downloaded
#' @param records_path       File path for downloaded taxa
#' @param records_extension  Which R file type? RDS or RDA
#' @param record_type        Adds a column to the data frame for the data source, EG ALA
#' @param keep_cols          The columns we want to keep - a character list created by you
#' @param world_raster       An Raster file of the enviro conditions used (assumed to be global)
#' @export combine_gbif_records
combine_gbif_records = function(taxa_list, 
                                records_path, 
                                records_extension, 
                                record_type, 
                                keep_cols, 
                                world_raster) {
  
  download = list.files(records_path, pattern = ".RData")
  length(download)
  
  ## Now these lists are getting too long for the combine step.
  ## Restrict them to just the strings that partially match the  taxa list for each run
  spp.download <- paste(taxa_list, records_extension, sep = "")
  download     <- download[download %in% spp.download ]
  message('downloaded taxa ', length(download), ' analyzed taxa ', length(taxa_list))
  
  ALL.POINTS <- download %>%
    
    ## Pipe the list into lapply
    lapply(function(x) {
      
      ## x = download[10]
      ## Create a character string of each .RData file
      f <- sprintf(paste0(records_path, "%s"), x)
      
      ## Load each file
      message('Reading GBIF data for ', x)
      d <- get(load(f))
      
      ## Check if the data frames have data
      if (nrow(d) >= 2) {
        
        if("decimalLatitude" %in% colnames(d)) {
          
          if("gbifID" %in% colnames(d)) {
            d <- d %>% dplyr::select(-gbifID)
          }
          
          if("eventDate" %in% colnames(d)) {
            d <- d %>% dplyr::select(-eventDate)
          }
          
          ## Need to print the object within the loop
          # names(d)[names(d) == 'decimalLatitude']  <- 'lat'
          # names(d)[names(d) == 'decimalLongitude'] <- 'lon'
          d <- d %>% rename(lat = decimalLatitude,
                            lon = decimalLongitude)
          
          ## Create the searchTaxon column - check how to put the data in here
          message ('Formatting occurrence data for ', x)
          searchtax <- gsub(records_extension, "",    x)
          
          ## Filter the data for each taxon
          message('filter records for ', searchtax)
          d <- d %>% dplyr::mutate(searchTaxon = searchtax) %>%
            dplyr::select(one_of(keep_cols)) %>% 
            
            filter(!is.na(lon) & !is.na(lat)) %>%
            filter(lon <= 180 & lat >= -90) %>%
            filter(lon <= 180 & lat >= -90) %>%
            filter(year >= 1950) %>%
            filter(!is.na(year))
          
        } else {
          message('No location data for ', x, ' skipping')
        }
        
      } else {
        message('No GBIF dat for ', x, ' skipping')
      }
      
      return(d)
    }) %>%
    
    ## Finally, bind all the rows together
    bind_rows()
  
  ## Clear the garbage
  gc()
  
  ## If there is GBIF data
  if (nrow(ALL.POINTS) > 0) {
    
    ## What proportion of the dataset has no lat/lon? Need to check this so we know the latest download is working
    formatC(nrow(ALL.POINTS), format = "e", digits = 2)
    (sum(is.na(ALL.POINTS$lat))            + nrow(subset(ALL.POINTS, year < 1950)))/nrow(ALL.POINTS)*100
    
    ## Almost none of the GBIF data has no scientificName. This is the right field to use for matching taxonomy
    (sum(is.na(ALL.POINTS$scientificName)) + nrow(subset(ALL.POINTS, scientificName == "")))/nrow(ALL.POINTS)*100
    
    ## Now get just the columns we want to keep.
    GBIF.TRIM <- ALL.POINTS %>%
      dplyr::select(one_of(keep_cols))
    
    ## Just get the newly downloaded taxa
    GBIF.TRIM = GBIF.TRIM[GBIF.TRIM$searchTaxon %in% taxa_list, ]
    formatC(nrow(GBIF.TRIM), format = "e", digits = 2)
    
    ## What are the unique taxa?
    length(unique(GBIF.TRIM$taxa))
    length(unique(GBIF.TRIM$searchTaxon))
    length(unique(GBIF.TRIM$scientificName))
    
    ## FILTER RECORDS TO THOSE WITH COORDINATES, AND AFTER 1950
    
    ## Now filter the GBIF records using conditions which are not too restrictive
    GBIF.CLEAN <- GBIF.TRIM 
    
    ## How many taxa are there?
    names(GBIF.CLEAN)
    length(unique(GBIF.CLEAN$searchTaxon))
    
    ## REMOVE POINTS OUTSIDE WORLDCLIM LAYERS
    
    ## Can use WORLDCIM rasters to get only records where wordlclim data is.
    message('Removing GBIF points in the ocean for ', length(taxa_list), ' taxa')
    xy_mat <- GBIF.CLEAN %>% dplyr::select(lon, lat) %>% as.matrix()
    
    ## Now get the XY centroids of the unique 1km * 1km WORLDCLIM blocks where GBIF records are found
    ## Get cell number(s) of WORLDCLIM raster from row and/or column numbers. Cell numbers start at 1 in the upper left corner,
    ## and increase from left to right, and then from top to bottom. The last cell number equals the number of raster cells
    xy <- cellFromXY(world_raster, xy_mat) %>%
      
      ## get the unique raster cells
      unique %>%
      
      ## Get coordinates of the center of raster cells for a row, column, or cell number of WORLDCLIM raster
      xyFromCell(world_raster, .) %>%
      na.omit()
    
    ## For some reason, we need to convert the xy coords to a spatial points data frame, in order to avoid this error:
    ## 'NAs introduced by coercion to integer range'
    xy <- SpatialPointsDataFrame(coords = xy, data = as.data.frame(xy),
                                 proj4string = CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
    
    ## Now extract the temperature values for the unique 1km centroids which contain GBIF data
    message('Removing GBIF points in the ocean for ', length(taxa_list), ' taxa')
    class(xy)
    z   = terra::extract(world_raster, xy)
    
    ## Then track which values of Z are on land or not
    onland = z %>% is.na %>%  `!` # %>% xy[.,]  cells on land or not
    
    ## Finally, filter the cleaned GBIF data to only those points on land.
    ## This is achieved with the final [onland]
    LAND.POINTS = filter(GBIF.CLEAN, cellFromXY(world_raster, xy_mat) %in%
                           unique(cellFromXY(world_raster,    xy_mat))[onland])
    
    ## how many records were on land?
    records.ocean = nrow(GBIF.CLEAN) - nrow(LAND.POINTS)  ## 91575 records are in the ocean
    
    ## Print the dataframe dimensions to screen
    nrow(LAND.POINTS)
    length(unique(LAND.POINTS$searchTaxon))
    
    ## Add a source column
    LAND.POINTS$SOURCE = 'GBIF'
    unique(LAND.POINTS$SOURCE)
    names(LAND.POINTS)
    
    ## Free some memory
    gc()
    
  } else {
    message('No GBIF data for this taxon, creating empty datframe to bind to GBIF data')
    LAND.POINTS  = setNames(data.frame(matrix(ncol = length(keep_cols), nrow = 0)), keep_cols)
  }
  return(LAND.POINTS)
}





#' @title Extract environmental records for occurrence data.
#' @description This function combines occurrence files from ALA and GBIF into one table, and extracts enviro values.
#' It assumes that both files come from the previous GBIF/ALA combine function.
#' @param records_df             Data frame of ALA records
#' @param site_df            Data frame of site records (only used if you have site data, e.g. I-naturalist)
#' @param taxa_list          List of taxa analyzed, used to cut the dataframe down
#' @param taxa_level         What taxonomic level to analyze at?
#' @param thin_records       Do you want to thin the records out? If so, it will be 1 record per 1km*1km grid cell
#' @param template_raster    A global R Raster used to thin records to 1 record per 1km grid cell
#' @param world_raster       An global R Raster of the enviro conditions used to extract values for all records
#' @param epsg                The projection system used. Currently, needs to be WGS84
#' @param env_vars           The actual variable names (e.g. bio1 = rainfall, etc.) Only needed for worldlcim
#' @param worldclim_divide    Are you using worldclim stored as long intergers? If so, divide by 10.
#' @param save_data          Do you want to save the data frame?
#' @param data_path          The file path used for saving the data frame
#' @param save_run           A run name to append to the data frame (e.g. bat taxa, etc.). Useful for multiple runs.
#' @export
combine_records_extract = function(records_df,
                                   add_sites,
                                   site_df,
                                   add_site,
                                   filter_taxo,
                                   taxa_list,
                                   taxa_level,
                                   template_raster,
                                   thin_records,
                                   world_raster,
                                   epsg,
                                   complete_var,
                                   raster_divide,
                                   env_variables,
                                   save_data,
                                   data_path,
                                   save_run) {
  
  ## Get just the Common columns
  message('Processing ' , length(unique(records_df$searchTaxon)), ' searched taxa')
  
  ## Now filter the records to those where the searched and returned taxa match
  ## More matching is in : 4_ALA_GBIF_TAXO_COMBINE.R from Green Cities
  ## The matching has to be done at the same taxonomic level
  if(filter_taxo) {
    
    message('fitler taxonomy')
    records_df <- records_df %>% dplyr::mutate(Match_SN_ST = str_detect(!!taxa_level, searchTaxon)) %>% 
      filter(Match_SN_ST == 'TRUE') %>% as.data.frame()
    
  } else {
    message('Do not filter taxonomy of searched taxa vs returned' )
    # records_df <- records_df %>% dplyr::mutate(Match_SN_ST = str_detect(!!taxa_level, searchTaxon))
  }
  
  ## Make sure the projection matches
  RECORDS.84 = records_df %>% 
    st_transform(., st_crs(epsg))
  
  ## Don't taxo match the site data :: this needs to be kept without exclusion
  if(add_sites) {
    
    RECORDS.COMBO.84 <- dplyr::bind_rows(list(RECORDS.84, 
                                              site_df))
    
  } else {
    message('Do not add site data')
    RECORDS.COMBO.84 <- RECORDS.84
  }
  
  # coords <- st_coordinates(RECORDS.84)
  
  if(thin_records == TRUE) {
    
    ## The length needs to be the same
    length(unique(RECORDS.COMBO.84$searchTaxon))
    RECORDS.COMBO.84 <- split(RECORDS.COMBO.84, RECORDS.COMBO.84$searchTaxon)
    occurrence_cells_all  <- lapply(RECORDS.COMBO.84, function(x) cellFromXY(template_raster, x))
    
    ## Check with a message, but could check with a fail
    message('Split prodcues ', length(occurrence_cells_all), ' data frames for ', length(taxa_list), ' taxa')
    
    ## Now get just one record within each 1*1km cell.
    RECORDS.COMBO.84.THIN <- mapply(function(x, cells) {
      x[!duplicated(cells), ]
    }, RECORDS.COMBO.84, occurrence_cells_all, SIMPLIFY = FALSE) %>% do.call(rbind, .)
    
    ## Check to see we have 19 variables + the taxa for the standard predictors, and 19 for all predictors
    message(round(nrow(RECORDS.COMBO.84.THIN)/nrow(RECORDS.COMBO.84)*100, 2), " % records retained at 1km resolution")
    
    ## Create points: the 'over' function seems to need geographic coordinates for this data...
    COMBO.POINTS   = RECORDS.COMBO.84.THIN[c("lon", "lat")]
    
  } else {
    message('dont thin the records out' )
    COMBO.POINTS     = RECORDS.COMBO.84[c("lon", "lat")]
    RECORDS.COMBO.84.THIN = RECORDS.COMBO.84
  }
  
  ## Bioclim variables
  ## Extract raster data
  message('Extracting raster values for ', length(taxa_list), ' taxa in the set ', "'", save_run, "'")
  message(projection(COMBO.POINTS));message(projection(world_raster))
  
  ## Extract the raster values
  COMBO.RASTER <- terra::extract(world_raster, COMBO.POINTS) %>%
    cbind(as.data.frame(RECORDS.COMBO.84.THIN), .)
  
  ## Change the raster values here: See http://worldclim.org/formats1 for description of the integer conversion.
  ## All worldclim temperature variables were multiplied by 10, so then divide by 10 to reverse it.
  if (raster_divide == TRUE) {
    
    ## Convert the worldclim grids
    message('Processing worldclim 1.0 data, divide the rasters by 10')
    
    COMBO.RASTER.CONVERT = as.data.table(COMBO.RASTER)
    COMBO.RASTER.CONVERT[, (env_variables [c(1:11)]) := lapply(.SD, function(x)
      x / 10 ), .SDcols  = env_variables   [c(1:11)]]
    COMBO.RASTER.CONVERT = as.data.frame(COMBO.RASTER.CONVERT)
    
  } else {
    message('Do not divide the rasters')
    COMBO.RASTER.CONVERT = COMBO.RASTER
  }
  
  ## Get the complete data - this is the step where the site records could be disappearing,
  ## as the records could be on the coast, etc.
  COMBO.RASTER.CONVERT = completeFun(COMBO.RASTER.CONVERT, c(names(world_raster)))
  
  message(length(unique(COMBO.RASTER.CONVERT$searchTaxon)),
          ' taxa processed of ', length(taxa_list), ' original taxa')
  
  ## What percentage of records are retained?
  message(round(nrow(COMBO.RASTER.CONVERT)/nrow(records_df)*100, 2),
          " % records retained after raster extraction")
  
  ## save data
  if(save_data == TRUE) {
    
    ## save .rds file for the next session
    saveRDS(COMBO.RASTER.CONVERT, paste0(data_path, 'COMBO_RASTER_CONVERT_',  save_run, '.rds'))
    return(COMBO.RASTER.CONVERT)
    
  } else {
    return(COMBO.RASTER.CONVERT)
  }
  gc()
}






#' @title Clean occurrence data
#' @description Takes a data frame of all taxa records, and flag records as institutional or spatial outliers.
#' It uses the CoordinateCleaner package https://cran.r-project.org/web/packages/CoordinateCleaner/index.html.
#' It assumes that the records data.frame is that returned by the combine_records_extract function
#' @param records            Data.frame. DF of all taxa records returned by the combine_records_extract function
#' @param capitals           Numeric. Remove records within an xkm radius of capital cites (see ?clean_coordinates)
#' @param centroids          Numeric. Remove records within an xkm radius around country centroids (see ?clean_coordinates)
#' @param save_run           Character string - run name to append to the data frame, useful for multiple runs.
#' @param save_data          Logical or character - do you want to save the data frame?
#' @param data_path          Character string - The file path used for saving the data frame
#' @export coord_clean_records
coord_clean_records = function(records,
                               multi_source,
                               occ_flag,
                               site_flag,
                               capitals,
                               centroids,
                               save_run,
                               data_path,
                               save_data) {
  
  ## Create a unique identifier. This is used for automated cleaing of the records, and also saving shapefiles
  ## But this will not be run for all taxa linearly. So, it probably needs to be a combination of taxa and number
  records$CC.OBS <- 1:nrow(records)
  records$CC.OBS <- paste0(records$CC.OBS, "_CC_", records$searchTaxon)
  records$CC.OBS <- gsub(" ",     "_",  records$CC.OBS, perl = TRUE)
  length(records$CC.OBS);length(unique(records$CC.OBS))
  records$taxa = records$searchTaxon
  
  if(multi_source){
    
    ## Split the site data up from the ALA data
    message('Subset data by source')
    ala_records  <- records %>% filter(SOURCE == occ_flag)
    site_records <- records %>% filter(SOURCE == site_flag) %>% dplyr::mutate(coord_summary = TRUE)
    
  } else {
    message('Do not subset data by source')
    ala_records  <- records %>% filter(SOURCE == 'ALA')
  }
  
  ## Rename the columns to fit the CleanCoordinates format and create a tibble.
  TIB.GBIF <- ala_records %>% dplyr::rename(coord_spp        = searchTaxon,
                                            decimallongitude = lon,
                                            decimallatitude  = lat) %>%
    
    ## Then create a tibble for running the spatial outlier cleaning
    timetk::tk_tbl() %>%
    
    ## Consider the arguments. We've already stripped out the records that fall outside
    ## the worldclim raster boundaries, so the sea test is probably not the most important
    ## Study area is the globe, but we are only projecting models onto Australia
    
    ## The geographic outlier detection is not working here. Try it in setp 6
    ## Also, the duplicates step is not working, flagging too many records
    clean_coordinates(.,
                      verbose         = TRUE,
                      tests = c("capitals",     "centroids", "equal", "gbif",
                                "institutions", "zeros"), ## duplicates flagged too many
                      
                      ## remove records within xkm  of capitals
                      ## remove records within xkm of country centroids
                      capitals_rad  = capitals,
                      centroids_rad = centroids) %>%
    
    ## The select the relevant columns and rename
    dplyr::select(., coord_spp, CC.OBS, .val,  .equ, .zer, .cap,
                  .cen,    .gbf,   .inst, .summary)
  
  ## Then rename
  summary(TIB.GBIF)
  names(TIB.GBIF) = c("coord_spp", "CC.OBS",    "coord_val",  "coord_equ",  "coord_zer",  "coord_cap",
                      "coord_cen", "coord_gbf", "coord_inst", "coord_summary")
  
  ## Flagging ~ x%, excluding the spatial outliers. Seems reasonable?
  message(round(with(TIB.GBIF, table(coord_summary)/sum(table(coord_summary))*100), 2), " % records removed")
  
  ## Is the taxa column the same as the searchTaxon column?
  COORD.CLEAN = left_join(ala_records, TIB.GBIF, by = "CC.OBS")
  
  ## Now subset to records that are flagged as outliers
  message(table(COORD.CLEAN$coord_summary))
  
  ## What percentage of records are retained?
  message(length(unique(COORD.CLEAN$searchTaxon)))
  
  ## Remove duplicate coordinates
  drops <- c("lon.1", "lat.1")
  COORD.CLEAN <- COORD.CLEAN[ , !(names(COORD.CLEAN) %in% drops)]
  unique(COORD.CLEAN$SOURCE)
  
  CLEAN.TRUE <- subset(COORD.CLEAN, coord_summary == TRUE)
  
  if(multi_source){
    
    ## Split the site data up from the ALA data
    message('Combine sources')
    CLEAN.TRUE <- bind_rows(CLEAN.TRUE, site_records)
    
  } else {
    message('Do not Combine sources')
  }
  
  message(round(nrow(CLEAN.TRUE)/nrow(records)*100, 2), " % records retained")
  message(table(COORD.CLEAN$coord_summary))
  
  if(save_data == TRUE) {
    ## save .rds file for the next session
    saveRDS(CLEAN.TRUE, paste0(data_path, 'COORD_CLEAN_', save_run, '.rds'))
    return(CLEAN.TRUE)
    
  } else {
    message('Return the cleaned occurrence data to the global environment')   ##
    return(CLEAN.TRUE)
  }
}





#' @title Check Spatial Outliers
#' @description This function takes a data frame of all taxa records,
#' flags records as spatial outliers (T/F for each record in the df), and saves images of the checks for each.
#' Manual cleaning of spatial outliers is very tedious, but automated cleaning makes mistakes, so checking is handy
#' It uses the CoordinateCleaner package https://cran.r-project.org/web/packages/CoordinateCleaner/index.html.
#' It assumes that the input dfs are those returned by the coord_clean_records function
#' @param all_df             Data.frame. DF of all taxa records returned by the coord_clean_records function
#' @param site_df            Data.frame of site records (only used if you have site data, e.g. I-naturalist)
#' @param land_shp           R object. Shapefile of the worlds land (e.g. https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-land/)
#' @param clean_path         Character string -  The file path used for saving the checks
#' @param record_limit       Numeric - limit for records
#' @param multi_source       Numeric - use multiple sources?
#' @param spatial_mult       Numeric. The multiplier of the interquartile range (method == 'quantile', see ?cc_outl)
#' @export check_spatial_outliers
check_spatial_outliers = function(occ_df,
                                  multi_source,
                                  site_flag,
                                  occ_flag,
                                  site_records,
                                  land_shp,
                                  clean_path,
                                  plot_points,
                                  record_limit,
                                  spatial_mult,
                                  prj) {
  
  ## Try plotting the points which are outliers for a subset of spp and label them
  if(plot_points == TRUE) {
    
    ALL.PLOT = SpatialPointsDataFrame(coords      = occ_df[c("lon", "lat")],
                                      data        = occ_df,
                                      proj4string = prj)
    
    CLEAN.TRUE = subset(occ_df, coord_summary == TRUE)
    CLEAN.PLOT = SpatialPointsDataFrame(coords      = CLEAN.TRUE[c("lon", "lat")],
                                        data        = CLEAN.TRUE,
                                        proj4string = prj)
    
    ## Create global and australian shapefile in the local coordinate system
    LAND.84 = land_shp %>%
      spTransform(prj)
    AUS.84 = AUS %>%
      spTransform(prj)
    
    ## spp = plot.taxa[1]
    plot.taxa <- as.character(unique(CLEAN.PLOT$searchTaxon))
    for (spp in plot.taxa) {
      
      ## Plot a subset of taxa
      CLEAN.PLOT.PI = CLEAN.PLOT[ which(CLEAN.PLOT$searchTaxon == spp), ]
      
      message("plotting occ data for ", spp, ", ",
              nrow(CLEAN.PLOT.PI), " records flagged as ",
              unique(CLEAN.PLOT.PI$coord_summary))
      
      ## Plot true and false points for the world
      ## Black == FALSE
      ## Red   == TRUE
      message('Writing map of global coord clean records for ', spp)
      png(sprintf("%s%s_%s", clean_path, spp, "global_check_cc.png"),
          16, 10, units = 'in', res = 500)
      
      par(mfrow = c(1,1))
      plot(AUS.84, main = paste0(nrow(subset(CLEAN.PLOT.PI, coord_summary == FALSE)),
                                 " Global clean_coord 'FALSE' points for ", spp),
           lwd = 0.01, asp = 1, bg = 'sky blue', col = 'grey')
      
      points(CLEAN.PLOT.PI,
             pch = ".", cex = 3.3, cex.lab = 3, cex.main = 4, cex.axis = 2,
             xlab = "", ylab = "", asp = 1,
             col = factor(ALL.PLOT$coord_summary))
      
      dev.off()
      
    }
    
  } else {
    message('Do not create maps of each taxon coord clean records')   ##
  }
  
  ## Split the table into ALA and site data
  if(multi_source){
    
    message('Subset data by source')
    occ_only <- occ_df %>% filter(SOURCE == occ_flag)
    site_df  <- occ_df %>% filter(SOURCE == site_flag) %>% dplyr::mutate(SPAT_OUT = TRUE)
    
  } else {
    message('Do not subset data by source')
    occ_only <- occ_df
  }
  
  
  ## Create a tibble to supply to coordinate cleaner
  test.geo = SpatialPointsDataFrame(coords      = occ_only[c("lon", "lat")],
                                    data        = occ_only,
                                    proj4string = prj)
  
  SDM.COORDS  <- test.geo %>%
    
    spTransform(., prj) %>% as.data.frame() %>%
    dplyr::select(searchTaxon, lon, lat, CC.OBS, SOURCE) %>%
    dplyr::rename(taxa          = searchTaxon,
                  decimallongitude = lon,
                  decimallatitude  = lat) %>%
    
    timetk::tk_tbl()
  
  ## Check how many records each spp has...
  COMBO.LUT <- SDM.COORDS %>%
    as.data.frame() %>%
    dplyr::select(taxa) %>%
    table() %>%
    as.data.frame()
  COMBO.LUT <- setDT(COMBO.LUT, keep.rownames = FALSE)[]
  names(COMBO.LUT) = c("taxa", "FREQUENCY")
  COMBO.LUT = COMBO.LUT[with(COMBO.LUT, rev(order(FREQUENCY))), ]
  
  ## Watch out here - this sorting could cause problems for the order of the data frame once it's stitched back together
  ## If we we use spp to join the data back together, will it preserve the order?
  LUT.100K = as.character(subset(COMBO.LUT, FREQUENCY < record_limit)$taxa)
  LUT.100K = trimws(LUT.100K [order(LUT.100K)])
  length(LUT.100K)
  
  ## Create a data frame of spp name and spatial outlier
  SPAT.OUT <- LUT.100K  %>%
    
    ## pipe the list of spp into lapply
    lapply(function(x) {
      
      ## Create the spp df by subsetting by spp
      f <- subset(SDM.COORDS, taxa == x)
      
      ## Run the spatial outlier detection
      message("Running spatial outlier detection for ", x)
      message(nrow(f), " records for ", x)
      sp.flag <- cc_outl(f,
                         lon     = "decimallongitude",
                         lat     = "decimallatitude",
                         species = "taxa",
                         method  = "quantile",
                         mltpl   = spatial_mult,
                         value   = "flagged",
                         verbose = TRUE)
      
      ## Now add attach column for spp, and the flag for each record
      d = cbind(searchTaxon = x,
                SPAT_OUT = sp.flag, f)[c("searchTaxon", "SPAT_OUT", "CC.OBS")]
      
      ## Remember to explicitly return the df at the end of loop, so we can bind
      return(d)
      
    }) %>%
    
    ## Finally, bind all the rows together
    bind_rows
  
  gc()
  
  ## Join the data back on
  SPAT.FLAG = join(as.data.frame(test.geo), SPAT.OUT) %>% dplyr::select(-lon.1, -lat.1) ## Join means the skipped spp are left out
  nrow(SPAT.FLAG)
  
  ## Try plotting the points which are outliers for a subset of spp and label them
  ## Get the first 10 spp
  if(plot_points == TRUE) {
    
    spat.taxa <- as.character(unique(SPAT.FLAG$searchTaxon))
    for (taxa in spat.taxa) {
      
      ## Plot a subset of taxa
      ## spp = spat.taxa[1]
      SPAT.PLOT <- SPAT.FLAG %>% filter(searchTaxon == taxa) %>%
        SpatialPointsDataFrame(coords      = .[c("lon", "lat")],
                               data        = .,
                               proj4string = prj)
      
      message("plotting occ data for ", spp, ", ",
              nrow(SPAT.PLOT ), " records")
      
      ## Plot true and false points for the world
      ## Black == FALSE
      ## Red   == TRUE
      message('Writing map of global coord clean records for ', taxa)
      png(sprintf("%s%s_%s", clean_path, taxa, "global_spatial_outlier_check.png"),
          16, 10, units = 'in', res = 500)
      
      par(mfrow = c(1,1))
      # plot(LAND.84, main = paste0(nrow(subset(SPAT.PLOT, SPAT_OUT == FALSE)),
      #                             " Spatial outlier 'FALSE' points for ", spp),
      #      lwd = 0.01, asp = 1, col = 'grey', bg = 'sky blue')
      # 
      # points(SPAT.PLOT,
      #        pch = ".", cex = 3.3, cex.lab = 3, cex.main = 4, cex.axis = 2,
      #        xlab = "", ylab = "", asp = 1,
      #        col = factor(SPAT.PLOT$SPAT_OUT))
      
      ## Plot true and false points for the world
      ## Black == FALSE
      ## Red   == TRUE
      plot(AUS.84, main = paste0("Australian points for ", taxa),
           lwd = 0.01, asp = 1, bg = 'sky blue', col = 'grey')
      
      points(SPAT.PLOT,
             pch = ".", cex = 3.3, cex.lab = 3, cex.main = 4, cex.axis = 2,
             xlab = "", ylab = "", asp = 1,
             col = factor(SPAT.PLOT$SPAT_OUT))
      
      dev.off()
    }
    
  } else {
    message('Do not create maps of each species spatial clean records')   ##
  }
  
  ## Could add the taxa site records in here
  if(site_records) {
    
    ##
    message('Combine the Spatially cleaned data with the site data' )
    SPAT.FLAG <- SPAT.FLAG %>% filter(SPAT_OUT == TRUE) %>% bind_rows(., site_df)
    SPAT.TRUE <- SpatialPointsDataFrame(coords      = SPAT.FLAG[c("lon", "lat")],
                                        data        = SPAT.FLAG,
                                        proj4string = prj)
    
    message('Cleaned ', paste0(unique(SPAT.TRUE$SOURCE), sep = ' '), ' records')
    return(SPAT.TRUE)
    
  } else {
    message('Dont add site data' )
    SPAT.TRUE <- SPAT.FLAG %>% filter(SPAT_OUT == TRUE)
  }
  return(SPAT.TRUE)
}





#' @title Calculate niches
#' @description Takes a data frame of all species records,
#' estimates geographic and environmental ranges for each, and creates a table of each.
#' It uses the AOO.computing function in the ConR package https://cran.r-project.org/web/packages/ConR/index.html
#' It assumes that the input df is that returned by the check_spatial_outliers function
#' @param coord_df           Data.frame. DF of all species records returned by the coord_clean_records function
#' @param aus_df             Data.frame of site records (only used if you have site data, e.g. I-naturalist)
#' @param world_shp          .Rds object. Shapefile of the worlds land (e.g. https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-land/)
#' @param kop_shp            .Rds object. Shapefile of the worlds koppen zones (e.g. https://www.climond.org/Koppen.aspx)
#' @param taxa_list       Character string - List of taxa analysed, used to cut the dataframe down
#' @param env_vars           Character string - List of environmental variables analysed
#' @param cell_size          Numeric. Value indicating the grid size in decimal degrees used for estimating Area of Occupancy (see ?AOO.computing)
#' @param save_run           Character string - run name to append to the data frame, useful for multiple runs.
#' @param save_data          Logical - do you want to save the data frame?
#' @param data_path          Character string - The file path used for saving the data frame
#' @export calc_enviro_niches
calc_enviro_niches = function(coord_df,
                              prj,
                              country_shp,
                              world_shp,
                              kop_shp,
                              #ibra_shp,
                              taxa_list,
                              env_vars,
                              cell_size,
                              save_run,
                              data_path,
                              save_data) {
  
  ## Create a spatial points object
  message('Estimating global niches for ', length(taxa_list), ' taxa across ',
          length(env_vars), ' climate variables')
  
  NICHE.1KM <- coord_df %>% as.data.frame () %>%
    filter(coord_summary == TRUE)
  NICHE.1KM.84 <- SpatialPointsDataFrame(coords      = NICHE.1KM[c("lon", "lat")],
                                         data        = NICHE.1KM,
                                         proj4string = prj)
  
  ## Use a projected, rather than geographic, coordinate system
  ## Not sure why, but this is needed for the spatial overlay step
  AUS.WGS   = spTransform(country_shp, prj)
  LAND.WGS  = spTransform(world_shp,   prj)
  KOP.WGS   = spTransform(kop_shp,     prj)
  
  ## Intersect the points with the Global koppen file
  message('Intersecting points with shapefiles for ', length(taxa_list), ' taxa')
  KOP.JOIN     = over(NICHE.1KM.84, KOP.WGS)
  
  ## Create global niche and Australian niche for website - So we need a subset for Australia
  ## The ,] acts just like a clip in a GIS
  NICHE.AUS <-  NICHE.1KM.84[AUS.WGS, ]
  
  ## Aggregate the number of Koppen zones (and IBRA regions) each taxon is found in
  COMBO.KOP <- NICHE.1KM.84 %>%
    cbind.data.frame(., KOP.JOIN)
  
  ## Aggregate the data
  KOP.AGG = tapply(COMBO.KOP$Koppen, COMBO.KOP$searchTaxon,
                   function(x) length(unique(x))) %>% ## group Koppen by taxa name
    as.data.frame()
  KOP.AGG =  setDT(KOP.AGG , keep.rownames = TRUE)[]
  names(KOP.AGG) = c("searchTaxon", "KOP_count")
  
  ## Run join between taxa records and spatial units :: SUA, POA and KOPPEN zones
  message('Joining occurence data to SUAs for ',
          length(taxa_list), ' taxa in the set ', "'", save_run, "'")
  
  ## CREATE NICHES FOR PROCESSED TAXA
  ## Create niche summaries for each environmental condition like this...commit
  ## Here's what the function will produce :
  NICHE.AUS.DF = NICHE.AUS %>%
    as.data.frame() %>%
    dplyr::select(., searchTaxon, one_of(env_vars))
  
  NICHE.GLO.DF = NICHE.1KM.84 %>%
    as.data.frame() %>%
    dplyr::select(., searchTaxon, one_of(env_vars))
  
  ## Currently, the niche estimator can't handle NAs
  NICHE.AUS.DF = completeFun(NICHE.AUS.DF, env_vars[1])
  NICHE.GLO.DF = completeFun(NICHE.GLO.DF, env_vars[1])
  
  message('Estimating global niches for ',
          length(taxa_list), ' taxa in the set ', "'", save_run, "'")
  
  GLOB.NICHE <- env_vars %>%
    
    ## Pipe the list into lapply
    lapply(function(x) {
      
      ## Now, use the niche width function on each colname
      ## Also, need to figure out how to make the aggregating column generic (species, genus, etc.)
      ## currently it only works hard-wired........
      niche_estimate(DF = NICHE.GLO.DF, colname = x)
      
    }) %>% as.data.frame()
  
  ## Remove duplicate Taxon columns and check the output
  GLOB.NICHE <- GLOB.NICHE %>% dplyr::select(-contains("."))
  message('Calculated global niches for ', names(GLOB.NICHE), ' variables')
  
  message('Estimating Australian niches for ', length(taxa_list), ' taxa in the set ', "'", save_run, "'")
  AUS.NICHE <- env_vars %>%
    
    ## Pipe the list into lapply
    lapply(function(x) {
      
      ## Now, use the niche width function on each colname
      ## Also, need to figure out how to make the aggregating column generic (species, genus, etc.)
      niche_estimate(DF = NICHE.AUS.DF, colname = x)
      
    }) %>%
    
    ## finally, create one dataframe for all niches
    as.data.frame
  
  ## Remove duplicate Taxon columns and check the output :: would be great to skip these columns when running the function
  AUS.NICHE <- GLOB.NICHE %>% dplyr::select(-contains("."))
  message('Calculated Australia niches for ', names(AUS.NICHE), ' variables')
  
  ## How are the AUS and GLOB niches related? Many taxa won't have both Australian and Global niches.
  ## So best to calculate the AUS niche as a separate table. Then, just use the global niche table for the rest of the code
  length(AUS.NICHE$searchTaxon); length(GLOB.NICHE$searchTaxon)
  
  ## Add the count of Australian records - this is not necessarily the same as maxent_records
  Aus_records = as.data.frame(table(NICHE.AUS.DF$searchTaxon))
  names(Aus_records) = c("searchTaxon", "Aus_records")
  identical(nrow(Aus_records), nrow(AUS.NICHE))
  
  ## Add the counts of Australian records for each taxon to the niche database
  GLOB.NICHE = join(Aus_records, GLOB.NICHE,  type = "right")
  
  ## Check the record and POA counts
  head(GLOB.NICHE$Aus_records,20)
  head(GLOB.NICHE$KOP_count,  20)
  
  ## Check
  message(round(nrow(AUS.NICHE)/nrow(GLOB.NICHE)*100, 2), " % taxa have records in Australia")
  dim(GLOB.NICHE)
  dim(AUS.NICHE)
  
  ## 5). CALCULATE AREA OF OCCUPANCY
  
  ## Given a dataframe of georeferenced occurrences of one, or more, taxa, this function provide statistics values
  ## (Extent of Occurrence, Area of Occupancy, number of locations, number of subpopulations) and provide a preliminary
  ## conservation status following Criterion B of IUCN.
  
  ## AREA OF OCCUPANCY (AOO).
  ## For every taxa in the list: calculate the AOO
  ## x = spp.geo[63]
  spp.geo = as.character(unique(COMBO.KOP$searchTaxon))
  
  GBIF.AOO <- spp.geo %>%
    
    ## Pipe the list into lapply
    lapply(function(x) {
      
      ## Subset the the data frame to calculate area of occupancy according the IUCN.eval
      DF = subset(COMBO.KOP, searchTaxon == x)[, c("lat", "lon", "searchTaxon")] %>% 
        .[!duplicated(.[,c('lat', 'lon')]),] 
      
      if(nrow(DF) > 2) {
        
        message('Calcualting AOO and EOO for ', x, ', using ', nrow(DF), ' records')
        AOO = AOO.computing(XY = DF, Cell_size_AOO = cell_size)  ## Grid size in decimal degrees
        
        ## Set the working directory
        # setwd(data_path)
        EOO  = EOO.computing(XY = DF, write_shp = TRUE, write_results = FALSE) 
        AOO  = as.data.frame(AOO)
        AOO$searchTaxon  <- rownames(AOO)
        EOO$searchTaxon  <- rownames(EOO)
        rownames(AOO)    <- NULL
        rownames(EOO)    <- NULL
        Extent           <- left_join(AOO, EOO, by = "searchTaxon") %>% 
          dplyr::select(searchTaxon, AOO, EOO)
        
        ## Return the area
        return(Extent)
        
      } else {
        message('Calcualting AOO, but NOT EOO for ', x, ', using ', nrow(DF), ' records')
        AOO = AOO.computing(XY = DF, Cell_size_AOO = cell_size)  ## Grid size in decimal degrees
        
        ## Set the working directory
        AOO  = as.data.frame(AOO)
        AOO$searchTaxon  <- rownames(AOO)
        rownames(AOO)    <- NULL
        Extent           <- AOO %>%
          dplyr::mutate(EOO = 0) %>% 
          dplyr::select(searchTaxon, AOO, EOO)
        
        ## Return the area
        return(Extent)
      }
    }) %>%
    
    ## Finally, create one data-frame for all niches
    bind_rows()
  head(GBIF.AOO)
  
  ## Now join on the geographic range and glasshouse data
  identical(nrow(GBIF.AOO), nrow(GLOB.NICHE))
  GLOB.NICHE <- list(GLOB.NICHE, GBIF.AOO, KOP.AGG) %>%
    reduce(left_join, by = "searchTaxon") %>%
    dplyr::select(searchTaxon, Aus_records, AOO, EOO, KOP_count, everything())
  
  if(save_data == TRUE) {
    
    ## save .rds file for the next session
    message('Writing 1km resolution niche and raster data for ',
            length(taxa_list), ' taxa in the set ', "'", save_run, "'")
    saveRDS(GLOB.NICHE, paste0(data_path, 'GLOBAL_NICHES_',    save_run, '.rds'))
    write_csv(GLOB.NICHE, paste0(data_path, 'GLOBAL_NICHES_',  save_run, '.csv'))
    return(GLOB.NICHE)
    
  } else {
    message(' Return niches to the environment for ', length(taxa_list), ' taxa analysed')
    return(GLOB.NICHE)
  }
}





#' @title Complete data frame
#' @description Remove taxa records with NA enviro (i.e. Raster) values - records outside the exetent of rasters
#' @param data         Data.frame of taxa records
#' @param desiredCols  Character string of columns to search for NA values EG c('rainfall', 'temp'), etc.
#' @return             A df with NA records removed
#' @export completeFun
completeFun <- function(data, desiredCols) {
  
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
  
}





#' @title Estimate niches
#' @description  Takes a data frame of all taxa records,
#' and estimates the environmental niche for each taxon
#' It assumes that the input df is that prepared by the calc_1km_niches function
#' @param  DF        Data.frame of all taxa records prepared by the calc_1km_niches function
#' @param  colname   Character string - the columns to estimate niches for E.G. 'rainfall', etc.
#' @return           Data.frame of estimated environmental niches for each taxon
#' @export niche_estimate
niche_estimate = function (DF,
                           colname) {
  
  ## R doesn't seem to have a built-in mode function
  ## This doesn't really handle multiple modes...
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  ## Use ddply inside a function to create niche widths and medians for each species
  ## Also, need to figure out how to make the aggregating column generic (species, genus, etc.)
  ## Try to un-wire the grouping column using !!
  summary = ddply(DF,
                  .(searchTaxon),           ## currently grouping column only works hard-wired
                  .fun = function (xx, col) {
                    
                    ## All the different columns
                    ## Calculate them all, and maybe supply a list of ones to keep
                    min      = min(xx[[col]])
                    max      = max(xx[[col]])
                    
                    q02      = quantile(xx[[col]], .02)
                    q05      = quantile(xx[[col]], .05)
                    q95      = quantile(xx[[col]], .95)
                    q98      = quantile(xx[[col]], .98)
                    
                    median   = median(xx[[col]])
                    mean     = mean(xx[[col]])
                    mode     = Mode(xx[[col]])
                    range    = max - min
                    q95_q05  = (q95 - q05)
                    q98_q02  = (q98 - q02)
                    
                    ## Then crunch them together
                    c(min, max, median, mode, mean, range, q02, q05, q95, q98, q95_q05, q98_q02)
                    
                  },
                  colname
  )
  
  ## Concatenate output
  ## Figure out how to make the aggregating column generic (species, genus, etc.)
  ## currently it only works hard-wired
  colnames(summary) = c("searchTaxon",
                        paste0(colname,  "_min"),
                        paste0(colname,  "_max"),
                        paste0(colname,  "_median"),
                        paste0(colname,  "_mode"),
                        paste0(colname,  "_mean"),
                        paste0(colname,  "_range"),
                        paste0(colname,  "_q02"),
                        paste0(colname,  "_q05"),
                        paste0(colname,  "_q95"),
                        paste0(colname,  "_q98"),
                        paste0(colname,  "_q95_q05"),
                        paste0(colname,  "_q98_q02"))
  
  ## return the summary of niche width and median
  return (summary)
}





#' @title Plot Histograms
#' @description This function takes a data frame of all taxa records,
#' and plots histograms and convex hulls for each taxon in global enviromental space
#' It assumes that the input df is that prepared by the check_spatial_outliers function
#' @param  coord_df           Data.frame of all taxa records prepared by the check_spatial_outliers function
#' @param  taxa_list       Character string - the taxa analysed
#' @param  range_path         Character string - file path for saving histograms and convex hulls
#' @export plot_range_histograms
plot_range_histograms = function(coord_df,
                                 taxa_list,
                                 range_path) {
  
  ## Subset the occurrence data to that used by maxent
  message('Plotting global environmental ranges for ', length(taxa_list), ' taxa')
  coord_df <- coord_df %>%
    as.data.frame()
  
  ## Plot histograms of temperature and rainfall
  ## spp = taxa_list[1]
  spp.plot = as.character(unique(coord_df$searchTaxon))
  for (spp in spp.plot) {
    
    ## Subset the spatial dataframe into records for each spp
    DF       <- coord_df[coord_df$searchTaxon %in% spp , ]
    DF.OCC   <- subset(coord_df, searchTaxon == spp & SOURCE != "INVENTORY")
    
    ## Create a new field RANGE, which is either site or NATURAL
    coord_spp <- coord_df %>%
      filter(searchTaxon == spp) %>%
      dplyr::mutate(RANGE = ifelse(SOURCE != "INVENTORY", "NATURAL", "site"))
    
    convex_hull <- sprintf("%s%s_%s", range_path, spp, "1km_convex_hull.png")
    if(!file.exists(convex_hull)) {
      
      ## Start PNG device
      message('Writing global convex hulls for ', spp)
      png(convex_hull, 16, 10, units = 'in', res = 500)
      
      ## Create convex hull and plot
      find_hull <- function(DF) DF[chull(DF$Annual_mean_temp, DF$Annual_precip), ]
      hulls     <- ddply(DF, "SOURCE", find_hull)
      plot      <- ggplot(data = DF, aes(x = Annual_mean_temp,
                                         y = Annual_precip, colour = SOURCE, fill = SOURCE)) +
        geom_point() +
        geom_polygon(data = hulls, alpha = 0.5) +
        labs(x = "Annual_mean_temp", y = "Annual_precip") +
        
        theme(axis.title.x     = element_text(colour = "black", size = 35),
              axis.text.x      = element_text(size = 20),
              
              axis.title.y     = element_text(colour = "black", size = 35),
              axis.text.y      = element_text(size = 20),
              
              panel.background = element_blank(),
              panel.border     = element_rect(colour = "black", fill = NA, size = 1.5),
              plot.title       = element_text(size   = 40, face = "bold"),
              legend.text      = element_text(size   = 20),
              legend.title     = element_text(size   = 20),
              legend.key.size  = unit(1.5, "cm")) +
        
        ggtitle(paste0("Convex Hull for ", spp))
      print(plot)
      
      ## close device
      dev.off()
    }
    
    temp_hist <- sprintf("%s%s_%s", range_path, spp, "temp_niche_histograms_1km_records.png")
    if(!file.exists(temp_hist)) {
      
      ## Check if file exists
      message('Writing global temp histograms for ', spp)
      png(temp_hist,
          16, 10, units = 'in', res = 500)
      
      ## Use the 'SOURCE' column to create a histogram for each source.
      temp.hist <- ggplot(coord_spp, aes(x = Annual_mean_temp, group = RANGE, fill = RANGE)) +
        
        geom_density(aes(x = Annual_mean_temp, y = ..scaled.., fill = RANGE),
                     color = 'black', alpha = 1) +
        
        ## Change the colors
        scale_fill_manual(values = c('NATURAL' = 'coral2',
                                     'Mayor'   = 'gainsboro'), na.value = "grey") +
        
        ggtitle(paste0("Worldclim temp niches for ", spp)) +
        
        ## Add themes
        theme(axis.title.x     = element_text(colour = "black", size = 35),
              axis.text.x      = element_text(size = 25),
              
              axis.title.y     = element_text(colour = "black", size = 35),
              axis.text.y      = element_text(size = 25),
              
              panel.background = element_blank(),
              panel.border     = element_rect(colour = "black", fill = NA, size = 3),
              plot.title       = element_text(size   = 40, face = "bold"),
              legend.text      = element_text(size   = 20),
              legend.title     = element_text(size   = 20),
              legend.key.size  = unit(1.5, "cm"))
      
      ## Print the plot and close the device
      print(temp.hist + ggtitle(paste0("Worldclim temp niches for ", spp)))
      dev.off()
    }
    
    temp_box <- sprintf("%s%s_%s", range_path, spp, "temp_niche_boxplots_1km_records.png")
    if(!file.exists(temp_hist)) {
      
      ## Check if file exists
      png(temp_box,
          10, 14, units = 'in', res = 500)
      
      ## Use the 'SOURCE' column to create a histogram for each source.
      temp.box = ggboxplot(coord_spp,
                           x    = 'RANGE',
                           y    = 'Annual_mean_temp',
                           fill = 'RANGE',
                           palette = c('coral2', 'gainsboro'), size = 0.6) +
        
        ## Use the classic theme
        theme_classic() +
        labs(y = 'Annual Mean Temp (°C)',
             x = '') +
        
        ## Add themes
        theme(axis.title.x     = element_text(colour = 'black', size = 25),
              axis.text.x      = element_blank(),
              
              axis.title.y     = element_text(colour = 'black', size = 35),
              axis.text.y      = element_text(size = 25),
              
              panel.border     = element_rect(colour = 'black', fill = NA, size = 1.2),
              plot.title       = element_text(size   = 25, face = 'bold'),
              legend.text      = element_text(size   = 20),
              legend.title     = element_blank(),
              legend.key.size  = unit(1.5, 'cm'))
      
      ## Print the plot and close the device
      print(temp.box + ggtitle(paste0("Worldclim temp niches for ", spp)))
      dev.off()
    }
    
    rain_hist <- sprintf("%s%s_%s", range_path, spp, "rain_niche_histograms_1km_records.png")
    if(!file.exists(rain_hist)) {
      
      ## Check if file exists
      message('Writing global rain histograms for ', spp)
      png(rain_hist,
          16, 10, units = 'in', res = 500)
      
      ## Use the 'SOURCE' column to create a histogram for each source.
      rain.hist = ggplot(coord_spp, aes(x = Annual_precip, group = RANGE, fill = RANGE)) +
        
        # geom_histogram(position = "identity", alpha = 0.8, binwidth = 15,
        #                aes(y =..density..))  +
        geom_density(aes(x = Annual_precip, y = ..scaled.., fill = RANGE),
                     color = 'black', alpha = 0.8) +
        
        ## Change the colors
        scale_fill_manual(values = c('NATURAL' = 'coral2',
                                     'site'   = 'gainsboro'), na.value = "grey") +
        
        ggtitle(paste0("Worldclim rain niches for ", spp)) +
        
        ## Add themes
        theme(axis.title.x     = element_text(colour = "black", size = 35),
              axis.text.x      = element_text(size = 25),
              
              axis.title.y     = element_text(colour = "black", size = 35),
              axis.text.y      = element_text(size = 25),
              
              panel.background = element_blank(),
              panel.border     = element_rect(colour = "black", fill = NA, size = 3),
              plot.title       = element_text(size   = 40, face = "bold"),
              legend.text      = element_text(size   = 20),
              legend.title     = element_text(size   = 20),
              legend.key.size  = unit(1.5, "cm"))
      
      ## Print the plot and close the device
      print(rain.hist + ggtitle(paste0("Worldclim rain niches for ", spp)))
      dev.off()
    }
    
    rain_box <- sprintf("%s%s_%s", range_path, spp, "rain_niche_boxplots_1km_records.png")
    if(!file.exists(rain_box)) {
      
      ## Check if file exists
      message('Writing global rain boxplots for ', spp)
      png(sprintf("%s%s_%s", range_path, spp, "rain_niche_boxplots_1km_records.png"),
          10, 14, units = 'in', res = 500)
      
      ## Use the 'SOURCE' column to create a histogram for each source.
      rain.box = ggboxplot(coord_spp,
                           x    = 'RANGE',
                           y    = 'Annual_precip',
                           fill = 'RANGE',
                           palette = c('coral2', 'gainsboro'), size = 0.6) +
        
        ## Use the classic theme
        theme_classic() +
        labs(y = 'Annual Precipitation (mm)',
             x = '') +
        
        ## Add themes
        theme(axis.title.x     = element_text(colour = 'black', size = 25),
              axis.text.x      = element_blank(),
              
              axis.title.y     = element_text(colour = 'black', size = 35),
              axis.text.y      = element_text(size = 25),
              
              panel.border     = element_rect(colour = 'black', fill = NA, size = 1.2),
              plot.title       = element_text(size   = 25, face = 'bold'),
              legend.text      = element_text(size   = 20),
              legend.title     = element_blank(),
              legend.key.size  = unit(1.5, 'cm'))
      
      ## Print the plot and close the device
      print(rain.box + ggtitle(paste0("Worldclim rain niches for ", spp)))
      dev.off()
    }
  }
}





#' @title Prepare SDM table.
#' @description  'This function takes a data frame of all taxa records,
#' And prepares a table in the 'taxa with data' (swd) format for modelling uses the Maxent algorithm.
#' It assumes that the input df is that returned by the coord_clean_records function'
#' @param coord_df           SF object. Spatial DF of all taxa records returned by the coord_clean_records function
#' @param taxa_list          Character string - the taxa analysed
#' @param BG_points          Logical - Do we want to include a dataframe of background points? Otherwise, BG points taken from taxa not modelled
#' @param sdm_table_vars     Character string - The enviro vars to be included in taxa models
#' @param save_run           Character string - append a run name to the output (e.g. 'bat_taxa')
#' @param save_shp           Logical - Save a shapefile of the SDM table (T/F)?
#' @param read_background    Logical - Read in an additional dataframe of background points (T/F)?
#' @param background_points  Data.frame. DF of extra taxa records 
#' @param save_data          Logical - do you want to save the data frame?
#' @param site_split         Logical - do you want to add sites to the table?
#' @param spat_out_remove    Logical - do you want to save the data frame?
#' @param data_path          Character string - The file path used for saving the data frame
#' @param project_path       Path of taxa records, with spatial outlier T/F flag for each record
#' @param world_epsg         Character string - The global geographic CRS to use (decimal degrees)
#' @param country_epsg       Numeric - The national projected CRS system to use (metres)
#' @export prepare_sdm_table
prepare_sdm_table = function(coord_df,
                             taxa_list,
                             site_flag,
                             occ_flag,
                             site_split,
                             spat_out_remove,
                             sdm_table_vars,
                             save_run,
                             read_background,
                             background_points,
                             save_data,
                             data_path,
                             country_epsg,
                             world_epsg) {
  
  ## Just add clean_df to this step.
  coord_df <- filter(coord_df, coord_summary == TRUE)
  
  ## Create a table with all the variables needed for SDM analysis
  message('Preparing SDM table for ', length(unique(coord_df$searchTaxon)),
          ' taxa in the set ', "'", save_run, "'",
          'using ', unique(coord_df$SOURCE), ' data')
  
  ## Select only the columns needed. This also needs to use the variable names
  length(unique(coord_df$searchTaxon))
  
  COMBO.RASTER.ALL  <- coord_df %>%
    dplyr::select(one_of(sdm_table_vars))
  
  ## Split the table into ALA and site data 
  COMBO.RASTER.ALA  <- COMBO.RASTER.ALL %>% filter(SOURCE == occ_flag)
  COMBO.RASTER.SITE <- COMBO.RASTER.ALL %>% filter(SOURCE == site_flag) %>% dplyr::mutate(SPAT_OUT = TRUE)
  
  
  ## Create a spatial points object, and change to a projected system to calculate distance more accurately
  ## This is the mollweide projection used for the SDMs
  SDM.DATA.ALL <- COMBO.RASTER.ALA %>%
    st_transform(., st_crs(country_epsg)) %>% 
    dplyr::select(-lat, -lon)
  
  ## Convert lat/lon to eastings and northings for projected coordinate system
  SDM.COORDS     <- st_coordinates(SDM.DATA.ALL)
  SDM.DATA.ALL$X <- SDM.COORDS$X
  SDM.DATA.ALL$Y <- SDM.COORDS$Y
  
  ## Create a unique identifier for spatial cleaning.
  ## This is used for automated cleaning of the records, and also saving shapefiles
  ## But this will not be run for all taxa linearly.
  ## So, it probably needs to be a combination of taxa and number
  SDM.DATA.ALL$SPOUT.OBS <- 1:nrow(SDM.DATA.ALL)
  SDM.DATA.ALL$SPOUT.OBS <- paste0(SDM.DATA.ALL$SPOUT.OBS, "_SPOUT_", SDM.DATA.ALL$searchTaxon)
  SDM.DATA.ALL$SPOUT.OBS <- gsub(" ",     "_",  SDM.DATA.ALL$SPOUT.OBS, perl = TRUE)
  length(SDM.DATA.ALL$SPOUT.OBS);length(unique(SDM.DATA.ALL$SPOUT.OBS))
  
  if(spat_out_remove) {
    
    ## Create a tibble to supply to coordinate cleaner
    SDM.COORDS  <- SDM.DATA.ALL %>%
      
      st_transform(., st_crs(world_epsg)) %>%
      as.data.frame() %>%
      dplyr::select(searchTaxon, lon, lat, SPOUT.OBS, SOURCE) %>%
      dplyr::rename(species          = searchTaxon,
                    decimallongitude = lon,
                    decimallatitude  = lat) %>%
      timetk::tk_tbl()
    
    ## Check
    message(identical(SDM.COORDS$index, SDM.COORDS$SPOUT.OBS))
    length(unique(SDM.COORDS$species))
    
    ## Check how many records each species has
    COMBO.LUT <- SDM.COORDS %>%
      as.data.frame() %>%
      dplyr::select(species) %>%
      table() %>%
      as.data.frame()
    COMBO.LUT <- setDT(COMBO.LUT, keep.rownames = FALSE)[]
    names(COMBO.LUT) = c("species", "FREQUENCY")
    COMBO.LUT = COMBO.LUT[with(COMBO.LUT, rev(order(FREQUENCY))), ]
    
    ## Watch out here - this sorting could cause problems for the order of
    ## the data frame once it's stitched back together
    LUT.100K = as.character(subset(COMBO.LUT, FREQUENCY < 100000)$species)
    LUT.100K = trimws(LUT.100K [order(LUT.100K)])
    length(LUT.100K)
    
    ## Create a data frame of species name and spatial outlier
    SPAT.OUT <- LUT.100K  %>%
      
      ## pipe the list of species into lapply
      lapply(function(x) {
        
        ## Create the species df by subsetting by species
        ## x = LUT.100K[1]
        f <- subset(SDM.COORDS, species == x)
        
        ## Run the spatial outlier detection
        message("Running spatial outlier detection for ", nrow(f), " records for ", x)
        sp.flag <- cc_outl(f,
                           lon     = "decimallongitude",
                           lat     = "decimallatitude",
                           species = "species",
                           method  = "quantile",
                           mltpl   = 10,
                           value   = "flagged",
                           verbose = TRUE)
        
        ## Now add attached column for taxa, and the flag for each record
        d = cbind(searchTaxon = x,
                  SPAT_OUT = sp.flag, f)[c("searchTaxon", "SPAT_OUT", "SPOUT.OBS")]
        
        ## Remember to explicitly return the df at the end of loop, so we can bind
        return(d)
        
      }) %>%
      
      ## Finally, bind all the rows together
      bind_rows
    gc()
    
    ## How many taxa are flagged as spatial outliers?
    message(table(SPAT.OUT$SPAT_OUT, exclude = NULL), ' flagged as outliers')
    
    ## FILTER DATA TO REMOVE SPATIAL OUTLIERS
    ## Join data :: Best to use the 'OBS' column here
    message('Is the number records identical before joining?',
            identical(nrow(SDM.COORDS), nrow(SPAT.OUT)))
    
    message('Is the order of records identical after joining?',
            identical(SDM.COORDS$species, SPAT.OUT$searchTaxon))
    
    ## This explicit join is required. Check the taxa have been analyzed in exactly the same order
    SPAT.FLAG <- left_join(as.data.frame(SDM.DATA.ALL), SPAT.OUT,
                           by = c("SPOUT.OBS", "searchTaxon"), match = "first")
    
    message('Is the order or records identical after joining?',
            identical(SDM.DATA.ALL$searchTaxon, SPAT.FLAG$searchTaxon))
    
    ## Check the join is working
    message('Checking spatial flags for ', length(unique(SPAT.FLAG$searchTaxon)),
            ' taxa in the set ', "'", save_run, "'")
    
    message(table(SPAT.FLAG$SPAT_OUT, exclude = NULL))
    
    ## Just get the records that were not spatial outliers.
    SDM.SPAT.ALL <- subset(SPAT.FLAG, SPAT_OUT == TRUE) %>% dplyr::select(-SPOUT.OBS)
    unique(SDM.SPAT.ALL$SPAT_OUT)
    unique(SDM.SPAT.ALL$SOURCE)
    length(unique(SDM.SPAT.ALL$searchTaxon))
    
    ## What percentage of records are retained?
    message(round(nrow(SDM.SPAT.ALL)/nrow(SPAT.FLAG)*100, 2),
            " % records retained after spatial outlier detection")
    
  } else {
    message('Do not remove spatial outliers')
    SDM.DATA.ALL$SPAT_OUT <- TRUE
    SDM.SPAT.ALL          <- SDM.DATA.ALL %>% as_Spatial()
  }
  
  if(site_split) {
    
    ## Need to convert to SPDF
    message('Combine the Spatially cleaned data with the site data')
    SPAT.TRUE <- SpatialPointsDataFrame(coords      = SDM.SPAT.ALL[c("lon", "lat")],
                                        data        = SDM.SPAT.ALL,
                                        proj4string = CRS(world_epsg)) %>% 
      st_as_sf() %>%
      st_transform(., st_crs(country_epsg))
    
    SPAT.SITE <- COMBO.RASTER.SITE %>% 
      st_transform(., st_crs(country_epsg))
    
    SPAT.TRUE.COMBO <- SPAT.TRUE %>% rbind(., SPAT.SITE) %>% as_Spatial()
    message('Cleaned ', paste0(unique(SPAT.TRUE$SOURCE), sep = ' '), ' records')
    
  } else {
    message('Do not add site data')
    SPAT.TRUE.COMBO <- SDM.SPAT.ALL
  } 
  
  ## CREATE BACKGROUND POINTS AND VARIBALE NAMES
  ## Use one data frame for all taxa analysis,
  ## to save mucking around with background points
  if(read_background) {
    
    message('Read in background data for taxa analaysed')
    background = background_points[!background_points$searchTaxon %in% taxa_list, ]
    
    ## The BG points step needs to be ironed out.
    ## For some analysis, we need to do other taxa (e.g. animals)
    SDM.SPAT.OCC.BG <- rbind(SPAT.TRUE.COMBO, background) %>% as_Spatial()
    
  } else {
    message('Dont read in Background data, creating it in this run')
    SDM.SPAT.OCC.BG = SPAT.TRUE.COMBO
  }
  
  ## save data
  if(save_data) {
    
    ## Save .rds file of the occurrence and BG points for the next session
    saveRDS(SDM.SPAT.OCC.BG,   paste0(data_path, 'SDM_SPAT_OCC_BG_',  save_run, '.rds'))
    write_csv(SDM.SPAT.OCC.BG, paste0(data_path, 'SDM_SPAT_OCC_BG_',  save_run, '.csv'))
    
    # st_write(SDM.SPAT.OCC.BG,
    #          dsn    = paste0(data_path, 'SDM_SPAT_OCC_BG_',  save_run, '.gpkg'),
    #          layer  = paste0('SDM_SPAT_OCC_BG_',  save_run),
    #          quiet  = TRUE,
    #          append = FALSE)
    
    return(SDM.SPAT.OCC.BG)
    
  } else {
    message('Return the occurrence + Background data to the global environment')   ##
    return(SDM.SPAT.OCC.BG)
  }
  gc()
}





#' @title Split SDM table
#' @description  'This function takes a shapefile of all taxa records,
#' and splits in into shp files for each taxon
#' @param taxa_list     Character string - List of species    
#' @param data_path     Character string - The file path used for saving the data frame
#' @param sdm_  Path of taxa records, with spatial outlier T/F flag for each record
#' @export split_shp
split_shp <- function(taxa_list,
                      sdm_df,
                      data_path) {
  
  for(taxa in taxa_list) {
    
    taxa_shp <- paste0(data_path, taxa, '_SDM_points.shp')
    
    if(!file.exists(taxa_shp)) {
      
      message('Subsetting ', taxa, ' shapefile')
      taxa_occ <- subset(sdm_df, searchTaxon == taxa | family == taxa) %>% 
        spTransform(., crs(sp_epsg3577))
      
      writeOGR(obj    = taxa_occ,
               dsn    = data_path,
               layer  = paste0(taxa, '_SDM_points'), 
               driver = 'ESRI Shapefile', overwrite_layer = FALSE)
      
    } else {
      message(taxa, ' SDM .shp already exists')}
  }
}





#' Identify and repair invalid geometry in spatial polygons data frames
#' @description Using functions from sf, check the geometry of a set of polygons. If the geometry invalid, it attempts to buffer the polygons with \code{sf::st_buffer(dist = 0)}. If the geometry is corrupt or fine, it does nothing.
#' @param polygons Spatial polygons (either sf or spatial polygons data frame). The polygons to be checked. Note that the function will halt if the geometry is corrupt and not return a value.
#' @param verbose Logical. If \code{TRUE} then the function will produce informative messages as it executes its steps. Useful for debugging. Defaults to \code{FALSE}.
#' @param force Logical. If \code{TRUE} then both valid and invalid polygons will be buffered by 0. This shouldn't be necessary, but is a feature for the paranoid.
#' @return The spatial polygons data frame \code{polygons1}. This will be unchanged if the geometry was valid or repaired if it was invalid.
#' @export repair_geometry
#' @details It uses the aim.analysis package https://https://github.com/nstauffer/aim.analysis
repair_geometry <- function(polygons,
                            verbose = FALSE,
                            force = FALSE) {
  if(class(polygons) == "SpatialPolygonsDataFrame") {
    polygons_sf <- sf::st_as_sf(polygons)
    spdf <- TRUE
  } else if ("sf" %in% class(polygons)) {
    polygons_sf <- polygons
    spdf <- FALSE
  } else {
    stop("polygons must either be a spatial polygons data frame or an sf object")
  }
  
  validity_check <- sf::st_is_valid(polygons_sf)
  
  if (any(is.na(validity_check))) {
    stop("The geometry of the polygons is corrupt. Unable to repair.")
  }
  
  if (!all(validity_check)) {
    if (verbose) {
      message("Invalid geometry found. Attempting to repair.")
    }
    output <- sf::st_buffer(x = polygons_sf,
                            dist = 0)
  } else if (force) {
    if (verbose) {
      message("No invalid geometry found. Attempting to repair anyway.")
    }
    output <- sf::st_buffer(x = polygons_sf,
                            dist = 0)
  } else {
    if (verbose) {
      message("No invalid geometry found.")
    }
    output <- polygons_sf
  }
  
  if (spdf) {
    output <- methods::as(output, "Spatial")
  }
  
  return(output)
}






#########################################################################################################################
####################################################  TBC  ##############################################################
#########################################################################################################################
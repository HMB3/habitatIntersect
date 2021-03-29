######################################## READ SPECIES DATA FOR SDM MODELLING ############################################


## For this analysis, we need lots of background records for the species we are analyzing.
## So this means getting lots of insects records and plant records


## Delete small files
## find . -name "*.tif" -type 'f' -size -160k -delete


## 1). TARGET INSECT SPECIES LISTS =============================================================


## Insect species
target.insect.spp = read_excel('./data/Taxonomy/Habitat_fire_recovery_invertebrates.xlsx',
                               sheet = 'Insect species') %>%
  .$searchTaxon %>% as.character() %>% unique() %>% str_trim() %>% .[!is.na(.)]


## Insect genera
target.insect.genera = read_excel('./data/Taxonomy/Habitat_fire_recovery_invertebrates.xlsx',
                                  sheet = 'Insect species') %>%
  .$Genus %>% as.character() %>% unique() %>% str_trim() %>% .[!is.na(.)]





## 2). EXTRA SPECIES LISTS =====================================================================



## Common cols between SITES and ALA
site_cols <- c("genus", 
               "species", 
               "family", 
               "lat", 
               "lon", 
               "country", 
               "state",
               "locality",
               "institutionCode", 
               "basisOfRecord")


## Read in all the bugs for the projects


## QLD SITES ----
QLD.insects = read_excel('./data/Taxonomy/Invertebrates/HETEROPTERA AUSTRALIA 26 March 2020.xlsx',
                         sheet = 'QLD BUGS')


QLD.insect.sites.ALA <-  QLD.insects %>% 
  
  ## Now clean up the data so it can be combined with the ALA
  mutate(searchTaxon = paste(Genus, species,  sep = " ")) %>%
  rename(genus           = Genus,
         locality        = Locality,
         country         = Country,
         family          = Family,
         lat             = Lat,
         lon             = Lon,
         institutionCode = Inst_Code,
         recordedBy      = Collector,
         state           = State_Prov,
         basisOfRecord   = Coll_Method) %>% 
  
  ## Change this to the order of the clean columns   
  select(searchTaxon, one_of(site_cols))


## What are these data sets? Are they the field data from 1993?
names(QLD.insects)


QLD.insect.genera     <- QLD.insects %>% select(Genus)  %>% distinct() %>% .$Genus
QLD.insect.family     <- QLD.insects %>% select(Family) %>% distinct() %>% .$Family
QLD.insect.spp        <- QLD.insects %>% mutate(searchTaxon = paste(Genus, species,  sep = " ")) %>% 
  select(searchTaxon) %>% distinct() %>% .$searchTaxon


QLD.insect.plants.genus <- QLD.insects %>% select(Host_Genus) %>% distinct() %>% .$Host_Genus %>% .[!is.na(.)]
QLD.insect.plants       <- QLD.insects %>% mutate(searchTaxon = paste(Host_Genus, Host_species,  sep = " ")) %>% 
  select(searchTaxon) %>% distinct() %>% .$searchTaxon %>% 
  gsub(' NA', '', .,) %>% gsub(' sp.', '', .,) %>% gsub('NA', '', .,) %>% unique()



## NSW SITES ----
NSW.insects = read_excel('./data/Taxonomy/Invertebrates/HETEROPTERA AUSTRALIA 26 March 2020.xlsx',
                         sheet = 'NSW BUGS')


##
NSW.insect.sites.ALA <-  NSW.insects %>% 
  
  ## Now clean up the data so it can be combined with the ALA
  mutate(searchTaxon = paste(Genus, species,  sep = " ")) %>%
  rename(genus           = Genus,
         locality        = Locality,
         country         = Country,
         family          = Family,
         lat             = Lat,
         lon             = Lon,
         institutionCode = Inst_Code,
         recordedBy      = Collector,
         state           = State_Prov,
         basisOfRecord   = Coll_Method) %>% 
  
  ## Change this to the order of the clean columns   
  select(searchTaxon, one_of(site_cols))


## 
NSW.insect.genera <- NSW.insects %>% select(Genus)  %>% distinct() %>% .$Genus
NSW.insect.family <- NSW.insects %>% select(Family) %>% distinct() %>% .$Family
NSW.insect.spp    <- NSW.insects %>% mutate(searchTaxon = paste(Genus, species,  sep = " ")) %>% 
  select(searchTaxon) %>% distinct() %>% .$searchTaxon


NSW.insect.plants.genus <- NSW.insects %>% select(Host_Genus) %>% distinct() %>% .$Host_Genus %>% .[!is.na(.)]
NSW.insect.plants       <- NSW.insects %>% mutate(searchTaxon = paste(Host_Genus, Host_species,  sep = " ")) %>% 
  select(searchTaxon) %>% distinct() %>% .$searchTaxon %>% 
  gsub(' NA', '', .,) %>% gsub(' sp.', '', .,) %>% gsub('NA', '', .,) %>% unique()





## VIC SITES ----
VIC.insects = read_excel('./data/Taxonomy/Invertebrates/HETEROPTERA AUSTRALIA 26 March 2020.xlsx',
                         sheet = 'VIC BUGS')


##
VIC.insect.sites.ALA <-  VIC.insects %>% 
  
  ## Now clean up the data so it can be combined with the ALA
  mutate(searchTaxon = paste(Genus, species,  sep = " ")) %>%
  rename(genus           = Genus,
         locality        = Locality,
         country         = Country,
         family          = Family,
         lat             = Lat,
         lon             = Lon,
         institutionCode = Inst_Code,
         recordedBy      = Collector,
         state           = State_Prov,
         basisOfRecord   = Coll_Method) %>% 
  
  ## Change this to the order of the clean columns   
  select(searchTaxon, one_of(site_cols))


VIC.insect.genera <- VIC.insects %>% select(Genus)  %>% distinct() %>% .$Genus
VIC.insect.family <- VIC.insects %>% select(Family) %>% distinct() %>% .$Family
VIC.insect.spp    <- VIC.insects %>% mutate(searchTaxon = paste(Genus, species,  sep = " ")) %>% 
  select(searchTaxon) %>% distinct() %>% .$searchTaxon


VIC.insect.plants.genus <- VIC.insects %>% select(Host_Genus) %>% distinct() %>% .$Host_Genus %>% .[!is.na(.)]
VIC.insect.plants <- VIC.insects %>% mutate(searchTaxon = paste(Host_Genus, Host_species,  sep = " ")) %>% 
  select(searchTaxon) %>% distinct() %>% .$searchTaxon %>% 
  gsub(' NA', '', .,) %>% gsub(' sp.', '', .,) %>% gsub('NA', '', .,) %>% unique()





## TAS SITES ----
TAS.insects = read_excel('./data/Taxonomy/Invertebrates/HETEROPTERA AUSTRALIA 26 March 2020.xlsx',
                         sheet = 'TAS BUGS')


##
TAS.insect.sites.ALA <-  TAS.insects %>% 
  
  ## Now clean up the data so it can be combined with the ALA
  mutate(searchTaxon = paste(Genus, species,  sep = " ")) %>%
  rename(genus           = Genus,
         locality        = Locality,
         country         = Country,
         family          = Family,
         lat             = Lat,
         lon             = Lon,
         institutionCode = Inst_Code,
         recordedBy      = Collector,
         state           = State_Prov,
         basisOfRecord   = Coll_Method) %>% 
  
  ## Change this to the order of the clean columns   
  select(searchTaxon, one_of(site_cols))


## 
TAS.insect.genera  <- TAS.insects %>% select(Genus)  %>% distinct() %>% .$Genus
TAS.insect.family  <- TAS.insects %>% select(Family) %>% distinct() %>% .$Family
TAS.insect.spp     <- TAS.insects %>% mutate(searchTaxon = paste(Genus, species,  sep = " ")) %>% 
  select(searchTaxon) %>% distinct() %>% .$searchTaxon


TAS.insect.plants.genus <- TAS.insects %>% select(Host_Genus) %>% distinct() %>% .$Host_Genus %>% .[!is.na(.)]
TAS.insect.plants <- TAS.insects %>% mutate(searchTaxon = paste(Host_Genus, Host_species,  sep = " ")) %>% 
  select(searchTaxon) %>% distinct() %>% .$searchTaxon %>% 
  gsub(' NA', '', .,) %>% gsub(' sp.', '', .,) %>% gsub('NA', '', .,) %>% unique()





## SA SITES ----
SA.insects = read_excel('./data/Taxonomy/Invertebrates/HETEROPTERA AUSTRALIA 26 March 2020.xlsx',
                        sheet = 'SA BUGS') 


##
SA.insect.sites.ALA <-  SA.insects %>% 
  
  ## Now clean up the data so it can be combined with the ALA
  mutate(searchTaxon = paste(Genus, species,  sep = " ")) %>%
  rename(genus           = Genus,
         locality        = Locality,
         country         = Country,
         family          = Family,
         lat             = Lat,
         lon             = Lon,
         institutionCode = Inst_Code,
         recordedBy      = Collector,
         state           = State_Prov,
         basisOfRecord   = Coll_Method) %>% 
  
  ## Change this to the order of the clean columns   
  select(searchTaxon, one_of(site_cols))


##
SA.insect.genera  <- SA.insects %>% select(Genus)  %>% distinct() %>% .$Genus
SA.insect.family  <- SA.insects %>% select(Family) %>% distinct() %>% .$Family
SA.insect.spp     <- SA.insects %>% mutate(searchTaxon = paste(Genus, species,  sep = " ")) %>% 
  select(searchTaxon) %>% distinct() %>% .$searchTaxon


SA.insect.plants.genus <- SA.insects %>% select(Host_Genus) %>% distinct() %>% .$Host_Genus %>% .[!is.na(.)]
SA.insect.plants <- SA.insects %>% mutate(searchTaxon = paste(Host_Genus, Host_species,  sep = " ")) %>% 
  select(searchTaxon) %>% distinct() %>% .$searchTaxon %>% 
  gsub(' NA', '', .,) %>% gsub(' sp.', '', .,) %>% gsub('NA', '', .,) %>% unique()





## WA SITES ----
WA.insects = read_excel('./data/Taxonomy/Invertebrates/HETEROPTERA AUSTRALIA 26 March 2020.xlsx',
                        sheet = 'WA BUGS')


##
WA.insect.sites.ALA <-  WA.insects %>% 
  
  ## Now clean up the data so it can be combined with the ALA
  mutate(searchTaxon = paste(Genus, species,  sep = " ")) %>%
  rename(genus           = Genus,
         locality        = Locality,
         country         = Country,
         family          = Family,
         lat             = Lat,
         lon             = Lon,
         institutionCode = Inst_Code,
         recordedBy      = Collector,
         state           = State_Prov,
         basisOfRecord   = Coll_Method) %>% 
  
  ## Change this to the order of the clean columns   
  select(searchTaxon, one_of(site_cols))


##
WA.insect.genera <- WA.insects %>% select(Genus)  %>% distinct() %>% .$Genus
WA.insect.family <- WA.insects %>% select(Family) %>% distinct() %>% .$Family
WA.insect.spp    <- WA.insects %>% mutate(searchTaxon = paste(Genus, species,  sep = " ")) %>% 
  select(searchTaxon) %>% distinct() %>% .$searchTaxon


WA.insect.plants.genus <- WA.insects %>% select(Host_Genus) %>% distinct() %>% .$Host_Genus %>% .[!is.na(.)]
WA.insect.plants <- WA.insects %>% mutate(searchTaxon = paste(Host_Genus, Host_species,  sep = " ")) %>% 
  select(searchTaxon) %>% distinct() %>% .$searchTaxon %>% 
  gsub(' NA', '', .,) %>% gsub(' sp.', '', .,) %>% gsub('NA', '', .,) %>% unique()





## NT SITES ----
NT.insects = read_excel('./data/Taxonomy/Invertebrates/HETEROPTERA AUSTRALIA 26 March 2020.xlsx',
                        sheet = 'NT BUGS') 

##
NT.insect.sites.ALA <-  NT.insects %>% 
  
  ## Now clean up the data so it can be combined with the ALA
  mutate(searchTaxon = paste(Genus, species,  sep = " ")) %>%
  rename(genus           = Genus,
         locality        = Locality,
         country         = Country,
         family          = Family,
         lat             = Lat,
         lon             = Lon,
         institutionCode = Inst_Code,
         recordedBy      = Collector,
         state           = State_Prov,
         basisOfRecord   = Coll_Method) %>% 
  
  ## Change this to the order of the clean columns   
  select(searchTaxon, one_of(site_cols))


## 
NT.insect.genera <- NT.insects %>% select(Genus) %>% distinct() %>% .$Genus
NT.insect.family <- NT.insects %>% select(Family) %>% distinct() %>% .$Family
NT.insect.spp    <- NT.insects %>% mutate(searchTaxon = paste(Genus, species,  sep = " ")) %>% 
  select(searchTaxon) %>% distinct() %>% .$searchTaxon


## 
NT.insect.plants.genus <- NT.insects %>% select(Host_Genus) %>% distinct() %>% .$Host_Genus %>% .[!is.na(.)]
NT.insect.plants <- NT.insects %>% mutate(searchTaxon = paste(Host_Genus, Host_species,  sep = " ")) %>% 
  select(searchTaxon) %>% distinct() %>% .$searchTaxon %>% 
  gsub(' NA', '', .,) %>% gsub(' sp.', '', .,) %>% gsub('NA', '', .,) %>% unique()





## ACT SITES ----
ACT.insects = read_excel('./data/Taxonomy/Invertebrates/HETEROPTERA AUSTRALIA 26 March 2020.xlsx',
                         sheet = 'ACT BUGS') 


##
ACT.insect.sites.ALA <-  ACT.insects %>% 
  
  ## Now clean up the data so it can be combined with the ALA
  mutate(searchTaxon = paste(Genus, species,  sep = " ")) %>%
  rename(genus           = Genus,
         locality        = Locality,
         country         = Country,
         family          = Family,
         lat             = Lat,
         lon             = Lon,
         institutionCode = Inst_Code,
         recordedBy      = Collector,
         state           = State_Prov,
         basisOfRecord   = Coll_Method) %>% 
  
  ## Change this to the order of the clean columns   
  select(searchTaxon, one_of(site_cols))


## 
ACT.insect.genera <- ACT.insects %>% select(Genus)  %>% distinct() %>% .$Genus
ACT.insect.family <- ACT.insects %>% select(Family) %>% distinct() %>% .$Family
ACT.insect.spp    <- ACT.insects %>% mutate(searchTaxon = paste(Genus, species,  sep = " ")) %>% 
  select(searchTaxon) %>% distinct() %>% .$searchTaxon


## 
ACT.insect.plants.genus <- ACT.insects %>% select(Host_Genus) %>% distinct() %>% .$Host_Genus %>% .[!is.na(.)]
ACT.insect.plants <- ACT.insects %>% mutate(searchTaxon = paste(Host_Genus, Host_species,  sep = " ")) %>% 
  select(searchTaxon) %>% distinct() %>% .$searchTaxon %>% 
  gsub(' NA', '', .,) %>% gsub(' sp.', '', .,) %>% gsub('NA', '', .,) %>% unique()





## 3). TOTAL TAXA LISTS =============================================================================


## 
all.insect.spp <- c(QLD.insect.spp, NSW.insect.spp, VIC.insect.spp, 
                    TAS.insect.spp, TAS.insect.spp, NT.insect.spp, 
                    ACT.insect.spp, SA.insect.spp) %>% unique() %>%
  
  str_trim() %>% textclean::strip() %>% capitalize()                  %>% 
  gsub('Sp ', '', .,)               %>% gsub('Spbbcqld Msp', '', .,)  %>% 
  gsub('Spbbqkn Msp', '', .,)       %>% gsub(".* Msp","", .,)         %>% 
  gsub('Spnsp ', '', .,)            %>% gsub(' sp', '', .,)           %>%
  gsub(' msp', '', .,)              %>%
  gsub(' C', '', .,) %>% str_trim() %>% unique()


## 
east.insect.spp <- c(QLD.insect.spp, NSW.insect.spp, VIC.insect.spp, ACT.insect.spp) %>% unique() %>%
  
  str_trim() %>% textclean::strip() %>% capitalize()                  %>% 
  gsub('Sp ', '', .,)               %>% gsub('Spbbcqld Msp', '', .,)  %>% 
  gsub('Spbbqkn Msp', '', .,)       %>% gsub(".* Msp","", .,)         %>% 
  gsub('Spnsp ', '', .,)            %>% gsub(' sp', '', .,)           %>%
  gsub(' msp', '', .,)              %>%
  gsub(' C', '', .,) %>% str_trim() %>% unique()


## Combine all the insect genera into one list
all.insect.genera <- c(QLD.insect.genera, NSW.insect.genera, VIC.insect.genera, 
                       TAS.insect.genera, TAS.insect.genera, NT.insect.genera, 
                       ACT.insect.genera, SA.insect.genera) %>% unique() %>%
  
  str_trim() %>% textclean::strip() %>% capitalize()                  %>% 
  gsub('Sp ', '', .,)               %>% gsub('Spbbcqld Msp', '', .,)  %>% 
  gsub('Spbbqkn Msp', '', .,)       %>% gsub(".* Msp","", .,)         %>% 
  gsub('Spnsp ', '', .,)            %>% gsub(' sp', '', .,)           %>%
  gsub(' msp', '', .,)              %>%
  gsub(' C', '', .,) %>% str_trim() %>% unique()


## Combine all the insect genera into one list
all.insect.families    <- c(QLD.insect.family, NSW.insect.family, VIC.insect.family, 
                            TAS.insect.family, TAS.insect.family, NT.insect.family, 
                            ACT.insect.family, SA.insect.family) %>% unique() %>% str_trim() %>% .[!is.na(.)] #%>% toupper()


all.insect.plant.spp    <- c(QLD.insect.plants, NSW.insect.plants, VIC.insect.plants, 
                             TAS.insect.plants, TAS.insect.plants, NT.insect.plants, 
                             ACT.insect.plants, SA.insect.plants) %>% unique() %>% str_trim() %>% .[!is.na(.)] %>% .[. != ""]


all.insect.plant.genera <- c(QLD.insect.plants, NSW.insect.plants, VIC.insect.plants, 
                             TAS.insect.plants, TAS.insect.plants, NT.insect.plants, 
                             ACT.insect.plants, SA.insect.plants) %>% unique() %>% str_trim() %>% .[!is.na(.)]


## Save taxa out for manual cleaning
# insect_species_data <- as_tibble(all.insect.spp)     %>% rename(searchTaxon = value)
# insect_genus_data <- as_tibble(all.insect.genera)    %>% rename(searchTaxon = value)
# plant_species_data  <- as_tibble(all.insect.plant.spp) %>% rename(searchTaxon = value)
# write_csv(insect_species_data, './data/Taxonomy/Invertebrates/insect_species_data.csv')
# write_csv(plant_species_data,  './data/Taxonomy/Invertebrates/insect_plant_data.csv')





## 4). TOTAL SITE DATA =============================================================================


## Combine all the site data into one table
all_insect_site_df_species <- bind_rows(QLD.insect.sites.ALA, 
                                        NSW.insect.sites.ALA,
                                        VIC.insect.sites.ALA,
                                        SA.insect.sites.ALA,
                                        WA.insect.sites.ALA,
                                        NT.insect.sites.ALA,
                                        ACT.insect.sites.ALA) %>%
  
  ## Remove the NA coordinates, and add a 'SOURCE' column
  completeFun(., c('lat', 'lon')) %>% 
  filter(lon < 180 & lat > -90)   %>% 
  mutate(SOURCE = 'SITE') 


## Genera
all_insect_site_df_genera <- all_insect_site_df_species  %>%
  select(-searchTaxon) %>% 
  rename(searchTaxon = genus) %>% select(searchTaxon, everything())

all_insect_site_df_families <- all_insect_site_df_species %>%
  select(-searchTaxon) %>% 
  rename(searchTaxon = family) %>% select(searchTaxon, everything())


## Create target species lists
target.insect.families <- all_insect_site_df_species  %>% 
  .[.$searchTaxon %in% target.insect.spp, ] %>% .$family %>% unique() %>% str_trim() %>% .[!is.na(.)]


## How big is this df?
nrow(all_insect_site_df_species)
length(unique(all_insect_site_df_species$searchTaxon))
length(unique(all_insect_site_df_species$genus))
length(unique(all_insect_site_df_species$family))





######################################## ---- TBC ---- ############################################
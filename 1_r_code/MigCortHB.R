# Analyses for "Does migration constrain glucocorticoid phenotypes?
# Testing corticosterone levels during breeding in migratory versus resident birds"

# Authors: Jennifer J. Uehling, Emma Regnier, and Maren N. Vitousek

# Helpful resource: https://ourcodingclub.github.io/tutorials/mcmcglmm/#mcmcglmm

# Prior to this code:
# --> Download HormoneBase from https://hormonebase.org/database/.
# --> Delete all records not from birds.
# --> Remove all records without corticosterone measurements.
# --> Research each bird species (and subspecies and population, where relevant)
#     to determine whether it is Migratory or Resident. For populations where
#     this is unclear, classify as "Unknown."

# Load packages ----------------------------------------------------------------

library(ape)
library(MCMCglmm)
library(geiger)
library(phytools)
library(dplyr)
library(phangorn)

# Load and filter data ---------------------------------------------------------

  ## Load HormoneBase/migration data -------------------------------------------

  data <- read.csv("0_data/HormoneBase_v1_Migration_Data_Jenny_2023.csv")

  table(data$Breeding_Cycle)

  # There are two entries that have a space after breeding -- fix that
  data$Breeding_Cycle[data$Breeding_Cycle == "Breeding "] <- "Breeding"
  table(data$Breeding_Cycle)

  # Add "species" column genus_species
  data$genus_species <- paste(data$Genus, data$Species, sep = "_")

  ## Pre-model data filtering and preparation -- pull out data to use in study ----

  # Figure out how many species have both breeding and non-breeding records
  table(data$Breeding_Cycle, data$Migratory_Status)
  non_breeding <- data[data$Breeding_Cycle == "Nonbreeding" ,]
  non_breeding <- non_breeding[non_breeding$Migratory_Status == "Migratory" | non_breeding$Migratory_Status == "Resident" ,]
  non_breeding <- non_breeding[non_breeding$Life_Stage != "Migration" ,]
  non_breeding <- non_breeding[non_breeding$Life_Stage != "Migration" ,]
  table(non_breeding$Migratory_Status)
  length(unique(non_breeding$genus_species))
  # Not many have both breeding and non-breeding records
  
  # Only use breeding data
  data <- data[data$Breeding_Cycle == "Breeding" ,]
  
  # Count number of species in dataset
  length(unique(data$genus_species))

  # Figure out how many measurements in each migratory status category
  table(data$Migratory_Status)

  # Take out data for which we do not have migratory status
  data_full <- data[data$Migratory_Status == "Migratory" | data$Migratory_Status == "Resident" ,]
  length(unique(data_full$genus_species))
  
  ## Population ID -- create this variable and combine populations as needed --------
  
  # Create "Pop_ID" column that contains latitude and longitude together
  data_full$Pop_ID <- paste(data_full$Latitude, data_full$Longitude, data_full$genus_species, sep = "_")
  str(data_full$Pop_ID) # make sure this is classified as a character
  
  # Examine the Pop_ID entries to ensure that there are no populations that are incorrectly identified
  
  # --> The "Population ID" value is not included on the publicly available HormoneBase database.
  # --> I created a "Pop_ID" variable by combining latitude, longitude, and species into a single character string.
  # --> However, upon visual inspection, I realized that some lat/long values were very close to each other.
  # --> I went through the dataset and checked references and lat/long values for species that had two populations
  #     reported with lat/long values that were very close. I also referenced the HormoneBase master spreadsheet
  #     ("Copy of HormoneBase.Master.Apr26_2018.xlsx", included in the folder 4_other_files).
  # --> When it was clear populations that were classified as two separate populations were actually the same
  #     population, I reclassified them so they all had the same Pop_ID value.
  
  # Create list of unique population IDs in this dataset
  Pop_ID_check <- select(data_full, c('Latitude', 'Longitude', 'Pop_ID'))
  Pop_ID_check_list <- unique(Pop_ID_check)
  Pop_ID_check_list$Latitude <- as.numeric(Pop_ID_check_list$Latitude)
  Pop_ID_check_list$Longitude <- as.numeric(Pop_ID_check_list$Longitude)
  
  write.csv(Pop_ID_check_list, "3_other_output/Pop_ID_list.csv", row.names=FALSE)
  
  # I took this .csv, made a copy, and renamed it "Pop_ID_list_for_checking." It
  # is saved as an Excel file in "3_other_output" in the R Project.
  
  # I manually checked the lat/long values that were close to each other and made
  # changes to Pop_ID classifications below as needed. I include my notes from
  # populations that I decided were separate, and populations that I decided were
  # the same.
  
  # Changes needed:
  
  # -77.217_166.467_Pygoscelis_adeliae (refs 14, 15, 76)
  # -77.219_166.475_Pygoscelis_adeliae (ref 275)
  # Same population
  data_full$Pop_ID[data_full$Pop_ID == "-77.219_166.475_Pygoscelis_adeliae"] <- "-77.217_166.467_Pygoscelis_adeliae"
  
  # -54.005301_-38.037807_Eudyptes_chrysolophus (ref 17, Fairy Point, Bird Island, South Georgia)
  # -54.003733_-38.030777_Eudyptes_chrysolophus (ref 422, Bird Island, South Georgia)
  # Same population -- both papers give same lat/long coordinates and mention Fairy Point. Master spreadsheet classifies them as same population.
  data_full$Pop_ID[data_full$Pop_ID == "-54.003733_-38.030777_Eudyptes_chrysolophus"] <- "-54.005301_-38.037807_Eudyptes_chrysolophus"
  
  # -49.39_69.35_Thalassarche_melanophrys (ref 39)
  # -49.348_70.217_Thalassarche_melanophrys (ref 50)
  # These are listed as different populations in the master spreadsheet, but the refs list them as the same.
  data_full$Pop_ID[data_full$Pop_ID == "-49.348_70.217_Thalassarche_melanophrys"] <- "-49.39_69.35_Thalassarche_melanophrys"
  
  # -44.039_-65.1988_Spheniscus_magellanicus
  # -42.083_-63.82_Spheniscus_magellanicus
  # Clearly separate populations.
  
  # -17.217_145.417_Malurus_melanocephalus (ref 8)
  # -17.38_145.39_Malurus_melanocephalus (ref 407)
  # In the HormoneBase database published online, the lat/long values for ref 8 are (-17.217, 145.417). These are incorrect.
  # In the master spreadsheet, the lat/long values for ref 8 are (-17.383,	145.417). # These values match what is in the paper.
  # In both the master spreadsheet and the HormoneBase database published online, the lat/long values for 407 are incorrect. They should be the same as ref 8.
  # Refs 8 and 407 are the same population, according to the lat/long values provided in the papers.
  # Change both of them so they have the correct lat/long in the Pop_ID field to minimize confusion.
  data_full$Pop_ID[data_full$Pop_ID == "-17.217_145.417_Malurus_melanocephalus"] <- "-17.383_145.417_Malurus_melanocephalus"
  data_full$Pop_ID[data_full$Pop_ID == "-17.38_145.39_Malurus_melanocephalus"] <- "-17.383_145.417_Malurus_melanocephalus"
  
  # -0.3667_-78.15_Zonotrichia_capensis (ref 278)
  # -0.35_-78.1_Zonotrichia_capensis (ref 311)
  # Same population
  data_full$Pop_ID[data_full$Pop_ID == "-0.35_-78.1_Zonotrichia_capensis"] <- "-0.3667_-78.15_Zonotrichia_capensis"
  
  # 27.16_-81.35_Aphelocoma_coerulescens
  # 27.166_-81.35_Aphelocoma_coerulescens
  # 27.167_-81.35_Aphelocoma_coerulescens
  # 27.3667_-81.2184_Aphelocoma_coerulescens
  # All scrub jay populations are the same -- refs 298, 299, 415, 315, 110
  data_full$Pop_ID[data_full$Pop_ID == "27.166_-81.35_Aphelocoma_coerulescens"] <- "27.16_-81.35_Aphelocoma_coerulescens"
  data_full$Pop_ID[data_full$Pop_ID == "27.167_-81.35_Aphelocoma_coerulescens"] <- "27.16_-81.35_Aphelocoma_coerulescens"
  data_full$Pop_ID[data_full$Pop_ID == "27.3667_-81.2184_Aphelocoma_coerulescens"] <- "27.16_-81.35_Aphelocoma_coerulescens"
  
  # 31.32_-89.31_Cardinalis_cardinalis
  # 31.35_-89.42_Cardinalis_cardinalis
  # Both ref 406, and they are the same population.
  data_full$Pop_ID[data_full$Pop_ID == "31.32_-89.31_Cardinalis_cardinalis"] <- "31.35_-89.42_Cardinalis_cardinalis"
  
  # 31.49_-110.55_Peucaea_carpalis (ref 185)
  # 31.760009_-110.867286_Peucaea_carpalis (ref 18)
  # Same population -- Santa Rita Experimental Range
  data_full$Pop_ID[data_full$Pop_ID == "31.49_-110.55_Peucaea_carpalis"] <- "31.760009_-110.867286_Peucaea_carpalis"

  # 33.944_-112.385_Melozone_aberti (ref 23, Hell's Canyon)
  # 33.45_-112.067_Melozone_aberti (ref 23 and 78, downtown Phoenix)
  # 33.317_-112.666_Melozone_aberti (ref 78, Robbins Butte)
  # 33.274_-112.281_Melozone_aberti (ref 85, Sierra Estrella)
  # 33.427_-111.943_Melozone_aberti (ref 85, includes Phoenix, Scottsdale, and Tempe)
  # 33.26_-111.95_Melozone_aberti (ref 320, many sites)
  # Even though the lat/long coordinates are similar, these should all be considered different populations
  
  # 33.274_-112.281_Toxostoma_curvirostre (ref 85, Sierra Estrella)
  # 33.427_-111.943_Toxostoma_curvirostre (ref 85, urban areas in Phoenix, Scottsdale, Tempe)
  # 33.43_-111.94_Toxostoma_curvirostre (ref 84, east-central Phoenix and Tempe)
  # 33.45_-112.067_Toxostoma_curvirostre (ref 22, 23, Phoenix)
  # 33.45176474_-111.8808921_Toxostoma_curvirostre (ref 648, Wingfield)
  # 33.944_-112.385_Toxostoma_curvirostre (ref 23, Hell's Canyon)
  # 34.684_-111.326_Toxostoma_curvirostre (ref 84, Four Peaks)
  # Even though the lat/long coordinates are similar, these should all be considered different populations
  
  # 33.429_-111.943_Passer_domesticus (ref 70, Tempe)
  # 33.45_-112.067_Passer_domesticus (ref 23, Phoenix)
  # 33.944_-112.385_Passer_domesticus (ref 23, Hell's Canyon)
  # Even though the lat/long coordinates are similar, these should all be considered different populations
  
  # 33.45_-112.067_Mimus_polyglottos (ref 23, Phoenix)
  # 33.944_-112.385_Mimus_polyglottos (ref 23, Hell's Canyon)
  # Even though the lat/long coordinates are similar, these should be considered different populations
  
  # 33.96575_-117.28552_Melospiza_melodia (ref 430, Box Springs Mountain Reserve)
  # 33.97423_-117.32733_Melospiza_melodia (ref UC Riverside, 430)
  # Even though the lat/long coordinates are similar, these should be considered different populations (urban and refuge)
  
  # 36.469_-121.91_Zonotrichia_leucophrys (ref 51, Garrapata State Park)
  # 36.6_-121.9_Zonotrichia_leucophrys (ref 51, Monterey)
  # Different populations -- Monterey is consider urban, and Garrapata State Park rural. 
  
  # 37.3_-119.26_Zonotrichia_leucophrys (ref 183, Tioga Pass, Mono County, CA)
  # 37.5_-119.15_Zonotrichia_leucophrys (ref 69, Tioga Pass, Mono County, CA)
  # 37.783_-122.417_Zonotrichia_leucophrys (ref 51, San Francisco)
  # 38.067_-122.883_Zonotrichia_leucophrys (ref 51, Point Reyes)
  # 38.314766_-123.069569_Zonotrichia_leucophrys (ref 60, Bodega Bay)
  # 38.328_-119.636_Zonotrichia_leucophrys (ref 71, Sonora Pass)
  # Ref 183 and 69 are same population (Tioga Pass, Mono County, CA)
  # Two locations for ref 51 are indeed different
  # Ref 60 is also different from all other locations -- although it does have sampling sites at Garrapata State Park and Point Reyes, it is labelled as distinct from ref 51's Garrapata and Point Reyes locations in master spreadsheet
  # Ref 71 is also different from all other locations
  data_full$Pop_ID[data_full$Pop_ID == "37.3_-119.26_Zonotrichia_leucophrys"] <- "37.5_-119.15_Zonotrichia_leucophrys"
  
  # 37.057_-80.629_Melospiza_melodia (ref 410, Claytor Lake State Park)
  # 37.138_-80.55_Melospiza_melodia (ref 410, Radford University Campus)
  # Rural and urban sites -- separate populations
  
  # 37.133333_-91.05_Passerina_cyanea (ref 314, Current River Conservation Area, 37'8' N, 91'3'W)
  # 37.317_-91_Passerina_cyanea (ref 413, Current River Conservation Area, 37'19' N, 91'00'W)
  # Must be different populations -- same authors, but they give different lat/long coordinates within the Current River Conservation Area

  # 37.367_-80.53_Junco_hyemalis (ref 444)
  # 37.375409_-80.523412_Junco_hyemalis (refs 265, 295)
  # ref 444: area surrounding the University of Virginia’s Mountain Lake Biological Station, USA (37,22 N, 80,32 W)
  # ref 265: near Pembroke, Virginia, at the University of Virginia’s Mountain Lake Biological Station
  # ref 295: Mountain Lake Biological Station in Pembroke, Virginia (37°22′N, 80°32′W)
  # These are all the same population
  data_full$Pop_ID[data_full$Pop_ID == "37.367_-80.53_Junco_hyemalis"] <- "37.375409_-80.523412_Junco_hyemalis"
  
  # 40.467_-74.65_Passer_domesticus (ref 101, Belle Mead, NJ)
  # 42_-73.75_Passer_domesticus (ref 89 & 90, Dutchess County, NY)
  # 42.2_-71_Passer_domesticus (ref 294, Medford, MA)
  # Different populations
  
  # 42.13989_-73.305822_Tachycineta_bicolor (ref 24, Threemile Pond, Great Barrington, MA)
  # 42.4_-73.3_Tachycineta_bicolor (ref 24, Lenox Road, Lenox, MA)
  # 42.44_-73.26_Tachycineta_bicolor (ref 24, Southwest Branch, Pittsfield, MA)
  # 42.65_-71.11_Tachycineta_bicolor (ref 25, Eastern MA)
  # Different populations -- sampling performed at three different sites in ref 24
  # Ref 25 refers to very broad area (eastern MA), classified as separate population
  
  # 43.333333_142.5_Cettia_diphone (ref 318)
  # 43.33617_142.38933_Cettia_diphone (ref 648)
  # Master spreadsheet lists these as the same population
  data_full$Pop_ID[data_full$Pop_ID == "43.33617_142.38933_Cettia_diphone"] <- "43.333333_142.5_Cettia_diphone"
  
  # 46.147_-0.425_Passer_domesticus (refs 46, 56)
  # 46.15_-0.4_Passer_domesticus (ref 33)
  # Master spreadsheet lists these as the same population, and papers all list Chize, France as location of study
  data_full$Pop_ID[data_full$Pop_ID == "46.147_-0.425_Passer_domesticus"] <- "46.15_-0.4_Passer_domesticus"
  
  # 46.843_-122.294_Zonotrichia_leucophrys (ref 71, Charles Lathrop Pack Forest Station, Puget Sound, WA)
  # 46.855541_-124.102679_Zonotrichia_leucophrys (ref 60, Twin Harbors State Park, WA)
  # 46.902685_-124.127893_Zonotrichia_leucophrys (ref 51, Westhaven State Park, Westport, WA)
  # 47.61_-122.333_Zonotrichia_leucophrys (refs 11 and 51, Seattle, WA)
  # Ref 60 -- Twin Harbors State Park also includes Cedar Rock Preserve on Shaw Island, and Pack Forest Experimental Station;
  # master spreadsheet lists this as separate site from others.
  # Westhaven and Seattle represented urban and rural sites for ref 51
  # Ref 11 just lists "Seattle" as site
  # 47.744_-121.087_Zonotrichia_leucophrys (ref 60, Steven's Pass Ski Area),
  # 48.731_-120.672_Zonotrichia_leucophrys (ref 60, Hart's Pass),
  # 49.28_-123.12_Zonotrichia_leucophrys (ref 13, Vancouver)
  # Ref 60 -- Steven's Pass Ski Area also includes Snoqualmie Pass Ski Area, Cle Elum Ranger District of Wenatchee National Forest;
  # listed as distinct population in master spreadsheet
  # All different populations
  
  # 47.74_8.96_Parus_major (ref 57, 289)
  # 47.766_9_Parus_major (ref 436)
  # Master spreadsheet lists these as the same population
  data_full$Pop_ID[data_full$Pop_ID == "47.766_9_Parus_major"] <- "47.74_8.96_Parus_major"
  
  # 48.429_-123.366_Melospiza_melodia (refs 52, 282, 75, Victoria)
  # 48.552727_-123.04669_Melospiza_melodia (ref 290, Western WA state)
  # 48.667_-122.367_Melospiza_melodia (ref 83, Skagit Valley Wildlife Recreation Area and Discovery Park in Seattle)
  # 49.12_-123.01_Melospiza_melodia (ref 283, Vancouver)
  # 49.2_-122.67_Melospiza_melodia (ref 280, 281, lower BC)
  # Ref 283 and 280 populations both in Vancouver, BC with same lat/long (49°12N,123°01W)
  # Ref 281 is just listed as "lower mainland of British Columbia, Canada," which is very non-specific. However,
  # master spreadsheet file lists it as same location as ref 280.
  # List 49.12_-123.01_Melospiza_melodia and 49.2_-122.67_Melospiza_melodia as the same.
  data_full$Pop_ID[data_full$Pop_ID == "49.12_-123.01_Melospiza_melodia"] <- "49.2_-122.67_Melospiza_melodia"
  # All other populations are distinct.
  
  # 58.617_-94.217_Setophaga_petechia (ref 317, Churchill River, Churchill, Manitoba)
  # 58.733_-93.817_Setophaga_petechia (ref 414, Churchill Northern Studies Centre, Manitoba)
  # Even though these populations are close, they are listed as separate in the master spreadsheet, and there
  # is no indication that the sampling took place in the same location. Classify as
  # separate populations.
  
  # 68.628_-149.595_Calcarius_lapponicus (ref 64, Toolik Lake Research Station, Brooks Range, Alaska)
  # 70_-150_Calcarius_lapponicus (ref 63, North Slope, Brooks Range, Alaska)
  # Ref 64: "This research was conducted during the summers of 1989 and 1990 at the Toolik Lake Research Station, University of Alaska, Fairbanks,
  # on the north slope of the Brooks Range, Alaska (68 N. lat)"
  # Ref 63: just says North Slope of the Brooks Range
  # There is not enough information to confidently classify this as the same population,
  # and the master spreadsheet classifies them as separate populations.
  # Classify as separate populations.
  
  # 78.9_12.217_Rissa_tridactyla (refs 3, 4, 74, 86, 267, 268, 284, 305, 417, 438)
  # 78.933_12.217_Rissa_tridactyla (ref 416) -- all the same population (as noted in ref 416)
  # However, ref 416 also notes that sampling was performed at Bear Island.
  # Master spreadsheet classifies these as the same population.
  data_full$Pop_ID[data_full$Pop_ID == "78.933_12.217_Rissa_tridactyla"] <- "78.9_12.217_Rissa_tridactyla"
  
  # Somateria mollissima around Svalbard Archipelago: difficulty in identifying distinct populations
  # 78.9_12.217_Somateria_mollissima (ref 53)
  # 78.917_12.117_Somateria_mollissima (ref 77)
  # 78.917_20.117_Somateria_mollissima (refs 68, 257, 258, 259, 271)
  # 78.933_12.217_Somateria_mollissima (ref 416)
  
  # Ref 53: "Body mass and clutch size may modulate prolactin and corticosterone levels in eiders"
    # Kongsfjorden, on the western coast of the Svalbard Archipelago (78° 55' N), Norway, in a colony nesting on the mainland and on the small island of Prins Heinrich
    # PRINS HEINRICH and western coast, no longitude given
 
  # Ref 77: "Corticosterone alone does not trigger a short term behavioural shift in incubating female common eiders Somateria mollissima, but does modify long term reproductive success"
    # Kongsfjorden, on the west coast of the Svalbard Archipelago (78°55 N, 12°07′ E), in a colony on Prins Heinrich Island
    # PRINS HEINRICH and western coast, 12°07′ E
  
  # Ref 68: "Triiodothyronine suppresses humoral immunity but not T-cell-mediated immune response in incubating female eiders (Somateria mollissima)"
    # Storholmen Island, Kongsfjorden, Svalbard Archipelago (78°55′N, 20°07′E)
    # STORHOLMEN ISLAND, 20°07′E
  
  # Ref 257: "Corticosterone selectively decreases humoral immunity in female eiders during incubation"
    # Prins Heinrich Island, Kongsfjorden, Svalbard Archipelago (78°55′N, 20°07′E)
    # PRINS HEINRICH, 20°07′E
  
  # Ref 258: "Fasting-induced changes of immunological and stress indicators in breeding female eiders"
    # Kongsfjorden on the western coast of the Svalbard Archipelago (78°55 N, 20°07 E) in a colony nesting on Storholmen Island
    # STORHOLMEN, 20°07 E
  
  # Ref 259: "Innate immunity, assessed by plasma NO measurements, is not suppressed during the incubation fast in eiders"
    # Storholmen Island, Kongsfjorden, Svalbard Archipelago (78°55′N, 20°07′E)
    # STORHOLEM, 20°07′E
  
  # Ref 271: "Phytohemagglutinin response and immunoglobulin index decrease during incubation fasting in female common eiders"
    # Kongsfjorden on the western coast of the Svalbard Archipelago (78°55′N, 20°07′E) in a colony on Storholmen Island
    # STORHOLEM, 20°07′E
  
  # Ref 416: "Polychlorinated biphenyl exposure and corticosterone levels in seven polar seabird species"
    # Bear Island (74°22' N, 19°05' E) and Kongsfjorden (78°54' N, 12°13' E)
    # Kongsfjorden, 12°13' E
  
  # Master spreadsheet: 53 and 416 classified as same population
  # Master spreadsheet: 68, 257, 271 classified as same population
  # Missing from master spreadsheet: 77, 258, 259
  
  # Classify together: STORHOLMEN: 68, 257 (even though it says Prins Heinrich Island, which is further west, it gives longitude points that match other refs), 258, 259, 271 --> already classified together
  # Classify together: PRINS HEINRICH: 53, 77, 416 (even though 416 also includes Bear Island, master spreadsheet grouped 416 with 53)
  
  data_full$Pop_ID[data_full$Pop_ID == "78.917_12.117_Somateria_mollissima"] <- "78.9_12.217_Somateria_mollissima"
  data_full$Pop_ID[data_full$Pop_ID == "78.933_12.217_Somateria_mollissima"] <- "78.9_12.217_Somateria_mollissima"
  
  # 78.9_12.217_Somateria_mollissima (ref 53)
  # 78.917_12.117_Somateria_mollissima (ref 77)
  # 78.917_20.117_Somateria_mollissima (refs 68, 257, 258, 259, 271)
  # 78.933_12.217_Somateria_mollissima (ref 416)
  
# Summarize dataset ------------------------------------------------------------

length(unique(data_full$genus_species))
table(data_full$Migratory_Status)

# Report relationship between latitude and migratory status --------------------

  ## Classify variables correctly ----------------------------------------------

  data_full$Latitude_abs <- as.numeric(data_full$Latitude)
  data_full$Latitude_abs <- abs(data_full$Latitude_abs) # Take absolute value of latitude
  data_full_for_lat <- data_full[!is.na(data_full$Latitude_abs) ,]
  data_full_for_lat <- subset(data_full_for_lat, select = c(Common_name, Latitude, Latitude_abs,
                                                   Migratory_Status, Pop_ID, genus_species))
  data_full_for_lat_unique <- unique(data_full_for_lat)
  
  ## Set priors for latitude model ---------------------------------------------
  
  # Priors for residuals are specified with "R"
  # Priors for random effects are specified with "G"
  
  # Less informative priors
  prior_less <- list(G=list(G1=list(V=1,nu=0.002)), # One random effect (genus_species), so G1 specified
                     R=list(V=1,nu=0.002))
  
  # More informative priors
  prior_more <- list(G=list(G1=list(V=1,nu=1)), # One random effect (genus_species), so G1 specified
                     R=list(V=1,nu=1))

  ## Create tree for latitude model --------------------------------------------

    ### Get list of unique species for tree ------------------------------------

    # Get list of unique species
    Mig_name <- unique(data_full_for_lat_unique$genus_species)
    Mig_name <- data.frame(Mig_name)

    # Make the row names the species names
    rownames(Mig_name) <- Mig_name[,1]

    ### Read in tree file and prune for this study -----------------------------

    # Read in tree file
    alltree <- read.tree("0_data/finalTree2018.tre")	

    # Take a look at tree
    plot(alltree)

    # Identify the elements of the tree to maintain
    Mig_bird_overlap <- name.check(alltree, Mig_name)

    # Get rid of taxa we don't need
    Mig_bird_tree <- drop.tip(alltree, Mig_bird_overlap$tree_not_data)
    plot(Mig_bird_tree)

    # Check to make sure the tree only has the taxa we need
    name.check(Mig_bird_tree, Mig_name)

    # Write tree in NEXUS file format
    write.nexus(Mig_bird_tree, file = "3_other_output/Mig_bird_tree_lat.nex")

    # Now read tree in this format
    tree <- read.nexus("3_other_output/Mig_bird_tree_lat.nex")
    plot(tree)

    # Check to see if tree is ultrametric
    is.ultrametric(tree)

    # Create inverse tree
    inv.tree <- inverseA(tree, nodes = "ALL", scale = TRUE)

  ## Latitude model -- less informative priors ---------------------------------

  modelMig_Lat <- MCMCglmm(Latitude_abs ~ 1 + Migratory_Status, 
                             random = ~genus_species, family="gaussian", ginverse = list(genus_species = inv.tree$Ainv), 
                             prior = prior_less, data = data_full_for_lat_unique, nitt = 2000000, burnin = 50000, thin = 200)

  autocorr(modelMig_Lat$VCV)

  # Check convergence for fixed effects
  plot(modelMig_Lat$Sol)

  # Check convergence for random effects
  plot(modelMig_Lat$VCV)

  summary(modelMig_Lat)

  # Calculate posterior probability of phylogenetic signal (Pagel's lambda) 
  # 'units' refers to residual variance, which is built into code already
  lambda <- modelMig_Lat$VCV[,'genus_species']/(modelMig_Lat$VCV[,'genus_species']+modelMig_Lat$VCV[,'units'])

  # Calculate the posterior mean (mean of the posterior distribution), 
  # Posterior mode (most likely value regarding the posterior distribution) and the 95% credible interval of lambda
  mean(lambda)
  posterior.mode(lambda)
  HPDinterval(lambda)

  ## Latitude model -- more informative priors ----------------------------

  modelMig_Lat <- MCMCglmm(Latitude_abs ~ 1 + Migratory_Status, 
                           random = ~genus_species, family="gaussian", ginverse = list(genus_species = inv.tree$Ainv), 
                           prior = prior_more, data = data_full_for_lat_unique, nitt = 2000000, burnin = 50000, thin = 200)
  
  autocorr(modelMig_Lat$VCV)
  
  # Check convergence for fixed effects
  plot(modelMig_Lat$Sol)
  
  # Check convergence for random effects
  plot(modelMig_Lat$VCV)
  
  summary(modelMig_Lat)
  
  # Calculate posterior probability of phylogenetic signal (Pagel's lambda) 
  # 'units' refers to residual variance, which is built into code already
  lambda <- modelMig_Lat$VCV[,'genus_species']/(modelMig_Lat$VCV[,'genus_species']+modelMig_Lat$VCV[,'units'])
  
  # Calculate the posterior mean (mean of the posterior distribution), 
  # Posterior mode (most likely value regarding the posterior distribution) and the 95% credible interval of lambda
  mean(lambda)
  posterior.mode(lambda)
  HPDinterval(lambda)

# Set priors for baseline and stress CORT models -------------------------------
  
# Priors for residuals are specified with "R"
# Priors for random effects are specified with "G"
  
# Less informative priors
prior_less <- list(G=list(G1=list(V=1,nu=0.002), # Three random effects, so G1, G2, and G3 specified
                            G2=list(V=1,nu=0.002),
                            G3=list(V=1,nu=0.002)),
                     R=list(V=1,nu=0.002))
  
# More informative priors
prior_more <- list(G=list(G1=list(V=1,nu=1), # Three random effects, so G1, G2, and G3 specified
                            G2=list(V=1,nu=1),
                            G3=list(V=1,nu=1)),
                     R=list(V=1,nu=1))

# Make dataset with combined male and female data -- base cort -----------------

# For our analyses, we need each row to represent a single population measure of
# CORT, with sex measurements divided, so each sex gets its own row for each 
# population. The HormoneBase dataset has the individual measurements for sex in
# males and females in the same row for each population. So, we have to reformat
# that dataset so that each row is a population average for either males or
# females.

base_cort <- subset(data_full, select = c(Common_name, genus_species, Pop_ID, Migratory_Status,
                                          Lab_ID, F_BC_Mean, M_BC_Mean))

base_cort_male_female <- data.frame()

for (i in 1:length(base_cort$Common_name)){
  
  entry <- base_cort[i,]
  
  if (entry$F_BC_Mean != "null"){
    ttt <- entry
    ttt$Sex <- "Female"
    ttt <- subset(ttt, select = c(Common_name, genus_species, Pop_ID, Migratory_Status,
                                  Lab_ID, F_BC_Mean, Sex))
    names(ttt)[names(ttt) == "F_BC_Mean"] <- "BC_Mean"
    base_cort_male_female <- rbind(base_cort_male_female, ttt)
    remove(ttt)
  }
  
  
  if (entry$M_BC_Mean != "null"){
    ttt <- entry
    ttt$Sex <- "Male"
    ttt <- subset(ttt, select = c(Common_name, genus_species, Pop_ID, Migratory_Status,
                                  Lab_ID, M_BC_Mean, Sex))
    names(ttt)[names(ttt) == "M_BC_Mean"] <- "BC_Mean"
    base_cort_male_female <- rbind(base_cort_male_female, ttt)
    remove(ttt)
  }

}

# Baseline CORT analysis -------------------------------------------------------
  
  ## Classify variables correctly ----------------------------------------------

  # Classify CORT as number
  base_cort_male_female$BC_Mean <- as.numeric(base_cort_male_female$BC_Mean)
  
  # Check structure of other variables
  str(base_cort_male_female$Migratory_Status)
  str(base_cort_male_female$Sex)
  str(base_cort_male_female$genus_species)
  str(base_cort_male_female$Lab_ID)
  str(base_cort_male_female$Pop_ID)
  
  # Determine characteristics of dataset
  length(unique(base_cort_male_female$genus_species))
  length(unique(base_cort_male_female$Pop_ID))
  ttt <- unique(subset(base_cort_male_female, select = c(Pop_ID, Migratory_Status)))
  table(ttt$Migratory_Status)
  length(base_cort_male_female$BC_Mean)
  table(base_cort_male_female$Migratory_Status)
  table(base_cort_male_female$Migratory_Status, base_cort_male_female$Sex) # Look at division between sexes
  
  ## Create tree for baseline CORT --------------------------------------

    ### Get list of unique species for tree --------------------------------------
    
    # Get list of unique species
    Mig_name <- unique(base_cort_male_female$genus_species)
    Mig_name <- data.frame(Mig_name)
    
    # Make the row names the species names
    rownames(Mig_name) <- Mig_name[,1]
    
    ### Read in tree file and prune for this study -----------------------------

    # Read in tree file
    alltree <- read.tree("0_data/finalTree2018.tre")	
  
    # Take a look at tree
    plot(alltree)
  
    # Identify the elements of the tree to maintain
    Mig_bird_overlap <- name.check(alltree, Mig_name)
  
    # Get rid of taxa we don't need
    Mig_bird_tree <- drop.tip(alltree, Mig_bird_overlap$tree_not_data)
    plot(Mig_bird_tree)
  
    # Check to make sure the tree only has the taxa we need
    name.check(Mig_bird_tree, Mig_name)

    # Write tree in NEXUS file format
    write.nexus(Mig_bird_tree, file = "3_other_output/Mig_bird_tree_baseline.nex")
  
    # Now read tree in this format
    tree <- read.nexus("3_other_output/Mig_bird_tree_baseline.nex")
    plot(tree)
    
    # Check to see if tree is ultrametric
    is.ultrametric(tree)
    
    # Create inverse tree
    inv.tree <- inverseA(tree, nodes = "ALL", scale = TRUE)

  ## Baseline CORT model -- less informative priors ----------------------------

  modelMig_BC_Mean <- MCMCglmm(log(BC_Mean) ~ 1 + Migratory_Status + Sex, 
                            random = ~genus_species + Lab_ID + Pop_ID, family="gaussian", ginverse = list(genus_species = inv.tree$Ainv), 
                            prior = prior_less, data = base_cort_male_female, nitt = 2000000, burnin = 50000, thin = 200)

  autocorr(modelMig_BC_Mean$VCV)

  # Check convergence for fixed effects
  plot(modelMig_BC_Mean$Sol)

  # Check convergence for random effects
  plot(modelMig_BC_Mean$VCV)

  summary(modelMig_BC_Mean)

  # Calculate posterior probability of phylogenetic signal (Pagel's lambda) 
  # 'units' refers to residual variance, which is built into code already
  lambda <- modelMig_BC_Mean$VCV[,'genus_species']/(modelMig_BC_Mean$VCV[,'genus_species']+modelMig_BC_Mean$VCV[,'units'])
  
  # Calculate the posterior mean (mean of the posterior distribution), 
  # Posterior mode (most likely value regarding the posterior distribution) and the 95% credible interval of lambda
  mean(lambda)
  posterior.mode(lambda)
  HPDinterval(lambda)
  
  ## Baseline CORT model -- more informative priors ----------------------------
  
  modelMig_BC_Mean <- MCMCglmm(log(BC_Mean) ~ 1 + Migratory_Status + Sex, 
                               random = ~genus_species + Lab_ID + Pop_ID, family="gaussian", ginverse = list(genus_species = inv.tree$Ainv), 
                               prior = prior_more, data = base_cort_male_female, nitt = 2000000, burnin = 50000, thin = 200)
  
  autocorr(modelMig_BC_Mean$VCV)
  
  # Check convergence for fixed effects
  plot(modelMig_BC_Mean$Sol)
  
  # Check convergence for random effects
  plot(modelMig_BC_Mean$VCV)
  
  summary(modelMig_BC_Mean)
  
  # Calculate posterior probability of phylogenetic signal (Pagel's lambda) 
  # 'units' refers to residual variance, which is built into code already
  lambda <- modelMig_BC_Mean$VCV[,'genus_species']/(modelMig_BC_Mean$VCV[,'genus_species']+modelMig_BC_Mean$VCV[,'units'])
  
  # Calculate the posterior mean (mean of the posterior distribution), 
  # Posterior mode (most likely value regarding the posterior distribution) and the 95% credible interval of lambda
  mean(lambda)
  posterior.mode(lambda)
  HPDinterval(lambda)

# Make dataset with combined male and female data -- stress cort ---------------
  
  stress_cort <- subset(data_full, select = c(Common_name, genus_species, Pop_ID, Migratory_Status,
                                            Lab_ID, F_SC_Mean, M_SC_Mean))
  
  stress_cort_male_female <- data.frame()
  
  for (i in 1:length(stress_cort$Common_name)){
    
    entry <- stress_cort[i,]
    
    if (entry$F_SC_Mean != "null"){
      ttt <- entry
      ttt$Sex <- "Female"
      ttt <- subset(ttt, select = c(Common_name, genus_species, Pop_ID, Migratory_Status,
                                    Lab_ID, F_SC_Mean, Sex))
      names(ttt)[names(ttt) == "F_SC_Mean"] <- "SC_Mean"
      stress_cort_male_female <- rbind(stress_cort_male_female, ttt)
      remove(ttt)
    }
    
    if (entry$M_SC_Mean != "null"){
      ttt <- entry
      ttt$Sex <- "Male"
      ttt <- subset(ttt, select = c(Common_name, genus_species, Pop_ID, Migratory_Status,
                                    Lab_ID, M_SC_Mean, Sex))
      names(ttt)[names(ttt) == "M_SC_Mean"] <- "SC_Mean"
      stress_cort_male_female <- rbind(stress_cort_male_female, ttt)
      remove(ttt)
    }
    
  }
  
# Stress CORT analysis ------------------------------------------------
  
  ## Classify variables correctly ----------------------------------------------
  
  # Classify CORT as number
  stress_cort_male_female$SC_Mean <- as.numeric(stress_cort_male_female$SC_Mean)
  
  # Check structure of other variables
  str(stress_cort_male_female$Migratory_Status)
  str(stress_cort_male_female$Sex)
  str(stress_cort_male_female$genus_species)
  str(stress_cort_male_female$Lab_ID)
  str(stress_cort_male_female$Pop_ID)
  
  # Determine characteristics of dataset
  length(unique(stress_cort_male_female$genus_species))
  length(unique(stress_cort_male_female$Pop_ID))
  ttt <- unique(subset(stress_cort_male_female, select = c(Pop_ID, Migratory_Status)))
  table(ttt$Migratory_Status)
  length(stress_cort_male_female$SC_Mean)
  table(stress_cort_male_female$Migratory_Status)
  table(stress_cort_male_female$Migratory_Status, stress_cort_male_female$Sex)
  
  ## Create tree for stress CORT -----------------------------------------------
  
  ### Get list of unique species for tree --------------------------------------
  
  # Get list of unique species
  Mig_name <- unique(stress_cort_male_female$genus_species)
  Mig_name <- data.frame(Mig_name)
  
  # Make the row names the species names
  rownames(Mig_name) <- Mig_name[,1]
  
  ### Read in tree file and prune for this study -----------------------------
  
  # Read in tree file
  alltree <- read.tree("0_data/finalTree2018.tre")	
  
  # Take a look at tree
  plot(alltree)
  
  # Identify the elements of the tree to maintain
  Mig_bird_overlap <- name.check(alltree, Mig_name)
  
  # Get rid of taxa we don't need
  Mig_bird_tree <- drop.tip(alltree, Mig_bird_overlap$tree_not_data)
  plot(Mig_bird_tree)
  
  # Check to make sure the tree only has the taxa we need
  name.check(Mig_bird_tree, Mig_name)
  
  # Write tree in NEXUS file format
  write.nexus(Mig_bird_tree, file = "3_other_output/Mig_bird_tree_stress.nex")
  
  # Now read tree in this format
  tree <- read.nexus("3_other_output/Mig_bird_tree_stress.nex")
  plot(tree)
  
  # Check to see if tree is ultrametric
  is.ultrametric(tree)
  
  # Create inverse tree
  inv.tree <- inverseA(tree, nodes = "ALL", scale = TRUE)

  ## Stress CORT model -- less informative priors ----------------------------
  
  modelMig_SC_Mean <- MCMCglmm(log(SC_Mean) ~ 1 + Migratory_Status + Sex, 
                               random = ~genus_species + Lab_ID + Pop_ID, family="gaussian", ginverse = list(genus_species = inv.tree$Ainv), 
                               prior = prior_less, data = stress_cort_male_female, nitt = 2000000, burnin = 50000, thin = 200)
  
  autocorr(modelMig_SC_Mean$VCV)
  
  # Check convergence for fixed effects
  plot(modelMig_SC_Mean$Sol)
  
  # Check convergence for random effects
  plot(modelMig_SC_Mean$VCV)
  
  summary(modelMig_SC_Mean)
  
  # Calculate posterior probability of phylogenetic signal (Pagel's lambda) 
  # 'units' refers to residual variance, which is built into code already
  lambda <- modelMig_SC_Mean$VCV[,'genus_species']/(modelMig_SC_Mean$VCV[,'genus_species']+modelMig_SC_Mean$VCV[,'units'])
  
  # Calculate the posterior mean (mean of the posterior distribution), 
  # Posterior mode (most likely value regarding the posterior distribution) and the 95% credible interval of lambda
  mean(lambda)
  posterior.mode(lambda)
  HPDinterval(lambda)
  
  ## Stress CORT model -- more informative priors ----------------------------
  
  modelMig_SC_Mean <- MCMCglmm(log(SC_Mean) ~ 1 + Migratory_Status + Sex, 
                               random = ~genus_species + Lab_ID + Pop_ID, family="gaussian", ginverse = list(genus_species = inv.tree$Ainv), 
                               prior = prior_more, data = stress_cort_male_female, nitt = 2000000, burnin = 50000, thin = 200)
  
  autocorr(modelMig_SC_Mean$VCV)
  
  # Check convergence for fixed effects
  plot(modelMig_SC_Mean$Sol)
  
  # Check convergence for random effects
  plot(modelMig_SC_Mean$VCV)
  
  summary(modelMig_SC_Mean)
  
  # Calculate posterior probability of phylogenetic signal (Pagel's lambda) 
  # 'units' refers to residual variance, which is built into code already
  lambda <- modelMig_SC_Mean$VCV[,'genus_species']/(modelMig_SC_Mean$VCV[,'genus_species']+modelMig_SC_Mean$VCV[,'units'])
  
  # Calculate the posterior mean (mean of the posterior distribution), 
  # Posterior mode (most likely value regarding the posterior distribution) and the 95% credible interval of lambda
  mean(lambda)
  posterior.mode(lambda)
  HPDinterval(lambda)

# Make a map with the locations of sampling ------------------------------------

# Load packages
library(ggplot2)
library(tidyverse)

# Classify latitude and longitude as numbers
data_full$Latitude <- as.numeric(data_full$Latitude)
data_full$Longitude <- as.numeric(data_full$Longitude)

# Create data for world coordinates
world_coordinates <- map_data("world") # geom_map() takes world coordinates as input to plot world map

# Plot world coordinates and sampling locations
map <- ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(map_id = region)) +
  geom_point(data = data_full, aes(x = Longitude, y = Latitude, fill = Migratory_Status), pch = 21, size = 3) +
  scale_fill_manual(values = c("Migratory" = "darkgray", "Resident" = "white")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16)) +
  xlab("Longitude") + 
  ylab("Latitude") +
  theme(legend.title = element_text(size=16),
        legend.text = element_text(size=12)) +
  guides(fill = guide_legend(title = "Migratory Status")) 

map

ggsave("2_figures/Sampling_locations.pdf",
       width = 11, 
       height = 5.25)

ggsave("2_figures/Sampling_locations.tiff",
       width = 11, 
       height = 5.25)

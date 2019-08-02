# July 29, 2019
# This script calculates all indicators from Tech Report for ESS and WSS

#Example data folder is stored in R:\Science\Population Ecology Division\Shared\!PED_Staff\DempseyD\indicators_data
path <- file.path("R:/Science/Population Ecology Division/Shared/!PED_Staff/DempseyD/indicators_data")


library(indicators)

# Import compiled data
yrs= c(1970:2015)

# fishery-independent data
RV <- read.csv(paste(path, "/RV/NotLengthBased/esswss_adjbiomass.csv", sep = ""), head = TRUE, sep = ",")
RV_length <- read.csv(paste(path, "/RV/LengthBased/esswss_adjbiomass_Length.csv", sep = ""), head = TRUE, sep = ",")
RV_length_TL <- read.csv(paste(path, "/RV/LengthBased/esswss_adjbiomass_Length_TL.csv", sep = ""), head = TRUE, sep = ",")

# commercial landings data
land_all <- read.csv(paste(path, "/Commercial/esswss_landings.csv", sep = ""), head = TRUE, sep = ",")
land_dat <- land_all[land_all$YEAR >= 1970 & land_all$YEAR <= 2015, ]

# Extra data
TL.table.simple <- read.csv(paste(path, "/Extra Info/TL_simple.csv", sep = ""), header = TRUE, sep = ",")
TL.table.length <- read.csv(paste(path, "/Extra Info/TL_length_esswss.csv", sep = ""), header = TRUE, sep = ",")

maxL.table <- read.csv(paste(path, "/Extra Info//MaxLength_esswss.csv", sep = ""), header = TRUE, sep = ",")
maxage.table <- read.csv(paste(path, "/Extra Info/MaxAge_esswss.csv", sep =""), header = TRUE, sep = ",")

IVI.table <- read.csv(paste(path, "/Extra Info/IVI_esswss.csv", sep = ""),header = TRUE, sep = ",")
prop.land.table <- read.csv(paste(path, "/Extra Info/Proportion_Landings_esswss.csv", sep = ""), header = TRUE, sep = ",")

Length_Weight <- read.csv(paste(path, "/Extra Info/Length_Weight_esswss.csv", sep = ""),header = TRUE, sep = ",") # read in length-at-weight data for ESS and WSS
id <- sapply(Length_Weight, is.factor)                                    # identify which columns are factors (ID)
Length_Weight[id] <- lapply(Length_Weight[id], as.character)              # convert factors to text  

pred.spp <- c(10, 11, 12, 13, 14, 15, 16, 19, 25, 27, 30, 31, 32, 33, 40, 41,   # vector of predatory species
              42, 43, 44, 49, 50, 51, 52, 59, 71, 72, 73, 111,  112, 114, 119, 
              122, 141, 142, 143, 149, 150, 156, 172, 190, 192, 200, 201, 202, 
              203, 204, 220, 221, 230, 231, 233, 238, 241, 246, 256, 300, 301, 303,
              304, 307, 320, 400, 410, 411, 412, 414, 500, 501, 604, 619, 620, 621, 
              623, 626, 640, 641, 642, 647, 712, 741, 747, 880, 2526, 4511, 4514)



# Diversity Indicators ----------------------------------------------------

# Species richness
S = speciesrichness(RV, group = "ALL", metric = "BIOMASS", yrs)

# Shannon
H = shannon(RV, "ALL", "ABUNDANCE", yrs)

# Margalef
marg = margalef(RV, "ALL", "ABUNDANCE", yrs)

ground_marg = margalef(RV, "GROUNDFISH", "ABUNDANCE", yrs)

# Pielou
pie = pielouSpeciesEvenness(RV, "ALL", "ABUNDANCE", yrs)

# Hill1
H1 = hillN1(RV, "ALL", "ABUNDANCE", yrs)

# Hill2
H2 = hillN2(RV, "ALL", "ABUNDANCE", yrs)

# Heips
Heips = heips(RV, "ALL", "ABUNDANCE", yrs)

# Kempton
Q = kemptonQ(RV, TL.table = TL.table.simple, metric = "ABUNDANCE", group = "ALL",
             years = yrs,  percentiles = c(.25, 0.75), minTL = 3)


# Structure and Functioning Indicators ------------------------------------

# Large fish indicator
LFI = indicators::largeFishIndicator(RV_length, metric = "BIOMASS", years = yrs)

# Large species indicator
LSI = largeSpeciesIndicator(RV, lmax.table = maxL.table, lmax=85,  metric = "BIOMASS", years = yrs)

# Proportion of predatory species
propPred = predatoryFish(RV, pred.spp, metric = "BIOMASS", years = yrs)

# Mean trophic level of community
TL = meanTrophicLevelCommunity(RV, TL.table = TL.table.simple, 
                               metric = "BIOMASS", length.based = FALSE, years = yrs)

# # Mean trophic level of community: LENGTH
TL_length = meanTrophicLevelCommunity(RV_length_TL, TL.table = NULL, 
                                      metric = "BIOMASS", length.based= TRUE, years = yrs)

# Mean length of community
ML_bio = meanLengthCommunity(RV_length, metric = "BIOMASS", years = yrs)

ML_abund = meanLengthCommunity(RV_length, metric = "ABUNDANCE", years = yrs)

# Community Condition
FultonK = communityFultonK(RV_length, LenWt.table = Length_Weight,  
                           years = yrs, group = "FINFISH")

K_Lbenth = communityFultonK(RV_length, LenWt.table = Length_Weight,
                            years = yrs, group = "LBENTHIVORE")

K_Mbenth = communityFultonK(RV_length, LenWt.table = Length_Weight,  
                            years = yrs, group = "MBENTHIVORE")

K_pisc = communityFultonK(RV_length, LenWt.table = Length_Weight, 
                          years = yrs, group = "PISCIVORE")

K_plank = communityFultonK(RV_length, LenWt.table = Length_Weight, 
                           years = yrs, group = "PLANKTIVORE")

K_zoo = communityFultonK(RV_length, LenWt.table = Length_Weight, 
                         years = yrs, group = "ZOOPISCIVORE")

# inv2dem
inv_dem = biomassratio(RV[RV$YEAR>=1999,], group1 = "INVERTEBRATES", group2 = "GROUNDFISH", 
                       metric = "BIOMASS", years = yrs)

# pel2dem
pel_dem = biomassratio(RV, group1 = "PELAGIC", group2 = "GROUNDFISH",
                       metric = "BIOMASS", years = yrs)

# Biomass of trophic guilds
bio = resourcePotential(RV, metric = "BIOMASS", group = "ALL", years = yrs)

bio_clup = resourcePotential(RV, metric = "BIOMASS", group = "CLUPEIDS", years = yrs)

bio_fin = resourcePotential(RV, metric = "BIOMASS", group = "FINFISH", years = yrs)

bio_flat = resourcePotential(RV, metric = "BIOMASS", group = "FLATFISH", years = yrs)

bio_for = resourcePotential(RV, metric = "BIOMASS", group = "FORAGE", years = yrs)

bio_gad = resourcePotential(RV, metric = "BIOMASS", group = "GADOIDS", years = yrs)

bio_ground = resourcePotential(RV, metric = "BIOMASS", group = "GROUNDFISH", years = yrs)

bio_pel = resourcePotential(RV, metric = "BIOMASS", group = "PELAGIC", years = yrs)

bio_invert = resourcePotential(RV[RV$YEAR>=1999,], metric = "BIOMASS", group = "INVERTEBRATES", years = yrs)

bio_Lbenth = resourcePotential(RV, metric = "BIOMASS", group = "LBENTHIVORE", years = yrs)

bio_Mbenth = resourcePotential(RV, metric = "BIOMASS", group = "MBENTHIVORE", years = yrs)

bio_pisc = resourcePotential(RV, metric = "BIOMASS", group = "PISCIVORE", years = yrs)

bio_plank = resourcePotential(RV, metric = "BIOMASS", group = "PLANKTIVORE", years = yrs)

bio_zoo = resourcePotential(RV, metric = "BIOMASS", group = "ZOOPISCIVORE", years = yrs)


# Stability and Resistance Indicators -------------------------------------

# Max LifeSpan
MMA = meanMaxAge(RV, age.table = maxage.table, "BIOMASS", years = yrs)

# Mean max length
MML_bio = meanMaxL(RV_length, maxL.table, metric = "BIOMASS", years = yrs)

MML_abund = meanMaxL(RV_length, maxL.table, metric = "ABUNDANCE", years = yrs)

# IVI Landings
IVI = IVILandings(land_dat, IVI.table = IVI.table, propland.table = prop.land.table, years = yrs)

# Biomass per TL
bio_TL = biomassPerTL(RV, TL.table = TL.table.simple, metric = "BIOMASS", TL.grouping = 1, years = yrs)

# inverse CV biomass
invCV_bio = invCVBiomass(RV, window = 5, years = yrs)

# Resource Potential Indicators -------------------------------------------

# Fishing in Balance
FIB = FishingInBalance(land_all, TE = 0.1,  base.start = 1968, base.end = 1970, years = yrs, 
                       TL.table = TL.table.length, propland.table = prop.land.table, cutoff = 0)

# Biomass 
bio = resourcePotential(RV, metric = "BIOMASS", group = "ALL", years = yrs)

# Biomass of fished groups
bio_clup = resourcePotential(RV, metric = "BIOMASS", group = "CLUPEIDS", years = yrs)

bio_fin = resourcePotential(RV, metric = "BIOMASS", group = "FINFISH", years = yrs)

bio_flat = resourcePotential(RV, metric = "BIOMASS", group = "FLATFISH", years = yrs)

bio_for = resourcePotential(RV, metric = "BIOMASS", group = "FORAGE", years = yrs)

bio_gad = resourcePotential(RV, metric = "BIOMASS", group = "GADOIDS", years = yrs)

bio_ground = resourcePotential(RV, metric = "BIOMASS", group = "GROUNDFISH", years = yrs)

bio_invert = resourcePotential(RV[RV$YEAR >= 1999,], metric = "BIOMASS", group = "INVERTEBRATES", years = c(1999:2015))

# Biomass of skates
bio_skate = resourcePotential(RV, metric = "BIOMASS", group = "SKATES", years = yrs)


# Fishing Pressure Indicators ---------------------------------------------

# Mean Trophic Level Landings 
MTLL = MeanTLLandings(land_dat, TL.table = TL.table.length, propland.table = prop.land.table,
                      cutoff = 0, years = yrs)

# Marine Trophic Index Landings 
MTI = MeanTLLandings(land_dat, TL.table = TL.table.length, propland.table = prop.land.table,
                     cutoff = 3.25, years = yrs)

# Diversity of target species
SR.L = speciesrichness(land_dat, metric = "CATCH", group = "ALL", years = yrs)

# landings by group
land_all = LandByGroup(land_dat, group = "ALL", years = yrs)

land_fin = LandByGroup(land_dat, group = "FINFISH", years = yrs)

land_clup = LandByGroup(land_dat, group = "CLUPEIDS", years = yrs)

land_ground = LandByGroup(land_dat, group = "GROUNDFISH", years = yrs)

land_flat = LandByGroup(land_dat, group = "FLATFISH", years = yrs)

land_gad = LandByGroup(land_dat, group = "GADOIDS", years = yrs)

land_for = LandByGroup(land_dat, group = "FORAGE", years = yrs)

land_invert = LandByGroup(land_dat, group = "INVERTEBRATES", years = yrs)

land_pel = LandByGroup(land_dat, group = "LARGE_PELAGIC", years = yrs)

# fishing pressure by group
FP_all = FishingPressure(X = RV, land = land_dat, group = "ALL", years = yrs)

FP_clup = FishingPressure(X = RV, land = land_dat, group = "CLUPEIDS", years = yrs)

FP_fin = FishingPressure(X = RV, land = land_dat, group = "FINFISH", years = yrs)

FP_flat = FishingPressure(X = RV, land = land_dat, group = "FLATFISH", years = yrs)

FP_for = FishingPressure(X = RV, land = land_dat, group = "FORAGE", years = yrs)

FP_gad = FishingPressure(X = RV, land = land_dat, group = "GADOIDS", years = yrs)

FP_ground = FishingPressure(X = RV, land = land_dat, group = "GROUNDFISH", years = yrs)

FP_invert = FishingPressure(X = RV[RV$YEAR >= 1999,], land = land_dat, group = "INVERTEBRATES", years = yrs)

FP_skate = FishingPressure(X = RV, land = land_dat, group = "SKATES", years = yrs)

# inverse fishing pressure by group
invFP_all = InverseFishingPressure(X = RV, land = land_dat, group = "ALL", years = yrs)

invFP_clup = InverseFishingPressure(X = RV, land = land_dat, group = "CLUPEIDS", years = yrs)

invFP_fin = InverseFishingPressure(X = RV, land = land_dat, group = "FINFISH", years = yrs)

invFP_flat = InverseFishingPressure(X = RV, land = land_dat, group = "FLATFISH", years = yrs)

invFP_for = InverseFishingPressure(X = RV, land = land_dat, group = "FORAGE", years = yrs)

invFP_gad = InverseFishingPressure(X = RV, land = land_dat, group = "GADOIDS", years = yrs)

invFP_ground = InverseFishingPressure(X = RV, land = land_dat, group = "GROUNDFISH", years = yrs)

invFP_invert = InverseFishingPressure(X = RV[RV$YEAR >= 1999,], land = land_dat, group = "INVERTEBRATES", years = yrs)

invFP_skate = InverseFishingPressure(X = RV, land = land_dat, group = "SKATES", years = yrs)



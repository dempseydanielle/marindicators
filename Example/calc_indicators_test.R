# July 24, 2019
# This script calculates all indicators from Tech Report for ESS and WSS
# It checks that the indicator values calculated from my indicators package
# match the values exported from Adam's code
# using function "all.equal" with tolerance = 1.5e-8

# set path to where you save Example folder!
#Example data folder is stored in R:\Science\Population Ecology Division\Shared\!PED_Staff\DempseyD\indicators_data
path <- file.path("R:/Science/Population Ecology Division/Shared/!PED_Staff/DempseyD/indicators_data")


library(indicators)

# Import indicators from /output/Combined Indicators
# These are the "target" values
TARGET = read.csv(paste(path, "/Target/esswsssetq.csv", sep = ""), head = TRUE, sep = ",")
TARGET = TARGET[order(TARGET$ID), ]

# Import compiled data
yrs= c(1970:2015)

# fishery-independent data
RV <- read.csv(paste(path, "/RV/NotLengthBased/esswss_adjbiomass.csv", sep = ""), head = TRUE, sep = ",")
RV_length <- read.csv(paste(path, "/RV/LengthBased/esswss_adjbiomass_Length.csv", sep = ""), head = TRUE, sep = ",")

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
all.equal(TARGET$SpeciesRichness, S$SpeciesRichness)

# Shannon
H = shannon(RV, "ALL", "ABUNDANCE", yrs)
all.equal(TARGET$ShannonDiversity, H$ShannonDiversity)

# Margalef
marg = margalef(RV, "ALL", "ABUNDANCE", yrs)
all.equal(TARGET$MargalefRichness, marg$MargalefRichness)

ground_marg = margalef(RV, "GROUNDFISH", "ABUNDANCE", yrs)
all.equal(TARGET$MargalefGroundfish, ground_marg$MargalefRichness)

# Pielou
pie = pielouSpeciesEvenness(RV, "ALL", "ABUNDANCE", yrs)
all.equal(TARGET$PielouEvenness, pie$PielouEvenness)

# Hill1
H1 = hillN1(RV, "ALL", "ABUNDANCE", yrs)
all.equal(TARGET$HillN1Diversity, H1$HillDiversity)

# Hill2
H2 = hillN2(RV, "ALL", "ABUNDANCE", yrs)
all.equal(TARGET$HillN2Dominance, H2$HillDominance)

# Heips
Heips = heips(RV, "ALL", "ABUNDANCE", yrs)
all.equal(TARGET$Heips, Heips$Heips)

# Kempton
Q = kemptonQ(RV, TL.table = TL.table.simple, metric = "ABUNDANCE", group = "ALL",
             years = yrs,  percentiles = c(.25, 0.75), minTL = 3)
all.equal(TARGET$KemptonQ, Q$KemptonQ)


# Structure and Functioning Indicators ------------------------------------

# Large fish indicator
LFI = indicators::largeFishIndicator(RV_length, metric = "BIOMASS", years = yrs)
all.equal(TARGET$LargeFishIndicator, LFI$LargeFishIndicator)

# Large species indicator
LSI = largeSpeciesIndicator(RV, lmax.table = maxL.table, lmax=85,  metric = "BIOMASS", years = yrs)
all.equal(TARGET$LargeSpeciesIndicator, LSI$LargeSpeciesIndicator)

# Proportion of predatory species
propPred = predatoryFish(RV, pred.spp, metric = "BIOMASS", years = yrs)
all.equal(TARGET$PropPredatoryFish, propPred$PropPredatoryFish)

# Mean trophic level of community
TL = meanTrophicLevelCommunity(RV, TL.table = TL.table.simple, metric = "BIOMASS", years = yrs)
all.equal(TARGET$MeanTrophicLevel, TL$MeanTLCommunity)

# # Mean trophic level of community: LENGTH
# TL_length = meanTrophicLevelCommunity(RV_length, TL.table = TL.table.length, metric = "BIOMASS", years = yrs)
# all.equal(TARGET$MeanTrophicLevelStanza, TL_length$MeanTLCommunity)

# Mean length of community
ML_bio = meanLengthCommunity(RV_length, metric = "BIOMASS", years = yrs)
all.equal(TARGET$MeanLengthBiomass, ML_bio$MeanLengthBIOMASS)

ML_abund = meanLengthCommunity(RV_length, metric = "ABUNDANCE", years = yrs)
all.equal(TARGET$MeanLengthAbundance, ML_abund$MeanLengthABUNDANCE)

# Community Condition
FultonK = communityFultonK(RV_length, LenWt.table = Length_Weight, metric = "ABUNDANCE",  
                           years = yrs,
                           group = "FINFISH")
all.equal(TARGET$CommunityCondition, FultonK$CommunityCondition)

# inv2dem
inv_dem = biomassratio(RV[RV$YEAR>=1999,], group1 = "INVERTEBRATES", group2 = "GROUNDFISH", 
                       metric = "BIOMASS", years = yrs)
all.equal(TARGET$BInvertebrateToDemersal, inv_dem$INVERTEBRATES2GROUNDFISH)

# pel2dem
pel_dem = biomassratio(RV, group1 = "PELAGIC", group2 = "GROUNDFISH",
                       metric = "BIOMASS", years = yrs)
all.equal(TARGET$BPelagicToDemersal, pel_dem$PELAGIC2GROUNDFISH)

# Biomass of trophic guilds
bio = resourcePotential(RV, metric = "BIOMASS", group = "ALL", years = yrs)
all.equal(TARGET$Biomass, bio$BIOMASS_ALL)

bio_clup = resourcePotential(RV, metric = "BIOMASS", group = "CLUPEIDS", years = yrs)
all.equal(TARGET$BiomassClupeids, bio_clup$BIOMASS_CLUPEIDS)

bio_fin = resourcePotential(RV, metric = "BIOMASS", group = "FINFISH", years = yrs)
all.equal(TARGET$BiomassFinfish, bio_fin$BIOMASS_FINFISH)

bio_flat = resourcePotential(RV, metric = "BIOMASS", group = "FLATFISH", years = yrs)
all.equal(TARGET$BiomassFlatfish, bio_flat$BIOMASS_FLATFISH)

bio_for = resourcePotential(RV, metric = "BIOMASS", group = "FORAGE", years = yrs)
all.equal(TARGET$BiomassForage, bio_for$BIOMASS_FORAGE)

bio_gad = resourcePotential(RV, metric = "BIOMASS", group = "GADOIDS", years = yrs)
all.equal(TARGET$BiomassGadoids, bio_gad$BIOMASS_GADOIDS)

bio_ground = resourcePotential(RV, metric = "BIOMASS", group = "GROUNDFISH", years = yrs)
all.equal(TARGET$BiomassGroundfish, bio_ground$BIOMASS_GROUNDFISH)

bio_pel = resourcePotential(RV, metric = "BIOMASS", group = "PELAGIC", years = yrs)
all.equal(TARGET$BiomassPelagic, bio_pel$BIOMASS_PELAGIC)

bio_invert = resourcePotential(RV[RV$YEAR>=1999,], metric = "BIOMASS", group = "INVERTEBRATES", years = yrs)
all.equal(TARGET$BiomassInvertebrates, bio_invert$BIOMASS_INVERTEBRATES)

bio_Lbenth = resourcePotential(RV, metric = "BIOMASS", group = "LBENTHIVORE", years = yrs)
all.equal(TARGET$BTGLargeBenthivore, bio_Lbenth$BIOMASS_LBENTHIVORE)

bio_Mbenth = resourcePotential(RV, metric = "BIOMASS", group = "MBENTHIVORE", years = yrs)
all.equal(TARGET$BTGMediumBenthivore, bio_Mbenth$BIOMASS_MBENTHIVORE)

bio_pisc = resourcePotential(RV, metric = "BIOMASS", group = "PISCIVORE", years = yrs)
all.equal(TARGET$BTGPiscivore, bio_pisc$BIOMASS_PISCIVORE)

bio_plank = resourcePotential(RV, metric = "BIOMASS", group = "PLANKTIVORE", years = yrs)
all.equal(TARGET$BTGPlanktivore, bio_plank$BIOMASS_PLANKTIVORE)

bio_zoo = resourcePotential(RV, metric = "BIOMASS", group = "ZOOPISCIVORE", years = yrs)
all.equal(TARGET$BTGZoopiscivore, bio_zoo$BIOMASS_ZOOPISCIVORE)


# Stability and Resistance Indicators -------------------------------------

# Max LifeSpan
MMA = meanMaxAge(RV, age.table = maxage.table, "BIOMASS", years = yrs)
all.equal(TARGET$MeanLifespan, MMA$MeanLifespan)

# Mean max length
MML_bio = meanMaxL(RV_length, maxL.table, metric = "BIOMASS", years = yrs)
all.equal(TARGET$MMLengthBiomass, MML_bio$MMLengthBIOMASS)

MML_abund = meanMaxL(RV_length, maxL.table, metric = "ABUNDANCE", years = yrs)
all.equal(TARGET$MMLengthAbundance, MML_abund$MMLengthABUNDANCE)

# IVI Landings
IVI = IVILandings(land_dat, IVI.table = IVI.table,
                  propland.table = prop.land.table, years = yrs)
all.equal(TARGET$Intrinsicvulnerabilityindex.L, IVI$IVILandings)

# Biomass per TL
bio_TL = biomassPerTL(RV, TL.table = TL.table.simple, metric = "BIOMASS", TL.grouping = 1, years = yrs)

all.equal(TARGET$BiomassTL2, bio_TL$BIOMASS_TL2)
all.equal(TARGET$BiomassTL3, bio_TL$BIOMASS_TL3)
all.equal(TARGET$BiomassTL4, bio_TL$BIOMASS_TL4)

# inverse CV biomass
invCV_bio = invCVBiomass(RV)
all.equal(TARGET$InverseCVBiomass, invCV_bio$invCVbiomass)


# Resource Potential Indicators -------------------------------------------

# Fishing in Balance
FIB = FishingInBalance(land_all, TE = 0.1,  base.start = 1968, base.end = 1970, years = yrs,
                       TL.table = TL.table.length, propland.table = prop.land.table, cutoff = 0)
all.equal(TARGET$FishinginBalance.L, FIB$FishinginBalance)

# Biomass 
bio = resourcePotential(RV, metric = "BIOMASS", group = "ALL", years = yrs)
all.equal(TARGET$Biomass, bio$BIOMASS_ALL)

# Biomass of fished groups
bio_clup = resourcePotential(RV, metric = "BIOMASS", group = "CLUPEIDS", years = yrs)
all.equal(TARGET$BiomassClupeids, bio_clup$BIOMASS_CLUPEIDS)

bio_fin = resourcePotential(RV, metric = "BIOMASS", group = "FINFISH", years = yrs)
all.equal(TARGET$BiomassFinfish, bio_fin$BIOMASS_FINFISH)

bio_flat = resourcePotential(RV, metric = "BIOMASS", group = "FLATFISH", years = yrs)
all.equal(TARGET$BiomassFlatfish, bio_flat$BIOMASS_FLATFISH)

bio_for = resourcePotential(RV, metric = "BIOMASS", group = "FORAGE", years = yrs)
all.equal(TARGET$BiomassForage, bio_for$BIOMASS_FORAGE)

bio_gad = resourcePotential(RV, metric = "BIOMASS", group = "GADOIDS", years = yrs)
all.equal(TARGET$BiomassGadoids, bio_gad$BIOMASS_GADOIDS)

bio_ground = resourcePotential(RV, metric = "BIOMASS", group = "GROUNDFISH", years = yrs)
all.equal(TARGET$BiomassGroundfish, bio_ground$BIOMASS_GROUNDFISH)

bio_invert = resourcePotential(RV[RV$YEAR >= 1999,], metric = "BIOMASS", group = "INVERTEBRATES", years = c(1999:2015))
all.equal(TARGET[TARGET$YEAR >=1999, "BiomassInvertebrates"], bio_invert$BIOMASS_INVERTEBRATES)

# Biomass of skates
bio_skate = resourcePotential(RV, metric = "BIOMASS", group = "SKATES", years = yrs)
all.equal(TARGET$BiomassSkates, bio_skate$BIOMASS_SKATES)


# Fishing Pressure Indicators ---------------------------------------------

# Mean Trophic Level Landings 
MTLL = MeanTLLandings(land_dat, TL.table = TL.table.length, propland.table = prop.land.table, cutoff = 0)
all.equal(TARGET$MeanTrophicLevel.L, MTLL$MeanTL.Landings)

# Marine Trophic Index Landings 
MTI = MeanTLLandings(land_dat, TL.table = TL.table.length, propland.table = prop.land.table, cutoff = 3.25)
all.equal(TARGET$MarineTrophicIndex.L, MTI$MarineTophicIndex.Landings)

# Diversity of target species
SR.L = speciesrichness(land_dat, metric = "CATCH", group = "ALL", years = yrs)
all.equal(TARGET$DiversityTargetSpp.L, SR.L$DiversityTargetSpp)

# landings by group
land_all = LandByGroup(land_dat, group = "ALL", years = yrs)
all.equal(TARGET$Landings.L,land_all$ALL_landings)

land_fin = LandByGroup(land_dat, group = "FINFISH", years = yrs)
all.equal(TARGET$LFinfish.L,land_fin$FINFISH_landings)

land_clup = LandByGroup(land_dat, group = "CLUPEIDS", years = yrs)
all.equal(TARGET$LClupeids.L,land_clup$CLUPEIDS)

land_ground = LandByGroup(land_dat, group = "GROUNDFISH", years = yrs)
all.equal(TARGET$LGroundfish.L,land_ground$GROUNDFISH_landings)

land_flat = LandByGroup(land_dat, group = "FLATFISH", years = yrs)
all.equal(TARGET$LFlatfish.L,land_flat$FLATFISH_landings)

land_gad = LandByGroup(land_dat, group = "GADOIDS", years = yrs)
all.equal(TARGET$LGadoids.L,land_gad$GADOIDS_landings)

land_for = LandByGroup(land_dat, group = "FORAGE", years = yrs)
all.equal(TARGET$LForageFish.L,land_for$FORAGE_landings)

land_invert = LandByGroup(land_dat, group = "INVERTEBRATES", years = yrs)
all.equal(TARGET$LInvertebrates.L,land_invert$INVERTEBRATES_landings)

land_pel = LandByGroup(land_dat, group = "LARGE_PELAGIC", years = yrs)
all.equal(TARGET$LLargePelagic.L,land_pel$LARGE_PELAGIC_landings)

# fishing pressure by group
FP_all = FishingPressure(X = RV, land = land_dat, group = "ALL", years = yrs)
all.equal(TARGET$FishingPressure.L, FP_all$FP_ALL)

FP_clup = FishingPressure(X = RV, land = land_dat, group = "CLUPEIDS", years = yrs)
all.equal(TARGET$FPClupeids.L, FP_clup$FP_CLUPEIDS)

FP_fin = FishingPressure(X = RV, land = land_dat, group = "FINFISH", years = yrs)
all.equal(TARGET$FPFinfish.L, FP_fin$FP_FINFISH)

FP_flat = FishingPressure(X = RV, land = land_dat, group = "FLATFISH", years = yrs)
all.equal(TARGET$FPFlatfish.L, FP_flat$FP_FLATFISH)

FP_for = FishingPressure(X = RV, land = land_dat, group = "FORAGE", years = yrs)
all.equal(TARGET$FPForageFish.L, FP_for$FP_FORAGE)

FP_gad = FishingPressure(X = RV, land = land_dat, group = "GADOIDS", years = yrs)
all.equal(TARGET$FPGadoids.L, FP_gad$FP_GADOIDS)

FP_ground = FishingPressure(X = RV, land = land_dat, group = "GROUNDFISH", years = yrs)
all.equal(TARGET$FPGroundfish.L, FP_ground$FP_GROUNDFISH)

FP_invert = FishingPressure(X = RV[RV$YEAR >= 1999,], land = land_dat, group = "INVERTEBRATES", years = yrs)
all.equal(TARGET$FPInvertebrates.L, FP_invert$FP_INVERTEBRATES)

FP_skate = FishingPressure(X = RV, land = land_dat, group = "SKATES", years = yrs)
all.equal(TARGET$FPSkates.L, FP_skate$FP_SKATES)

# inverse fishing pressure by group
invFP_all = InverseFishingPressure(X = RV, land = land_dat, group = "ALL", years = yrs)
all.equal(TARGET$InverseFishingPressure.L, invFP_all$invFP_ALL)

invFP_clup = InverseFishingPressure(X = RV, land = land_dat, group = "CLUPEIDS", years = yrs)
all.equal(TARGET$InverseFPClupeids.L, invFP_clup$invFP_CLUPEIDS)

invFP_fin = InverseFishingPressure(X = RV, land = land_dat, group = "FINFISH", years = yrs)
all.equal(TARGET$InverseFPFinfish.L, invFP_fin$invFP_FINFISH)

invFP_flat = InverseFishingPressure(X = RV, land = land_dat, group = "FLATFISH", years = yrs)
all.equal(TARGET$InverseFPFlatfish.L, invFP_flat$invFP_FLATFISH)

invFP_for = InverseFishingPressure(X = RV, land = land_dat, group = "FORAGE", years = yrs)
all.equal(TARGET$InverseFPForageFish.L, invFP_for$invFP_FORAGE)

invFP_gad = InverseFishingPressure(X = RV, land = land_dat, group = "GADOIDS", years = yrs)
all.equal(TARGET$InverseFPGadoids.L, invFP_gad$invFP_GADOIDS)

invFP_ground = InverseFishingPressure(X = RV, land = land_dat, group = "GROUNDFISH", years = yrs)
all.equal(TARGET$InverseFPGroundfish.L, invFP_ground$invFP_GROUNDFISH)

invFP_invert = InverseFishingPressure(X = RV[RV$YEAR >= 1999,], land = land_dat, group = "INVERTEBRATES", years = yrs)
all.equal(TARGET$InverseFPInvertebrates.L, invFP_invert$invFP_INVERTEBRATES)

invFP_skate = InverseFishingPressure(X = RV, land = land_dat, group = "SKATES", years = yrs)
all.equal(TARGET$InverseFPSkates.L, invFP_skate$invFP_SKATES)



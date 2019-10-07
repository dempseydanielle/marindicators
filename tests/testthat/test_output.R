context("Output indicators")
library(marindicators)

data(X)
data(X_length)
data(land)
data(species.groups)
data(species.info)
data(Length_Weight)
data(example_inds)

trophicguild.groups <- c("LBENTHIVORE", "MBENTHIVORE", "PISCIVORE", "PLANKTIVORE",
                         "ZOOPISCIVORE")
condition.groups <- c("FINFISH", "LBENTHIVORE", "MBENTHIVORE", "PISCIVORE",
                      "PLANKTIVORE", "ZOOPISCIVORE")
resource.groups <- c("ALL", "CLUPEIDS", "FINFISH", "FLATFISH", "FORAGE",
                     "GADOIDS", "GROUNDFISH", "PELAGIC", "SKATES")
ratio.groups <- data.frame(rbind(c("PELAGIC", "GROUNDFISH"), c("PREDATORS", "ALL")))
names(ratio.groups) <- c("group1", "group2")

landings.groups <- c("ALL", "CLUPEIDS.L", "FLATFISH.L", "GROUNDFISH.L")
FP.groups <- data.frame(rbind(c("ALL", "ALL"),
                              c("CLUPEIDS", "CLUPEIDS.L"),
                              c("FLATFISH", "FLATFISH.L"),
                              c("GROUNDFISH", "GROUNDFISH.L")))
names(FP.groups) <- c("group.X", "group.land")

check_inds <- extractAll(X = X, X_length = X_length, land = land,
                           speciesinfo.table = species.info, species.table = species.groups,
                           LenWt.table = Length_Weight,
                           LSI.group = "ALL", LFI.group = "ALL",
                           guild.groups = trophicguild.groups,
                           resource.groups = resource.groups, condition.groups = condition.groups,
                           ratio.groups = ratio.groups,
                           max.length = 85, maxlength.group = "FINFISH",
                           minTL.FiB = 0, base.start = 2014, base.end = 2015,
                           landings.groups = landings.groups, FP.groups = FP.groups,
                           years = c(2014:2019),
                           raw = TRUE, std = TRUE, export.path = NULL)


test_that("dimensions are correct",{
  
  expect_equal(ncol(check_inds), ncol(example_inds))
  expect_equal(nrow(check_inds), nrow(example_inds))
  expect_equal(names(check_inds), names(example_inds))
  
})


test_that("raw Biodiversity indicators are correct",{
  expect_equal(check_inds$SpeciesRichness_ALL, example_inds$SpeciesRichness_ALL)
  expect_equal(check_inds$ShannonDiversity_ALL, example_inds$ShannonDiversity_ALL)
  expect_equal(check_inds$MargalefRichness_ALL, example_inds$MargalefRichness_ALL)
  expect_equal(check_inds$PielouEvenness_ALL, example_inds$PielouEvenness_ALL)
  expect_equal(check_inds$HillDiversity_ALL, example_inds$HillDiversity_ALL)
  expect_equal(check_inds$HillDominance_ALL, example_inds$HillDominance_ALL)
  expect_equal(check_inds$Heips_ALL, example_inds$Heips_ALL)
  expect_equal(check_inds$KemptonQ_ALL_0, example_inds$KemptonQ_ALL_0)
})

test_that("standardized Biodiversity indicators are correct",{
  expect_equal(check_inds$SpeciesRichness_ALL_s, example_inds$SpeciesRichness_ALL_s)
  expect_equal(check_inds$ShannonDiversity_ALL_s, example_inds$ShannonDiversity_ALL_s)
  expect_equal(check_inds$MargalefRichness_ALL_s, example_inds$MargalefRichness_ALL_s)
  expect_equal(check_inds$PielouEvenness_ALL_s, example_inds$PielouEvenness_ALL_s)
  expect_equal(check_inds$HillDiversity_ALL_s, example_inds$HillDiversity_ALL_s)
  expect_equal(check_inds$HillDominance_ALL_s, example_inds$HillDominance_ALL_s)
  expect_equal(check_inds$Heips_ALL_s, example_inds$Heips_ALL_s)
  expect_equal(check_inds$KemptonQ_ALL_0_s, example_inds$KemptonQ_ALL_0_s)
})

test_that("raw Structure and Functioning indicators are correct",{
  expect_equal(check_inds$BIOMASS_LBENTHIVORE, example_inds$BIOMASS_LBENTHIVORE)
  expect_equal(check_inds$PELAGIC_GROUNDFISH, example_inds$PELAGIC_GROUNDFISH)
  expect_equal(check_inds$LargeSpeciesIndicator, example_inds$LargeSpeciesIndicator)
  expect_equal(check_inds$MeanTLCommunity, example_inds$MeanTLCommunity)
  expect_equal(check_inds$LargeFishIndicator, example_inds$LargeFishIndicator)
  expect_equal(check_inds$MeanLengthBIOMASS, example_inds$MeanLengthBIOMASS)
  expect_equal(check_inds$CCondition_FINFISH, example_inds$CCondition_FINFISH)
})

test_that("standardized Structure and Functioning indicators are correct",{
  expect_equal(check_inds$BIOMASS_LBENTHIVORE_s, example_inds$BIOMASS_LBENTHIVORE_s)
  expect_equal(check_inds$PELAGIC_GROUNDFISH_s, example_inds$PELAGIC_GROUNDFISH_s)
  expect_equal(check_inds$LargeSpeciesIndicator_s, example_inds$LargeSpeciesIndicator_s)
  expect_equal(check_inds$MeanTLCommunity_s, example_inds$MeanTLCommunity_s)
  expect_equal(check_inds$LargeFishIndicator_s, example_inds$LargeFishIndicator_s)
  expect_equal(check_inds$MeanLengthBIOMASS_s, example_inds$MeanLengthBIOMASS_s)
  expect_equal(check_inds$CCondition_FINFISH_s, example_inds$CCondition_FINFISH_s)
})

test_that("raw Stability and Resistance indicators are correct",{
  expect_equal(check_inds$BIOMASS_TL2, example_inds$BIOMASS_TL2)
  expect_equal(check_inds$BIOMASS_TL4, example_inds$BIOMASS_TL4)
  expect_equal(check_inds$IVILandings, example_inds$IVILandings)
  expect_equal(check_inds$CVBiomass, example_inds$CVBiomass)
  expect_equal(check_inds$MeanLifespan, example_inds$MeanLifespan)
  expect_equal(check_inds$MMLength_BIOMASS, example_inds$MMLength_BIOMASS)
})

test_that("standardized Stability and Resistance indicators are correct",{
  expect_equal(check_inds$BIOMASS_TL2_s, example_inds$BIOMASS_TL2_s)
  expect_equal(check_inds$BIOMASS_TL4_s, example_inds$BIOMASS_TL4_s)
  expect_equal(check_inds$IVILandings_s, example_inds$IVILandings_s)
  expect_equal(check_inds$CVBiomass_s, example_inds$CVBiomass_s)
  expect_equal(check_inds$MeanLifespan_s, example_inds$MeanLifespan_s)
  expect_equal(check_inds$MMLength_BIOMASS_s, example_inds$MMLength_BIOMASS_s)
})

test_that("raw Resource Potential indicators are correct",{
  expect_equal(check_inds$ABUNDANCE_ALL, example_inds$ABUNDANCE_ALL)
  expect_equal(check_inds$BIOMASS, example_inds$BIOMASS)
  expect_equal(check_inds$FishinginBalance, example_inds$FishinginBalance)
})

test_that("standardized Resource Potential indicators are correct",{
  expect_equal(check_inds$ABUNDANCE_ALL_s, example_inds$ABUNDANCE_ALL_s)
  expect_equal(check_inds$BIOMASS_s, example_inds$BIOMASS_s)
  expect_equal(check_inds$FishinginBalance_s, example_inds$FishinginBalance_s)
})

test_that("raw Fishing Pressure indicators are correct",{
  expect_equal(check_inds$DiversityTargetSpp, example_inds$DiversityTargetSpp)
  expect_equal(check_inds$FP_ALL, example_inds$FP_ALL)
  expect_equal(check_inds$FP_CLUPEIDS, example_inds$FP_CLUPEIDS)
  expect_equal(check_inds$landings_FLATFISH.L, example_inds$landings_FLATFISH.L)
  expect_equal(check_inds$MeanTL.Landings, example_inds$MeanTL.Landings)
  expect_equal(check_inds$MTI.Landings_3.25, example_inds$MTI.Landings_3.25)
})

test_that("standardized Fishing Pressure indicators are correct",{
  expect_equal(check_inds$DiversityTargetSpp_s, example_inds$DiversityTargetSpp_s)
  expect_equal(check_inds$FP_ALL_s, example_inds$FP_ALL_s)
  expect_equal(check_inds$FP_CLUPEIDS_s, example_inds$FP_CLUPEIDS_s)
  expect_equal(check_inds$landings_FLATFISH.L_s, example_inds$landings_FLATFISH.L_s)
  expect_equal(check_inds$MeanTL.Landings_s, example_inds$MeanTL.Landings_s)
  expect_equal(check_inds$MTI.Landings_3.25_s, example_inds$MTI.Landings_3.25_s)
})

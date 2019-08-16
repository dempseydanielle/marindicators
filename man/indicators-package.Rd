\name{indicators-package}
\docType{package}
\title{
  Marine Ecosystem Indicators Package
}

\description{ The \pkg{indicators} package provides functions to calculate indicators for 4 marine ecosystem attributes and 1 marine ecosystem pressure.
 }

\details{ A comprehensive evaluation and selection of robust ecological indicators is one of the key steps in the process of implementing an Ecosystem Approach to fisheries management and to track progress towards meeting ecosystem goals. The "Guidance framework for the selection and evaluation of ecological indicators" (Bundy, Gomez, and Cook, 2017) developed an Indicator Selection Guidance Framework to select and evaluate potential indicators for ecosystem monitoring and assessment at different spatial scales using established selection criteria. The framework was successfully tested for the Scotian Shelf Bioregion, and a final suite of 30 ecological indicators derived from fisheries independent and dependent data was selected.

The \pkg{indicators} package provides functions to calculate the full suite of marine ecosystem indicators calculated in Step 4 of the guidance framework (Bundy, Gomez, and Cook, 2017). Each indicator provides information an ecosystem attribute or pressure. Ecosystem attributes represent broad key features of healthy ecosystems (Shin et al. 2010), and include: Biodiversity, Ecosystem Structure and Functioning, Ecosystem Stability and Resistance to Perturbations, and Resource Potential. Ecosystems can be impacted by multiple human and climate pressures (Halpern et al. 2008, 2012, Rombouts et al. 2013). Following the guidance framework, the \pkg{indicators} package focuses on calculating indicators related to Fishing Pressure. For more information, including attribute definitions and related sub-attributes, please refer to the guidance framework document (Bundy, Gomez, and Cook, 2017).

The tables below list the indicators for each ecosystem attribute and pressure, and the \pkg{indicators} function used to calculate each indicator. Indicators chosen for the final suite for the Scotian Shelf are shown in bold

Table 1: Biodiversity Indicators
  \tabular{ll}{
  \bold{INDICATOR} \tab \bold{FUNCTION} \cr
  ---------------------------------------------\tab--------------------- \cr
  \bold{Heips Evenness Index} \tab heips() \cr
  Hill's Diversity Index \tab hillN1() \cr
  Hill's Dominance \tab hillN2() \cr
  Kempton's Q \tab kemptonQ() \cr
  \bold{Margalef's Species Richness} \tab margalef() \cr
  Margalef's Species Richness (group) \tab margalef() \cr
  Pielou's Species Evenness \tab pielouEvenness() \cr
  Shannon's Diversity Index \tab shannon() \cr
  Species Richness \tab speciesRichness()
  }
  
Table 2: Ecosystem Structure and Functioning Indicators
  \tabular{ll}{
  \bold{INDICATOR} \tab \bold{FUNCTION} \cr
  ---------------------------------------------\tab----------------------------- \cr
  Biomass Ratio \tab biomassRatio() \cr
  \bold{Biomass of Trophic Guilds*} \tab resourcePotential \cr
  \bold{Community Condition} \tab communityCondition() \cr
  \bold{Trophic Guild Condition**} \tab communityCondition() \cr
  \bold{Large Fish Indicator} \tab largeFishIndicator() \cr
  Large Species Indicator \tab largeSpeciesIndicator() \cr
  \bold{Mean Length Weighted by Abundance} \tab meanLengthCommunity() \cr
  Mean Length Weighted by Biomass \tab meanLengthCommunity() \cr
  Mean Trophic Level of Community \tab meanTLCommunity() \cr
  Mean Trophic Level of Community (by length) \tab meanTLCommunity()\cr
  Proportion of Predatory Fish \tab biomassRatio()
  }*Biomass of piscivores and zoopiscivores were included in the final suite for the Scotian Shelf
  **Condition of large benthivores, medium benthivores, piscivores, and zoopiscivores were included in the final suite for the Scotian Shelf \cr

Table 3: Ecosystem Stability and Resistance to Perturbations Indicators
  \tabular{ll}{
  \bold{INDICATOR} \tab \bold{FUNCTION} \cr
  ---------------------------------------------\tab----------------------------- \cr
  \bold{Biomass per Trophic Level*} \tab biomassPerTL() \cr
  \bold{Intrinsic Vulnerability Index of Landings} \tab IVILandings() \cr
  \bold{CV of Biomass} \tab CVBiomass() \cr
  \bold{Mean Lifespan} \tab meanMaxAge() \cr
  Mean Maximum Length (weighted by Biomass) \tab meanMaxLength() \cr
  Mean Maximum Length (weighted by abundance) \tab meanMaxLength()
  } *Biomass at trophic level 2 was included in the final suite for the Scotian Shelf \cr 

Table 4: Resource Potential Indicators
  \tabular{ll}{
  \bold{INDICATOR} \tab \bold{FUNCTION} \cr
  ---------------------------------------------\tab----------------------------- \cr
  \bold{Biomass} \tab resourcePotential() \cr
  Biomass of Finfish \tab resourcePotential() \cr
  \bold{Biomass of Invertebrates} \tab resourcePotential() \cr
  \bold{Biomass of Fished Groups*} \tab resourcePotential() \cr
  \bold{Biomass of Skates} \tab resourcePotential() \cr
  Fishing in Balance \tab fishingInBalance()
  } *Biomass of gadoids and flatfish were included in the final suite for the Scotian Shelf \cr 
  
  Table 5: Fishing Pressure Indicators
  \tabular{ll}{
  \bold{INDICATOR} \tab \bold{FUNCTION} \cr
  ---------------------------------------------\tab----------------------------- \cr
  \bold{Diversity of Target Species} \tab speciesRichness() \cr
  \bold{Fishing Pressure} \tab FishingPressure() \cr
  \bold{Fishing Pressure on Groups*} \tab fishingPressure() \cr
  \bold{Landings} \tab landings() \cr
  \bold{Landings of Fished Groups**} \tab landings() \cr
  \bold{Marine Trophic Index} \tab meanTLLandings() \cr
  \bold{Mean Trophic Level of Landings}  \tab meanTLLandings()
  }*Fishing pressure on clupeids was included in the final suite for the Scotian Shelf
  **Landings of skates, flatfish, and large pelagics were included in the final suite of the Scotian Shelf \cr 

The data required to apply the \pkg{indicators} package depends on the indicator(s) to be calculated. In general, indicators related to ecosystem attibutes require annual fishery-independent survey data, while indicators related to ecosystem pressures require annual commercial landings data. Additional information on the species recorded is required to calculate some indicators, including: trophic level, intrinsic vulnerability index, maximum recorded age, maximum recorded length, and annual length-at-weight data. See the help page for a specific function to determine its data requirements.
}

\references{
Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
 selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
 Aquat. Sci. 3232: xii + 212 p.
 
 Halpern BS, Longo C, Hardy D, McLeod KL, Samhouri JF, Katona SK, Kleisner K, Lester SE, O’Leary J, Ranelletti M, Rosenberg AA, Scarborough C, Selig ER, Best BD, Brumbaugh DR, Chapin FS, Crowder LB, Daly KL, Doney SC, Elfes C, Fogarty MJ, Gaines SD, Jacobsen KI, Karrer LB, Leslie HM, Neeley E, Pauly D, Polasky S, Ris B, St Martin K, Stone GS, Sumaila UR, Zeller D. 2012. An index to assess the health and benefits of the global ocean. Nature 488:615–620
 
 Halpern BS, Walbridge S, Selkoe KA, Kappel C V, Micheli F, D’Agrosa C, Bruno JF, Casey KS, Ebert C, Fox HE, Fujita R, Heinemann D, Lenihan HS, Madin EMP,Perry MT, Selig ER, Spalding M, Steneck R, Watson R, D’Agrosa C, Bruno JF, Casey KS, Ebert C, Fox HE, Fujita R, Heinemann D, Lenihan HS, Madin EMP, Perry MT, Selig ER, Spalding M, Steneck R, Watson R. 2008. A global map of human impact on marine ecosystems. Science (80- ) 319:948–52
 
 Rombouts I, Beaugrand G, Fizzala X, Gaill F, Greenstreet SPR, Lamare S, Loc’h F Le, McQuatters-Gollop a., Mialet B, Niquil N, Percelay J, Renaud F, Rossberg a. G, Féral JP. 2013. Food web indicators under the Marine Strategy Framework Directive: From complexity to simplicity? Ecol Indic 29:246–254
 
 Shin YJ, Shannon LJ, Bundy A, Coll M, Aydin K, Bez N, Blanchard JL, Borges, MF, Diallo I, Diaz E, Heymans JJ, Hill L, Johannesen E, Jouffre D, Kifani S, Labrosse P, Link JS, Mackinson S, Masski H, Möllmann C, Neira S, Ojaveer H, Ould Mohammed Abdallahi ., Perry I, Thiao D, Yemane D, and Cury P. 2010. Using indicators for evaluating, comparing and communicating the ecological status of exploited marine ecosystems. Part 2: Setting the scene. ICES Journal of Marine Science, 67: 692-716
 }


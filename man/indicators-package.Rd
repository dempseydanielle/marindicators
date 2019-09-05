\name{indicators-package}
\docType{package}
\title{
  Marine Ecosystem Indicators Package
}

\description{ The \pkg{indicators} package provides functions to calculate indicators for 4 marine ecosystem attributes and 1 marine ecosystem pressure from fisheries independent trawl survey data and fisheries landings data.
 }

\details{ A comprehensive evaluation and selection of robust ecological indicators is one of the key steps in the process of implementing an Ecosystem Approach to fisheries management and to track progress towards meeting ecosystem goals. The "Guidance framework for the selection and evaluation of ecological indicators" (Bundy, Gomez, and Cook, 2017) developed an Indicator Selection Guidance Framework to select and evaluate potential indicators for ecosystem monitoring and assessment at different spatial scales using established selection criteria. Over 100 indicators were initially selected to represent the ecosystem attributes and pressure defined in Step 1 of the framework: Biodiversity, Ecosystem Structure and Functioning, Ecosystem Stability and Resistance to Perturbations, Resource Potential, and Fishing Pressure. The framework was successfully tested for the Scotian Shelf Bioregion, resulting in a final suite of 30 non-redundant ecological and fishing pressure indicators derived from fisheries independent and dependent data.

The \pkg{indicators} package provides functions to calculate the full suite of marine ecosystem indicators screened in Step 4 of the guidance framework (Table 1; Bundy, Gomez, and Cook, 2017). Each indicator provides information on an ecosystem attribute or pressure. For more information, including attribute definitions and related sub-attributes, please refer to the guidance framework document (Bundy, Gomez, and Cook, 2017).

Table 1: Indicators for each ecosystem attribute and pressure and the \pkg{indicators} function used to calculate each indicator. 
  \tabular{lll}{
  \bold{ATTRIBUTE} \tab \bold{INDICATOR} \tab \bold{FUNCTION} \cr
  Biodiversity \tab Heips Evenness Index \tab \code{heips()} \cr
  Biodiversity \tab Hill's Diversity Index \tab \code{hillN1()} \cr
  Biodiversity \tab Hill's Dominance \tab \code{hillN2()} \cr
  Biodiversity \tab Kempton's Q \tab \code{kemptonQ()} \cr
  Biodiversity \tab Margalef's Species Richness \tab  \code{margalef()} \cr
  Biodiversity \tab Margalef's Species Richness (group) \tab \code{margalef()} \cr
  Biodiversity \tab Pielou's Species Evenness \tab \code{pielouEvenness()} \cr
  Biodiversity \tab Shannon's Diversity Index \tab \code{shannon()} \cr
  Biodiversity \tab Species Richness \tab \code{speciesRichness()} \cr
  
  Structure & Functioning \tab Biomass Ratio \tab \code{biomassRatio()} \cr 
  Structure & Functioning \tab Biomass of Trophic Guilds \tab \code{resourcePotential} \cr
  Structure & Functioning \tab Community Condition \tab \code{communityCondition()} \cr
  Structure & Functioning \tab Trophic Guild Condition \tab \code{communityCondition()} \cr
  Structure & Functioning \tab Large Fish Indicator \tab \code{largeFishIndicator()} \cr
  Structure & Functioning \tab Large Species Indicator \tab \code{largeSpeciesIndicator()} \cr
  Structure & Functioning \tab Mean Length Weighted by Abundance \tab \code{meanLengthCommunity()} \cr
  Structure & Functioning \tab Mean Length Weighted by Biomass \tab \code{meanLengthCommunity()} \cr
  Structure & Functioning \tab Mean Trophic Level of Community \tab \code{meanTLCommunity()} \cr
  Structure & Functioning \tab Proportion of Predatory Fish \tab \code{biomassRatio()} \cr

  Stability & Resistance \tab Biomass per Trophic Level \tab \code{biomassPerTL()} \cr
  Stability & Resistance \tab Intrinsic Vulnerability Index of Landings \tab \code{IVILandings()} \cr
  Stability & Resistance \tab CV of Biomass \tab \code{CVBiomass()} \cr
  Stability & Resistance \tab Mean Lifespan \tab \code{meanMaxAge()} \cr
  Stability & Resistance \tab Mean Max Length (weighted by biomass) \tab \code{meanMaxLength()} \cr
  Stability & Resistance \tab Mean Max Length (weighted by abundance) \tab \code{meanMaxLength()} \cr

  Resource Potential \tab Biomass \tab \code{resourcePotential()} \cr
  Resource Potential \tab Biomass of Finfish \tab \code{resourcePotential()} \cr
  Resource Potential \tab Biomass of Invertebrates \tab \code{resourcePotential()} \cr
  Resource Potential \tab Biomass of Fished Groups \tab \code{resourcePotential()} \cr
  Resource Potential \tab Biomass of Skates \tab \code{resourcePotential()} \cr
  Resource Potential \tab Fishing in Balance \tab \code{fishingInBalance()} \cr
  
  Fishing Pressure \tab Diversity of Target Species \tab \code{speciesRichness()} \cr
  Fishing Pressure \tab Fishing Pressure \tab \code{fishingPressure()} \cr
  Fishing Pressure \tab Fishing Pressure on Groups \tab \code{fishingPressure()} \cr
  Fishing Pressure \tab Landings \tab \code{landings()} \cr
  Fishing Pressure \tab Landings of Fished Groups \tab \code{landings()} \cr
  Fishing Pressure \tab Marine Trophic Index \tab \code{meanTLLandings()} \cr
  Fishing Pressure \tab Mean Trophic Level of Landings  \tab \code{meanTLLandings()}
  }
  
The following indicators were included in the final suite for the Scotian Shelf:
\tabular{l}{
Heips Evenness Index \cr
Margalef’s Species Richness \cr
Biomass of Trophic Guilds (piscivores and zoopiscivores) \cr
Community Condition \cr
Trophic Guild Condition (large benthivores, medium benthivores, piscivores, and zoopiscivores) \cr
Large Fish Indicator \cr
Mean Length Weighted by Abundance \cr
Biomass at Trophic Level 2 \cr
Intrinsic Vulnerability Index of Landings \cr
CV of Biomass \cr
Mean Lifespan \cr 
Biomass of Community \cr
Biomass of Invertebrates \cr
Biomass of Fished Groups (gadoids and flatfish) \cr
Biomass of Skates \cr
Diversity of Target Species \cr
Fishing Pressure on the community \cr
Fishing Pressure on Clupeids \cr
Total Landings \cr
Landings of Fished Groups (skates, flatfish, and large pelagics) \cr
Marine Trophic Index \cr
Mean Trophic Level of Landings 
}

  
The data required to apply the \pkg{indicators} package depends on the indicator(s) to be calculated. In general, indicators related to ecosystem attibutes require annual fishery-independent survey data, while indicators related to ecosystem pressures require annual commercial landings data. Additional information on the species recorded is required to calculate some indicators, including: trophic level, intrinsic vulnerability index, maximum recorded age, maximum recorded length, and annual length-at-weight data. See the help page for a specific function to determine its data requirements.
}

\references{
Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
 selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
 Aquat. Sci. 3232: xii + 212 p.
 
 }


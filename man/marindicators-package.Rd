\name{marindicators-package}
\docType{package}
\title{
  Calculates marine ecosystem indicators
}

\description{ The \pkg{marindicators} package provides functions to calculate indicators for four marine ecosystem attributes and one marine ecosystem pressure from fisheries independent trawl survey data and fisheries landings data.
 }

\details{ A comprehensive evaluation and selection of robust ecological indicators is one of the key steps in the process of implementing an Ecosystem Approach to fisheries management and to track progress towards meeting ecosystem goals. The "Guidance framework for the selection and evaluation of ecological indicators" (Bundy, Gomez, and Cook, 2017) developed an Indicator Selection Guidance Framework to select and evaluate potential indicators for ecosystem monitoring and assessment at different spatial scales using established selection criteria. 

The framework was successfully tested for the Scotian Shelf Biogregion. In Step 1, four ecosystem attributes and one pressure were defined: Biodiversity, Ecosystem Structure and Functioning, Ecosystem Stability and Resistance to Perturbations, Resource Potential, and Fishing Pressure. In Step 2, over 100 indicators were selected to represent these attributes and pressure. The qualitative screening in Step 3 reduced this set to 68 indicators (Table 1), which were calculated in Step 4. The quantitative screening in Step 5 resulted in a final suite of 30 non-redundant ecological and fishing pressure indicators derived from fisheries independent and dependent data (Step 6). For more information, including attribute definitions and the methods used to select the final suite of indicators, please refer to the guidance framework document (Bundy, Gomez, and Cook, 2017).

The \pkg{marindicators} package provides functions to calculate the suite of marine ecosystem indicators calculated in Step 4 of the guidance framework (Table 1; Bundy, Gomez, and Cook, 2017). Some functions may be used to calculate additional indicators, for example by specifying different species groups or using alternative values for arguments with default values. 

Table 1: Indicators for each ecosystem attribute and pressure and the \pkg{marindicators} function used to calculate each indicator. 
  \tabular{lll}{
  \bold{ATTRIBUTE} \tab \bold{INDICATOR} \tab \bold{FUNCTION} \cr
  Biodiversity \tab Margalef's Species Richness \tab  \code{margalef()} \cr
  Biodiversity \tab Margalef's Species Richness (group) \tab \code{margalef()} \cr
  Biodiversity \tab Species Richness \tab \code{speciesRichness()} \cr
  Biodiversity \tab Shannon's Diversity Index \tab \code{shannon()} \cr
  Biodiversity \tab Hill's Diversity Index \tab \code{hillN1()} \cr
  Biodiversity \tab Kempton's Q \tab \code{kemptonQ()} \cr 
  Biodiversity \tab Hill's Dominance \tab \code{hillN2()} \cr
  Biodiversity \tab Pielou's Species Evenness \tab \code{pielouEvenness()} \cr
  Biodiversity \tab Heips Evenness Index \tab \code{heips()} \cr
  
  Structure & Functioning \tab Large Fish Indicator \tab \code{largeFishIndicator()} \cr
  Structure & Functioning \tab Large Species Indicator \tab \code{largeSpeciesIndicator()} \cr
  Structure & Functioning \tab Proportion of Predatory Fish \tab \code{biomassRatio()} \cr
  Structure & Functioning \tab Biomass of Trophic Guilds \tab \code{resourcePotential} \cr
  Structure & Functioning \tab Biomass Ratio(s) \tab \code{biomassRatio()} \cr 
  Structure & Functioning \tab Mean Length Weighted by Abundance \tab \code{meanLengthCommunity()} \cr
  Structure & Functioning \tab Mean Length Weighted by Biomass \tab \code{meanLengthCommunity()} \cr 
  Structure & Functioning \tab Mean Trophic Level of Community \tab \code{meanTLCommunity()} \cr
  Structure & Functioning \tab Community Condition \tab \code{communityCondition()} \cr
  Structure & Functioning \tab Trophic Guild Condition \tab \code{communityCondition()} \cr

  Stability & Resistance \tab Mean Lifespan \tab \code{meanMaxAge()} \cr
  Stability & Resistance \tab Mean Max Length (weighted by abundance) \tab \code{meanMaxLength()} \cr
  Stability & Resistance \tab Mean Max Length (weighted by biomass) \tab \code{meanMaxLength()} \cr
  Stability & Resistance \tab Intrinsic Vulnerability Index of Landings \tab \code{IVILandings()} \cr
  Stability & Resistance \tab Biomass per Trophic Level \tab \code{biomassPerTL()} \cr
  Stability & Resistance \tab CV of Biomass \tab \code{CVBiomass()} \cr

  Resource Potential \tab Biomass \tab \code{resourcePotential()} \cr
  Resource Potential \tab Biomass of Key Groups \tab \code{resourcePotential()} \cr
  Resource Potential \tab Fishing in Balance \tab \code{fishingInBalance()} \cr
  
  Fishing Pressure \tab Fishing Pressure \tab \code{fishingPressure()} \cr
  Fishing Pressure \tab Fishing Pressure on Groups \tab \code{fishingPressure()} \cr
  Fishing Pressure \tab Marine Trophic Index \tab \code{meanTLLandings()} \cr
  Fishing Pressure \tab Mean Trophic Level of Landings  \tab \code{meanTLLandings()} \cr
  Fishing Pressure \tab Diversity of Target Species \tab \code{speciesRichness()} \cr
  Fishing Pressure \tab Landings \tab \code{landings()} \cr
  Fishing Pressure \tab Landings of Fished Groups \tab \code{landings()}

  }
  
The following indicators were included in the final suite for the Scotian Shelf (Step 6 of the guidance framework):
\tabular{l}{
Margalef’s Species Richness \cr
Heips Evenness Index \cr
Large Fish Indicator \cr
Biomass of Trophic Guilds (piscivores and zoopiscivores) \cr
Mean Length Weighted by Abundance \cr
Mean Trophic Level of Community \cr
Community Condition \cr
Trophic Guild Condition (large benthivores, medium benthivores, piscivores, and zoopiscivores) \cr
Mean Lifespan \cr 
Intrinsic Vulnerability Index of Landings \cr
Biomass at Trophic Level 2 \cr
Coefficient of Variation of Biomass \cr
Biomass of Community \cr
Biomass of Invertebrates \cr
Biomass of Fished Groups (gadoids and flatfish) \cr
Biomass of Skates \cr
Fishing Pressure on the community \cr
Fishing Pressure on Clupeids \cr
Marine Trophic Index \cr
Mean Trophic Level of Landings \cr
Diversity of Target Species \cr
Total Landings \cr
Landings of Fished Groups (skates, flatfish, and large pelagics) 

}

  
The data required to apply the \pkg{marindicators} package depends on the indicator(s) to be calculated. In general, indicators related to ecosystem attibutes require annual fishery-independent survey data, while indicators related to ecosystem pressure require annual commercial landings data. Additional species information is required to calculate some indicators, including: trophic level, intrinsic vulnerability index, maximum recorded age, maximum recorded length, and annual length-at-weight data. See the vignette or the help page for a specific function for more information on data requirements.
}

\references{
Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
 selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
 Aquat. Sci. 3232: xii + 212 p.
 
 }


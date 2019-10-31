\name{marindicators-package}
\docType{package}
\title{
  Calculates Marine Ecosystem and Fishing Pressure Indicators
}

\description{ The \pkg{marindicators} package provides functions to calculate indicators for four marine ecosystem attributes (Biodiversity, Ecosystem Structure and Functioning, Ecosystem Stability and Resistance to Perturbations, Resource Potential) and one marine ecosystem pressure (Fishing Pressure) from fishery independent trawl survey data and commercial landings data. All indicators were selected and evaluated using the “Indicator Selection Guidance Framework” developed by Bundy, Gomez, and Cook (2017).
 }

\details{ A comprehensive evaluation and selection of robust ecological indicators is one of the key steps in the process of implementing an Ecosystem Approach to fisheries management and to track progress towards meeting ecosystem goals. The indicators included in this package were selected using the “Indicator Selection Guidance Framework” developed by Bundy, Gomez, and Cook (2017) to select and evaluate potential indicators for ecosystem monitoring and assessment at different spatial scales using established selection criteria.

The framework was successfully tested for the Scotian Shelf Biogregion, Canada. In Step 1, four ecosystem attributes and one pressure were defined: Biodiversity, Ecosystem Structure and Functioning, Ecosystem Stability and Resistance to Perturbations, Resource Potential, and Fishing Pressure. In Step 2, 186 indicators were selected to represent these attributes and pressure. The qualitative screening in Step 3 reduced this set to 68 indicators (Table 1), which were calculated in Step 4. The quantitative screening in Step 5 resulted in a final suite of 30 non-redundant ecological and fishing pressure indicators derived from fishery independent and dependent data (Step 6). For more information, including attribute definitions and the methods used to select the final suite of indicators, please refer to the guidance framework document (Bundy, Gomez, and Cook, 2017).

The \pkg{marindicators} package provides functions to calculate the suite of marine ecosystem indicators calculated in Step 4 of the guidance framework (Table 1; Bundy, Gomez, and Cook, 2017). Some functions may be used to calculate additional indicators, for example by specifying different species groups or using alternative values for arguments with default values. 

Table 1: Indicators for each ecosystem attribute and pressure and the \pkg{marindicators} function used to calculate each indicator. Indicators included in the final suite for the Scotian Shelf Bioregion are highlighted in \bold{bold}.
  \tabular{lll}{
  \bold{ATTRIBUTE} \tab \bold{INDICATOR} \tab \bold{FUNCTION} \cr
  Biodiversity \tab All Biodiversity indicators \tab \code{allBiodiversity()} \cr
  Biodiversity \tab \bold{Margalef's Species Richness} \tab  \code{margalef()} \cr
  Biodiversity \tab Species Richness \tab \code{speciesRichness()} \cr
  Biodiversity \tab Shannon's Diversity Index \tab \code{shannon()} \cr
  Biodiversity \tab Hill's Diversity Index \tab \code{hillN1()} \cr
  Biodiversity \tab Kempton's Q \tab \code{kemptonQ()} \cr 
  Biodiversity \tab Hill's Dominance \tab \code{hillN2()} \cr
  Biodiversity \tab Pielou's Species Evenness \tab \code{pielouEvenness()} \cr
  Biodiversity \tab \code{Heips Evenness Index} \tab \code{heips()} \cr
   \tab \tab \cr
  Structure & Functioning \tab All Structure & Functioning indicators \tab \code{allStructure()} \cr
  Structure & Functioning \tab \bold{Large Fish Indicator} \tab \code{largeFishIndicator()} \cr
  Structure & Functioning \tab Large Species Indicator \tab \code{largeSpeciesIndicator()} \cr
  Structure & Functioning \tab Proportion of Predatory Fish \tab \code{biomassRatio()} \cr
  Structure & Functioning \tab \bold{Biomass of Trophic Guilds}\eqn{^1} \tab \code{resourcePotential} \cr
  Structure & Functioning \tab Biomass Ratio(s) \tab \code{biomassRatio()} \cr 
  Structure & Functioning \tab \bold{Mean Length (weighted by abundance)} \tab \code{meanLengthCommunity()} \cr
  Structure & Functioning \tab Mean Length (weighted by biomass) \tab \code{meanLengthCommunity()} \cr 
  Structure & Functioning \tab \bold{Mean Trophic Level of Community} \tab \code{meanTLCommunity()} \cr
  Structure & Functioning \tab \bold{Community Condition} \tab \code{communityCondition()} \cr
  Structure & Functioning \tab \bold{Trophic Guild Condition}\eqn{^2} \tab \code{communityCondition()} \cr
 \tab \tab \cr
  Stability & Resistance \tab All Stability & Resistance indicators \tab \code{allStability()} \cr
  Stability & Resistance \tab \bold{Mean Lifespan} \tab \code{meanMaxAge()} \cr
  Stability & Resistance \tab Mean Max Length (weighted by abundance) \tab \code{meanMaxLength()} \cr
  Stability & Resistance \tab Mean Max Length (weighted by biomass) \tab \code{meanMaxLength()} \cr
  Stability & Resistance \tab \bold{Intrinsic Vulnerability Index of Landings} \tab \code{IVILandings()} \cr
  Stability & Resistance \tab \bold{Biomass per Trophic Level}\eqn{^3} \tab \code{biomassPerTL()} \cr
  Stability & Resistance \tab \bold{Coefficient of Variation of Biomass} \tab \code{CVBiomass()} \cr
 \tab \tab \cr
  Resource Potential \tab All Resource Potential indicators \tab \code{allPotential()} \cr
  Resource Potential \tab \bold{Biomass} \tab \code{resourcePotential()} \cr
  Resource Potential \tab \bold{Biomass of Key Groups}\eqn{^4} \tab \code{resourcePotential()} \cr
  Resource Potential \tab Fishing in Balance \tab \code{fishingInBalance()} \cr
   \tab \tab \cr
  Fishing Pressure \tab All Fishing Pressure indicators \tab \code{allPressure()} \cr
  Fishing Pressure \tab \bold{Fishing Pressure} \tab \code{fishingPressure()} \cr
  Fishing Pressure \tab \bold{Fishing Pressure on Groups}\eqn{^5} \tab \code{fishingPressure()} \cr
  Fishing Pressure \tab \bold{Marine Trophic Index} \tab \code{meanTLLandings()} \cr
  Fishing Pressure \tab \bold{Mean Trophic Level of Landings}  \tab \code{meanTLLandings()} \cr
  Fishing Pressure \tab \bold{Diversity of Target Species} \tab \code{speciesRichness()} \cr
  Fishing Pressure \tab \bold{Landings} \tab \code{landings()} \cr
  Fishing Pressure \tab \bold{Landings of Fished Groups}\eqn{^6} \tab \code{landings()}
  } 


\eqn{^1}Biomass of piscivores and zoopiscivores \cr
\eqn{^2}Condition of large benthivores, medium benthivores, piscivores, and zoopiscivores \cr 
\eqn{^3}Biomass at trophic level 2 \cr
\eqn{^4}Biomass of invertebrates, gadoids, flatfish, and skates \cr
\eqn{^5}Fishing Pressure on clupeids \cr
\eqn{^6} Landings of skates, flatfish, and large pelagics \cr
  

The data required to apply the \pkg{marindicators} package depends on the indicator(s) to be calculated. In general, indicators related to ecosystem attibutes require annual fishery independent survey data, while indicators related to the ecosystem pressure require annual commercial landings data. Additional species information is required to calculate some indicators, including: trophic level, intrinsic vulnerability index, maximum recorded age, maximum recorded length, and annual length-at-weight data. See the vignette or the help page for a specific function for more information on data requirements.

The functions in this package provide the user with three options for calculating indicators: \cr
1. Individual functions, which calculate a single indicator for one or more user-defined species groups. \cr
2. “allAttribute” functions, which call the individual functions to calculate and return all (or a subset) of the indicators for an attribute or pressure. \cr
3. \code{extractAll()} function, which calls the “allAttribute” functions to calculate and return all of the indicators. 

See the vignette "Introduction to \code{marindicators}" or the help files below for more details on these functions.

}

\references{
Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
 selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
 Aquat. Sci. 3232: xii + 212 p.
 
 }


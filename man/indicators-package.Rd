\name{indicators-package}
\docType{package}
\title{
  Marine Ecosystem Indicators Package
}

\description{ The \pkg{indicators} package provides functions to calculate indicators for 5 marine ecosystem attributes.
 }

\details{ A comprehensive evaluation and selection of robust ecological indicators is one of the key steps in the process of implementing Ecosystem Approach and to track progress towards meeting ecosystem goals. The "Guidance framework for the selection and evaluation of ecological indicators" (Bundy, Gomez,and Cook, 2017) developed an Indicator Selection Guidance Framework to select and evaluate potential indicators for ecosystem monitoring and assessment at different spatial scales using established selection criteria. The framework was successfully tested for the Scotian Shelf Bioregion, and a final suite of 30 ecological indicators derived from fisheries dependent and independent data was selected.

The \pkg{indicators} package provides functions to calculate the full suite of marine ecosystem indicators calculated in Step 4 of the guidance framework (Bundy, Gomez,and Cook, 2017). Each indicator provides information an ecosystem attribute or pressure. Ecosystem attributes represent broad key features of healthy ecosystems (Shin et al. 2010), and include: Biodiversity, Ecosystem Structure and Functioning, Ecosystem Stability and Resistance to Perturbations, and Resource Potential. Ecosystems can be impacted by multiple human and climate pressures (Halpern et al. 2008, 2012, Rombouts et al. 2013). Following the guidance framework, the \pkg{indicators} package focuses on calculating indicators related to Fishing Pressure. For more information, including attribute definitions and related sub-attributes, please refer to the guidance framework document (Bundy, Gomez,and Cook, 2017).

The tables below list the indicators for each ecosystem attribute and pressure (indicators chosen for the final suite for the Scotian Shelf are shown in bold), and the \pkg{indicators} function used to calculate each indicator.

Table 1: Biodiversity Indicators
  \tabular{l | l}{
  \bold{INDICATOR} \tab \bold{FUNCTION} \cr
  ---------------------------------------------\tab--------------------- \cr
  \bold{Heips Evenness Index} \tab heips() \cr
  Hill's Diversity Index \tab hillN1() \cr
  Hill's Dominance \tab hillN2() \cr
  Kempton's Q \tab kempton() \cr
  \bold{Margalef's Species Richness} \tab margalef() \cr
  Margalef's Species Richness (group) \tab margalef() \cr
  Pielou's Species Evenness \tab pielouSpeciesEvenness() \cr
  Shannon's Diversity Index \tab shannon() \cr
  Species Richness \tab speciesrichness()
  }
  
Table 2: Ecosystem Structure and Functioning Indicators
  \tabular{l | l}{
  \bold{INDICATOR} \tab \bold{FUNCTION} \cr
  ---------------------------------------------\tab----------------------------- \cr
  Biomass Ratio \tab biomassratio() \cr
  \bold{Biomass of Trophic Guilds} \tab resourcepotential \cr
  Community Condition \tab communityFultonK() \cr
  Trophic Guild Condition \tab communityFultonK()() \cr
  \bold{Large Fish Indicator} \tab largeFishIndicator() \cr
  Large Species Indicator \tab largeSpeciesIndicator() \cr
  \bold{Mean Length Weighted by Abundance} \tab meanLengthCommunity() \cr
  Mean Length Weighted by Biomass \tab meanLengthCommunity() \cr
  Mean Trophic Level of Community \tab meanTrophicLevelCommunity() \cr
  Mean Trophic Level of Community (by length) \tab meanTrophicLevelCommunity()\cr
  Proportion of Predatory Fish \tab predatoryFish()
  }

The data required for to apply the \pkg{indicators} package depends on the indicator(s) to be calculated. In general, indicators related to ecosystem attibutes require annual fishery-independent survey data, while indicators related to ecosystem pressures require annual commercial landings data. Additional information on the species recorded is required to calculate some indicators, including: trophic level, intrinsic vulnerability index, maximum recorded age, maximum recorded length annual length-at-weight data, and proportion of landings. See the help page for a specific function to determine its data requirements.


}

\author{ The \pkg{vegan} development team is Jari Oksanen,
F. Guillaume Blanchet, Roeland Kindt, Pierre Legendre, Peter
R. Minchin, R. B. O'Hara, Gavin L. Simpson, Peter Solymos, M. Henry
H. Stevens, Helene Wagner.  Many other people have contributed to
individual functions: see credits in function help pages.
}

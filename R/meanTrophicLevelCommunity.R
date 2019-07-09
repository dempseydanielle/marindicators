#'@title Calculates the mean trophic level of the community
#'@description This function takes a dataframe with columns **** and calculates
#'  the mean trophic level of the community, weighted by biomass
#'@details Mean trophic level (TL): \deqn{TL = \Sigma TL_i*B_i)/\Sigma B_i}
#'  \eqn{TL_i} is trophic level of species \eqn{i}, and \eqn{B_i} is the biomass
#'  of species \eqn{i}.
#'
#'  This indicator is based on trophic levels of all species with available
#'  biomass time series, weighted by annual species-specific biomass, to reflect
#'  the structure of the community.
#'
#'  Recommended data: Fishery independent surveys, fish and invertebrates.
#'@param X add text here
#'@param lpred.data.table to delete?
#'@param metric set to biomass
#'@family ecosystem structure and function indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Christensen 1998 (not in tech report)
#'
#'  Shannon et al 2014
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}
#'@export


meanTrophicLevelCommunity <- function(X, TL.table = NA,  metric='BIOMASS') {
                                      #pred.data.table='indiseas_wss_tl', metric='BIOMASS') {
                                        
  if (TL.table = NA) {
    load("R/sysdata.rda/indiseas_TL.rda")
    TL.table <- indiseas_TL
    rm(indiseas_TL)
  }

  X <- merge(X, TL.table, by='SPECIES')
  
# find a better way to do this
  # loop over years?
	ind[i] <- aggregate(Y[metric]*Y['AVG(TL)'], by=Y['ID'], FUN=sum)[,2]/aggregate(Y[metric], by = Y['ID'], FUN=sum)[,2]
	
	ind
		}

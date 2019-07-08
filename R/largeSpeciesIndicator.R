#'@title Calculates the Large Species Indicator
#'@description This function takes a dataframe with columns **** and calculates
#'  the Large Species Indicator (LSI)
#'@details Large Species Indicator (LSI): \deqn{LSI = \Sigma B_i(L_{max} >85
#'  cm)/\Sigma B_i} where \eqn{B_i} is biomass of individual species, \eqn{i}, and
#'  \eqn{L_{max}} is the maximum asymptotic length (cm; here the default is 85
#'  cm).
#'
#'  Recommended data: Fishery independent surveys, fish.
#'@param X add text here
#'@param lmax (set to 85)
#'@param metric add text here
#'@param linf.data.table to delete
#'@family ecosystem structure and function indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Shephard S, Fung T, Houle JE, Farnsworth KD, Reid DG, Rossberg AG (2012)
#'  Size-selective fishing drives species composition in the Celtic Sea. ICES J
#'  Mar Sci 69:223-234 
#'
#'  Houle JE, Farnsworth KD, Rossberg AG, Reid DG (2012) Assessing the
#'  sensitivity and specificity of fish community indicators to management
#'  action. Can J Fish Aquat Sci 69:1065-1079
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}
#'@export



largeSpeciesIndicator <- function(X, lmax=85, linf.table = NA, metric='BIOMASS',
                                  start.year, end.year) {

  if (is.na(linf.table)) {
    load("R/sysdata.rda/indiseas_MaxLength.rda")
    ss <- indiseas_MaxLength$SPECIES[ol$MAXLEN99>lmax]
  }
  else{	ss <- linf.table$SPECIES[ol$MAXLEN99>lmax]}
 
  years = c(start.year:end.year)
  ind = data.frame(NULL)
	
  for (i in 1:length(years)){
    
    year.i = years[i]
    X.i = X[X$years, ]
    
    A.i <- sum(X.i[X.i$SPECIES %in% ss, metric])  # biomass of species > 85 cm
    B.i <- sum(X.i[, metric])                   # total biomass
    ind[i] = A.i/B.i
  }
	
  ind
	
}









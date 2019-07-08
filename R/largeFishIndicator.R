#'@title Calculates the Large Fish Indicator
#'@description This function takes a dataframe with columns **** and calculates
#'  the Large Fish Indicator (LFI)
#'@details Large Fish Indicator (LFI): \deqn{LFI = \Sigma B_m(L >50 cm)/\Sigma
#'  B_m} \eqn{B_m} is biomass of individuals in a body size class centred at
#'  mass m, and \eqn{L} is the length (cm) of an individual. This indicator describes
#'  the proportion (by weight) of the fish community that is larger than some
#'  length threshold (default here is 35 cm, **check this: i.e., the proportion
#'  of biomass occupying the top predator trophic level).
#'
#'  Recommended data: Fishery independent surveys, fish.
#'@param X add text here
#'@param metric add text here
#'@param large.fish say that set to 35
#'@family ecosystem structure and function indicators
#'@references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'  selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'  Aquat. Sci. 3232: xii + 212 p.
#'
#'  Greenstreet SP, Fraser HM, Rogers SI, Trenkel VM, Simpson SD, Pinnegar JK
#'  (2012) Redundancy in metrics describing the composition, structure, and
#'  functioning of the North Sea demersal fish community. ICES J Mar Sci 69:8-22
#'
#'  Houle JE, Farnsworth KD, Rossberg AG, Reid DG (2012) Assessing the
#'  sensitivity and specificity of fish community indicators to management
#'  action. Can J Fish Aquat Sci 69:1065-1079
#'@author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'  \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#'@export



largeFishIndicator <- function(X, metric=c('BIOMASS','ABUNDANCE'), large.fish = 35, start.year, end.year) {
  
  years = c(start.year:end.year)
  ind = data.frame(NULL)
  
  for (i in 1:length(years)) {
    
    year.i = years[i]
    X.i <- X[X$year = year.i, ]
    
    LF <- X.i$FLEN >= large.fish        # check that this subsetting works
    ind[i] <- sum(X.i[LF, metric])/sum(X.i[,metric])
  }
  
  ind
}







#' @title Also calculates Margalef's species richness (see Margalef.R)
#' @description This function takes a dataframe with columns **** and calculates
#'   Margalef's species richness
#' @details Margalef's species richness: \deqn{S_{Marg} = (S_y - 1)/log(F_y)}
#'   \eqn{S_y} is the count of the number of species recorded in all trawl
#'   catches collected in year \eqn{y}. \eqn{F} is the total count of all
#'   individuals caught in year \eqn{y}.
#'
#'   **Recommended data: Fishery independent surveys, fish and invertebrates.
#'
#' @param X is probably a dataframe with certain columns.
#' @param group is where you select which groups to include
#' @param metric is where you choose if you want to calculate using biomass or
#'   abundance
#' @family biodiversity indicators
#' @references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Greenstreet SP, Fraser HM, Rogers SI, Trenkel VM, Simpson SD, Pinnegar JK
#'   (2012) Redundancy in metrics describing the composition, structure, and
#'   functioning of the North Sea demersal fish community. ICES J Mar Sci
#'   69:8–22
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export

margalef <- function(X, group=c('FINFISH','ALL'), metric=c('BIOMASS','ABUNDANCE'),
                                    years = c(start.year:end.year))  {
  # Need to put in a groundfish only statement
  if(group == 'FINFISH') X <- X[as.numeric(X$SPECIES)<1000,]
  
  zero_index = which(X[,metric] == 0)             # index of where metric observations are zero
  if(length(zero_index) > 0){                     # message showing number of observations removed
    X = X[-zero_index, ]                          # remove the rows where metric is zero 
    print(paste(length(zero_index), "observations of zero removed from metric")) 
  }
  
  source("R/speciesrichness.R") # do I need this?
  S <- speciesrichness(X = X, group = group, metric = metric, years = years) # calculate species richness for each year
  
  ind = vector(length = length(years))            # inititalize vector to store indicator values
  
  for(i in 1:length(years)){
    
    year.i = years[i]
    X.i = X[X$YEAR == year.i, metric]

    logF.i = log(sum(X.i)) 
    
    ind[i] = (S[i] - 1)/logF.i
  }
  
  ind
}

#' @title Calculates the invertebrate to demersal biomass ratio
#' @description This function takes a dataframe with columns **** and calculates
#'   the invertebrate to demersal biomass ratio
#' @details **Recommended data: Fishery independent surveys, fish and
#'   invertebrates.
#' @param X add text here
#' @param syear add text here
#' @family ecosystem structure and function indicators
#' @references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Bundy A, Heymans JJ, Morissette L, Savenkoff C (2009) Seals, cod and forage
#'   fish: A comparative exploration of variations in the theme of stock
#'   collapse and ecosystem change in four Northwest Atlantic ecosystems. Prog
#'   Oceanogr 81:188–206
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export

inv2dem <- function(X, years = c(start.year:end.year)) {
	
  #V <- data.frame(ID=NA,BIOMASS.x=NA,BIOMASS.y=NA,RATIO=NA)
  
  uI = unique(X$ID)                          # extract the spatial scale ID's
  ind <- NULL                                 # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){            # loop over all spatal scales
    
    X.j = X[X$ID == uI[j], ]          # subset data to spatial scale j
    
    for (i in 1:length(years)){                     # loop over each year
      
      year.i = years[i]                             # set years.i to current year  
      X.ij = X.j[X.j$YEAR == year.i, ]              # subset data to include only current year
      
      inv.i <- resourcePotential(X = X.ij, group='INVERTEBRATES', metric='BIOMASS')  # calculate biomass of invertebrates
      dem.i <- resourcePotential(X = X.ij, group='GROUNDFISH', metric='BIOMASS')     # calculate biomass of demersal fish
    
      ind.i = inv.i$BIOMASS / dem.i$BIOMASS         # calculate invertebrate to demersal ratio
      
      ind.i = data.frame(uI[j], year.i, ind.i)      # create a dataframe with spatial scale ID, year, and indicator value
       ind = rbind(ind, ind.i)                      # bind ind.i to ind dataframe
    
  }
}
names(ind) = c("ID", "YEAR", "Invert2Demersal")    # name the ind dataframe
ind                                                # return vector of indicator values for years c(start.year:end.year) 

}


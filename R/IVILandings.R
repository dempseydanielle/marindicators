#' @title Calculates the intrinsic vulnerability index of the catch
#' @description This function takes a dataframe with columns **** and calculates
#'   the intrinsic vulnerability index (IVI) of the catch
#' @details The weighted intrinsic vulnerability index (IVI) of species in the
#'   catch was estimated as: \deqn{IVI = \Sigma(IVI_j * C_j)/\Sigma C_j} where
#'   the sum is over all species, j. The vulnerability for each species was
#'   determined by considering several traits including maximum length, age at
#'   first maturity, longevity, von Bertalanffy growth parameter, natural
#'   mortality, fecundity, spatial behaviour and geographic range (e.g. species
#'   with larger body size, higher longevity, higher age at maturity, and lower
#'   growth rates have higher vulnerability indexes and should be less able to
#'   sustain high fishing mortality; Cheung et al. 2005). The index values
#'   ranges from 1 to 100, with 100 being the most vulnerable. See Appendix 1
#'   from Cheung et al., 2007:
#'   \url{http://www.int-res.com/articles/suppl/m333p001_app.pdf}
#'
#'   Need to add here about the indiseasIVI and how if their region has
#'   different species they will have to make and save their own data table
#'   
#'   Need to make sure the wd is set to the Project Directory
#'
#'   Recommended data: Commercial fisheries landings, fish
#' @param land add text here
#' @param path add text --> note that option to put propotion of landings
#' @family stability and resistance indicators
#' @references  Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Cheung, William W. L., watson, Reg, Morato, Telmo, Pitcher, Tony J., Pauly,
#'   Daniel (2007) Intrinsic vulnerability in the global fish catch. Mar Ecol
#'   Prog Ser 333: 1 - 12
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export


IVILandings <- function(land, IVI.table = "scotianshelf", prop.land.table = "scotianshelf",
                        years = c(start.year:end.year)) {
  
  if (prop.land.table == "scotianshelf"){            # if analyzing Scotian Shelf, import built-in data
    load("R/sysdata.rda/indiseas_proplandings.rda")  # for species that are not identified to the lowest level
    prop.land.table = prop.land                      # in the landings data, estimate the proportion of each 
    names(prop.land.table)[1] <- "SPECIES"           # landed
    rm(prop.land)
  }
  
  if (IVI.table == "scotianshelf"){                  # if analyzing Scotian Shelf, import built-in data
    load("R/sysdata.rda/indiseas_IVI.rda")           # Instrinsic vulnerability Index for Scotian Shelf species
    IVI.table = indiseasIVI
    rm(indiseasIVI)
  }

  land <- merge(land, prop.land.table)    # maybe add by = "SPECIES" here
  land <- merge(land, IVI.table)          # , by = "SPECIES"
  
  if(exists("prop.land.table")){
    land$CATCH <- land$CATCH * land$PROPORTION_OF_LANDINGS  # multiply catch by proportion of landings
  }                                                         # if required
  
  land$IV_num <- land$CATCH * land$IVI                      # multiply catch by IVI (for numerator)
  
  uI = unique(land$ID)                   # extract the spatial scale ID's
  ind <- NULL                            # initialize dataframe for storing indicator values
  
  for (j in 1:length(uI)){               # loop over all spatal scales
    
    land.j = land[land$ID == uI[j], ]    # subset data to spatial scale j
    
    for (i in 1:length(years)){          # loop over each year
      
      year.i = years[i]                             # set years.i to current year  
      land.ij = land.j[land.j$YEAR == year.i, ]     # subset data to include only current year
      
      ind.i <- merge(aggregate(IV_num ~ YEAR + ID, data = land.ij, FUN = sum), # calculate sum over all species of C*IVI
               aggregate(CATCH ~ YEAR + ID, data = land.ij, FUN=sum))          # calculate sum over all species of C
      
      ind.i <- ind.i[,3]/ind.i[,4]                                             # calculate IVI of landings
      
      ind.i = data.frame(uI[j], year.i, ind.i)     # create a dataframe with spatial scale ID, year, and indicator value
      ind = rbind(ind, ind.i)                      # bind ind.i to ind dataframe
    }
  }
  
  names(ind) = c("ID", "YEAR", "IVILandings")          # name the ind dataframe
  ind                                                  # return dataframe for years c(start.year:end.year) 
  
}







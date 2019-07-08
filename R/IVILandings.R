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
#' @param path add text
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


# maybe add a path to the IVI data here
# but they can use their own data if they have it
# maybe IVI = NULL
# if IVI = NULL(get the data from somewhere in the package)
IVILandings <- function(land = dat, IVI.table = NA, prop.land = NA) {
	
	prop.land = read.csv("extra info/indiseas_allcodes2res.csv")	# take this out of final version! 
	# Shoudl be included from main script as an argument 
  # or just write something in to be the proporation of the catch to make more general?
	if (is.na(IVI.table)) load("R/sysdata.rda/indiseasIVI.rda")
	
  names(prop,land)[1]<-'SPECIES'
		#In Land there duplicate entires for some species which allows for proportions of total landings to be calucaulted  as aggregate(LAND*PROPORTION_OF_LANDINGS~YEAR,data=Land,FUN=sum)
	# DD: seems like this is not very generic! might need to change
	
	# will have to put in a warning if there are species in the dataframe that
	# are not included in indiSeasIVI
	Land <- merge(land, prop.land)
		if(is.na(path)) IVI <- sqlQuery(channel,paste('select * from indiseas_IVI;'))
		if(!is.na(path)) IVI <- read.csv(file.path(path,"extra info","indiseasIVI.csv"))
		
	  IV <- merge(Land,IVI)
		IV$IV1 <- IV$CATCH*IV$PROPORTION_OF_LANDINGS*IV$IVI
		IV$CI <- IV$CATCH*IV$PROPORTION_OF_LANDINGS
		
		IV2 <- merge(aggregate(IV1~YEAR+NAMES,data=IV,FUN=sum),aggregate(CI~YEAR+NAMES,data=IV,FUN=sum))
		IV2$INDI <- IV2[,3]/IV2[,4]
		return(IV2[,c('YEAR','NAMES','INDI')])
}







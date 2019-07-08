#' @title Calculates the Fishing-in-Balance Index
#' @description This function takes a dataframe with columns **** and calculates
#'   the Fishing-in-Balance (FiB) Index
#' @details Fishing-in-Balance (FiB) Index: \deqn{FiB = log(Y_k*(1/TE)^{TL_k}) -
#'   log(Y_0 * (1/TE)^{TL_0})} where \eqn{Y} is the catch, \eqn{TL} is the mean
#'   trophic level in the catch, \eqn{TE} is the transfer efficiency, \eqn{k} is
#'   any year, and 0 refers to any year used as a baseline. By default, \eqn{TE}
#'   is set to 0.10 (Pauly and Christensen 1995).
#'
#'   This indicator captures changes in fishing strategies and their impact on
#'   system productivity: a positive FiB index indicates that the fishery has
#'   expanded and/or bottom-up effects are occurring, and there is more catch
#'   than expected, while a negative FiB index indicates it is likely that the
#'   fishing impact is so high that the ecosystem function is impaired and the
#'   ecosystem is less productive owing to excessive fishery removals
#'   (Christensen 2000, Fu et al. 2012).
#'
#'   **Recommended data: Commercial fisheries landings, fish and invertebrates.
#' @param X add text here
#' @param metric add text here
#' @param gp add text here
#' @param yr add text here
#' @param user.defined add text here
#' @param group add text here
#' @param path add text here
#' @importFrom stats aggregate
#' @family resource potential indicators
#' @references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Christensen 2000 (not in Tech Report refs)
#'
#'   Fu C, Gaichas S, Link JS, Bundy A, Boldt JL, Cook AM, Gamble R, Utne KR,
#'   Liu H, Friedland KD (2012) Relative importance of fisheries, trophodynamic
#'   and environmental drivers in a series of marine ecosystems. Mar Ecol Prog
#'   Ser 459:169–184
#'
#'   Pauly D, Christensen V, Walters C (2000) Ecopath, Ecosim, and Ecospace as
#'   tools for evaluating ecosystem impact of fisheries. ICES J Mar Sci
#'   57:697–706
#'
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}, Catalina Gomez
#' @export

# give option to CHOOSE years for the baseline --> base_start; base_end
FishingInBalance<- function (land = dat, TE = 0.1, base.start, base.end, start.year, end.year) {
	
  years = c(start.year:end.year)
  
  mTL <- MeanTLLandings(land = land) # calculate the mean trophic level of the landings for all years??
	land.total <- stats::aggregate(CATCH ~ YEAR, data = land, FUN=sum) #  I think this is just total landings for each year
	
	land.0 <- aggregate(CATCH~NAMES,data=ll[ll$YEAR %in% base.start:base.end,], FUN=mean) # BASELINE catch
 	mTL.0 <- aggregate(mTL~NAMES,data=mTL[mTL$YEAR %in% base.start:base.end,], FUN=mean) # BASELINE Mean trophic level
	
 	# loop over all years
	ind <- data.frame(NULL)
	for (i in 1:length(years)){
	  ind[i] <- log(land.total$CATCH[i]*(1/TE)^mtl[i]) - log(land.0*(1/TE)^mTL.0) # could subtract the baseline
	  # from each element of dataframe at the end, but I like to keep together for the equation
	}
	
	ind
}







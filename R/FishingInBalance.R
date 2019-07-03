#' @title Calculates the Fishing-in-Balance Index
#' @description This function takes a dataframe with columns **** and calculates
#'   the Fishing-in-Balance (FiB) Index
#' @details Fishing-in-Balance (FiB) Index: \deqn{FiB = log(Y_k*(1/TE)^(TL_k)) -
#'   log(Y_0 * (1/TE)^(TL_0))} where \eqn{Y} is the catch, TL is the mean
#'   trophic level in the catch, TE is the transfer efficiency, k is any year
#'   and 0 refers to any year used as a baseline. By default, TE is set to 0.10
#'   (Pauly and Christensen 1995).
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
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export


FishingInBalance<- function (land=dat,TE=0.1) {
	mTL <- MeanTLLandings(land=land)
	ll <- aggregate(CATCH~YEAR+NAMES,data=land,FUN=sum)
	l0 <- aggregate(CATCH~NAMES,data=ll[ll$YEAR %in% 1968:1970,],FUN=mean)
	mTL0 <- aggregate(mTL~NAMES,data=mTL[mTL$YEAR %in% 1968:1970,],FUN=mean)
	
	nn <- unique(l0$NAMES)
	outs <- list()
		for(i in 1:length(nn)) {
			lan <- ll[ll$NAMES==nn[i],'CATCH']
			yy <- ll[ll$NAMES==nn[i],'YEAR']
			mtl <- mTL[mTL$NAMES==nn[i],'mTL']
			mtlo <- mTL0[mTL0$NAMES==nn[i],'mTL']
			llo <- l0[l0$NAMES==nn[i],'CATCH']
		outs[[i]] <- data.frame(NAMES=rep(nn[i],length(lan)),YEAR=yy,INDI=log(lan*(1/TE)^mtl)-log(llo*(1/TE)^mtlo))	
		}
	return(do.call(rbind,outs))
}

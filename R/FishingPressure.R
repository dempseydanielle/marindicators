#' @title Calculates fishing pressure
#' @description This function takes a dataframe with columns **** and calculates
#'   fishing pressure on a fished group
#' @details Fishing pressure (FP): \deqn{FP = Y_FG/B_FG} where \eqn{B_FG} is the
#'   biomass of the fished group(s) and \eqn{Y_FG} is the landed catch of the
#'   group(s). **Give examples of groups that can be used in function
#'
#'   This indicator measures the level of exploitation or total fishing pressure
#'   at the ecosystem level. Change in this indicator can result from change in
#'   \eqn{B_FG}, \eqn{Y_FG} or both. If \eqn{B_FG} and \eqn{Y_FG} change in the
#'   same direction, exploitation rate may not change.
#'
#'   Recommended data: B: fishery independent surveys, Y: commercial fisheries
#'   landings
#' @param path add text here
#' @param land add text here
#' @param group add text here
#' @param groups add text here
#' @param qadj add text here
#' @param qadjPostStrat add text here
#' @family fishing pressure indicators
#' @references Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
#'   selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
#'   Aquat. Sci. 3232: xii + 212 p.
#'
#'   Shin YJ, Bundy A, Shannon LJ, Simier M, Coll M, Fulton EA, Link JS, Jouffre
#'   D, Ojaveer H, MacKinson S, Heymans JJ, Raid T (2010) Can simple be useful
#'   and reliable? Using ecological indicators to represent and compare the
#'   states of marine ecosystems. ICES J Mar Sci 67:717–731
#'
#' @author  Danielle Dempsey, Alida Bundy, Adam Cooke, Mike McMahon,
#'   \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export




FishingPressure <- function(path,land=dat,group,groups,qadj=T,qadjPostStrat=qadjPostStrat) {
	la  <- LandByGroup(land,group)
	out <- indicatorSpaceTime(path=path,indicator='resourcePotential',ags=c(group,'BIOMASS',user.defined=F),groups=groups,qadjusted=qadj,saveIndicatorData=F,yrs=1970:2015,qadjPostStrat=qadjPostStrat)
	names(la)[which(names(la)=='NAMES')] <- 'ID'
	fp <- merge(out,la,by=c('YEAR','ID'))
	fp$FP <- fp$CATCH/fp$BIOMASS
	return(fp[,c('YEAR','ID','FP')])
	}
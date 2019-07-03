#' @title Calculates inverse fishing pressure
#' @description This function takes a dataframe with columns **** and calculates
#'   inverse fishing pressure on a fished group (***not in Table S4)
#' @details Fishing pressure (InverseFP): \deqn{FP = B_FG/Y_FG} where \eqn{B_FG} is the
#'   biomass of the fished group(s) and \eqn{Y_FG} is the landed catch of the
#'   group(s). **Give examples of groups that can be used in function
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

InverseFishingPressure <- function(path,land=dat,group,groups,qadj=T,qadjPostStrat=qadjPostStrat) {
	out <- FishingPressure(path,land,group,groups,qadj=qadj,qadjPostStrat=qadjPostStrat)
	out$FP <- 1/out$FP
	return(out)
}
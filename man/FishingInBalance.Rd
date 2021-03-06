% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FishingInBalance.R
\name{fishingInBalance}
\alias{fishingInBalance}
\title{Calculates the Fishing-in-Balance Index}
\usage{
fishingInBalance(
  land,
  TL.table,
  minTL = 0,
  TE = 0.1,
  base.start,
  base.end,
  years
)
}
\arguments{
\item{land}{A dataframe of commercial landings data with columns \code{YEAR},
\code{ID}, \code{SPECIES} and \code{CATCH}. \code{YEAR} indicates the year
the landing was recorded, \code{ID} is an area code indicating where the
landing was recorded, \code{SPECIES} is a numeric code indicating the
species landed, and \code{CATCH} is the corresponding landed weight.}

\item{TL.table}{A dataframe with columns \code{SPECIES} and the corresponding
\code{TL_LAND} (trophic level). Entries in the \code{SPECIES} column should
be the unique values of species codes in \code{land} (or a subset thereof).
Other columns in \code{TL.table} are ignored.}

\item{minTL}{The minimum trophic level of species to include. Default is
\code{minTL = 0}.}

\item{TE}{Trophic efficiency. Default is \code{TE = 0.1}, i.e., a trophic
efficiency of 10\%.}

\item{base.start}{Year indicating the beginning of the baseline period. The
average landings and average mean trophic level of the landings over the
baseline period are used as baseline values to calculate FiB (see Details).
\code{land} must include data for the baseline period.}

\item{base.end}{Year indicating the end of the baseline period. The average
landings and average mean trophic level of the landings over the baseline
period are used as baseline values to calculate FiB (see Details).
\code{land} must include data for the baseline period.}

\item{years}{A vector of years for which to calculate indicator.}
}
\value{
Returns a dataframe with three columns: \code{ID}, \code{YEAR}, and
 \code{FishinginBalance}.

 If there are no observations in land for spatial scale \eqn{j} and year
 \eqn{i}, indicator value is set to \code{NA}.
}
\description{
This function calculates the Fishing-in-Balance (FiB) Index of
 fisheries landings for \eqn{j} areas and \eqn{i} years.
}
\details{
Fishing-in-Balance (FiB) Index: \deqn{FiB = log(Y_k*(1/TE)^{TL_k}) -
 log(Y_0 * (1/TE)^{TL_0})} where \eqn{Y} is the catch, \eqn{TL} is the mean
 trophic level in the catch, \eqn{TE} is the transfer efficiency, \eqn{k} is
 any year, and 0 refers to any year used as a baseline. By default, \eqn{TE}
 is set to 0.10 (Pauly and Christensen 1995).

 This indicator captures changes in fishing strategies and their impact on
 system productivity: a positive FiB index indicates that the fishery has
 expanded and/or bottom-up effects are occurring, and there is more catch
 than expected, while a negative FiB index indicates it is likely that the
 fishing impact is so high that the ecosystem function is impaired and the
 ecosystem is less productive owing to excessive fishery removals (Pauly et
 al., 2000).
}
\examples{
data(land)
data(species.info)
fishingInBalance(land, TL.table = species.info, minTL = 0, TE = 0.1,
   base.start = 2014, base.end = 2015, years = c(2014:2019))
}
\references{
Bundy A, Gomez C, Cook AM. 2017. Guidance framework for the
 selection and evaluation of ecological indicators. Can. Tech. Rep. Fish.
 Aquat. Sci. 3232: xii + 212 p.

 Pauly D, Christensen V, Walters C. 2000. Ecopath, Ecosim, and Ecospace as
 tools for evaluating ecosystem impact of fisheries. ICES J Mar Sci 57:697
 706
}
\seealso{
Other resource potential indicators: 
\code{\link{allPotential}()},
\code{\link{resourcePotential}()}
}
\author{
Danielle Dempsey, Adam Cook \email{Adam.Cook@dfo-mpo.gc.ca},
 Catalina Gomez, Alida Bundy
}
\concept{resource potential indicators}

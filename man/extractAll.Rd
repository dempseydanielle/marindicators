% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractAll.R
\name{extractAll}
\alias{extractAll}
\title{Calculates indicators from all attributes}
\usage{
extractAll(
  X,
  X_length,
  land,
  speciesinfo.table,
  species.table,
  years,
  metric.bio = "ABUNDANCE",
  group.bio = "ALL",
  percentiles = c(0.25, 0.75),
  minTL.bio = 0,
  LSI.group,
  max.length = 85,
  LFI.group,
  large.fish = 35,
  LenWt.table,
  guild.groups,
  condition.groups,
  ratio.groups,
  maxlength.group,
  TL.grouping = 1,
  wind = 5,
  negative = FALSE,
  resource.groups,
  minTL.FiB = 0,
  TE = 0.1,
  base.start,
  base.end,
  landings.groups,
  FP.groups,
  minTL.FP = c(0, 3.25),
  raw = TRUE,
  std = TRUE,
  glob.env = TRUE,
  export.path = NULL,
  export.id = NULL
)
}
\arguments{
\item{X}{A dataframe of fishery independent data derived from research vessel
survey data or model output, with columns \code{YEAR}, \code{ID},
\code{SPECIES}, \code{BIOMASS} and \code{ABUNDANCE}. \code{YEAR} indicates
the year the observation was recorded, \code{ID} is an area code indicating
where the observation was recorded, \code{SPECIES} is a numeric code
indicating the species sampled, and \code{ABUNDANCE} is the corresponding
abundance (stratified and corrected for catchability as required).}

\item{X_length}{A dataframe of fishery independent data derived from research
vessel survey data or model output, with columns \code{YEAR}, \code{ID},
\code{SPECIES}, \code{LENGTH}, \code{BIOMASS} and \code{ABUNDANCE}.
\code{YEAR} indicates the year the observation was recorded, \code{ID} is an
area code indicating where the observation was recorded, and \code{SPECIES}
is a numeric code indicating the species sampled. \code{LENGTH} is the
length class (cm) and \code{ABUNDANCE} is the corresponding abundance at
length (stratified and corrected for catchability as required). Species for
which there are no length data should be assigned \code{LENGTH = -99}. These
observations are removed by the function.}

\item{land}{A dataframe of commercial landings data with columns \code{YEAR},
\code{ID}, \code{SPECIES} and \code{CATCH}. \code{YEAR} indicates the year
the landing was recorded, \code{ID} is an area code indicating where the
landing was recorded, \code{SPECIES} is a numeric code indicating the
species landed, and \code{CATCH} is the corresponding landed weight. If
\code{land = NULL}, the landings-based indicators will not be calculated.}

\item{speciesinfo.table}{A table with columns \code{SPECIES} and the
corresponding \code{TL}, \code{MAXLENGTH}, \code{MAXAGE},  \code{IVI}, and
\code{TL_LAND} (trophic level, maximum recorded age, maximum recorded
length, intrinsic vulnerability index, and trophic level of the landings).
Entries in the \code{SPECIES} column should be the unique values of species
codes in \code{X}/\code{X_length} (or a subset thereof). If there are
different species codes in \code{X} and \code{land}, the Fishing-in-Balance,
Intrinsic Vulnerability Index of Landings, Mean Trophic Level of the
Landings, and Marine Trophic Index should be calculated using their
respective single functions (see manual or vignette).}

\item{species.table}{A table where the column names match the entries in
\code{condition.groups}, \code{FP.groups}, \code{group.bio},
\code{guild.groups}, \code{landings.group}, \code{LFI.group},
\code{LSI.group}, \code{maxlength.group}, \code{ratio.groups}, and
\code{resource.groups}. Column entries are the species codes
indicating the species from \code{X} (or \code{X_length}) included in each
group. \code{species.table} may also include columns for other species
groups; these will be ignored.}

\item{years}{A vector of years for which to calculate indicators.}

\item{metric.bio}{A character string indicating which column in \code{X} to
use to calculate the biodiversity indicators. Default is \code{metric =
"ABUNDANCE"}.}

\item{group.bio}{A character string indicating which species to include in the
calculation of the Biodiversity indicators. If \code{group = "ALL"}, all
species will be included; otherwise, \code{group.bio} should match a column
name in \code{species.table}. Default is \code{group.bio = "ALL"}.}

\item{percentiles}{The percentiles used to determine R1 and R2 for calculating
Kempton's Q. Default is \code{percentiles = c(0.25, 0.75)}.}

\item{minTL.bio}{Minimum trophic level for species included to calculate
Kempton's Q. Default is \code{minTL.bio = 0}.}

\item{LSI.group}{A character string indicating the species group for which to
calculate the Large Species Indicator. Must be set to \code{"ALL"} or match
a column name in \code{species.table}. If \code{LSI.group = NULL}, the Large
Species Indicator will not be calculated.}

\item{max.length}{The threshold for large species (cm). Default is
\code{max.length = 85} (i.e., large species are those with \code{MAXLENGTH}
>= 85 cm).}

\item{LFI.group}{A character string indicating the species group for which to
calculate the Large Fish Indicator. Must be set to \code{"ALL"} or match a
column name in \code{species.table}. If \code{LFI.group = NULL}, the Large
Fish Indicator will not be calculated.}

\item{large.fish}{Threshold for large fish (cm). Default is \code{large.fish =
35} (i.e., large fish are those with \code{X_length$LENGTH} >= 35 cm).}

\item{LenWt.table}{A table of annual length at weight data with 5 columns.
\code{YEAR}, \code{ID}, and \code{SPECIES} are as described in
\code{X_length}. \code{LENGTH} is fish length at the corresponding
\code{WEIGHT} (fish weight).}

\item{guild.groups}{A vector indicating the species groups for which to
calculate the resource potential. Each entry must be a character string
matching the name of a column in \code{species.table}. If \code{guild.groups
= NULL}, these indicators will not be calculated.}

\item{condition.groups}{A vector indicating the species groups for which to
calculate Fulton's community condition factor. Each entry must be a
character string matching the name of a column in \code{species.table}. If
\code{condition.groups = NULL}, the community condition indicators will not
be calculated.}

\item{ratio.groups}{A dataframe with two columns, which must be named
\code{group1} and \code{group2}. Each row holds the group names for one
biomass ratio, with the numerator in column \code{group1} and the
denominator in column \code{group2}. Each entry must be a character string
matching the name of a column in \code{species.table} or \code{"ALL"}. If
\code{ratio.groups = NULL}, biomass ratio indicators will not be calculated.}

\item{maxlength.group}{A character string indicating the species group for
which to calculate the mean maximum length of fish in the community. Must be
set to \code{"ALL"} or match a column name in \code{species.table}. If
\code{maxlength.group = NULL}, the mean maximum length will not be
calculated.}

\item{TL.grouping}{Size of the trophic level bin for which to aggregate
biomass when calculating Biomass per Trophic Level. For example, if
\code{TL.grouping = 1}, trophic levels are binned from 1.00 - 1.99, 2.00 -
2.99, etc. If TL.grouping = 0.5, trophic levels are binned from 1.00 - 1.49,
1.50 - 1.99, 2.00 - 2.49, 2.50 - 2.99, etc. Default is \code{TL.grouping =
1} so that biomass is aggregated over discrete trophic levels.}

\item{wind}{Window for the moving average used to calculate the Coefficient of
Variation of the Biomass. The first and last \code{floor(wind/2)} values of
the indicator are assigned \code{NA} to account for the moving average.
Default is \code{wind = 5} years.}

\item{negative}{If \code{negative = TRUE}, the Coefficient of Variation of the
Biomass and the Intrinsic Vulnerability Index of the Landings will be
multiplied by -1 so that their expected response is to decrease with
increasing fishing pressure. Default is \code{negative = FALSE}.}

\item{resource.groups}{A vector indicating the species groups for which to
calculate the resource potential. Each entry must be a character string
matching the name of a column in \code{species.table}. If
\code{resource.groups = NULL}, these indicators will not be calculated.}

\item{minTL.FiB}{The minimum trophic level of species to include to calculate
Fishing-in-Balance.}

\item{TE}{Trophic efficiency, used to calculate FiB. Default is \code{TE =
0.1}, i.e., a trophic efficiency of 10\%. If \code{TE = NULL}, FiB will not
be calculated.}

\item{base.start}{Year indicating the beginning of the baseline period for
calculating FiB. The average landings and average mean trophic level of the
landings over the baseline period are used as baseline values to calculate
FiB. \code{land} must include data for the baseline period. If
\code{base.start = NULL}, FiB will not be calculated.}

\item{base.end}{Year indicating the end of the baseline period for calculating
FiB. The average landings and average mean trophic level of the landings
over the baseline period are used as baseline values to calculate FiB.
\code{land} must include data for the baseline period. If \code{base.end =
NULL}, FiB will not be calculated.}

\item{landings.groups}{A vector indicating the species groups for which to
calculate the landings. Each entry must be a character string matching the
name of a column in \code{species.table}. If \code{landings.groups = NULL},
no Landings indicators will be calculated.}

\item{FP.groups}{A dataframe with two columns, which must be named
\code{group.land} and \code{group.X}. Each row holds the group names to
calculate the fishing pressure on a target group, with the numerator in
column \code{group.land} and the denominator in column \code{group.X}. Each
entry must be a character string matching the name of a column in
\code{species.table}. If \code{FP.groups = NULL}, no fishing pressure
indicators will be calculated.}

\item{minTL.FP}{A vector containing minimum trophic level to include when
calculating the mean trophic level of the landings. Default is \code{minTL =
c(0, 3.25)}, which will return the mean trophic level of the landings and
the marine trophic index.}

\item{raw}{A logical value. If \code{raw = TRUE}, the raw indicator values are
returned by the function. If \code{raw = FALSE}, the raw indicator values
are not returned. Default is \code{raw = TRUE}. Either \code{raw} or
\code{std} must be \code{TRUE}.}

\item{std}{A logical value. If \code{std = TRUE}, the standardized indicator
values for each area \code{ID} are returned by the function. Indicators are
standardized using z-scores, i.e., by subtracting the mean and dividing by
the standard deviation (ignoring \code{NA} values). If \code{std = FALSE},
the standardized indicator values are not returned. Default is \code{std =
TRUE}. Either \code{raw} or \code{std} must be \code{TRUE}.}

\item{glob.env}{Logical value indicating whether to return output to global
environment. Default is \code{glob.env = TRUE}.}

\item{export.path}{File path indicating where to save a .csv file of
calculated indicators (named allIndicators_export.id.csv; see below). If
\code{export.file = NULL}, the indicator dataframe will not be exported as a
.csv file. Default is \code{export.path = NULL}.}

\item{export.id}{Character string to modify the name of the .csv file (if
export.path is specified), for example an area name or date of analysis. The
exported .csv file is named allIndicators_export.id.csv. Default is
\code{export.id = NULL}.}
}
\value{
Returns a dataframe with columns \code{ID}, \code{YEAR}, and
 indicators corresponding to the arguments supplied to the function.
 Standardized indicators are noted with "_s" in the name.
}
\description{
This function calculates all of the indicators described in this
 package. The user can choose whether the function returns the indicator
 dataframe to the global environment, exports the dataframe to a .csv file,
 or both. The user can also choose whether the function returns the raw
 indicator values, the standardized (z-score) values, or both.
}
\details{
This function calculates the indicators for each attribute and
 pressure: Biodiversity, Structure and Functioning, Stability and Resistance,
 Resource Potential, and Fishing Pressure.

 See the help file for the individual functions for information on how each
 indicator is calculated.
}
\examples{
# Compile data
data(X)
data(X_length)
data(land)
data(species.table)
data(species.info)
data(Length_Weight)

# Species groups of interest
trophicguild.groups <- c("LBENTHIVORE", "MBENTHIVORE", "PISCIVORE", "PLANKTIVORE",
   "ZOOPISCIVORE")
condition.groups <- c("FINFISH", "PISCIVORE", "PLANKTIVORE", "ZOOPISCIVORE")
resource.groups <- c("ALL", "CLUPEIDS", "FINFISH", "FLATFISH", "FORAGE",
   "GADOIDS", "GROUNDFISH", "PELAGIC", "SKATES")
ratio.groups <- data.frame(rbind(c("PELAGIC", "GROUNDFISH"), c("PREDATORS", "ALL")))
names(ratio.groups) <- c("group1", "group2")

landings.groups <- c("ALL", "CLUPEIDS.L", "FLATFISH.L", "GROUNDFISH.L")
FP.groups <- data.frame(rbind(c("ALL", "ALL"),
    c("CLUPEIDS", "CLUPEIDS.L"),
    c("FLATFISH", "FLATFISH.L"),
    c("GROUNDFISH", "GROUNDFISH.L")))
names(FP.groups) <- c("group.X", "group.land")

# Calculate raw and standardized indicators
extractAll(X = X, X_length = X_length, land = land,
    speciesinfo.table = species.info, species.table = species.table, years = c(2014:2019),
    LSI.group = "ALL", LFI.group = "ALL", LenWt.table = Length_Weight,
    guild.groups = trophicguild.groups, condition.groups = condition.groups,
    ratio.groups = ratio.groups,
    maxlength.group = "FINFISH",
    resource.groups = resource.groups,
    minTL.FiB = 0, base.start = 2014, base.end = 2015,
    landings.groups = landings.groups, FP.groups = FP.groups,
    raw = TRUE, std = TRUE, export.path = NULL)
}
\author{
Danielle Dempsey, Adam Cook \email{Adam.Cook@dfo-mpo.gc.ca},
 Catalina Gomez, Alida Bundy
}

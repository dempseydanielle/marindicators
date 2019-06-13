.onAttach <- function(libname, pkgname) {
  localVer = utils::packageDescription('indicators')$Version
  packageStartupMessage(paste0("Version: ", localVer))
}

.onLoad <- function(libname, pkgname){
  options(stringsAsFactors = FALSE)
}

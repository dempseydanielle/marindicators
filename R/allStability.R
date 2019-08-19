


allStability <- function(X, X_length, land, 
                         species.table, speciesinfo.table,  
                         TL.grouping = 1, window = 5, negative = FALSE, years){
  
  if("BIOMASS" %in% colnames(X)) {
    inds <- createDataframe(unique(X$ID), years)
  } else {if("BIOMASS" %in% colnames(X_length)) {
      inds <- createDataframe(unique(X_length$ID), years)
  } else {inds <- createDataframe(unique(land$ID), years)}
  }
  
  if("BIOMASS" %in% colnames(X)){
    
    # CV biomass
    CV_bio = CVBiomass(X, window = 5, negative = negative, years = years)
    inds <- merge(inds, CV_bio)
    
    # Max LifeSpan
    if("MAXAGE" %in% colnames(speciesinfo.table)){
      MMA = meanMaxAge(X, age.table = speciesinfo.table, "BIOMASS", years = years)
      
      inds <- merge(inds, MMA)
    }
    
    # Biomass per TL
    if("TL" %in% colnames(speciesinfo.table)){
      bio_TL = biomassPerTL(X, TL.table = speciesinfo.table, metric = "BIOMASS", 
                            TL.grouping = TL.grouping, years = years)
      inds <- merge(inds, bio_TL)
    }
  } 
  
  # Length-based indicators
  if("LENGTH" %in% colnames(X_length)){
    
    # Mean max length
    if("MAXLENGTH" %in% colnames(speciesinfo.table)){
      MML_bio = meanMaxLength(X_length, group = "FINFISH", species.table = species.table,
                        lmax.table = speciesinfo.table, metric = "BIOMASS", years = years)
      inds <- merge(inds, MML_bio)
 
      MML_abund = meanMaxLength(X_length, group = "FINFISH",  species.table = species.table,
                          lmax.table = speciesinfo.table, metric = "ABUNDANCE", years = years)
      inds <- merge(inds, MML_abund)
    }
  }
  
  # Landings-based indicators
  if("CATCH" %in% colnames(land)){
    
    # IVI Landings
    if("IVI" %in% colnames(speciesinfo.table)){
      IVI = IVILandings(land, IVI.table = speciesinfo.table, negative = negative, years = years)
      inds <- merge(inds, IVI)
    }
  }
  
  inds
}
  
  
  
  



multiBoyce <- function(spp, models, occ, Movwin = 100){
  boyces <- list()
  
  for(i in 1:length(spp)){
    svMisc::progress(i, max.value = length(spp))
    tmpModels <- models[[i]]
    tmpOCC <- occ[[i]][1:2]
    boyces[[i]] <- ecospat::ecospat.boyce(fit = tmpModels, obs = tmpOCC, res = Movwin, PEplot = TRUE)
  }
  names(boyces) <- spp
  return(boyces)
}

extractBOYCE <- function(spp, boyces){
  # F ratios
  ratios <- list()
  ratio2 <- list()
  ratio3 <- list()
  ratio4 <- list()
  ratio5 <- list()
  #Boyce metric
  boyce <- list()
  # Habitat suitability
  hs <- list()
  hs2 <- list()
  hs3 <- list()
  hs4 <- list()
  hs5 <- list()
  
  for(j in 1:length(boyces)){
    print(spp[j])
    # F ratios
    ratios[[j]] <- mean(boyces[[j]]$F.ratio)
    ratio2[[j]] <- sd(boyces[[j]]$F.ratio)
    ratio3[[j]] <- min(boyces[[j]]$F.ratio)
    ratio4[[j]] <- max(boyces[[j]]$F.ratio)
    ratio5[[j]] <- var(boyces[[j]]$F.ratio)
    # Boyce metric
    boyce[[j]] <- boyces[[j]]$Spearman.cor
    
    # Habitat suitability
    hs[[j]] <- mean(boyces[[j]]$HS)
    hs2[[j]] <- sd(boyces[[j]]$HS)
    hs3[[j]] <- min(boyces[[j]]$HS)
    hs4[[j]] <- max(boyces[[j]]$HS)
    hs5[[j]] <- var(boyces[[j]]$HS)
  }
  # F ratios
  ratioMEAN <- unlist(ratios)
  ratioSD <- unlist(ratio2)
  ratioMIN <- unlist(ratio3)
  ratioMAX <- unlist(ratio4)
  ratioVAR <- unlist(ratio5)
  # Boyce metric
  boyceMetric <- unlist(boyce)
  # Habitat suitability
  hsMEAN <- unlist(hs)
  hsSD <- unlist(hs2)
  hsMIN <- unlist(hs3)
  hsMAX <- unlist(hs4)
  hsVAR <- unlist(hs5)
  
  summaryBOYCE <- data.frame(cbind(spp, ratioMEAN, ratioSD, ratioMIN, ratioMAX, ratioVAR,
                        boyceMetric, hsMEAN, hsSD, hsMIN, hsMAX, hsVAR))
  names(summaryBOYCE) <- c("Species", "ratioMEAN", "ratioSD", "ratioMIN", "ratioMAX", "ratioVAR",
                           "Boyce", "hsMEAN", "hsSD", "hsMIN", "hsMAX", "hsVAR")
  return(summaryBOYCE)
  
}



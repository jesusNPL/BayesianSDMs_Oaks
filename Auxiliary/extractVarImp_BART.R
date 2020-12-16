
#direct <- "Models/"
#spps <- c("Quercus_agrifolia", "Quercus_alba", "Quercus_arizonica")

extractVARIMP_BART <- function(sppNames, directory) {
  source("https://raw.githubusercontent.com/cjcarlson/embarcadero/master/R/varimp.R")
  varIMPs <- list()

  for(i in 1:length(sppNames)) {
    print(sppNames[i])
    spp <- sppNames[i]
    mod <- readRDS(paste0(directory, spp, ".rds"))
    vartmp <- varimp(mod)
    vartmp$Species <- spp
    varIMPs[[i]] <- vartmp
  }
  varImp <- do.call(rbind, varIMPs)
  return(varImp)
}

varImp_oaks <- extractVARIMP_BART(sppNames = spps, directory = direct)

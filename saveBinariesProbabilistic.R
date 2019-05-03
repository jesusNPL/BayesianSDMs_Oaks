library(raster)
library(ecospat)

setwd("Ensembles/RS")

##### Load predictions #####
lf <- list.files(pattern = "tif$")
quercus <- stack(lf)

##### Load species occurrences #####
OCC <- read.csv("OCC/PAM_quercus_NEW_FINAL.csv")[, -1]
dim(OCC)
nombres <- names(OCC)
names(OCC) <- c("x", "y", nombres[3:134])

# Species names
sppnames <- names(quercus)

writeBinary2 <- function(pamOBS, sppStack, spNames, cover = 0.9){
  dir.create("Binaries2")
  for(sp in 1:length(spNames)){
    print(spNames[sp])
    occ <- pamOBS[spNames[sp]]
    occ <- cbind(pamOBS[, 1:2], occ)
    occ <- occ[occ[, 3] == 1, ]
    #names(occ) <- c("x", "y", spNames[sp])
    
    cutOFF <- ecospat.mpa(sppStack[[sp]], occ[, 1:2], perc = cover)
    sppBinary <- ecospat.binary.model(sppStack[[sp]], cutOFF)
    plot(sppBinary)
    writeRaster(sppBinary, filename = paste("Binaries2/", spNames[sp], sep = ""), format = "GTiff", overwrite = TRUE)
  }
}

writeBinary2(pamOBS = tm, sppStack = quercus, spNames = sppnames)


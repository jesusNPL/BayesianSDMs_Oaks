library(raster)
library(ecospat)

#setwd("Ensembles/RS")

##### Load predictions #####
#lf <- list.files(pattern = "tif$")
#quercus <- stack(lf)

##### Load species occurrences #####
#OCC <- read.csv("OCC/PAM_quercus_NEW_FINAL.csv")[, -1]
#dim(OCC)
#nombres <- names(OCC)
#names(OCC) <- c("x", "y", nombres[3:134])

#sppnames <- names(sppStack) # Species names

writeBinary2 <- function(pamOBS, sppStack, spNames, cover = 0.9){
  dir.create("Binaries2")
# for loop to assing species predictions and write predictions based on probabilities of occurrence
  for(sp in 1:length(spNames)){
    print(spNames[sp])
    # Separate individual species to establish cutoffs
    occ <- pamOBS[spNames[sp]]
    occ <- cbind(pamOBS[, 1:2], occ)
    occ <- occ[occ[, 3] == 1, ]
    # Make cutoffs
    cutOFF <- ecospat.mpa(sppStack[[sp]], occ[, 1:2], perc = cover)
    # reclassify continuous raster to a binary map using cutoffs
    sppBinary <- ecospat.binary.model(sppStack[[sp]], cutOFF)
    plot(sppBinary)
    # Save the binary map
    writeRaster(sppBinary, filename = paste("Binaries2/", spNames[sp], sep = ""), format = "GTiff", overwrite = TRUE)
    print("Binary predictions completed ! Check your results in the Binaries directory.")
  }
}

# Usage:
# 1. Load species occurrences, this must be an incidence matrix. This is the same incidence matrix that is used to make the species ensembles.
# 2. A raster stack of N species
# 3. Coverage "cover" or percentage of occurrences used for establish the cutoffs. 0.9 or 90% default 

#writeBinary2(pamOBS = OCC, sppStack = quercus, spNames = sppnames, cover = 1)


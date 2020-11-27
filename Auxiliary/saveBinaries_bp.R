#setwd("Ensembles/RS")

library(raster)
#lf <- list.files(pattern = "tif$")
#quercus <- stack(lf)

writeBinary <- function(sppStack, nSP, thr = 0.1){
  dir.create("Binaries")
  spnames <- names(sppStack)
  for(sp in 1:nSP){
    print(spnames[sp])
    spp <- sppStack[[sp]]
    spp[spp < thr] <- 0
    spp[spp > 0] <- 1
    plot(spp)
    writeRaster(spp, filename = paste("Binaries/", spnames[sp], sep = ""), format = "GTiff", overwrite = TRUE)
    print("Binary predictions completed ! Check your results in the Binaries directory.")
  }
}

#writeBinary(sppStack = quercus, nSP = length(names(quercus)), thr = 0.1)


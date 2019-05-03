

###Load layers (change pattern if not using ascii)###
layers <- list.files(path = "", pattern = 'asc', full.names = TRUE)
###Set working directory###
setwd("/Users/lewis_jones/Desktop/Practice")
###run for loop for binary conversion###
for (i in layers) { # look for each ascii file in layers
  binary<-raster(i)
  ###set desired threshold###
  binary[binary < 0.296927723] <- 0
  binary[binary > 0] <- 1
  plot(binary)
  name <- i
  writeRaster(binary, filename=name, format="ascii", overwrite=TRUE)
}
dev.off()

setwd("Ensembles/RS")
library(raster)
lf <- list.files(pattern = "tif$")
quercus <- stack(lf)

dir.create("Binaries")

writeBinary <- function(sppStack, nSP, thr = 0.1){
  spnames <- names(sppStack)
  for(sp in 1:nSP){
    print(spnames[sp])
    spp <- sppStack[[sp]]
    spp[spp < thr] <- 0
    spp[spp > 0] <- 1
    writeRaster(spp, filename = paste("Binaries/", spnames[sp], sep = ""), format = "GTiff", overwrite = TRUE)
  }
}

writeBinary(sppStack = quercus, nSP = length(names(quercus)), thr = 0.1)






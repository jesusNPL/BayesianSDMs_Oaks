#setwd("Ensembles/RS_WC")

library(raster)
library(letsR)

pamFromRaster <- function(sppStack, resol = 0.5, thr = 0.1, saveResults = FALSE){
  library(raster)
  # Prepare information
  r <- raster(extent(sppStack))
  res(r) <- resol
  valores <- values(r)
  xy <- xyFromCell(r, 1:length(valores))
  # Create a PAM
  tmp <- data.frame(extract(sppStack, xy))
  tmp[tmp > thr] <- 1
  tmp[is.na(tmp)] <- 0
  pam <- cbind(xy, tmp)
  if(saveResults == TRUE)
  {
  write.csv(pam, "PAM_from_ENM.csv")
  }
  return(pam)
}

# Example
#lf <- list.files(pattern = "tif$") # list of raster files stored in your hard drive
#quercus <- stack(lf)
#res(quercus)
#extent(quercus)

#quercusSR <- calc(quercus, fun = sum) # stacked species richnnes
#plot(quercusSR)

# To obtain a presence-abcense matrix (PAM) you just need to inform the next elements:
# 1. a stack object
# 2. the spatial resolution
# 3. Given that predictions under ENM are continuous, we need to specify a threshold to estimate the presence or absence of a determinate species in a given cell or pixel
# Important, changing resolution and threshold will produce different results.

#resul <- pamFromRaster(sppStack = quercus, resol = 0.25, thr = 0.1)

#resul[1:10, 1:10]
#sr <- rowSums(resul[3:130])
#sr2 <- sr[sr >= 1]

# Create an empty raster
#r <- raster(extent(quercus))
#res(r) <- 0.25
# Recover the species richness of your clade of interest using the PAM
#map <- rasterize(resul[1:2], r, field = sr, fun = sum)
#plot(map)

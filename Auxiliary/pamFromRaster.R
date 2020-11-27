setwd("Ensembles/RS_WC")
#source("https://raw.githubusercontent.com/jesusNPL/RS-SDM_ENM/master/PamFromRaster.R")

library(raster)
library(letsR)

pamFromRaster <- function(sppStack, resol = 0.5, thr = 0.1){
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
  return(pam)
}


lf <- list.files(pattern = "tif$")
quercus <- stack(lf)
res(quercus)
extent(quercus)
quercusSR <- calc(quercus, fun = sum)
plot(quercusSR)

resul <- pamFromRaster(sppStack = quercus, resol = 0.25, thr = 0.1)

resul[1:10, 1:10]
sr <- rowSums(resul[3:130])
sr2 <- sr[sr >= 1]

r <- raster(extent(quercus))
res(r) <- 0.25

map <- rasterize(resul[1:2], r, field = sr, fun = sum)
plot(map)



r <- raster(extent(quercus))
res(r) <- 0.25
#values(r) <- 0
valores <- values(r)
xy <- xyFromCell(r, 1:length(valores))


tmp <- extract(quercus, xy)
dim(xy)
tmp2 <- data.frame(tmp)
dim(tmp2)

tmp4 <- tmp2
tmp4[tmp4 > 0 ] <- 1
tmp4[is.na(tmp4)] <- 0
tmp3 <- cbind(xy, tmp4)
write.csv(tmp3, "pamPrueba.csv")

sr <- rowSums(tmp3[3:130])

map <- rasterize(xy, r, field = sr, fun = sum)
plot(map)

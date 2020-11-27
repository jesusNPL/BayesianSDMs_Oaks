library(sdm)
library(raster)

##### Occurrence and environmental data formated #####
modelsOAKS <- read.sdm("sdmData/sdmData_OAKS.sdm")
#sdmData_72SPP <- read.sdm("sdmData_72SPP.sdm")

setwd("EnviData/Yearly")

###### Environmental data #####
lf <- list.files(pattern = "tif$")
rs <- stack(lf)
plot(rs)

setwd("../..")

temp <- raster("EnviData/Temperature/MeanTemperature_correct.tif")

prec <- raster("EnviData/Precipitation/MeanPrecipitation_correct.tif")

alt <- raster("EnviData/Altitude_correct.tif")

rmEnvi <- ls()

envi <- stack(rs$LAI_MAX, rs$LAI_MEAN, rs$LAI_SEASON, rs$LAI_MIN, temp, prec, alt)
plot(envi)

rm(rmEnvi, temp, prec, alt, lf, rs)

##### Ensembles #####

# using the models ID 

# Multiple species
getModelInfo(modelsOAKS)

modelsInfo <- getModelInfo(modelsOAKS)
ids <- modelsInfo[1:2]

seq(from = 1, to = 16, each = 4)

##### Make ensambles #####
modelsInfo <- getModelInfo(modelsOAKS)
ids <- rep(split(seq_len(length(modelsInfo$modelID)), rep(1:128, each = 4)), each = 1)
ids

### Separate species models based in its ID
sppnames <- unique(modelsInfo$species)
sppnames

nSP <- length(unique(modelsInfo$species))

indModels <- list()

for(sp in 1:nSP){
  print(sppnames[sp])
  
  id <- as.numeric(ids[[sp]])
  
  indModels[[sp]] <- modelsOAKS[[id, drop = FALSE]]
}

rm(modelsOAKS)

### Save species ensambles one by one

for(spp in 1:length(indModels)){
  
  print(sppnames[spp])
  
  tmp <- indModels[[spp]]
  
  print(getModelInfo(tmp))
  
  ensemble(tmp, newdata = envi, filename = paste0("Ensembles/", sppnames[spp], ".tif", sep = ""), 
           setting = list(method = 'weighted', stat = 'TSS', opt = 2))
  
  print("Ensemble model completed ! Check your results in the Ensembles directory.")
}


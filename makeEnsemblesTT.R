library(sdm)
library(raster)

##### Occurrence and environmental data formated #####
modelsOAKS <- read.sdm("sdmData/sdmData_OAKS.sdm")

setwd("EnviData/Yearly")

###### Environmental data #####


##### Ensembles #####

modelsOAKS <- read.sdm("sdmData/sdmData_OAKS.sdm")
# using the models ID

# Multiple species
getModelInfo(modelsOAKS)

modelsInfo <- getModelInfo(modelsOAKS)

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

  #print(getModelInfo(tmp))
  ### January
  ensemble(tmp, newdata = envi_01Jan, filename = paste0("Ensembles/01January/jan_", sppnames[spp], ".tif", sep = ""),
           setting = list(method = 'weighted', stat = 'TSS', opt = 2))
  ### February
  ensemble(tmp, newdata = envi_02Feb, filename = paste0("Ensembles/02February/feb_", sppnames[spp], ".tif", sep = ""),
           setting = list(method = 'weighted', stat = 'TSS', opt = 2))
  ### March
  ensemble(tmp, newdata = envi_03Mar, filename = paste0("Ensembles/03March/mar_", sppnames[spp], ".tif", sep = ""),
           setting = list(method = 'weighted', stat = 'TSS', opt = 2))
  ### April
  ensemble(tmp, newdata = envi_04Apr, filename = paste0("Ensembles/04April/apr_", sppnames[spp], ".tif", sep = ""),
           setting = list(method = 'weighted', stat = 'TSS', opt = 2))
  ### May
  ensemble(tmp, newdata = envi_05May, filename = paste0("Ensembles/05May/may_", sppnames[spp], ".tif", sep = ""),
           setting = list(method = 'weighted', stat = 'TSS', opt = 2))
  ### June
  ensemble(tmp, newdata = envi_06Jun, filename = paste0("Ensembles/06June/jun_", sppnames[spp], ".tif", sep = ""),
           setting = list(method = 'weighted', stat = 'TSS', opt = 2))
  ### July
  ensemble(tmp, newdata = envi_07Jul, filename = paste0("Ensembles/07July/jul_", sppnames[spp], ".tif", sep = ""),
           setting = list(method = 'weighted', stat = 'TSS', opt = 2))
  ### August
  ensemble(tmp, newdata = envi_08Aug, filename = paste0("Ensembles/08August/aug_", sppnames[spp], ".tif", sep = ""),
           setting = list(method = 'weighted', stat = 'TSS', opt = 2))
  ### September
  ensemble(tmp, newdata = envi_09Sep, filename = paste0("Ensembles/09September/sep_", sppnames[spp], ".tif", sep = ""),
           setting = list(method = 'weighted', stat = 'TSS', opt = 2))
  ### October
  ensemble(tmp, newdata = envi_10Oct, filename = paste0("Ensembles/10October/oct_", sppnames[spp], ".tif", sep = ""),
           setting = list(method = 'weighted', stat = 'TSS', opt = 2))
  ### November
  ensemble(tmp, newdata = envi_11Nov, filename = paste0("Ensembles/11November/nov_", sppnames[spp], ".tif", sep = ""),
           setting = list(method = 'weighted', stat = 'TSS', opt = 2))
  ### December
  ensemble(tmp, newdata = envi_12Dec, filename = paste0("Ensembles/12December/dec_", sppnames[spp], ".tif", sep = ""),
           setting = list(method = 'weighted', stat = 'TSS', opt = 2))

  print("Ensemble model completed ! Check your results in the Ensembles directory.")
}

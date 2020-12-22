setwd("Z:/3-Personal Research Folders/Jesus")

source("https://raw.githubusercontent.com/jesusNPL/BayesianSDMs_Oaks/master/Auxiliary/summary_BART.R")
source("https://raw.githubusercontent.com/jesusNPL/BayesianSDMs_Oaks/master/Auxiliary/makeMAPS_BART.R")
source("https://raw.githubusercontent.com/jesusNPL/BayesianSDMs_Oaks/master/Auxiliary/savePredictions_BART.R")
source("https://raw.githubusercontent.com/jesusNPL/BayesianSDMs_Oaks/master/Auxiliary/extractVarImp_BART.R")

dir.create("NEW_oakSDM/BayesianPredictions3/Binary")
### Load list of BART predictions (change pattern if not using TIF) 

### Inits
load("NEW_oakSDM/DATA/OCC/MODEL/oak_OCC_spt_FULL.RData")  # Species names, occurrences
load("NEW_oakSDM/DATA/OCC/MODEL/oak_OCC_spt_missing_FINAL.RData")

direct <- "NEW_oakSDM/BayesianPredictions3/" # Directory where the Bayesian models are
direct_unc <- "NEW_oakSDM/BayesianPredictions_uncertainty/Reproject/"

suffix <- "_Prediction.tif" # file extension
suffix_unc <- "_unc.tif" # file extension

##### Extract summary from BART #####
spp_full_final <- unique(c(spp_full, spp))


spp_SUM <- list()

for(i in 1:length(spp_full_final)) {
  print(spp_full_final[i])
  #if (spp_full[i] == "Quercus_polymorpha") { next
    
  #}
  mod <- readRDS(paste0("NEW_oakSDM/Calibration3/", spp_full_final[i], ".rds"))
  spp_SUM[[i]] <- extractSummary.BART(model = mod, species = spp_full_final[i])
  
}

OAK_summary <- do.call(rbind, spp_SUM)

write.csv(OAK_summary, 
          file = "NEW_oakSDM/BART_oak_evaluation.csv")
save(OAK_summary, 
     file = "NEW_oakSDM/BART_oak_evaluation.RData")

##### Prepare uncertainty raster #####

dir.create("NEW_oakSDM/BayesianPredictions_uncertainty")

for(k in 1:length(spp_full_final)) {
  print(spp_full_final[k])
  
  #if (spp_full[k] == "Quercus_polymorpha") { next
    
  #}
  
  spp <- spp_full_final[k]
  tmpRAS <- readRDS(paste0(direct, spp, "_prediction.rds"))
  
  uncRAS <- tmpRAS[[3]] - tmpRAS[[2]]
  plot(uncRAS)
  writeRaster(uncRAS, filename = paste0("NEW_oakSDM/BayesianPredictions_uncertainty/", 
                                        spp, "_uncertainty", sep = ""), 
              format = "GTiff", overwrite = TRUE)
  Sys.sleep(3)
  dev.off()
}

##### Make binary predictions based on TSS thresholds #####
load("NEW_oakSDM/BART_oak_evaluation.RData")

makeBinary.BART(sppNames = OAK_summary[, "Species"], 
                threshold = OAK_summary[, "Threshold_TSS"], 
                direction = direct, suffix = suffix)

makeBinary.BART(sppNames = OAK_summary[, "Species"], 
                threshold = OAK_summary[, "Threshold_TSS"], 
                direction = direct_unc, suffix = suffix_unc)

##### Predictions and Binary and uncertainty stacking #####

## Reproject predictions to the Continental US for plotting proposes  
env <- raster("NEW_oakSDM/DATA/Envi/US_bio01.tif")

for(j in 1:length(spp_full_final)){
  print(spp_full_final[j])
  
  #if (spp_full[j] == "Quercus_polymorpha") { next 
  #}
  
  spp <- spp_full_final[j]
  
  bb <- extent(env)
  
  pred <- raster(paste0(direct, spp, "_Prediction.tif"))
  #predC <- crop(pred, bb)
  predC <- extend(pred, env)
  predC <- mask(predC, env)
  predC[is.na(predC)] <- 0
  plot(predC)
  writeRaster(predC, filename = paste0("NEW_oakSDM/BayesianPredictions3/Reproject/", 
                                       spp, "_pred", sep = ""), 
              format = "GTiff", overwrite = TRUE)
  
  bin <- raster(paste0(direct, "Binary/", spp, "_binary.tif"))
  #binC <- crop(bin, bb)
  binC <- extend(bin, env)
  binC <- mask(binC, env)
  binC[is.na(binC)] <- 0
  plot(binC)
  writeRaster(binC, filename = paste0("NEW_oakSDM/BayesianPredictions3/Binary/Reproject/", 
                                      spp, "_bin", sep = ""), 
              format = "GTiff", overwrite = TRUE)
  
  unc <- raster(paste0("NEW_oakSDM/BayesianPredictions_uncertainty/", spp, "_uncertainty.tif"))
  #uncC <- crop(unc, bb)
  uncC <- extend(unc, env)
  uncC <- mask(uncC, env)
  uncC[is.na(uncC)] <- 0
  plot(uncC)
  writeRaster(uncC, filename = paste0("NEW_oakSDM/BayesianPredictions_uncertainty/Reproject/", 
                                      spp, "_unc", sep = ""), 
              format = "GTiff", overwrite = TRUE)
  
}

rm(list = ls())

## Stacking rasters 
lst_pred <- list.files("NEW_oakSDM/BayesianPredictions3/Reproject")
setwd("NEW_oakSDM/BayesianPredictions3/Reproject")

oak_pred <- stack(lst_pred)
setwd("../../..")

lst_bin <- list.files("NEW_oakSDM/BayesianPredictions3/Binary/Reproject")
setwd("NEW_oakSDM/BayesianPredictions3/Binary/Reproject")

oak_bin <- stack(lst_bin)
setwd("../../../..")

lst_unc <- list.files("NEW_oakSDM/BayesianPredictions_uncertainty/Reproject/Binary")
setwd("NEW_oakSDM/BayesianPredictions_uncertainty/Reproject/Binary")

oak_unc <- stack(lst_unc)
setwd("../../../..")
## Saving stacks
oak_bin <- readAll(oak_bin)

SDMs_predictions_BART <- stackSave(oak_pred, filename = "NEW_oakSDM/SR_predictions/SDMs_predictions_BART")
SDMs_binary_BART <- stackSave(oak_bin, filename = "NEW_oakSDM/SR_predictions/SDMs_binary_BART")
SDMs_uncertainty_BART <- stackSave(oak_unc, filename = "NEW_oakSDM/SR_predictions/SDMs_uncertainty_BART")

writeRaster(oak_bin, filename = "NEW_oakSDM/SR_predictions/SSDM_binary_BART", 
            format = "GTiff", overwrite = TRUE)

write.csv(names(oak_bin), file = "NEW_oakSDM/SR_predictions/SSDM_binaryNames_BART.csv")

##### Species richness mapping #####
oak_SR_pred <- calc(oak_pred, fun = sum)
oak_SR_bin <- calc(oak_bin, fun = sum)
oak_SR_unc <- calc(oak_unc, fun = sum)

plot(oak_SR_pred)
plot(oak_SR_bin)
plot(oak_SR_unc)

writeRaster(oak_SR_pred, filename = paste0("NEW_oakSDM/SR_predictions/SR_probability_BART", sep = ""), 
            format = "GTiff", overwrite = TRUE)

writeRaster(oak_SR_bin, filename = paste0("NEW_oakSDM/SR_predictions/SR_binary_BART", sep = ""), 
            format = "GTiff", overwrite = TRUE)

writeRaster(oak_SR_unc, filename = paste0("NEW_oakSDM/SR_predictions/SR_uncertainty_BART", sep = ""), 
            format = "GTiff", overwrite = TRUE)

## force data into memory
oak_SR_pred <- readAll(oak_SR_pred)
save(oak_SR_pred, file = "NEW_oakSDM/SR_predictions/SR_probability_BART.RData")

oak_SR_bin <- readAll(oak_SR_bin)
save(oak_SR_bin, file = "NEW_oakSDM/SR_predictions/SR_binary_BART.RData")

oak_SR_unc <- readAll(oak_SR_unc)
save(oak_SR_unc, file = "NEW_oakSDM/SR_predictions/SR_uncertainty_BART.RData")

##### extract important covariables (predictors) #####
direct <- "NEW_oakSDM/Calibration3/" # Directory where the Bayesian models are

covarIMP <- extractVARIMP_BART(sppNames = spp_full_final, directory = direct)

write.csv(covarIMP, 
          file = "NEW_oakSDM/BART_oak_varImp.csv")
save(covarIMP, 
     file = "NEW_oakSDM/BART_oak_varImp.RData")

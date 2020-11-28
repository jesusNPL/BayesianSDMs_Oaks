# Function to save Binary predictions under BART SDM

#dir.create("BayesianPredictions/Binary")
### Load list of BART predictions (change pattern if not using TIF) 
#lts <- list.files(path = "NEW_oakSDM/BayesianPredictions", pattern = '.tif$', full.names = TRUE)

### Inits
#load("NEW_oakSDM/Calibration/params.RData") # Species names, covariate names
#direct <- "NEW_oakSDM/BayesianPredictions/"
#suffix <- "_Predictions.tif"

makeBinary.BART <- function(sppNames, threshold, direction, suffix) {
  require(raster)
  
  for(k in 1:length(sppNames)) {
    print(lst[sppNames])
    
    thresh <- threshold[k]
    pred <- raster::raster(paste0(direction, sppNames[k], suffix))
    plot(pred)
    Sys.sleep(5)
    
    pred[pred < thresh] <- 0 
    pred[pred > 0] <- 1
    
    writeRaster(pred, filename = paste0("NEW_oakSDM/BayesianPredictions/Binaries/", sppNames[k], 
                                       "_binary", sep = ""), format = "GTiff", overwrite = TRUE)
    
    print(paste0("Your Binary SDM based on BART TSS threshold ", sppNames[k], 
                 " is complete, please check folder BayesianPredictions/Binaries", sep = ""))
  }
}

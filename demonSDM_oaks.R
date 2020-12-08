setwd("Z:/3-Personal Research Folders/Jesus")

#require(embarcadero)
require(raster)
require(rworldmap)
require(dismo)
require(rgdal)
require(rgeos)
library(sdm)

#load("NEW_oakSDM/DATA/COVARS/oak_COVARS.RData") # Data.frames used for model calibration
load("NEW_oakSDM/Calibration/params.RData") # Species names, covariate names
load("NEW_oakSDM/DATA/OCC/MODEL/oak_OCC_spt.RData") # Spatial points for each Oak species restricted to continental US
#load("NEW_oakSDM/DATA/Envi/oak_ENVI.RData") # 

##### Prepare environmental data for model predictions #####
env <- list.files("NEW_oakSDM/DATA/Envi", pattern = "tif$")
setwd("NEW_oakSDM/DATA/Envi")
envi <- stack(env[1:10])
setwd("../../..")

USA <- readOGR("Z:/3-Personal Research Folders/Jesus/ModelsTT/DATA/Spatial/states_21basic/states.shp")
USA <- USA[-which(USA$STATE_NAME %in% c("Hawaii", "Alaska")), ]
USA <- spTransform(USA, crs(envi))

USA2 <- aggregate(USA, dissolve = TRUE)

plot(USA2)
plot(spp_lst[[10]], add = TRUE)

##### Set species specific accessible area #####
#spp_envi <- list()
dir.create("NEW_oakSDM/Calibration_SDM")
dir.create("NEW_oakSDM/DATA/COVARS_SDM")
dir.create("NEW_oakSDM/Predictions_SDM")
dir.create("NEW_oakSDM/Ensemble_SDM")
dir.create("NEW_oakSDM/DATA/Spatial/Accessible_Area_SDM")

predNames <- c("Prediction", "Lowbound", "Highbound")

for(j in 1:length(spp)){
  print(spp[j])
  
  tmp <- spp_lst[[j]]
  
  ##### Species specific accesible area
  bb <- bbox(tmp)
  e <- extent(c(bb[1]-4, bb[3]+4, bb[2]-4, bb[4]+4))
  p <- as(e, 'SpatialPolygons')
  crs(p) <- crs(envi)
  #plot(p, add = TRUE)
  shapefile(p, paste0("NEW_oakSDM/DATA/Spatial/Accessible_Area_SDM/AA_", spp[j], ".shp", sep = ""))
  out <- gIntersection(USA2, p, byid = TRUE)
  enviSPP <- raster::crop(envi, out)
  #spp_envi[[j]] <- raster::mask(enviSPP, out)
  plot(enviSPP[[1]])
  plot(tmp, add = TRUE)
  
  ##### Covariate information
  #pres.cov <- data.frame(raster::extract(enviSPP, spp_lst[[j]]))
  #pres.cov$Species <- spp[j]
  #pres.cov$Presence <- 1
  # Generate the data
  #absence <- randomPoints(mask = enviSPP[[1]], n = round(nrow(spp_lst[[j]])*1.1, 0), 
   #                       p = spp_lst[[j]], ext = extent(enviSPP))
  #> longitude/latitude
  #abs.cov <- data.frame(raster::extract(enviSPP, absence))
  #abs.cov$Species <- spp[j]
  #abs.cov$Presence <- 0
  #all.cov <- rbind(pres.cov, abs.cov)
  #all.cov <- all.cov[complete.cases(all.cov), ]
  #all.cov <- na.omit(all.cov)
  #head(all.cov)
  #saveRDS(all.cov, file = paste0("NEW_oakSDM/DATA/COVARS_SDM/", spp[j], "_PA_Covars.rds", sep = ""))
  
  ##### Calibrate models using different algorithms
  bgs <- nrow(tmp@data)
  oakDATA <- sdmData(Species ~ ., train = tmp, predictors = enviSPP, bg = list(n = bgs*2)) 
  
  oakSDM <- sdm(Species ~ ., oakDATA, methods = c("glm", "gam", "mars", "svm", "rf", "brt", "maxlike"), 
                replication = "sub", test.percent = 30, n = 30, 
                parallelSettings = list(ncore = 32, method = "parallel"))
  
  saveRDS(oakSDM, file = paste0("NEW_oakSDM/Calibration_SDM/", spp[j], ".rds", sep = "")) 
  
  print(paste0("SDM models completed! Saving model for ", spp[j], 
               " in Calibration_SDM folder...", sep = " "))
  
  ##### Predict models 
  # Prediction for individual algorithm
  oakPredictions <- predict(oakSDM, newdata = enviSPP, 
                            filename = paste0("NEW_oakSDM/Predictions_SDM/", spp[j], "_predictions.tif"), 
                            parallelSettings = list(ncore = 32, method = "parallel"))
  
  # Ensemble prediction
  oakEnsemble <- ensemble(oakSDM, enviSPP, setting = list(method = "weighted", stat = "TSS"), 
                             parallelSettings = list(ncore = 32, method = "parallel"))
  
  oakEnsemble <- sdmvspecies::rescale(oakEnsemble)
  
  saveRDS(oakEnsemble, paste0("NEW_oakSDM/Ensemble_SDM/", spp[j], "_ensemble.rds", sep = ""))
  
  writeRaster(oakEnsemble, filename = paste0("NEW_oakSDM/Ensemble_SDM/", spp[j], "_ensemble"),  
              format = "GTiff", bylayer = TRUE, overwrite = TRUE)
  
  print(paste0("Your SDM based on a Ensemble of different algorithms for ", spp[j], 
               " is complete, please check folder Predicitons_SDM", sep = ""))
  
}

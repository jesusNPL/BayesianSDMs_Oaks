setwd("Z:/3-Personal Research Folders/Jesus")

#require(embarcadero)
require(raster)
require(rworldmap)
require(dismo)
require(rgdal)
require(rgeos)
library(sdm)
library(maptools)

#load("NEW_oakSDM/DATA/COVARS/oak_COVARS.RData") # Data.frames used for model calibration
load("NEW_oakSDM/Calibration/params.RData") # Species names, covariate names
load("NEW_oakSDM/DATA/OCC/MODEL/oak_OCC_spt_FULL.RData") # Spatial points for each Oak species restricted to continental US
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
plot(spp_lst_full[[10]], add = TRUE)

##### Set species specific accessible area #####
#spp_envi <- list()
dir.create("NEW_oakSDM/Calibration_SDM")
dir.create("NEW_oakSDM/DATA/COVARS_SDM")
dir.create("NEW_oakSDM/Predictions_SDM")
dir.create("NEW_oakSDM/Ensemble_SDM")
dir.create("NEW_oakSDM/DATA/Spatial/Accessible_Area_SDM")

predNames <- c("Prediction", "Lowbound", "Highbound")

for(j in 1:length(spp_full)){
  print(spp_full[j])
  
  tmp <- spp_lst_full[[j]]
  
  ##### Species specific accesible area
  bb <- bbox(tmp)
  e <- extent(c(bb[1]-4, bb[3]+4, bb[2]-4, bb[4]+4))
  p <- as(e, 'SpatialPolygons')
  crs(p) <- crs(envi)
  #plot(p, add = TRUE)
  shapefile(p, paste0("NEW_oakSDM/DATA/Spatial/Accessible_Area_SDM/AA_", spp_full[j], ".shp", sep = ""), 
            overwrite = TRUE)
  out <- gIntersection(USA2, p, byid = TRUE)
  enviSPP <- raster::crop(envi, out)
  #spp_envi[[j]] <- raster::mask(enviSPP, out)
  #plot(enviSPP[[1]])
  #plot(tmp, add = TRUE)
  
  ##### Covariate information
  pres.cov <- data.frame(raster::extract(enviSPP, spp_lst_full[[j]]))
  pres.cov$Species <- spp_full[j]
  pres.cov$Presence <- 1
  # Generate the data
  absence <- randomPoints(mask = enviSPP[[1]], n = round(nrow(spp_lst_full[[j]])*1.1, 0), 
                          p = spp_lst_full[[j]], ext = extent(enviSPP))
  #> longitude/latitude
  abs.cov <- data.frame(raster::extract(enviSPP, absence))
  abs.cov$Species <- spp_full[j]
  abs.cov$Presence <- 0
  all.cov <- rbind(pres.cov, abs.cov)
  all.cov <- all.cov[complete.cases(all.cov), ]
  all.cov <- na.omit(all.cov)
  head(all.cov)
  saveRDS(all.cov, file = paste0("NEW_oakSDM/DATA/COVARS_SDM/", spp_full[j], "_PA_Covars.rds", sep = ""))
  
  ##### create species in the format of the package sdm. This is stupid!!!
  presence <- data.frame(coordinates(tmp))
  presence$Occurrence <- 1 
  
  absence <- data.frame(absence)
  names(absence) <- c("Longitude", "Latitude")
  absence$Occurrence <- 0
  
  species <- rbind(presence, absence)
  species$Species <- spp_full[[j]]
  
  coordinates(species) <- ~ Longitude + Latitude
  crs(species) <- crs(envi)
  
  #writeOGR(obj = species, dsn = "NEW_oakSDM/DATA/OCC/MODEL/PresAbs_SDM", 
   #        layer = spp_full[j], driver = "ESRI Shapefile")
  shapefile(species, paste0("NEW_oakSDM/DATA/OCC/MODEL/PresAbs_SDM/", spp_full[j], ".shp", sep = ""), 
                         overwrite = TRUE)

  #plot(enviSPP[[1]])
  #plot(species[species$Occurrence == 1, ], col = "blue", add = TRUE, pch = 16)
  #points(species[species$Occurrence == 0, ], col = "red", pch = 16)
  
  ##### Calibrate models using different algorithms
  #crs(tmp) <- crs(enviSPP)
  #bgs <- nrow(tmp@data)
  oakDATA <- sdmData(formula = Occurrence~US_bio01+US_bio04+US_bio06+US_bio10+US_bio12+US_bio15+US_elevation+US_LAIcum+US_LAImin+US_LAIseason, 
                     train = species, predictors = enviSPP) #, bg = list(n = bgs*2)) 
  
  oakSDM <- sdm(Occurrence~., data = oakDATA, methods = c("glm", "gam", "mars", "svm", "rf", "brt", "maxlike"), 
                replication = "sub", test.percent = 30, n = 10, 
                parallelSettings = list(ncore = 14, method = "parallel"))
  
  write.sdm(oakSDM, file = paste0("NEW_oakSDM/Calibration_SDM/", spp_full[j], sep = ""), overwrite = TRUE) 
  
  print(paste0("SDM models completed! Saving model for ", spp_full[j], 
               " in Calibration_SDM folder...", sep = " "))
  
  ##### Predict models 
  # Prediction for individual algorithm
  #oakPredictions <- predict(oakSDM, newdata = enviSPP, 
   #                         filename = paste0("NEW_oakSDM/Predictions_SDM/", spp_full[j], "_predictions.img"), 
    #                        parallelSettings = list(ncore = 14, method = "parallel"))
  
  # Ensemble prediction
  oakEnsemble <- ensemble(oakSDM, enviSPP, setting = list(method = "weighted", stat = "TSS"), 
                             parallelSettings = list(ncore = 14, method = "parallel"))
  
  oakEnsemble <- sdmvspecies::rescale(oakEnsemble)
  
  saveRDS(oakEnsemble, paste0("NEW_oakSDM/Ensemble_SDM/", spp_full[j], "_ensemble.rds", sep = ""))
  
  writeRaster(oakEnsemble, filename = paste0("NEW_oakSDM/Ensemble_SDM/", spp_full[j], "_ensemble"),  
              format = "GTiff", bylayer = TRUE, overwrite = TRUE)
  
  print(paste0("Your SDM based on a Ensemble of different algorithms for ", spp_full[j], 
               " is complete, please check folder Predicitons_SDM", sep = ""))
  
}

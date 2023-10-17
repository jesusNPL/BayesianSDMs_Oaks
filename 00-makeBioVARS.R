# Script to construct bioclimatic variables using data from remote sensing

setwd("Z:/3-Personal Research Folders/Jesus")

require(raster)
require(rworldmap)
require(dismo)
require(rgdal)

AM <- subset(countriesCoarse, countriesCoarse$continent == "South America" | countriesCoarse$continent == "North America")

NAs <- subset(countriesCoarse, countriesCoarse$ADMIN == "United States of America" | countriesCoarse$ADMIN == "Mexico")
plot(NAs)

##### Original RS bioclim #####
lst <- list.files(path = "RSData/BioCLIM_RS", pattern = "tif$")
setwd("RSData/BioCLIM_RS/")

RS_bios <- stack(lst)
setwd("../../..")

RS_bios_lst <- as.list(RS_bios)

biosNames <- names(RS_bios)

RS_bios_noNA <- list()

for(r in 1:length(biosNames)){
  
  tm <- as.name(biosNames[r])
  print(tm)
  
  RS_bios_lst[[r]][RS_bios_lst[[r]][] == -9999] <- NA
  RS_bios_noNA[[r]] <- trim(RS_bios_lst[[r]])
  Sys.sleep(5) # Force R to wait 5 seconds for ploting
  plot(RS_bios_noNA[[r]])
}

RS_bios_noNA <- stack(RS_bios_noNA)
plot(RS_bios_noNA)

writeRaster(RS_bios, "RSData/BioCLIM_RS/Clean", 
            format = "GTiff", bylayer = TRUE, overwrite = TRUE, suffix = biosNames)

rm(RS_bio1_noNA, RS_bios, RS_bios_lst, RS_bios_noNA)

##### Build bioclimatic variables #####
lst <- list.files(path = "RSData/BioCLIM_RS/Clean/", pattern = "tif$")

setwd("RSData/BioCLIM_RS/Clean")

PREC <- lst[1:12]
Tmax <- lst[13:24]
Tmin <- lst[37:48]

PREC <- stack(PREC)
namesPREC <- names(PREC)
PREC_lst <- as.list(PREC)

Tmax <- stack(Tmax)
namesTmax <- names(Tmax)
Tmax_lst <- as.list(Tmax)

Tmin <- stack(Tmin)
namesTmin <- names(Tmin)
Tmin_lst <- as.list(Tmin)

setwd("../../..")

## Clean monthly data
RS_PREC_noNA <- list()
RS_Tmax_noNA <- list()
RS_Tmin_noNA <- list()

for(i in 1:length(namesPREC)){
  
  tm <- as.name(namesPREC[i])
  print(tm)
  
  PREC_lst[[i]][PREC_lst[[i]][] == -9999] <- NA
  RS_PREC_noNA[[i]] <- trim(PREC_lst[[i]])
  
  Tmax_lst[[i]][Tmax_lst[[i]][] == -9999] <- NA
  RS_Tmax_noNA[[i]] <- trim(Tmax_lst[[i]])
  
  Tmin_lst[[i]][Tmin_lst[[i]][] == -9999] <- NA
  RS_Tmin_noNA[[i]] <- trim(Tmin_lst[[i]])
  
  # Force R to wait 5 seconds for ploting
  plot(RS_PREC_noNA[[i]])
  Sys.sleep(5)
  plot(RS_Tmax_noNA[[i]])
  Sys.sleep(5)
  plot(RS_Tmin_noNA[[i]])
  Sys.sleep(5)
}

RS_PREC_noNA <- stack(RS_PREC_noNA)
precNames <- names(RS_PREC_noNA)

RS_Tmax_noNA <- stack(RS_Tmax_noNA)
tmaxNames <- names(RS_Tmax_noNA)

RS_Tmin_noNA <- stack(RS_Tmin_noNA)
tminNames <- names(RS_Tmin_noNA)

### Build bioclimatic variables
RS_bios <- biovars(prec = RS_PREC_noNA, tmin = RS_Tmin_noNA, tmax = RS_Tmax_noNA)
biosNames <- names(RS_bios)

writeRaster(RS_bios, "RSData/BioCLIM_RS/BIOCLIM_MODIS/RS_03m", 
            format = "GTiff", bylayer = TRUE, overwrite = TRUE, suffix = biosNames)


plot(RS_bios[[1:4]])
plot(RS_bios[[16:19]])

## save cleaned monthly data 
writeRaster(RS_PREC_noNA, "RSData/BioCLIM_RS/CLEAN/Clean", 
            format = "GTiff", bylayer = TRUE, overwrite = TRUE, suffix = precNames)

writeRaster(RS_Tmax_noNA, "RSData/BioCLIM_RS/CLEAN/Clean", 
            format = "GTiff", bylayer = TRUE, overwrite = TRUE, suffix = tmaxNames)

writeRaster(RS_Tmin_noNA, "RSData/BioCLIM_RS/CLEAN/Clean", 
            format = "GTiff", bylayer = TRUE, overwrite = TRUE, suffix = tminNames)

rm(PREC, PREC_lst, Tmax, Tmax_lst, Tmin, Tmin_lst, RS_PREC_noNA, RS_Tmax_noNA, RS_Tmin_noNA)




##### Resample to 1km - this part is experimental ##### 

##### -------------------------------------------------------------- #####
##### Machine Learning Interpolation #####
setwd("Z:\\3-Personal Research Folders\\Jesus")

require(raster)
require(MACHISPLIN)
require(rgeos)
require(rgdal)
require(gbm)

lst <- list.files(path = "RSData/BioCLIM_RS/MOD11C3v6.0-CHIRPSv2.0_MONTHLY_03m/CLEAN", pattern = "tif$")

setwd("RSData/BioCLIM_RS/MOD11C3v6.0-CHIRPSv2.0_MONTHLY_03m/CLEAN")

PREC <- lst[1:12]
Tmax <- lst[13:24]
Tmin <- lst[25:36]

PREC <- stack(PREC)
namesPREC <- names(PREC)
#PREC_lst <- as.list(PREC)

Tmax <- stack(Tmax)
namesTmax <- names(Tmax)
#Tmax_lst <- as.list(Tmax)

Tmin <- stack(Tmin)
namesTmin <- names(Tmin)
#Tmin_lst <- as.list(Tmin)

setwd("../../../..")

pred <- list.files(path = "RSData/BioCLIM_RS/Topography", pattern = "tif$")
setwd("RSData/BioCLIM_RS/Topography")

pred <- stack(pred)

setwd("../../..")

## Clean monthly data

##### Crop data to continental US #####
states.shp <- readOGR("Z:/3-Personal Research Folders/Jesus/ModelsTT/DATA/Spatial/states_21basic/states.shp")
states.shp <- states.shp[-which(states.shp$STATE_NAME %in% c("Hawaii", "Alaska")), ]

USA <- spTransform(states.shp, crs(pred))
plot(USA)

prec_US <- crop(PREC, USA)
prec_US <- mask(prec_US, USA)

tmax_US <- crop(Tmax, USA)
tmax_US <- mask(tmax_US, USA)

tmin_US <- crop(Tmin, USA)
tmin_US <- mask(tmin_US, USA)

pred_US <- crop(pred, USA)
pred_US <- mask(pred_US, USA)

plot(pred_US[[2]])
plot(USA, add = T)
dev.off()

##### Prepare data for interpolation #####
envi <- rasterToPoints(prec_US[[1]], fun = NULL, spatial = FALSE)
coords <- envi[, 1:2]

prec_vals <- data.frame(extract(prec_US, coords))
tmax_vals <- data.frame(extract(tmax_US, coords))
tmin_vals <- data.frame(extract(tmin_US, coords))

prec_data <- cbind(coords, prec_vals)
tmax_data <- cbind(coords, tmax_vals)
tmin_data <- cbind(coords, tmin_vals)

save(prec_data, tmax_data, tmin_data, USA, pred_US, prec_US, tmax_US, tmin_US,   
     file = "RSData/BioCLIM_RS/BIOCLIM_MODIS/Interpolation/datatoInterpolate.RData")

## remove unnecessary data from environment
rm(PREC, pred, Tmax, Tmin, prec_vals, tmax_vals, tmin_vals, prec_US, tmax_US, tmin_US, envi, coords)

##### Perform interpolation #####
pred_US2 <- stack(pred_US$elevation_1KMmd_SRTM, pred_US$slope_1KMmd_SRTM, pred_US$roughness_1KMmd_SRTM)
names(pred_US2) <- c("elevation", "slope", "roughness")

load("RSData/BioCLIM_RS/BIOCLIM_MODIS/Interpolation/datatoInterpolate.RData")
rm(pred_US, tmax_US, tmin_US, prec_US)

names(prec_data) <- c("long", "lat", "month_01", "month_02", "month_03", "month_04", 
               "month_05", "month_06", "month_07", "month_08", 
               "month_09", "month_10", "month_11", "month_12")

memory.limit(size = 500000)

head(prec_data)

prec_data_p1  <- prec_data[, c(1:4)]
head(prec_data_p1)

prec_data_p2  <- prec_data[, c(1:2, 5:6)]
prec_data_p3  <- prec_data[, c(1:2, 7:8)]
prec_data_p4  <- prec_data[, c(1:2, 9:10)]
prec_data_p5  <- prec_data[, c(1:2, 11:12)]
prec_data_p6  <- prec_data[, c(1:2, 13:14)]

PREC_inter_p1 <- machisplin.mltps(int.values = prec_data_p1, covar.ras = pred_US2, n.cores = 2)

save(PREC_inter_p1,  
     file = "RSData/BioCLIM_RS/BIOCLIM_MODIS/Interpolation/PREC_US_RES.RData")

PREC_inter <- machisplin.mltps(int.values = prec_data, covar.ras = pred_US, n.cores = 2)
PREC_inter <- machisplin.mltps(int.values = prec_data, covar.ras = pred_US, n.cores = 2)


RS_bios_US <- mask(RS_bios_US, states.shp)

WC_bios_US <- crop(wc_bios, states.shp)
WC_bios_US <- mask(WC_bios_US, states.shp)

LAI_cum_US <- crop(LAIcum, states.shp)

fact <- res(RS_bios_US[[2]])/res(LAI_cum_US)

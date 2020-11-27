

for (i in 1:length(sp_list)){ 
  print(sp_list[i])
  oak_tmp <- droplevels(subset(oaks_filtered, Species == sp_list[i]))
  
  if(nrow(oak_tmp) < 50){
  }
  if(nrow(oak_tmp) >= 50 & nrow(oak_tmp) <= 100){  
  }
  if(nrow(oak_tmp) > 100 & nrow(oak_tmp) <= 500){  
  }
  if(nrow(oak_tmp) > 500 & nrow(oak_tmp) <= 750){  
  }
  if(nrow(oak_tmp) > 750 & nrow(oak_tmp) <= 1000){ 
  }
  if(nrow(oak_tmp) > 1000 & nrow(oak_tmp) <= 2500){ 
  }
  if(nrow(oak_tmp) > 2500 & nrow(oak_tmp) <= 5000){  
  }
  if(nrow(oak_tmp) > 5000 & nrow(oak_tmp) <= 7500){ 
  }
  if(nrow(oak_tmp) > 7500 & nrow(oak_tmp) <= 10000){  
  }
  if(nrow(oak_tmp) > 1000){ 
  }
  
    thinning <- thin(
      oak_tmp, 
      verbose = FALSE, 
      long.col = "longitude", 
      lat.col = "latitude",
      spec.col = "Species",
      thin.par = 0.5, # points have at least a minimum distance of 1 km from each other
      reps = 1, 
      locs.thinned.list.return = TRUE, 
      write.files = FALSE, 
      out.dir = "DATA/OCC/MODEL")
    
    thinning <- as.data.frame(thinning)
    thinning$Species <- rep(sp_list[i], nrow(thinning))
    oaks_thinned_50_100 <- rbind(oaks_thinned_50_100, thinning)
} 


setwd("C:/Users/jpintole/Dropbox/RS_SDM")
library(raster)
library(rgdal)
library(tidyr)
library(dplyr)
library(rworldmap)
library(spThin)
##### Occurrence data #####
oaks <- read.csv("DATA/OCC/Quercus_data.csv")
head(oaks)
names(oaks)
unique(oaks$Species)

oaksHIP <- oaks[, c(15, 14, 3)]
head(oaksHIP)

oaksiDig <- read.csv("DATA/OCC/iDigBio_oaks_missing/oaks_iDigBio.csv")
head(oaksiDig)

oaksiDig <- oaksiDig[, c(1, 2, 5)]
names(oaksiDig) <- c("longitude", "latitude", "Species")
head(oaksiDig)

oaksNEW <- rbind(oaksHIP, oaksiDig)
head(oaksNEW)
rm(oaks, oaksHIP, oaksiDig)

oaksNEW <- oaksNEW %>% 
  drop_na(longitude)
## Data exploration ##
unique(oaksNEW$Species)

to_filter <- oaksNEW %>% 
  count(Species) %>% 
  filter(n >= 20)

oaks_filtered <- data.frame(oaksNEW[oaksNEW$Species %in% to_filter$Species, ])
length(unique(oaks_filtered$Species))

write.csv(oaks_filtered, file = "DATA/OCC/MODEL/oak_OCC_filtered.csv")

sp_list <- sort(as.character(unique(oaks_filtered$Species)))

oaks_thinned_50_100 <- NULL # between 50 and 100 OCC
oaks_thinned_100_500 <- NULL # between 101 and 500 OCC
oaks_thinned_500_1000 <- NULL # between 501 and 1000 OCC
oaks_thinned_1000_2500 <- NULL # between 1001 and 2500 OCC
oaks_thinned_2500_5000 <- NULL # between 2501 and 5000 OCC
oaks_thinned_5000_7500 <- NULL # between 5001 and 7500 OCC
oaks_thinned_7500_10000 <- NULL # between 7500 and 10000 OCC
oaks_thinned_10000 <- NULL # More than 10000 OCC

thinning <- NULL

for (i in 1:length(sp_list)){ 
  print(sp_list[i])
  oak_tmp <- droplevels(subset(oaks_filtered, Species == sp_list[i]))
  
  if(nrow(oak_tmp) < 50){ next
  }
  if(nrow(oak_tmp) >= 50 & nrow(oak_tmp) <= 100){ 
    thinning <- thin(
      oak_tmp, 
      verbose = FALSE, 
      long.col = "longitude", 
      lat.col = "latitude",
      spec.col = "Species",
      thin.par = 0.5, # points have at least a minimum distance of 1 km from each other
      reps = 1, 
      locs.thinned.list.return = TRUE, 
      write.files = FALSE, 
      out.dir = "DATA/OCC/MODEL")
    
    thinning <- as.data.frame(thinning)
    thinning$Species <- rep(sp_list[i], nrow(thinning))
    oaks_thinned_50_100 <- rbind(oaks_thinned_50_100, thinning)
    }

  if(nrow(oak_tmp) > 100 & nrow(oak_tmp) <= 500){ 
    thinning <- thin(
      oak_tmp, 
      verbose = FALSE, 
      long.col = "longitude", 
      lat.col = "latitude",
      spec.col = "Species",
      thin.par = 1, # points have at least a minimum distance of 1 km from each other
      reps = 1, 
      locs.thinned.list.return = TRUE, 
      write.files = FALSE, 
      out.dir = "DATA/OCC/MODEL")
    
    thinning <- as.data.frame(thinning)
    thinning$Species <- rep(sp_list[i], nrow(thinning))
    oaks_thinned_100_500 <- rbind(oaks_thinned_100_500, thinning)
    }
    
  if(nrow(oak_tmp) > 500 & nrow(oak_tmp) <= 1000){ 
    thinning <- thin(
      oak_tmp, 
      verbose = FALSE, 
      long.col = "longitude", 
      lat.col = "latitude",
      spec.col = "Species",
      thin.par = 1.5, # points have at least a minimum distance of 1 km from each other
      reps = 1, 
      locs.thinned.list.return = TRUE, 
      write.files = FALSE, 
      out.dir = "DATA/OCC/MODEL")
    
    thinning <- as.data.frame(thinning)
    thinning$Species <- rep(sp_list[i], nrow(thinning))
    oaks_thinned_500_1000 <- rbind(oaks_thinned_500_1000, thinning)
    }
    
  if(nrow(oak_tmp) > 1000 & nrow(oak_tmp) <= 2500){ 
    thinning <- thin(
      oak_tmp, 
      verbose = FALSE, 
      long.col = "longitude", 
      lat.col = "latitude",
      spec.col = "Species",
      thin.par = 2, # points have at least a minimum distance of 1 km from each other
      reps = 1, 
      locs.thinned.list.return = TRUE, 
      write.files = FALSE, 
      out.dir = "DATA/OCC/MODEL")
    
    thinning <- as.data.frame(thinning)
    thinning$Species <- rep(sp_list[i], nrow(thinning))
    oaks_thinned_1000_2500 <- rbind(oaks_thinned_1000_2500, thinning)
    }

  if(nrow(oak_tmp) > 2500 & nrow(oak_tmp) <= 5000){ 
    thinning <- thin(
      oak_tmp, 
      verbose = FALSE, 
      long.col = "longitude", 
      lat.col = "latitude",
      spec.col = "Species",
      thin.par = 2.5, # points have at least a minimum distance of 1 km from each other
      reps = 1, 
      locs.thinned.list.return = TRUE, 
      write.files = FALSE, 
      out.dir = "DATA/OCC/MODEL")
    
    thinning <- as.data.frame(thinning)
    thinning$Species <- rep(sp_list[i], nrow(thinning))
    oaks_thinned_2500_5000 <- rbind(oaks_thinned_2500_5000, thinning)
    }

  if(nrow(oak_tmp) > 5000 & nrow(oak_tmp) <= 7500){ 
    thinning <- thin(
      oak_tmp, 
      verbose = FALSE, 
      long.col = "longitude", 
      lat.col = "latitude",
      spec.col = "Species",
      thin.par = 3, # points have at least a minimum distance of 1 km from each other
      reps = 1, 
      locs.thinned.list.return = TRUE, 
      write.files = FALSE, 
      out.dir = "DATA/OCC/MODEL")
    
    thinning <- as.data.frame(thinning)
    thinning$Species <- rep(sp_list[i], nrow(thinning))
    oaks_thinned_5000_7500 <- rbind(oaks_thinned_5000_7500, thinning)
    }
    
  if(nrow(oak_tmp) > 7500 & nrow(oak_tmp) <= 10000){ 
    thinning <- thin(
      oak_tmp, 
      verbose = FALSE, 
      long.col = "longitude", 
      lat.col = "latitude",
      spec.col = "Species",
      thin.par = 3.5, # points have at least a minimum distance of 1 km from each other
      reps = 1, 
      locs.thinned.list.return = TRUE, 
      write.files = FALSE, 
      out.dir = "DATA/OCC/MODEL")
    
    thinning <- as.data.frame(thinning)
    thinning$Species <- rep(sp_list[i], nrow(thinning))
    oaks_thinned_7500_10000 <- rbind(oaks_thinned_7500_10000, thinning)
    }

  if(nrow(oak_tmp) > 10000){ 
    thinning <- thin(
      oak_tmp, 
      verbose = FALSE, 
      long.col = "longitude", 
      lat.col = "latitude",
      spec.col = "Species",
      thin.par = 5, # points have at least a minimum distance of 1 km from each other
      reps = 1, 
      locs.thinned.list.return = TRUE, 
      write.files = FALSE, 
      out.dir = "DATA/OCC/MODEL")
    
    thinning <- as.data.frame(thinning)
    thinning$Species <- rep(sp_list[i], nrow(thinning))
    oaks_thinned_10000 <- rbind(oaks_thinned_10000, thinning) 
  }
  
}

spp50 <- to_filter %>% filter(n < 50)
oaks_thinned_50 <- data.frame(oaks_filtered[oaks_filtered$Species %in% spp50$Species, ])
names(oaks_thinned_50) <- names(oaks_thinned_100_500)

save(oaks_thinned_50, oaks_thinned_50_100, oaks_thinned_100_500,
     oaks_thinned_500_1000, oaks_thinned_1000_2500, oaks_thinned_2500_5000, 
     oaks_thinned_5000_7500, oaks_thinned_7500_10000, oaks_thinned_10000, 
     file = "DATA/OCC/MODEL/Thinned_OCC/oak_OCC_thinned.RData")

oaks_OCC_thinned <- rbind(oaks_thinned_50, oaks_thinned_50_100, oaks_thinned_100_500,
                          oaks_thinned_500_1000, oaks_thinned_1000_2500, oaks_thinned_2500_5000, 
                          oaks_thinned_5000_7500, oaks_thinned_7500_10000, oaks_thinned_10000)

unique(oaks_OCC_thinned$Species)

write.csv(oaks_OCC_thinned, file = "DATA/OCC/MODEL/oak_OCC_thinned.csv")

dim(oaksNEW)
dim(oaks_filtered)
dim(oaks_OCC_thinned)

#setwd("Dropbox/RS_SDM")

#direct = "Models/SDM/"
#spp_lst <- c("Quercus_acutidens", "Quercus_agrifolia", "Quercus_ajoensis")

extractMETRICS_SDM <- function(sppNames, directory) { 
  library(sdm)
  library(dplyr)
  library(tidyr)
  
  metricSPP <- list()
  
  for(i in 1:length(sppNames)) {
    print(sppNames[i]) 
    spp <- sppNames[i]
    
    model <- read.sdm(paste0(direct, spp, ".sdm"))
    modInfo <- getModelInfo(model)
    modInfo <- subset(modInfo, success == "TRUE")
    
    modEval <- getEvaluation(model)
    modCOMB <- cbind(modInfo, modEval)
    
    metricBYmodel <- modCOMB %>% 
      dplyr::select(method, AUC, COR, Deviance, TSS) %>% 
      group_by(method) %>% 
      summarize(mean_AUC = mean(AUC, na.rm = TRUE),
                sd_AUC = sd(AUC, na.rm = TRUE),
                mean_COR = mean(COR, na.rm = TRUE), 
                sd_COR = sd(COR, na.rm = TRUE), 
                mean_Deviance = mean(Deviance, na.rm = TRUE), 
                sd_Deviance = sd(Deviance, na.rm = TRUE), 
                mean_TSS = mean(TSS, na.rm = TRUE), 
                sd_TSS = sd(TSS, na.rm = TRUE))
    
    metricEnsemble <- modCOMB %>% 
      dplyr::select(method, AUC, COR, Deviance, TSS) %>% 
      summarize(mean_AUC = mean(AUC, na.rm = TRUE),
                sd_AUC = sd(AUC, na.rm = TRUE),
                mean_COR = mean(COR, na.rm = TRUE), 
                sd_COR = sd(COR, na.rm = TRUE), 
                mean_Deviance = mean(Deviance, na.rm = TRUE), 
                sd_Deviance = sd(Deviance, na.rm = TRUE), 
                mean_TSS = mean(TSS, na.rm = TRUE), 
                sd_TSS = sd(TSS, na.rm = TRUE)) 
    metricEnsemble$method <- "Ensemble"
    metricEnsemble <- metricEnsemble[, c(9, 1:8)]
    
    metricTMP <- rbind(metricBYmodel, metricEnsemble)
    metricTMP$Species <- spp
    
    metricSPP[[i]] <- data.frame(metricTMP)
    
  }
  
  metrics <- do.call(rbind, metricSPP)
  return(metrics)
}


#extractMETRICS_SDM(sppNames = spp_lst, directory = direct)
       
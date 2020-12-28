SRmetrics <- function(predSR, obsSR) { 
  
  SRdev <- abs(obsSR - predSR)/max(obsSR)
  SRdiff <- (obsSR - predSR)
  SR_change <- ((predSR - obsSR)/obsSR)
  # negative SR_change values suggest SR community underprediction
  # positive SR_change values suggest SR community overprediction
  RRL <- log(predSR/obsSR)
  results <- data.frame(SR_obs = obsSR, SR_pred = predSR,  
                        Deviation = SRdev, Difference = SRdiff, Change = SR_change, LRR = RRL)
  return(results)
}


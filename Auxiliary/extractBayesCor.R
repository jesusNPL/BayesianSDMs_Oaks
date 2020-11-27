
extractCOR <- function(model){
  require(BayesianFirstAid)
  mcmcFIT <- as.data.frame(model)
  
  means <- mean(mcmcFIT$rho)
  medians <- median(mcmcFIT$rho)
  SDs <- sd(mcmc(mcmcFIT$rho))
  quantiles <- quantile(mcmcFIT$rho, probs = c(0.05, 0.95))
  
  statistics <- c("mean" = means, "median" = medians, "sd" = SDs, "quantile" = quantiles)
  
  obj <- list(mcmcFIT, statistics)
  return(obj)
}


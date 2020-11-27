# Calabrese correction: based on Calabrese et al. (2014) Methods in Ecology and Evolution 23: 99-112.
nLL.Calabrese <- function(par,sr,probs) {
  require(poibin)
  logit = function(x) {x=ifelse(x<0.0001,0.0001,ifelse(x>0.9999,.9999,x)); log(x/(1 - x))}
  invlogit = function(x) {exp(x)/(1+exp(x))}
  
  
  bysite <- function(j) {
    logit.probs <- logit(as.numeric(probs[j,]))
    corr.probs <- invlogit( logit.probs + par[1]*sr[j] + par[2] )
    dp <- dpoibin(sr[j],as.numeric(corr.probs))
    log(ifelse(dp<.0001,.0001,dp))
  }
  - sum(sapply(seq_len(length(sr)),bysite)) 	# optim will perform minimization but we aim for maximum likelihood and thus invert
}

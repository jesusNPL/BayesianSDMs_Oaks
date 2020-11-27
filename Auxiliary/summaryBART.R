summary.BART <- function(model, plots = TRUE) {
  library(ROCR)
  library(ggplot2)
  library(patchwork)
  
  if(class(model)=='bart') {fitobj <- model$fit} else 
    if(class(model)=='rbart') {fitobj <- model$fit[[1]] }
  
  cat("Call: ", paste(model$call), '\n \n')
  
  cat("Predictor list: \n", 
      paste(attr(fitobj$data@x, "term.labels"), sep=' '), "\n", "\n")
  
  true.vector <- fitobj$data@y 
  
  pred <- prediction(colMeans(pnorm(model$yhat.train)), true.vector)
  
  perf.tss <- performance(pred,"sens","spec")
  tss.list <- (perf.tss@x.values[[1]] + perf.tss@y.values[[1]] - 1)
  tss.df <- data.frame(alpha=perf.tss@alpha.values[[1]],tss=tss.list)
  
  auc <- performance(pred,"auc")@y.values[[1]]
  cat('Area under the receiver-operator curve', "\n")
  cat('AUC =', auc, "\n", "\n")
  
  thresh <- min(tss.df$alpha[which(tss.df$tss==max(tss.df$tss))])
  cat('Recommended threshold (maximizes true skill statistic)', "\n")
  cat('Cutoff = ', thresh, "\n")
  cat('TSS = ', tss.df[which(tss.df$alpha==thresh),'tss'], "\n")
  cat('Resulting type I error rate: ',1-perf.tss@y.values[[1]][which(perf.tss@alpha.values[[1]]==thresh)], "\n") # Type I error rate
  cat('Resulting type II error rate: ', 1-perf.tss@x.values[[1]][which(perf.tss@alpha.values[[1]]==thresh)], "\n") # Type II error rate
  
  if(plots == TRUE){
    
    x <- performance(pred, "tpr", "fpr")
    rocdf <- data.frame(fpr=x@x.values[[1]],
                        tpr=x@y.values[[1]])
    g1 <- ggplot(rocdf, aes(x=fpr,y=tpr)) + geom_line() + 
      ggtitle('Receiver-operator curve') + 
      xlab('False positive rate') + 
      ylab('True positive rate') + 
      geom_abline(intercept=0,slope=1,col='red')+ 
      theme_classic()
    
    pnormdf <- data.frame(pnorm = colMeans(pnorm(model$yhat.train)))
    g2 <- ggplot(pnormdf, aes(pnorm)) + geom_histogram(stat='bin', binwidth=0.05) + 
      ylab('Number of training data points') + ggtitle('Fitted values') + 
      xlab('Predicted probability') + 
      theme_classic()
    
    #hist(pnorm(model$yhat.train), xlab='Predicted y', main='Fitted values')
    
    g3 <- ggplot(tss.df, aes(x=alpha,y=tss)) + geom_line() + 
      ggtitle('Threshold-performance curve') + 
      xlab('Threshold') + 
      ylab('True skill statistic') + 
      geom_vline(xintercept=thresh,col='red')+ 
      theme_classic()
    
    obsf <- data.frame(fitted=pnorm(colMeans(model$yhat.train)),
                       classified=as.numeric(pnorm(colMeans(model$yhat.train))>thresh),
                       observed=fitobj$data@y)
    
    g4 <- ggplot(obsf, aes(x=fitted, y=factor(observed), 
                           fill=factor(classified), color=factor(classified))) + 
      geom_jitter(height = 0.2, size=0.9) + xlab('Predicted probability') + 
      ggtitle('Classified fitted values') + 
      ylab('True classification') +  
      #labs(fill = "Thresholded", col='Thresholded') + 
      theme_classic() + theme(legend.position = 'none') + 
      geom_vline(xintercept=thresh,col='black') 
    
    g1 + g2 + g3 + g4 + plot_layout(ncol = 2)
    
  }
  
}


extractSummary.BART <- function(model, species) {
  library(ROCR)
  
  if(class(model)=='bart') {fitobj <- model$fit} else 
    if(class(model)=='rbart') {fitobj <- model$fit[[1]] }
  
  cat("Call: ", paste(model$call), '\n \n')
  
  cat("Predictor list: \n", 
      paste(attr(fitobj$data@x, "term.labels"), sep=' '), "\n", "\n")
  
  true.vector <- fitobj$data@y 
  
  pred <- prediction(colMeans(pnorm(model$yhat.train)), true.vector)
  
  perf.tss <- performance(pred,"sens","spec")
  tss.list <- (perf.tss@x.values[[1]] + perf.tss@y.values[[1]] - 1)
  tss.df <- data.frame(alpha=perf.tss@alpha.values[[1]],tss=tss.list)
  
  auc <- performance(pred,"auc")@y.values[[1]]
  cat('Area under the receiver-operator curve', "\n")
  cat('AUC =', auc, "\n", "\n")
  
  thresh <- min(tss.df$alpha[which(tss.df$tss==max(tss.df$tss))])
  cat('Recommended threshold (maximizes true skill statistic)', "\n")
  cat('Cutoff = ', thresh, "\n")
  cat('TSS = ', tss.df[which(tss.df$alpha==thresh),'tss'], "\n")
  cat('Resulting type I error rate: ',1-perf.tss@y.values[[1]][which(perf.tss@alpha.values[[1]]==thresh)], "\n") # Type I error rate
  cat('Resulting type II error rate: ', 1-perf.tss@x.values[[1]][which(perf.tss@alpha.values[[1]]==thresh)], "\n") # Type II error rate
  
  metrics <- data.frame(Species = species, 
                        AUC = auc, 
                        Threshold_TSS = thresh, 
                        TSS = tss.df[which(tss.df$alpha==thresh),'tss'], 
                        type_I_error = 1-perf.tss@y.values[[1]][which(perf.tss@alpha.values[[1]]==thresh)], 
                        type_II_error = 1-perf.tss@x.values[[1]][which(perf.tss@alpha.values[[1]]==thresh)] 
  )
  return(metrics)
}






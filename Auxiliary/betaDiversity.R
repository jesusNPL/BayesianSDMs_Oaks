tempbetagrid <- function(oc1, oc2, index = "sorensen", phylotree, phylobeta = F){
  require(betapart)
  require(picante)
  require(phytools)
  tempturn <- numeric(nrow(oc1))
  tempnest <- numeric (nrow(oc1))
  tempbeta <- numeric(nrow(oc1))
  tempturnbeta <- numeric (nrow(oc1))
  for(i in 1:nrow(oc1) ){
    svMisc::progress(i, max.value = nrow(oc1), progress.bar = FALSE)
    namesoc1 <- names(oc1)[oc1[i, ] == 1]
    namesoc2 <- names(oc2)[oc2[i, ] == 1]
    both <- namesoc1[namesoc1%in%namesoc2]
    bothmat <- rbind(rep(1, length(both)), rep(1, length(both)))
    colnames(bothmat) <- both
    namoc1 <- namesoc1[namesoc1%in%namesoc2 == FALSE]
    nam1mat <- rbind(rep(1, length(namoc1)), rep(0, length(namoc1)))
    colnames(nam1mat) <- namoc1
    namoc2 <- namesoc2[namesoc2%in%namesoc1 == FALSE]
    nam2mat <- rbind(rep(0, length(namoc2)), rep(1, length(namoc2)))
    colnames(nam2mat) <- namoc2
    matcomp <- cbind(bothmat, nam1mat, nam2mat)
    forprune <- t(data.frame(names(data.frame(matcomp))))
    colnames(forprune) <- forprune
    ifelse(phylobeta == T, betas <- phylo.beta.pair(matcomp, prune.sample(forprune, phylotree), index.family = index), betas <- beta.pair(matcomp, index.family = index) )
    tempturn[i] <- betas[[1]]
    tempnest[i] <- betas[[2]]
    tempbeta[i] <- betas[[3]]
    tempturnbeta[i] <- betas[[1]]/betas[[3]]}
  return(data.frame(turnover = tempturn, nestedness = tempnest, beta = tempbeta, turn.beta = tempturnbeta))
  }

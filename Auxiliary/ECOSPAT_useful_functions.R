# Ecospat useful functions with examples
library(ecospat)

##### Cohen's Kappa #####
# ecospat.cohen.kappa
# Calculates Cohen's kappa and variance estimates, within a 95 percent confidence interval.
Pred <- ecospat.testData$glm_Agrostis_capillaris
Sp.occ <- ecospat.testData$Agrostis_capillaris
th <- 0.39 # threshold
xtab <- table(Pred >= th, Sp.occ)
xtab
ecospat.cohen.kappa(xtab)
ecospat.plot.kappa(Pred, Sp.occ)

##### Maximum TSS #####
# ecospat.max.tss
# Calculates values for True skill statistic (TSS) along different thresholds, 
# considering this time 0.01 increments (i.e. 99 thresholds).
data(ecospat.testData)
Pred <- ecospat.testData$glm_Agrostis_capillaris
Sp.occ <- ecospat.testData$Agrostis_capillaris
TSS100 <- ecospat.max.tss(Pred, Sp.occ)
ecospat.plot.tss(Pred, Sp.occ)

##### Model Evaluation For A Given Threshold Value #####
# ecospat.meva.table
# Calculates values of a series of different evaluations metrics for a model and 
# for a given threshold value
Pred <- ecospat.testData$glm_Agrostis_capillaris
Sp.occ <- ecospat.testData$Agrostis_capillaris

meva <- ecospat.meva.table (Pred, Sp.occ, 0.39)
meva

##### Community Evaluation #####
# ecospat.CommunityEval
# Calculate several indices of accuracy of community predictions.
# This function calculates several indices of accuracy of community predictions 
# based on stacked predictions of species ditribution models. 
# In case proba is set to FALSE the function returns one value per index and per site. 
# In case proba is set to TRUE the function generates presences-absences based on the 
# predicted probabilities and returns one value per index, per site and per trial.

data(ecospat.testData)
eval <- ecospat.testData[c(53,62,58,70,61,66,65,71,69,43,63,56,68,57,55,60,54,67,59,64)]
eval[1:10, 1:3]
pred <- ecospat.testData[c(73:92)]
pred[1:10, 1:3]

x <- ecospat.CommunityEval(eval, pred, proba = FALSE, ntir = 10)

y <- ecospat.CommunityEval(eval, pred, proba = TRUE, ntir = 100)

##### Pairwise Co-Occurrence Analysis With Calculation Of The C-Score Index #####
# ecospat.Cscore
# The function tests for nonrandom patterns of species co-occurrence in a presence-absence 
# matrix. It calculates the C-score index for the whole community and for each species pair. 
# Null communities have column sum fixed.
data <- ecospat.testData[c(53,62,58,70,61,66,65,71,69,43,63,56,68,57,55,60,54,67,59,64)]
nperm <- 10000
outpath <- getwd()
ecospat.Cscore(data, nperm, outpath)

##### Calculate Boyce Index #####
# ecospat.boyce
# Calculate the Boyce index as in Hirzel et al. (2006). 
# The Boyce index is used to assess model performance.

obs <- (ecospat.testData$glm_Saxifraga_oppositifolia
        [which(ecospat.testData$Saxifraga_oppositifolia==1)])

ecospat.boyce(fit = ecospat.testData$glm_Saxifraga_oppositifolia, obs, nclass = 0, 
               window.w = "default", res = 100, PEplot = TRUE)

##### Calculate Phylogenetic Diversity Measures #####
# ecospat.calculate.pd
# Calculate all phylogenetic diversity measures listed in 
# Schweiger et al., 2008 (see full reference below).
fpath <- system.file("extdata", "ecospat.testTree.tre", package = "ecospat")
tree <- read.tree(fpath)
data <- ecospat.testData[9:52] 

pd <- ecospat.calculate.pd(tree, data, method = "spanning", type = "species", root = FALSE, 
                           average = FALSE, verbose = TRUE )

plot(pd)




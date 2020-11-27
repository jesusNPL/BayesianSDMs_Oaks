# probability ranking rule - adapted from the ecospat package
SESAM.prr <- function (proba, sr) {
  projSR <- round(round(as.vector(sr[[1]])))
  new.prob.prr <- proba
  dataSSDM_p <- proba
  for (i in 1:nrow(proba)) {
    print(paste("test.prr, processing row ", i, sep = ""))
    SR <- projSR[i]
    if (SR > 0) {
      predcom <- dataSSDM_p[i, ]
      predcom_p <- dataSSDM_p[i, ]
      com <- order(predcom_p, decreasing = TRUE)
      pres <- com[1:SR]
      predcom[, pres] <- 1
      predcom[, -pres] <- 0
    }
    else {
      predcom[, ] <- 0
    }
    new.prob.prr[i, ] <- predcom
  }
  new.prob.prr
}

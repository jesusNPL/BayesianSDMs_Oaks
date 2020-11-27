makeEvalTABLE <- function(Evaluations){
  tabla <- data.frame("DevianceMEAN" = apply(Evaluations$deviation.rich.pred, 1, mean), 
                      "DevianceSD" = apply(Evaluations$deviation.rich.pred, 1, sd), 
                      "OverpredictionMEAN" = apply(Evaluations$overprediction, 1, mean), 
                      "OverpredictionSD" = apply(Evaluations$overprediction, 1, sd), 
                      "UnderpredictionMEAN" = apply(Evaluations$underprediction, 1, mean), 
                      "UnderpredictionSD" = apply(Evaluations$underprediction, 1, sd), 
                      "SuccessMEAN" = apply(Evaluations$prediction.success, 1, mean), 
                      "SuccessSD" = apply(Evaluations$prediction.success, 1, sd), 
                      "SensitivityMEAN" = apply(Evaluations$sensitivity, 1, mean), 
                      "SensitivitySD" = apply(Evaluations$sensitivity, 1, sd), 
                      "SpecificityMEAN" = apply(Evaluations$specificity, 1, mean), 
                      "SpecificitySD" = apply(Evaluations$specificity, 1, sd), 
                      "kappaMEAN" = apply(Evaluations$kappa, 1, mean), 
                      "kappaSD" = apply(Evaluations$kappa, 1, sd), 
                      "tssMEAN" = apply(Evaluations$TSS, 1, mean), 
                      "tssSD" = apply(Evaluations$TSS, 1, sd), 
                      "SorensenMEAN" = apply(Evaluations$similarity, 1, mean), 
                      "SorensenSD" = apply(Evaluations$similarity, 1, sd), 
                      "JaccardMEAN" = apply(Evaluations$Jaccard, 1, mean), 
                      "JaccardSD" = apply(Evaluations$Jaccard, 1, sd))
  return(tabla)
}

makeEvalTABLEII <- function(Evaluations){
  tabla <- data.frame("DevianceMAX" = apply(Evaluations$deviation.rich.pred, 1, max), 
                      "DevianceMIN" = apply(Evaluations$deviation.rich.pred, 1, min), 
                      "OverpredictionMAX" = apply(Evaluations$overprediction, 1, max), 
                      "OverpredictionMIN" = apply(Evaluations$overprediction, 1, min), 
                      "UnderpredictionMAX" = apply(Evaluations$underprediction, 1, max), 
                      "UnderpredictionMIN" = apply(Evaluations$underprediction, 1, min), 
                      "SuccessMAX" = apply(Evaluations$prediction.success, 1, max), 
                      "SuccessMIN" = apply(Evaluations$prediction.success, 1, min), 
                      "SensitivityMAX" = apply(Evaluations$sensitivity, 1, max), 
                      "SensitivityMIN" = apply(Evaluations$sensitivity, 1, min), 
                      "SpecificityMAX" = apply(Evaluations$specificity, 1, max), 
                      "SpecificityMIN" = apply(Evaluations$specificity, 1, min), 
                      "kappaMAX" = apply(Evaluations$kappa, 1, max), 
                      "kappaMIN" = apply(Evaluations$kappa, 1, min), 
                      "tssMAX" = apply(Evaluations$TSS, 1, max), 
                      "tssMIN" = apply(Evaluations$TSS, 1, min), 
                      "SorensenMAX" = apply(Evaluations$similarity, 1, max), 
                      "SorensenMIN" = apply(Evaluations$similarity, 1, min), 
                      "JaccardMAX" = apply(Evaluations$Jaccard, 1, max), 
                      "JaccardMIN" = apply(Evaluations$Jaccard, 1, min))
  return(tabla)
}
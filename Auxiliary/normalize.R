normalize <- function(ras){
        resu <- (ras-minValue(ras))/(maxValue(ras)-minValue(ras))
}

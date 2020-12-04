library(terra)

### Inits
#load("NEW_oakSDM/Calibration/params.RData") # Species names, covariate names
#direct <- "NEW_oakSDM/BayesianPredictions/"
#suffix <- "_Predictions.tif"


makeMAPS.BART <- function(sppNames, direction) {
        require(raster)
        library(rasterVis)
        library(RColorBrewer)
        
        if (!dir.exists(paste0(direction, "Uncertainty"))) {
                dir.create(paste0(direction, "Uncertainty"))
        }
        
        if (!dir.exists(paste0(direction, "MAPS"))) {
                dir.create(paste0(direction, "MAPS"))
        }
        
        mapTheme <- rasterTheme(region = rev(brewer.pal(11, "Spectral")),
                                layout.widths = list(right.padding = 10),
                                axis.line = list(col = "transparent"),
                                tick = list(col = 'transparent'))
        
        
        mapTheme2 <- rasterTheme(region = brewer.pal(9, "Blues"),
                                layout.widths = list(right.padding = 10),
                                axis.line = list(col = "transparent"),
                                tick = list(col = 'transparent'))
        
        for(k in 1:length(sppNames)) {
                print(sppNames[k])
                
                tmpRAS <- readRDS(paste0(direction, sppNames[k], "_prediction.rds"))
                
                uncRAS <- tmpRAS[[3]] - tmpRAS[[2]]
                writeRaster(uncRAS, filename = paste0(direction, "Uncertainty/", sppNames[k], 
                                                    "_uncertainty", sep = ""), format = "GTiff", 
                            overwrite = TRUE)
                
                pdf(paste0(direction, "MAPS/", sppNames[k], "_probabilityMAP.pdf"), 
                    width = 10, height = 8, bg = "transparent")
                levelplot(tmpRAS[[1]],
                          maxpixels = 1e10,
                          margin = FALSE,
                          par.settings = mapTheme,
                          scales = list(x = list(draw = FALSE),
                                        y = list(draw = FALSE)),
                          zlim = c(0, 1))
                
                grid::grid.text('Probability of presence',
                                rot = 90,
                                y = unit(0.5, "npc"),
                                x = unit(0.925, "npc"),
                                gp = grid::gpar(fontsize = 15))
                dev.off()
                
                pdf(paste0(direction, "MAPS/", sppNames[k], "_uncertaintyMAP.pdf"), 
                    width = 10, height = 8, bg = "transparent")
                levelplot(uncRAS,
                          maxpixels = 1e10,
                          margin = FALSE,
                          par.settings = mapTheme2,
                          scales = list(x = list(draw = FALSE),
                                        y = list(draw = FALSE)),
                          zlim = c(0, 1))
                grid::grid.text('Posterior width',
                                rot = 90,
                                y = unit(0.5, "npc"),
                                x = unit(0.925, "npc"),
                                gp = grid::gpar(fontsize = 15))
                dev.off()
                print(paste0("Your MAPS based on BART models for ", sppNames[k], 
                             " are complete, please check folder MAPS", sep = ""))
        }
}

# README ------------------------------------------------------------------

# this script separates and compiles the simulation study results by 
# influence metric


# libraries ---------------------------------------------------------------

library(tidyverse)


# select parameters -------------------------------------------------------

# should be same as number of iterations used in the sim_study script
n = 100

# methods to loop through
methods = c("wbcv", "ia")

# list of design parameters to loop through
plumes <- c("simple", "mid", "complex")
designs <- c("random", "grid", "expert")
wellnrs <- c(6, 12, 24)
errors <- c("additive", "multiplicative")

# loop through plumes -----------------------------------------------------

for (a in plumes) {
  plume = a
  
  
# loop through designs ----------------------------------------------------
  
  for (b in designs) {
    design = b
    
    
# loop through well numbers -----------------------------------------------
    
    for (c in wellnrs) {
      wells = c
      
      
# loop through error types ------------------------------------------------
      
      for (d in errors) {
        error = d
        

# loop through methods ----------------------------------------------------

        for (e in methods) {
          method = e

  
# load well network -------------------------------------------------------
          
          load(paste0("data/", design, "_", wells, ".RData"))
          
          
# create dfs for results --------------------------------------------------
          
          wbcv_results <- well_coords
          leverage_results <- data.frame("well.id" = rep(c(1:wells), each=20))
          standresids_results <- data.frame("well.id" = rep(c(1:wells), each=20))
          cd_results <- data.frame("well.id" = rep(c(1:wells), each=20))
          diffits_results <- data.frame("well.id" = rep(c(1:wells), each=20))
          hp_results <- data.frame("well.id" = rep(c(1:wells), each=20))
          covratio_results <- data.frame("well.id" = rep(c(1:wells), each=20))
          
          
# start loop --------------------------------------------------------------
          
          for (i in 1:n) {
            
          
# if method is wbcv -------------------------------------------------------
          
            if (method == "wbcv") {
              
# load results ------------------------------------------------------------
              
              # load results
              load(paste0("results/wbcv_", plume, "_", design, "_", wells, "_", error, "_", i, ".RData"))
              
              # load full model rmse values
              # load(paste0("results_gwsdat_default/rmse", "_", plume, "_", design, "_", wells, "_", error, "_", i, ".RData"))
              
          
# data manipulation -------------------------------------------------------
          
              # creating new column
              results_df <- as.data.frame(t(as.data.frame(listOfRMSEs)), row.names = c(1:wells))
              
              # renaming column
              colnames(results_df) <- c(paste0("RMSE", i))
              
              # appending new column
              wbcv_results[ ,ncol(wbcv_results) + 1] <- results_df
            
          
# plotting wbcv results ---------------------------------------------------
              # optional
              
              # RMSE = wbcv_results[,paste0("RMSE", i)]
              # 
              # plt <- ggplot(
              #   data = results,
              #   mapping = aes(x = X1, y = X2, label = rownames(results))) +
              #   geom_point(mapping = aes(color = RMSE), size = 8) +
              #   scale_color_gradient2(midpoint = (min(RMSE) + max(RMSE))/2,
              #                         high = "brown4", mid = "orange", low = "yellow") +
              #   geom_text(hjust=0.4, vjust=0.3) +
              #   annotate("label", x=4, y=37,
              #            label = paste0("complete model's RMSE = ", rmse),
              #            hjust=0, vjust=0, size=4)
              # 
              # 
              # # save plot
              # png(filename = paste0("results/plt_", plume, "_", design, "_", wells, "_", error, "_", i, ".png"))
              # plot(plt)
              # dev.off()
            
          
# end of wbcv processing --------------------------------------------------
          
# if method is ia ---------------------------------------------------------
          
            } else if (method == "ia") {
              
              
# load results ------------------------------------------------------------
              
              # load results
              load(paste0("results/ia_", plume, "_", design, "_", wells, "_", error, "_", i, ".RData"))
              
          
# data manipulation -------------------------------------------------------
          
              # goal: for each ia metric have a df that contains the well id
              # and the results for each of the 100 iterations.
              
              ##### Need to change column IDs below!!! imetrics df changed
          
# leverages ---------------------------------------------------------------
          
              # isolating well.id and leverages
              leverages <- as.data.frame(imetrics[,2])
              
              # renaming results to indicate iteration number
              colnames(leverages) <- c(paste0("leverage", i))
              
              # adding new column to df in each iteration
              leverage_results[ ,ncol(leverage_results) + 1] <- leverages
              
              
# standardized residuals --------------------------------------------------
              
              # isolating well.id and standres
              standres <- as.data.frame(imetrics[,4])
              
              # renaming results to indicate iteration number
              colnames(standres) <- c(paste0("standres", i))
              
              # adding new column to df in each iteration
              standresids_results[ ,ncol(standresids_results) + 1] <- standres
              
              
# cook's distance ---------------------------------------------------------
              
              # isolating well.id and cd
              cd <- as.data.frame(imetrics[,5])
              
              # renaming results to indicate iteration number
              colnames(cd) <- c(paste0("cd", i))
              
              # adding new column to df in each iteration
              cd_results[ ,ncol(cd_results) + 1] <- cd
              
              
# diffits -----------------------------------------------------------------
              
              # isolating well.id and diffits
              diffits <- as.data.frame(imetrics[,7])
              
              # renaming results to indicate iteration number
              colnames(diffits) <- c(paste0("diffits", i))
              
              # adding new column to df in each iteration
              diffits_results[ ,ncol(diffits_results) + 1] <- diffits
              
          
# hadi's potential --------------------------------------------------------
              
              # isolating well.id and hp
              hp <- as.data.frame(imetrics[,8])
              
              # renaming results to indicate iteration number
              colnames(hp) <- c(paste0("hp", i))
              
              # adding new column to df in each iteration
              hp_results[ ,ncol(hp_results) + 1] <- hp
              
              
# covratio ----------------------------------------------------------------
              
              # isolating well.id and covratio
              covratio <- as.data.frame(imetrics[,9])
              
              # renaming results to indicate iteration number
              colnames(covratio) <- c(paste0("covratio", i))
              
              # adding new column to df in each iteration
              covratio_results[ ,ncol(covratio_results) + 1] <- covratio
              
              
# end of ia processing ----------------------------------------------------
          
            } else {
              
              print("Error! Something must have gone wrong.")
              
            }
            
            
# end loop ----------------------------------------------------------------
          
          }  
          
# save compiled results ---------------------------------------------------
          
          if (method == "wbcv") {
            save(wbcv_results, file = paste0("results/wbcv_", plume, "_", design, "_", wells, "_", error, "_compiled.RData"))
          } else if (method == "ia") {
            save(leverage_results, file = paste0("results/leverage_", plume, "_", design, "_", wells, "_", error, "_compiled.RData"))
            save(standresids_results, file = paste0("results/standres_", plume, "_", design, "_", wells, "_", error, "_compiled.RData"))
            save(cd_results, file = paste0("results/cd_", plume, "_", design, "_", wells, "_", error, "_compiled.RData"))
            save(diffits_results, file = paste0("results/diffits_", plume, "_", design, "_", wells, "_", error, "_compiled.RData"))
            save(hp_results, file = paste0("results/hp_", plume, "_", design, "_", wells, "_", error, "_compiled.RData"))
            save(covratio_results, file = paste0("results/covratio_", plume, "_", design, "_", wells, "_", error, "_compiled.RData"))
          } else {
            print("Error when saving results")
          }
          
        }
      }
    }
  }
}







# README ------------------------------------------------------------------

# this script analyses the simulation study results 
# compares wbcv and ia orders, finds the winner


# libraries ---------------------------------------------------------------

library(tidyverse)


# select parameters -------------------------------------------------------

# should be same as number of iterations used in the sim_study script
n = 100

# list of design parameters to loop through
plumes <- c("simple", "mid", "complex")
designs <- c("random", "grid", "expert")
wellnrs <- c(6, 12, 24)
errors <- c("additive", "multiplicative")


# start loops -------------------------------------------------------------


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
        
        
# load compiled results ---------------------------------------------------
        
        load(paste0("results/wbcv_", plume, "_", design, "_", wells, "_", error, "_compiled.RData"))
        load(paste0("results/leverage_", plume, "_", design, "_", wells, "_", error, "_compiled.RData"))
        load(paste0("results/standres_", plume, "_", design, "_", wells, "_", error, "_compiled.RData"))
        load(paste0("results/cd_", plume, "_", design, "_", wells, "_", error, "_compiled.RData"))
        load(paste0("results/diffits_", plume, "_", design, "_", wells, "_", error, "_compiled.RData"))
        load(paste0("results/hp_", plume, "_", design, "_", wells, "_", error, "_compiled.RData"))
        load(paste0("results/covratio_", plume, "_", design, "_", wells, "_", error, "_compiled.RData"))
        
        
# data manipulation -------------------------------------------------------
        
        
# removing coordinates from wbcv_results ----------------------------------
        
        wbcv_summary <- wbcv_results %>%
            select(-(X1:X2))
        
        
# calculating summary of ia metrics per well ------------------------------
        
        # median leverage per well for each iteration
        leverage_summary <- leverage_results %>%
          group_by(well.id) %>%
          summarise_all(.funs = median)
        
        # variance of standardised residuals per well for each iteration
        standres_summary <- standresids_results %>%
          group_by(well.id) %>%
          summarise_all(.funs = mad)
        
        # median cd per well for each iteration
        cd_summary <- cd_results %>%
          group_by(well.id) %>%
          summarise_all(.funs = median)
        
        # median diffits per well for each iteration
        diffits_summary <- diffits_results %>%
          group_by(well.id) %>%
          summarise_all(.funs = median)
        
        # median hp per well for each iteration
        hp_summary <- hp_results %>%
          group_by(well.id) %>%
          summarise_all(.funs = median)
        
        # median covratio per well for each iteration
        covratio_summary <- covratio_results %>%
          group_by(well.id) %>%
          summarise_all(.funs = median)
        
        
# wbcv order --------------------------------------------------------------
        
        # creating empty data frame
        wbcv_order <- data.frame(matrix(ncol = 0, nrow = wells))
        
        # appending the well order of each iteration to the above df
        for (j in 1:n) {
          order <- order(wbcv_summary[,paste0("RMSE",j)], decreasing=TRUE)
          wbcv_order[ ,ncol(wbcv_order) + 1] <- order
        }
        
        
# ia metric orders --------------------------------------------------------
        
        # same process as above for wbcv
        
        # leverage
        leverage_order <- data.frame(matrix(ncol = 0, nrow = wells))
        
        for (k in 1:n) {
          order <- order(leverage_summary[,paste0("leverage",k)], decreasing=TRUE)
          leverage_order[ ,ncol(leverage_order) + 1] <- order
        }
        
        # standres
        standres_order <- data.frame(matrix(ncol = 0, nrow = wells))
        
        for (l in 1:n) {
          order <- order(standres_summary[,paste0("standres",l)], decreasing=TRUE)
          standres_order[ ,ncol(standres_order) + 1] <- order
        }
        
        # cd
        cd_order <- data.frame(matrix(ncol = 0, nrow = wells))
        
        for (m in 1:n) {
          order <- order(cd_summary[,paste0("cd",m)], decreasing=TRUE)
          cd_order[ ,ncol(cd_order) + 1] <- order
        }
        
        # diffits
        diffits_order <- data.frame(matrix(ncol = 0, nrow = wells))
        
        for (s in 1:n) {
          order <- order(diffits_summary[,paste0("diffits",s)], decreasing=TRUE)
          diffits_order[ ,ncol(diffits_order) + 1] <- order
        }
        
        # hp
        hp_order <- data.frame(matrix(ncol = 0, nrow = wells))
        
        for (o in 1:n) {
          order <- order(hp_summary[,paste0("hp",o)], decreasing=TRUE)
          hp_order[ ,ncol(hp_order) + 1] <- order
        }
        
        # covratio
        covratio_order <- data.frame(matrix(ncol = 0, nrow = wells))
        
        for (p in 1:n) {
          order <- order(covratio_summary[,paste0("covratio",p)], decreasing=TRUE)
          covratio_order[ ,ncol(covratio_order) + 1] <- order
        }
        
        
# computing well order differences ----------------------------------------
        
        # creating empty data frames for each metric
        leverage_scores <- data.frame(matrix(ncol = 0, nrow = 1))
        standres_scores <- data.frame(matrix(ncol = 0, nrow = 1))
        cd_scores <- data.frame(matrix(ncol = 0, nrow = 1))
        diffits_scores <- data.frame(matrix(ncol = 0, nrow = 1))
        hp_scores <- data.frame(matrix(ncol = 0, nrow = 1))
        covratio_scores <- data.frame(matrix(ncol = 0, nrow = 1))
        
        # for each well within each iteration this calculates the sum of differences 
        # in well rankings for each metric and appends the results to the above dfs
        for (q in 1:n) {
          
          leverage_diff_list <- list()
          standres_diff_list <- list()
          cd_diff_list <- list()
          diffits_diff_list <- list()
          hp_diff_list <- list()
          covratio_diff_list <- list()
          
          for (r in 1:wells) {
            
            assign(paste0("leverage_diff", r), abs(which(wbcv_order[,q] == r) - which(leverage_order[,q] == r)))
            leverage_diff_list[[r]] = get(paste0("leverage_diff", r))
            
            assign(paste0("standres_diff", r), abs(which(wbcv_order[,q] == r) - which(standres_order[,q] == r)))
            standres_diff_list[[r]] = get(paste0("standres_diff", r))
            
            assign(paste0("cd_diff", r), abs(which(wbcv_order[,q] == r) - which(cd_order[,q] == r)))
            cd_diff_list[[r]] = get(paste0("cd_diff", r))
            
            assign(paste0("diffits_diff", r), abs(which(wbcv_order[,q] == r) - which(diffits_order[,q] == r)))
            diffits_diff_list[[r]] = get(paste0("diffits_diff", r))
            
            assign(paste0("hp_diff", r), abs(which(wbcv_order[,q] == r) - which(hp_order[,q] == r)))
            hp_diff_list[[r]] = get(paste0("hp_diff", r))
            
            assign(paste0("covratio_diff", r), abs(which(wbcv_order[,q] == r) - which(covratio_order[,q] == r)))
            covratio_diff_list[[r]] = get(paste0("covratio_diff", r))
          }
          
          leverage_scores[ ,ncol(leverage_scores) + 1] <- Reduce("+", leverage_diff_list)
          standres_scores[ ,ncol(standres_scores) + 1] <- Reduce("+", standres_diff_list)
          cd_scores[ ,ncol(cd_scores) + 1] <- Reduce("+", cd_diff_list)
          diffits_scores[ ,ncol(diffits_scores) + 1] <- Reduce("+", diffits_diff_list)
          hp_scores[ ,ncol(hp_scores) + 1] <- Reduce("+", hp_diff_list)
          covratio_scores[ ,ncol(covratio_scores) + 1] <- Reduce("+", covratio_diff_list)
          
        }
        
        
# mean well order difference for each metric ------------------------------
        
        mean_scores <- data.frame(leverage = rowMeans(leverage_scores),
                                  standres = rowMeans(standres_scores),
                                  cd = rowMeans(cd_scores),
                                  diffits = rowMeans(diffits_scores),
                                  hp = rowMeans(hp_scores),
                                  covratio = rowMeans(covratio_scores))
        
        # the metric with the least mean difference in well placements
        winner <- which.min(mean_scores)
        
        
# save mean differences ---------------------------------------------------
        
        save(mean_scores, file = paste0("results/", plume, "_", design, "_", wells, "_", error, "_mean_diffs.RData"))
        save(winner, file = paste0("results/", plume, "_", design, "_", wells, "_", error, "_winner.RData"))
        
        
# save difference scores --------------------------------------------------
        
        save(leverage_scores, file = paste0("results/", plume, "_", design, "_", wells, "_", error, "_leverage_scores.RData"))
        save(standres_scores, file = paste0("results/", plume, "_", design, "_", wells, "_", error, "_standres_scores.RData"))
        save(cd_scores, file = paste0("results/", plume, "_", design, "_", wells, "_", error, "_cd_scores.RData"))
        save(diffits_scores, file = paste0("results/", plume, "_", design, "_", wells, "_", error, "_diffits_scores.RData"))
        save(hp_scores, file = paste0("results/", plume, "_", design, "_", wells, "_", error, "_hp_scores.RData"))
        save(covratio_scores, file = paste0("results/", plume, "_", design, "_", wells, "_", error, "_covratio_scores.RData"))
        
        # saving orders too
        save(wbcv_order, file = paste0("results/", plume, "_", design, "_", wells, "_", error, "_wbcv_order.RData"))
        save(leverage_order, file = paste0("results/", plume, "_", design, "_", wells, "_", error, "_leverage_order.RData"))
        save(standres_order, file = paste0("results/", plume, "_", design, "_", wells, "_", error, "_standres_order.RData"))
        save(cd_order, file = paste0("results/", plume, "_", design, "_", wells, "_", error, "_cd_order.RData"))
        save(diffits_order, file = paste0("results/", plume, "_", design, "_", wells, "_", error, "_diffits_order.RData"))
        save(hp_order, file = paste0("results/", plume, "_", design, "_", wells, "_", error, "_hp_order.RData"))
        save(covratio_order, file = paste0("results/", plume, "_", design, "_", wells, "_", error, "_covratio_order.RData"))

  
# end loops ---------------------------------------------------------------

      }
    }
  }
}


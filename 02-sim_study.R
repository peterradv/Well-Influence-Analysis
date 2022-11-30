
# README ------------------------------------------------------------------

# simulation study script:
# 1. perform well-based cross-validation
# 2. compute influence analysis metrics for each observation


# libraries ---------------------------------------------------------------

library(tidyverse)


# load gwsdat code --------------------------------------------------------

source('sm-st.R')


# select parameters -------------------------------------------------------

# number of iterations per scenario 
n = 100

# signal to noise ratio for applying random noise
snr = 0.15

# list of design parameters to loop through
plumes <- c("simple", "mid", "complex")
designs <- c("random", "grid", "expert")
wellnrs <- c(6, 12, 24)
errors <- c("additive", "multiplicative")

# model parameters
nseg<-c(6,6,6)
bdeg<-2
pord<-1
lambda.rel.time<-1
lambda.rel<-c(1,1,lambda.rel.time)
computeD=FALSE
ndims = 3

  

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
        
# start main loop ---------------------------------------------------------
        
        for (i in 1:n) {


# data loading and manipulation -------------------------------------------
        
        
# loading selected plume data ---------------------------------------------
        
          if (plume == "simple"){
            load("data/simple_plume.RData") # simple
          } else if (plume == "mid") {
            load("data/mid_plume.RData") # mid
          } else if (plume == "complex") {
            load("data/complex_plume.RData") # complex
          } else {
            print("Please select plume type.")
          }
        
        
# loading selected network design -----------------------------------------
        
          if (design == "random" & wells == 6) {
            load("data/random_6.RData") # random placement 6 wells
          } else if (design == "random" & wells == 12) {
            load("data/random_12.RData") # random placement 12 wells
          } else if (design == "random" & wells == 24) {
            load("data/random_24.RData") # random placement 24 wells
          } else if (design == "grid" & wells == 6) {
            load("data/grid_6.RData") # grid placement 6 wells
          } else if (design == "grid" & wells == 12) {
            load("data/grid_12.RData") # grid placement 12 wells
          } else if (design == "grid" & wells == 24) {
            load("data/grid_24.RData") # grid placement 24 wells
          } else if (design == "expert" & wells == 6) {
            load("data/expert_6.RData") # expert placement 6 wells
          } else if (design == "expert" & wells == 12) {
            load("data/expert_12.RData") # expert placement 12 wells
          } else if (design == "expert" & wells == 24) {
            load("data/expert_24.RData") # expert placement 24 wells
          } else {
            print("Please select network design.")
          }
        

# create observations data ------------------------------------------------
          
          observations <- left_join(well_coords, true.data)
        
        
# adding noise based on error type ----------------------------------------
          
          if (error == "additive") {
            set.seed(i)
            observations$y <- observations$y + rnorm(nrow(observations), 0, snr)
          } else if (error == "multiplicative") {
            set.seed(i)
            observations$y <- observations$y * rnorm(nrow(observations), 1, snr)
          } else {
            print("Please select error type.")
          }
          
          
# log-transforming data ---------------------------------------------------
          
          observations$y <- log(1+observations$y)
        
        
# well-based cross-validation ---------------------------------------------
        
          # wbcv.start <- Sys.time()
        
# creating missing well data sets -----------------------------------------
        
          # prepare list of observation data frames (duplicates of observations)
          listOfFiles <- rep(list(observations),wells)
          
          # empty list for removed well data sets
          removedList <- list()
          
          # creating list of data sets with removed wells
          for (j in seq(listOfFiles)) {
            removedList[[j]]= (
              listOfFiles[[j]] %>% filter(well.id != j)
            )
          }
          
          # renaming dfs of removedList to indicate which well was removed)
          names(removedList) <- paste0("removed_", c(1:wells))
        
        
# fitting models ----------------------------------------------------------
        
          # empty list of models, will append models in the loop
          listOfModels <- list()
          
          # loop through removedList, fit models and save them in a list
          for (k in seq(removedList)) {
            listOfModels[[k]] = sm.st(
              x = removedList[[k]][,2:3],
              t = removedList[[k]][,4],
              y = removedList[[k]][,5],
              lambda = log.post,
              nseg = nseg
            )
          }
        
          # renaming models in list
          names(listOfModels) <- paste0("m_", c(1:wells))
        
# extracting fitted values ------------------------------------------------
          
          # create a list of the data of the removed wells
          listOfRemoved <- list()
          
          # we need this for making the predictions at these coordinates and times
          for (q in seq(listOfFiles)) {
            listOfRemoved[[q]]= (
              listOfFiles[[q]] %>% filter(well.id == q)
            )
          }
          
          # empty list of fitted values, will append to this in the loop
          listOfFitted <- list()
          
          # loop through models and extract predictions for the coordinates of the removed well
          for (l in seq(listOfModels)) {
            listOfFitted[[l]] = predict.smst(
              listOfModels[[l]], listOfRemoved[[l]][,1:4]
            )
          }
          
          # rename items of fitted values list
          names(listOfFitted) <- paste0("fitted_m_", c(1:wells))
        
        
# computing RMSEs ---------------------------------------------------------
        
          # creating empty list to collect RMSEs
          listOfRMSEs <- list()
          
          # calculate RMSE for the removed wells (observation - predicted at the removed well)
          for (m in seq(removedList)) {
            listOfRMSEs[[m]] = sqrt(
              sum(
                (listOfRemoved[[m]]$y - listOfFitted[[m]])^2
              )/length(listOfFitted[[m]])
            )
          }
          
          # renaming elements in RMSE list
          names(listOfRMSEs) <- paste0("rmse_", c(1:wells))
          
          # wbcv.end <- Sys.time()
          # wbcv.time <- wbcv.end - wbcv.start
        
        
# full model RMSE ---------------------------------------------------------
        
          # ia.start <- Sys.time() 
          
          model <- sm.st(
            x = observations[,2:3],
            t = observations[,4],
            y = observations[,5],
            lambda = log.post,
            nseg = nseg
          )
          
          rmse <- sqrt(sum((observations[,5] - fitted.smst(model))^2)/nrow(observations))
          
          
# influence analysis ------------------------------------------------------
        
        
# hat martix --------------------------------------------------------------
        
          # computing B and P
          mat    <- st.matrices(model$X, t(apply(model$X, 2, range)), ndims = ndims, 
                                pord=pord, bdeg=bdeg, lambda.rel=lambda.rel, computeD=computeD)
          B      <- mat$B
          P      <- mat$P
          
          # computing H
          hatmat <- B %*% solve((t(B) %*% B + model$lambda * P)) %*% t(B)
          
        
# leverages ---------------------------------------------------------------
        
          # leverages are the diagonal elements of the projection matrix
          leverages <- as.data.frame(diag(hatmat))
          
          # adding well ID to leverages
          imetrics <- cbind(observations$well.id, leverages)
          
          # renaming columns
          imetrics <- imetrics %>%
            rename(well.id = `observations$well.id`) %>%
            rename(leverage = `diag(hatmat)`)
          
        
# residuals ---------------------------------------------------------------
        
          # computing residuals from the model
          residuals <- as.data.frame(residuals.smst(model))
          
          # adding residuals to the results
          imetrics <- cbind(imetrics, residuals)
          
          # renaming columns
          imetrics <- imetrics %>%
            rename(residual = `V1`)
          
        
# standardised residuals --------------------------------------------------
        
          # number of observations
          N <- length(imetrics$residual)
          
          # effective degrees of freedom
          df <- sum(diag(hatmat))
          
          # calculate standard error of residuals - internal studentization
          RSE <- sqrt(sum(residuals^2)/(N-df))
          
          # calculate standardized residuals and add to results
          imetrics <- imetrics %>%
            mutate(standresids = residual/(RSE*sqrt(1-leverage)))
          
        
# cook's distance ---------------------------------------------------------
        
          # adding cook's D column to results
          imetrics <- imetrics %>%
            mutate(cd = (1/df)*(standresids^2)*(leverage/(1-leverage)))
          
        
# dffits -----------------------------------------------------------------
        
          # standard error estimate for external studentization
          RSEe <- data.frame()
          
          # calculate standard error of residuals - external studentization
          for (z in 1:nrow(residuals)) {
            eresids <- residuals %>% filter(row_number() != z)
            rsee <- sqrt(sum(eresids^2)/(N-df-1))
            RSEe[z,1] <- rsee
          }
          
          # externally standardized residuals
          esr <- data.frame()
          
          # calculate externally studentized residuals
          for (v in 1:nrow(residuals)) {
            e <- residuals[v,]/(RSEe[v,]*sqrt(1-leverages[v,]))
            esr[v,1] <- e
          }
          
          # adding externally standardised residuals to imetrics
          imetrics <- cbind(imetrics, esr)
          
          # renaming column
          imetrics <- imetrics %>% 
            rename(esr = V1)
          
          # adding diffits column
          imetrics <- imetrics %>%
            mutate(diffits = esr*sqrt(leverage/(1-leverage)))
          
        
# hadi's potential --------------------------------------------------------
        
          # adding hadi potential column
          imetrics <- imetrics %>%
            mutate(hp = (df*(residual/sqrt(1-leverage))^2)/(1-(1-leverage)*(residual/sqrt(1-leverage))^2) + leverage/(1-leverage))
          
        
# covratio ----------------------------------------------------------------
        
          # adding covratio column
          imetrics <- imetrics %>%
            mutate(covratio = 1/(((((N-df-1)/(N-df))+((esr^2)/(N-df)))^df)*(1-leverage)))
          
          # ia.end <- Sys.time()
          # ia.time <- ia.end - ia.start
          
        
# saving results ----------------------------------------------------------
        
          save(listOfRMSEs, file = paste0("results/wbcv_", plume, "_", design, "_", wells, "_", error, "_", i, ".RData"))
          #save(rmse, file = paste0("results/rmse_", plume, "_", design, "_", wells, "_", error, "_", i, ".RData"))
          #save(observations, file = paste0("results/obs_", plume, "_", design, "_", wells, "_", error, "_", i, ".RData"))
          #save(listOfModels, file = paste0("results/listofmodels_", plume, "_", design, "_", wells, "_", error, "_", i, ".RData"))
          #save(model, file = paste0("results/model_", plume, "_", design, "_", wells, "_", error, "_", i, ".RData"))
          save(imetrics, file = paste0("results/ia_", plume, "_", design, "_", wells, "_", error, "_", i, ".RData"))
          #save(wbcv.time, file = paste0("results/time_wbcv_", plume, "_", design, "_", wells, "_", error, "_", i, ".RData"))
          #save(ia.time, file = paste0("results/time_ia_", plume, "_", design, "_", wells, "_", error, "_", i, ".RData"))

          
# end main loop -----------------------------------------------------------
          
        } # end of main loop
        
          
# end error types loop ----------------------------------------------------

      } # end of error types loop
      

# end well numbers loop ---------------------------------------------------
      
    } # end of wells loop
    

# end network designs loop ------------------------------------------------
    
  } # end of design loop


# end plume loop -----------------------------------------------------------

} # end of plume loop


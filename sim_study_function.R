

well_influence_sim <- function(plume, design, wells, error, snr, nseg, bdeg) {
  
  # data loading and manipulation -------------------------------------------
  
  source("sm-st.R")
  
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
    #set.seed(i)
    observations$y <- observations$y + rnorm(nrow(observations), 0, snr)
  } else if (error == "multiplicative") {
    #set.seed(i)
    observations$y <- observations$y * rnorm(nrow(observations), 1, snr)
  } else {
    print("Please select error type.")
  }
  
  
  # log-transforming data ---------------------------------------------------
  
  observations$y <- log(1+observations$y)
  
  
  # well-based cross-validation ---------------------------------------------
  
  
  # creating missing well data sets -----------------------------------------
  
  # prepare list of observation data frames (duplicates of observations)
  listOfFiles <- rep(list(observations),wells)
  
  # empty list for removed well data sets
  removedList <- list()
  
  # creating list of data sets with removed wells
  for (e in seq(listOfFiles)) {
    removedList[[e]]= (
      listOfFiles[[e]] %>% filter(well.id != e)
    )
  }
  
  # renaming dfs of removedList to indicate which well was removed)
  names(removedList) <- paste0("removed_", c(1:wells))
  
  
  # fitting models ----------------------------------------------------------
  
  # empty list of models, will append models in the loop
  listOfModels <- list()
  
  pord<-1
  lambda.rel.time<-1
  lambda.rel<-c(1,1,lambda.rel.time)
  computeD=FALSE
  ndims = 3
  
  # loop through removedList, fit models and save them in a list
  for (d in seq(removedList)) {
    listOfModels[[d]] = sm.st(
      x = removedList[[d]][,2:3],
      t = removedList[[d]][,4],
      y = removedList[[d]][,5],
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
  for (c in seq(listOfFiles)) {
    listOfRemoved[[c]]= (
      listOfFiles[[c]] %>% filter(well.id == c)
    )
  }
  
  # empty list of fitted values, will append to this in the loop
  listOfFitted <- list()
  
  # loop through models and extract predictions for the coordinates of the removed well
  for (b in seq(listOfModels)) {
    listOfFitted[[b]] = predict.smst(
      listOfModels[[b]], listOfRemoved[[b]][,1:4]
    )
  }
  
  # rename items of fitted values list
  names(listOfFitted) <- paste0("fitted_m_", c(1:wells))
  
  
  # computing RMSEs ---------------------------------------------------------
  
  # creating empty list to collect RMSEs
  listOfRMSEs <- list()
  
  # calculate RMSE for the removed wells (observation - predicted at the removed well)
  for (a in seq(removedList)) {
    listOfRMSEs[[a]] = sqrt(
      sum(
        (listOfRemoved[[a]]$y - listOfFitted[[a]])^2
      )/length(listOfFitted[[a]])
    )
  }
  
  # renaming elements in RMSE list
  names(listOfRMSEs) <- paste0("rmse_", c(1:wells))
  
  
  # full model RMSE ---------------------------------------------------------
  
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
  
  
  # create dfs for results --------------------------------------------------
  
  wbcv_results <- well_coords
  leverage_results <- data.frame("well.id" = rep(c(1:wells), each=20))
  standresids_results <- data.frame("well.id" = rep(c(1:wells), each=20))
  cd_results <- data.frame("well.id" = rep(c(1:wells), each=20))
  diffits_results <- data.frame("well.id" = rep(c(1:wells), each=20))
  hp_results <- data.frame("well.id" = rep(c(1:wells), each=20))
  covratio_results <- data.frame("well.id" = rep(c(1:wells), each=20))
  
  
  # data manipulation -------------------------------------------------------
  
  # creating new column
  results_df <- as.data.frame(t(as.data.frame(listOfRMSEs)), row.names = c(1:wells))
  
  # renaming column
  colnames(results_df) <- c(paste0("RMSE", 1))
  
  # appending new column
  wbcv_results[ ,ncol(wbcv_results) + 1] <- results_df
  
  
  # leverages ---------------------------------------------------------------
  
  # isolating well.id and leverages
  leverages <- as.data.frame(imetrics[,2])
  
  # renaming results to indicate iteration number
  colnames(leverages) <- c(paste0("leverage", 1))
  
  # adding new column to df in each iteration
  leverage_results[ ,ncol(leverage_results) + 1] <- leverages
  
  
  # standardized residuals --------------------------------------------------
  
  # isolating well.id and standres
  standres <- as.data.frame(imetrics[,4])
  
  # renaming results to indicate iteration number
  colnames(standres) <- c(paste0("standres", 1))
  
  # adding new column to df in each iteration
  standresids_results[ ,ncol(standresids_results) + 1] <- standres
  
  
  # cook's distance ---------------------------------------------------------
  
  # isolating well.id and cd
  cd <- as.data.frame(imetrics[,5])
  
  # renaming results to indicate iteration number
  colnames(cd) <- c(paste0("cd", 1))
  
  # adding new column to df in each iteration
  cd_results[ ,ncol(cd_results) + 1] <- cd
  
  
  # diffits -----------------------------------------------------------------
  
  # isolating well.id and diffits
  diffits <- as.data.frame(imetrics[,7])
  
  # renaming results to indicate iteration number
  colnames(diffits) <- c(paste0("diffits", 1))
  
  # adding new column to df in each iteration
  diffits_results[ ,ncol(diffits_results) + 1] <- diffits
  
  
  # hadi's potential --------------------------------------------------------
  
  # isolating well.id and hp
  hp <- as.data.frame(imetrics[,8])
  
  # renaming results to indicate iteration number
  colnames(hp) <- c(paste0("hp", 1))
  
  # adding new column to df in each iteration
  hp_results[ ,ncol(hp_results) + 1] <- hp
  
  
  # covratio ----------------------------------------------------------------
  
  # isolating well.id and covratio
  covratio <- as.data.frame(imetrics[,9])
  
  # renaming results to indicate iteration number
  colnames(covratio) <- c(paste0("covratio", 1))
  
  # adding new column to df in each iteration
  covratio_results[ ,ncol(covratio_results) + 1] <- covratio
  
  
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
  wbcv_order <- data.frame(matrix(ncol = 0, nrow = as.numeric(wells)))
  
  # appending the well order of each iteration to the above df
  for (j in 1:1) {
    order <- order(wbcv_summary[,paste0("RMSE",j)], decreasing=TRUE)
    wbcv_order[ ,ncol(wbcv_order) + 1] <- order
  }
  
  
  # ia metric orders --------------------------------------------------------
  
  # same process as above for wbcv
  
  # leverage
  leverage_order <- data.frame(matrix(ncol = 0, nrow = as.numeric(wells)))
  
  for (k in 1:1) {
    order <- order(leverage_summary[,paste0("leverage",k)], decreasing=TRUE)
    leverage_order[ ,ncol(leverage_order) + 1] <- order
  }
  
  # standres
  standres_order <- data.frame(matrix(ncol = 0, nrow = as.numeric(wells)))
  
  for (l in 1:1) {
    order <- order(standres_summary[,paste0("standres",l)], decreasing=TRUE)
    standres_order[ ,ncol(standres_order) + 1] <- order
  }
  
  # cd
  cd_order <- data.frame(matrix(ncol = 0, nrow = as.numeric(wells)))
  
  for (m in 1:1) {
    order <- order(cd_summary[,paste0("cd",m)], decreasing=TRUE)
    cd_order[ ,ncol(cd_order) + 1] <- order
  }
  
  # diffits
  diffits_order <- data.frame(matrix(ncol = 0, nrow = as.numeric(wells)))
  
  for (s in 1:1) {
    order <- order(diffits_summary[,paste0("diffits",s)], decreasing=TRUE)
    diffits_order[ ,ncol(diffits_order) + 1] <- order
  }
  
  # hp
  hp_order <- data.frame(matrix(ncol = 0, nrow = as.numeric(wells)))
  
  for (o in 1:1) {
    order <- order(hp_summary[,paste0("hp",o)], decreasing=TRUE)
    hp_order[ ,ncol(hp_order) + 1] <- order
  }
  
  # covratio
  covratio_order <- data.frame(matrix(ncol = 0, nrow = as.numeric(wells)))
  
  for (p in 1:1) {
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
  for (q in 1:1) {
    
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
  
  
  if (wells == 6) {
    maxd = 18
  } else if (wells == 12) {
    maxd = 72
  } else  if (wells == 24) {
    maxd = 288
  } else {
    print("ERROR! Invalid well number.")
  }
  
  
  # data frame for plotting
  scores <- rbind(leverage_scores, standres_scores, cd_scores, diffits_scores, hp_scores, covratio_scores)
  rownames(scores) <- c("leverages", "MADsr", "CD", "DIFFITS", "HP", "COVRATIO" )
  colnames(scores) <- c(1:1)
  scores <- as.data.frame(t(as.data.frame(scores)))
  
  scores_bp <- scores/maxd
  scores_bp <- scores_bp %>%
    gather(key = "method", value = "diff_score")
  
  return(scores_bp)
}



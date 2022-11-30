
# README ------------------------------------------------------------------

# plotting some results

# libraries ---------------------------------------------------------------

library(tidyverse)


# parameters --------------------------------------------------------------

plumes <- c("simple", "mid", "complex")
designs <- c("random", "grid", "expert")
wells <- c(6, 12, 24)
errors <- c("additive", "multiplicative")

# 1 example
# plumes <- c("complex")
# designs <- c("expert")
# wells <- c(24)
# errors <- c("multiplicative")


# starting main loop ------------------------------------------------------

# looping through plumes
for (a in plumes) {
  plume = a
  
  #looping through designs
  for (b in designs) {
    design = b
    
    #looping through wells
    for (c in wells) {
      well = c
      
      if (well == 6) {
        maxd = 18
      } else if (well == 12) {
        maxd = 72
      } else  if (well == 24) {
        maxd = 288
      } else {
        print("ERROR! Invalid well number.")
      }
      
      #looping through errors
      for (d in errors) {
        error = d
        
        # insert plotting code here
        # load data ---------------------------------------------------------------
        
        # mean difference scores
        load(paste0("results/", plume, "_", design, "_", well, "_", error, "_mean_diffs.RData"))
        
        # difference scores
        load(paste0("results/", plume, "_", design, "_", well, "_", error, "_leverage_scores.RData"))
        load(paste0("results/", plume, "_", design, "_", well, "_", error, "_standres_scores.RData"))
        load(paste0("results/", plume, "_", design, "_", well, "_", error, "_cd_scores.RData"))
        load(paste0("results/", plume, "_", design, "_", well, "_", error, "_diffits_scores.RData"))
        load(paste0("results/", plume, "_", design, "_", well, "_", error, "_hp_scores.RData"))
        load(paste0("results/", plume, "_", design, "_", well, "_", error, "_covratio_scores.RData"))
        
        
        # line plots --------------------------------------------------------------
        
        # line plots of difference scores - 1 per scenario
        
        # data frame for plotting
        scores <- rbind(leverage_scores, standres_scores, cd_scores, diffits_scores, hp_scores, covratio_scores)
        rownames(scores) <- c("leverages", "standardised residuals", "Cook's Distance", "DIFFITS", "Hadi's Potential", "COVRATIO" )
        colnames(scores) <- c(1:100)
        scores <- as.data.frame(t(as.data.frame(scores)))
        
        # plotting
        line_plt <- ggplot(scores, aes(x=1:nrow(scores))) +
          xlab("Iteration") +
          ylab("Difference Score") +
          geom_line(aes(y = `leverages`, color = "leverages"), size = 1.5) +
          geom_line(aes(y = `standardised residuals`, color = "standarised residuals"), size = 1.5) +
          geom_line(aes(y = `Cook's Distance`, color = "Cook's Distance"), size = 1.5) +
          geom_line(aes(y = `DIFFITS`, color = "DIFFITS"), size = 1.5) +
          geom_line(aes(y = `Hadi's Potential`, color = "Hadi's Potential"), size = 1.5) +
          geom_line(aes(y = `COVRATIO`, color = "COVRATIO"), size = 1.5) +
          ggtitle(paste0("Difference Scores in Each Iteration.
                 Scenario: ", plume, " ", design, " ", well, " ", error, "."))
        
        # save plot
        png(filename = paste0("results_default/figures/plt_", plume, "_", design, "_", well, "_", error, "_line.png"), 
            width = 1000, height = 800, pointsize = 6, res = 120)
        plot(line_plt)
        dev.off()
        
        
        # boxplots of mean scores -------------------------------------------------
        
        # data frame for plotting
        # non-standardised
        # scores_bp <- scores %>%
        #   gather(key = "method", value = "diff_score")
        
        # data frame for plotting
        # standardised
        scores_bp <- scores/maxd
        scores_bp <- scores_bp %>%
          gather(key = "method", value = "diff_score")
        
        # plotting
        box_plt <- ggplot(scores_bp, aes(x=reorder(method, diff_score, FUN=mean), y=diff_score)) + 
          ylab("Standardised Difference Score (d/maxd)") +
          xlab("IA Metric") +
          geom_boxplot(notch = FALSE) +
          geom_jitter(width = .1) +
          #stat_summary(aes(label = ..y..), fun="mean", geom="text", size=4, vjust = -1) +
          stat_summary(fun="mean", geom="point", shape=21, size=3, fill="grey") +
          ggtitle(paste0("Spread of Standardised Difference Scores. Scenario: ", plume, " ", design, " ", well, " ", error, "."))
        
        # save plot
        png(filename = paste0("results_default/figures/plt_", plume, "_", design, "_", well, "_", error, "_box.png"), 
            width = 1000, height = 800, pointsize = 6, res = 120)
        plot(box_plt)
        dev.off()
        
        
        # percentile plot ---------------------------------------------------------
        
        # data frame for plotting
        # non-standardised
        # scores_pp <- data.frame(
        #   method = c("leverages", "standres", "CD", "DIFFITS", "HP", "COVRATIO"),
        #   mean = c(mean_scores[1,1], mean_scores[1,2], mean_scores[1,3], 
        #            mean_scores[1,4], mean_scores[1,5], mean_scores[1,6]),
        #   lower = c(quantile(scores[,1], probs = .025), quantile(scores[,2], probs = .025), 
        #             quantile(scores[,3], probs = .025), quantile(scores[,4], probs = .025), 
        #             quantile(scores[,5], probs = .025), quantile(scores[,6], probs = .025)), 
        #   upper = c(quantile(scores[,1], probs = .975), quantile(scores[,2], probs = .975), 
        #             quantile(scores[,3], probs = .975), quantile(scores[,4], probs = .975), 
        #             quantile(scores[,5], probs = .975), quantile(scores[,6], probs = .975)))
        
        # data frame for plotting
        # standardised
        scores_pp <- data.frame(
          method = c("leverages", "standres", "CD", "DIFFITS", "HP", "COVRATIO"),
          mean = c(mean_scores[1,1]/maxd, mean_scores[1,2]/maxd, mean_scores[1,3]/maxd, 
                   mean_scores[1,4]/maxd, mean_scores[1,5]/maxd, mean_scores[1,6]/maxd),
          lower = c(quantile(scores[,1], probs = .025)/maxd, quantile(scores[,2], probs = .025)/maxd, 
                    quantile(scores[,3], probs = .025)/maxd, quantile(scores[,4], probs = .025)/maxd, 
                    quantile(scores[,5], probs = .025)/maxd, quantile(scores[,6], probs = .025)/maxd), 
          upper = c(quantile(scores[,1], probs = .975)/maxd, quantile(scores[,2], probs = .975)/maxd, 
                    quantile(scores[,3], probs = .975)/maxd, quantile(scores[,4], probs = .975)/maxd, 
                    quantile(scores[,5], probs = .975)/maxd, quantile(scores[,6], probs = .975)/maxd))
        
        # plotting
        per_plt <- ggplot(scores_pp, aes(x=reorder(method, mean))) +
          ylab("Standardised Difference Score (d/maxd)") +
          xlab("IA metric") +
          geom_linerange(aes(ymin=lower, ymax=upper), size=2.5, color="grey") +
          geom_point(aes(y=mean), size=1.7, shape=16) +
          geom_point(aes(y=lower), size=1.7, shape=25, fill="black") +
          geom_point(aes(y=upper), size=1.7, shape=24, fill="black") +
          ggtitle(paste0("95% Variability Bands and Means of Standardised Difference Scores. 
                 Scenario: ", plume, " ", design, " ", well, " ", error, "."))
        
        # save plot
        png(filename = paste0("results_default/figures/plt_", plume, "_", design, "_", well, "_", error, "_summary.png"), 
            width = 1000, height = 800, pointsize = 6, res = 120)
        plot(per_plt)
        dev.off()
        
      }
    }
  }
}



# try plotting code here outside the loop ---------------------------------



# load data ---------------------------------------------------------------

# load(paste0("results/", plume, "_", design, "_", wells, "_", error, "_mean_diffs.RData"))
# 
# # these do not exist yet
# load(paste0("results/", plume, "_", design, "_", wells, "_", error, "_leverage_scores.RData"))
# load(paste0("results/", plume, "_", design, "_", wells, "_", error, "_standres_scores.RData"))
# load(paste0("results/", plume, "_", design, "_", wells, "_", error, "_cd_scores.RData"))
# load(paste0("results/", plume, "_", design, "_", wells, "_", error, "_diffits_scores.RData"))
# load(paste0("results/", plume, "_", design, "_", wells, "_", error, "_hp_scores.RData"))
# load(paste0("results/", plume, "_", design, "_", wells, "_", error, "_covratio_scores.RData"))
# 
# 
# # line plots --------------------------------------------------------------
# 
# # line plots of difference scores - 1 per scenario
# 
# # data frame for plotting
# scores <- rbind(leverage_scores, standres_scores, cd_scores, diffits_scores, hp_scores, covratio_scores)
# rownames(scores) <- c("leverages", "standardised residuals", "Cook's Distance", "DIFFITS", "Hadi's Potential", "COVRATIO" )
# colnames(scores) <- c(1:100)
# scores <- as.data.frame(t(as.data.frame(scores)))
# 
# # plotting
# line_plt <- ggplot(scores, aes(x=1:nrow(scores))) +
#   xlab("Iteration") +
#   ylab("Sum of differences in well ranking") +
#   geom_line(aes(y = `leverages`, color = "leverages"), size = 1.5) +
#   geom_line(aes(y = `standardised residuals`, color = "standarised residuals"), size = 1.5) +
#   geom_line(aes(y = `Cook's Distance`, color = "Cook's Distance"), size = 1.5) +
#   geom_line(aes(y = `DIFFITS`, color = "DIFFITS"), size = 1.5) +
#   geom_line(aes(y = `Hadi's Potential`, color = "Hadi's Potential"), size = 1.5) +
#   geom_line(aes(y = `COVRATIO`, color = "COVRATIO"), size = 1.5) 
#   # ggtitle(paste0("Sums of differences in well rankings compared to WBCV across all realizations.
#   #                Scenario: ", plume, " ", design, " ", well, " ", error, "."))
# 
# # save plot
# # png(filename = paste0("results/plt_", plume, "_", design, "_", well, "_", error, "_line.png"))
# # plot(line_plt)
# # dev.off()
# 
# 
# # boxplots of mean scores -------------------------------------------------
# 
# # data frame for plotting
# scores_bp <- scores %>%
#   gather(key = "method", value = "diff_score")
# 
# # plotting
# box_plt <- ggplot(scores_bp, aes(x=reorder(method, diff_score, FUN=mean), y=diff_score)) + 
#   ylab("Difference score") +
#   xlab("IA metric") +
#   geom_boxplot(notch = FALSE) +
#   geom_jitter(width = .1) +
#   #stat_summary(aes(label = ..y..), fun="mean", geom="text", size=4, vjust = -1) +
#   stat_summary(fun="mean", geom="point", shape=21, size=3, fill="grey") 
#   # ggtitle(paste0("Mean Difference Scores. Scenario: ", plume, " ", design, " ", well, " ", error, "."))
# 
# # save plot
# # png(filename = paste0("results/plt_", plume, "_", design, "_", well, "_", error, "_box.png"))
# # plot(box_plt)
# # dev.off()
# 
# 
# # percentile plot ---------------------------------------------------------
# 
# # data frame for plotting
# # try to standardise scores here
# scores_pp <- data.frame(method = c("leverages", "standres", "CD", "DIFFITS", "HP", "COVRATIO"),
#                  mean = c(mean_scores[1,1], mean_scores[1,2], mean_scores[1,3], 
#                           mean_scores[1,4], mean_scores[1,5], mean_scores[1,6]),
#                  lower = c(quantile(scores[,1], probs = .025), quantile(scores[,2], probs = .025), 
#                            quantile(scores[,3], probs = .025), quantile(scores[,4], probs = .025), 
#                            quantile(scores[,5], probs = .025), quantile(scores[,6], probs = .025)), 
#                  upper = c(quantile(scores[,1], probs = .975), quantile(scores[,2], probs = .975), 
#                            quantile(scores[,3], probs = .975), quantile(scores[,4], probs = .975), 
#                            quantile(scores[,5], probs = .975), quantile(scores[,6], probs = .975)))
# 
# # plotting
# per_plot <- ggplot(scores_pp, aes(x=reorder(method, mean))) +
#   ylab("Difference score") +
#   xlab("IA metric") +
#   geom_linerange(aes(ymin=lower, ymax=upper), size=2.5, color="grey") +
#   geom_point(aes(y=mean), size=1.7, shape=16) +
#   geom_point(aes(y=lower), size=1.7, shape=25, fill="black") +
#   geom_point(aes(y=upper), size=1.7, shape=24, fill="black") 
#   # ggtitle(paste0("95% Confidence Bands and Means of Difference Scores. 
#   #                Scenario: ", plume, " ", design, " ", well, " ", error, "."))
# 
# # save plot
# # png(filename = paste0("results/plt_", plume, "_", design, "_", well, "_", error, "_percentiles.png"))
# # plot(per_plt)
# # dev.off()
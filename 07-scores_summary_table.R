
library(tidyverse)

# creating variables
plumes <- c("simple", "mid", "complex")
designs <- c("random", "grid", "expert")
wells <- c(6, 12, 24)
errors <- c("additive", "multiplicative")

# creating empty data frame for results
# results <- data.frame(matrix(ncol = 10, nrow = 0))
# names <- c("plume", "design", "well", "error", "leverage", "MAD", "CD", "DFFITS", "HP", "COVRATIO")
# colnames(results) <- names

# creating empty data frame for standardised results
results_standard <- data.frame(matrix(ncol = 10, nrow = 0))
names <- c("plume", "design", "well", "error", "leverage", "MAD", "CD", "DFFITS", "HP", "COVRATIO")
colnames(results_standard) <- names

# attaching results row-wise to the prepared data frame
for (a in plumes) {
  
  plume = a
  
  for (b in designs) {
    
    design = b
    
    for (c in wells) {
      
      well = c
      
      for (d in errors) {
        
        error = d
        
        # max D
        if (well == 6) {
          maxd = 18
        } else if (well == 12) {
          maxd = 72
        } else  if (well == 24) {
          maxd = 288
        } else {
          print("ERROR! Invalid well number.")
        }
        
        # appending rows
        # load(paste0("results/", plume, "_", design, "_", well, "_", error, "_mean_diffs.RData"))
        # row <- cbind(plume, design, well, error, mean_scores[1,])
        # results[nrow(results) + 1, ] <- row
        
        # appending rows to standardised results
        load(paste0("results/", plume, "_", design, "_", well, "_", error, "_mean_diffs.RData"))
        row <- cbind(plume, design, well, error, mean_scores[1,]/maxd)
        results_standard[nrow(results_standard) + 1, ] <- row
        
      }
    }
  }
}

# saving results
# save(results, file = "results_default/scores_summary_table.RData")
# write.csv(results, "results_default/scores_summary_table.csv")

# saving standardised results
save(results_standard, file = "results/standard_scores_summary_table.RData")
write.csv(results_standard, "results/standard_scores_summary_table.csv")


library(tidyverse)

# select scenario by setting these
plume <- "complex" #c("simple", "mid", "complex")
design <- "expert" #c("random", "grid", "expert")
well <- 12 #c(6, 12, 24)
error <- "multiplicative" #c("additive", "multiplicative")

# loading network design
load(paste0("data/", design, "_", well, ".RData"))

# for cd
load(paste0("results_default/", plume, "_", design, "_", well, "_", error, "_cd_order.RData"))

# for wbcv 
# load(paste0("results_default/", plume, "_", design, "_", well, "_", error, "_wbcv_order.RData"))

# plotting monitoring network
ggplot(data=well_coords, 
       mapping = aes(x=X1, y=X2)) +
  geom_point() +
  geom_label(aes(label = well.id)) +
  xlim(1, 100) +
  ylim(1, 35)

# creating empty data frame to append to in a loop
well_placements <- data.frame(matrix(ncol = 1, nrow = well))
colnames(well_placements) <- c("well.id")
well_placements[,1] <- c(1:well)

# adding "rank" column
# for cd order
cd_pos <- rowid_to_column(cd_order)

# for wbcv order
# cd_pos <- rowid_to_column(wbcv_order)

# renaming columns
colnames(cd_pos) <- c("rank", 1:100)

# finding the rank of each well. adjust number of wells here depending on the scenario
for (i in 2:101) {
  well_pos <- c(which(cd_pos[,i] == 1),
                which(cd_pos[,i] == 2),
                which(cd_pos[,i] == 3),
                which(cd_pos[,i] == 4),
                which(cd_pos[,i] == 5),
                which(cd_pos[,i] == 6),
                which(cd_pos[,i] == 7),
                which(cd_pos[,i] == 8),
                which(cd_pos[,i] == 9),
                which(cd_pos[,i] == 10),
                which(cd_pos[,i] == 11),
                which(cd_pos[,i] == 12))
  well_placements[ ,ncol(well_placements) + 1] <- well_pos
}

# calculating mean rank positions for each well across the 100 iterations of the selected scenario
mean_pos <- well_placements %>%
  rowwise() %>%
  mutate(mean = mean(c(`V2`:`V101`))) %>%
  select(well.id, mean) %>%
  arrange(by=mean, decreasing = TRUE)

# save results in a table
# for cd order
save(mean_pos, file = paste0("results_default/", plume, "_", design, "_", well, "_", error, "_mean_well_rank.RData"))

# for wbcv order
# save(mean_pos, file = paste0("results_default/", plume, "_", design, "_", well, "_", error, "_wbcv_mean_well_rank.RData"))







# README ------------------------------------------------------------------

# generate well networks for the simulated data


# libraries ---------------------------------------------------------------

library(tidyverse)


# load data ---------------------------------------------------------------

# simple plume
load("data/complex_plume.RData")


# list of coordinates -----------------------------------------------------

# collect list of coordinates
all_coords <- unique(true.data[,c('X1','X2')])

# plot of all coordinates
# plot(all_coords)


# select wells ------------------------------------------------------------


# random ------------------------------------------------------------------

set.seed(45)

n1 = 12
n2 = 6
n3 = 24

# randomly select a number of rows to be wells
well_coords <- all_coords[sample(nrow(all_coords), n1), ]

# grid --------------------------------------------------------------------

# select wells on a grid (3*4) 12
well_coords <- all_coords %>%
  filter(X1>10.3 & X1<10.4 | X1>36.8 & X1<36.9| X1>63.4 & X1<63.5 | X1>90.0 & X1<90.1) %>%
  filter(X2>5.1 & X2<5.2 | X2>17.4 & X2<17.5 | X2>30.2 & X2<30.3)

# select wells on a grid (2*3) 6
well_coords <- all_coords %>%
  filter(X1>25.5 & X1<25.6 | X1>50.8 & X1<50.9 | X1>75.4 & X1<75.5) %>%
  filter(X2>12.4 & X2<12.5 | X2>25.1 & X2<25.2)

# select wells on a grid (4*6) 24
well_coords <- all_coords %>%
  filter(X1>5.6 & X1<5.7 | X1>23.5 & X1<23.6 | X1>40.8 & X1<40.9 | X1>59.4 & X1<59.5 | X1>77.4 & X1<77.5 | X1>95.3 & X1<95.4) %>%
  filter(X2>5.1 & X2<5.2 | X2>13.3 & X2<13.4 | X2>21.7 & X2<21.8 | X2>30.2 & X2<30.3)

# to check for appropriate intervals
seq(5, 95, length.out = 6)
seq(5, 30, length.out = 4)


# expert ------------------------------------------------------------------

# select wells (12)
well_coords <- all_coords %>%
  filter(X1>10.3 & X1<10.4 & X2>17.2 & X2<17.3 | # well 1 coordinates (intervals)
           X1>20.2 & X1<20.3 & X2>17.2 & X2<17.3 | # well 2
           X1>37.5 & X1<37.6 & X2>17.2 & X2<17.3 | # well 3
           X1>50.1 & X1<50.2 & X2>17.2 & X2<17.3 | # well 4
           X1>90.0 & X1<90.1 & X2>10.1 & X2<10.2 | # well 5
           X1>30.2 & X1<30.3 & X2>10.1 & X2<10.2 | # well 6
           X1>30.2 & X1<30.3 & X2>25.1 & X2<25.2 | # well 7
           X1>50.1 & X1<50.2 & X2>7.1 & X2<7.2 | # well 8
           X1>50.1 & X1<50.2 & X2>28.1 & X2<28.2 | # well 9
           X1>25.5 & X1<25.6 & X2>5.1 & X2<5.2 | # well 10
           X1>25.5 & X1<25.6 & X2>30.2 & X2<30.3 | # well 11
           X1>90.0 & X1<90.1 & X2>25.1 & X2<25.2 ) # well 12

# select wells on a grid (6)
well_coords <- all_coords %>%
  filter(X1>10.3 & X1<10.4 & X2>17.2 & X2<17.3 | # well 1 coordinates (intervals)
           X1>25.5 & X1<25.6 & X2>19.0 & X2<19.1 | # well 2
           X1>50.1 & X1<50.2 & X2>15.1 & X2<15.2 | # well 3
           X1>80.0 & X1<80.1 & X2>25.1 & X2<25.2 | # well 4
           X1>75.4 & X1<75.5 & X2>5.1 & X2<5.2 | # well 5
           X1>50.1 & X1<50.2 & X2>25.1 & X2<25.2 ) # well 6

# select wells on a grid (24)
well_coords <- all_coords %>%
  filter(X1>10.3 & X1<10.4 & X2>17.2 & X2<17.3 | # well 1 coordinates (intervals)
           X1>20.2 & X1<20.3 & X2>17.2 & X2<17.3 | # well 2
           X1>37.5 & X1<37.6 & X2>17.2 & X2<17.3 | # well 3
           X1>50.1 & X1<50.2 & X2>17.2 & X2<17.3 | # well 4
           X1>90.0 & X1<90.1 & X2>10.1 & X2<10.2 | # well 5
           X1>30.2 & X1<30.3 & X2>10.1 & X2<10.2 | # well 6
           X1>30.2 & X1<30.3 & X2>25.1 & X2<25.2 | # well 7
           X1>50.1 & X1<50.2 & X2>7.1 & X2<7.2 | # well 8
           X1>50.1 & X1<50.2 & X2>28.1 & X2<28.2 | # well 9
           X1>25.5 & X1<25.6 & X2>5.1 & X2<5.2 | # well 10
           X1>25.5 & X1<25.6 & X2>30.2 & X2<30.3 | # well 11
           X1>90.0 & X1<90.1 & X2>25.1 & X2<25.2 | # well 12
           X1>12.2 & X1<12.3 & X2>8.0 & X2<8.1 | # well 13
           X1>12.2 & X1<12.3 & X2>26.1 & X2<26.2 | # well 14
           X1>18.2 & X1<18.3 & X2>12.4 & X2<12.5 | # well 15
           X1>18.2 & X1<18.3 & X2>22.4 & X2<22.5 | # well 16
           X1>28.2 & X1<28.3 & X2>17.2 & X2<17.3 | # well 17
           X1>43.5 & X1<43.6 & X2>11.0 & X2<11.1 | # well 18
           X1>43.5 & X1<43.6 & X2>27.0 & X2<27.1 | # well 19
           X1>53.4 & X1<53.5 & X2>12.4 & X2<12.5 | # well 20
           X1>53.4 & X1<53.5 & X2>22.4 & X2<22.5 | # well 21
           X1>75.4 & X1<75.5 & X2>17.2 & X2<17.3 | # well 22
           X1>70.1 & X1<70.2 & X2>3.0 & X2<3.1 | # well 23
           X1>70.1 & X1<70.2 & X2>32.0 & X2<32.1 ) # well 24


# name wells --------------------------------------------------------------

well_coords <- well_coords %>%
  rowid_to_column("well.id")


# plot results ------------------------------------------------------------

# plot of well coordinates
ggplot(data=well_coords, 
       mapping = aes(x=X1, y=X2)) +
  geom_point() +
  geom_label(aes(label = well.id)) +
  xlim(1, 100) +
  ylim(1, 35)


# plotting true data
ggplot(data = true.data,
       mapping = aes(x = X1, y = X2)) +
  geom_point(mapping = aes(color = y)) +
  geom_text(data = well_coords, label = well_coords$well.id)


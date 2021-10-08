####################################################################################################
# bootstrapping 
####################################################################################################
#Import libraries
library(tidyverse)
library(vegan)
#library(devtools)
#library("factoextra")
library(ggfortify)

####################################################################################################
#Import data
covmat_all <- read.csv("Data/covmat_all.csv")
covmat_all <- covmat_all %>% select(-X.2,-X.1)
slope_data <- read.csv("Data/slope.csv")
area_data <- read.csv("Data/area.csv")

#break up covmat_all by region and treatment
#organize the sections based on region, time, and treatment
covmat_south_d <- filter(covmat_all, Region == "3.South" & Treatment == "D")
covmat_center_d <- filter(covmat_all, Region == "2.Center" & Treatment == "D")
covmat_north_d <- filter(covmat_all, Region == "1.North" & Treatment == "D")
covmat_south_w <- filter(covmat_all, Region == "3.South" & Treatment == "W")
covmat_center_w <- filter(covmat_all, Region == "2.Center" & Treatment == "W")
covmat_north_w <- filter(covmat_all, Region == "1.North" & Treatment == "W")

# Make all blank dataframes for angle difference data
#Permuated Area
area_south_d <- data_frame() # set dataframe reciving permuated data
area_center_d <- data_frame()
area_north_d <- data_frame()
area_south_w <- data_frame()
area_center_w <- data_frame()
area_north_w <- data_frame()

#Permuted Slope
slope_south_d <- data_frame() # set dataframe reciving permuated data
slope_center_d <- data_frame()
slope_north_d <- data_frame()
slope_south_w <- data_frame()
slope_center_w <- data_frame()
slope_north_w <- data_frame()



#North Permutation 

# row shuffling
for(i in 1:1000) {
  set.seed(i)
  rows <- sample(nrow(covmat_south_d),replace = FALSE,) # randomize row indices
  all.s.d.shuf <- covmat_south_d[rows, ] # shuffle rows
  pre.s.d.shuf <- all.s.d.shuf [1:36,] # make random pre dataset
  peak.s.d.shuf <- all.s.d.shuf [37:60,] # make random post dataset
  
  s_pre_d <- cov(pre.s.d.shuf[,1:2]) # get covariance from permuated loadings
  s_peak_d <- cov(peak.s.d.shuf[,1:2]) 
  #Area
  s_pre_d_area <- sum(s_pre_d[1,1],s_pre_d[2,2]) #sum permutated variance to get area
  s_peak_d_area <- sum(s_peak_d[1,1],s_peak_d[2,2])
  area_south_d[i,1] <- (s_peak_d_area - s_pre_d_area)
  #Slope
  s_d_slope <- (s_peak_d[1,2]) - (s_pre_d[1,2]) #calc permuted slope difference
  slope_south_d[i,1] <- s_d_slope
}
  
# Permuated P-value for Slope
ex_l <- length(slope_south_d[slope_south_d >= slope_data[2,2]])
per_pval <- ex_l/dim(slope_south_d)[1] 
per_pval





























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



#South Dry Permutation 

# row shuffling
for(i in 1:10000) {
  set.seed(i)
  rows <- sample(nrow(covmat_south_d),replace = FALSE,) # randomize row indices
  all.s.d.shuf <- covmat_south_d[rows, ] # shuffle rows
  peak.s.d.shuf <- all.s.d.shuf [1:36,] # make random pre dataset
  pre.s.d.shuf <- all.s.d.shuf [37:60,] # make random post dataset
  
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


# Permuated P-value for Area
ex_l <- length(area_south_d[area_south_d >= area_data[1,2]])
per_s.d.a.pval <- ex_l/dim(area_south_d)[1] 
per_s.d.a.pval # p=0.49

  
# Permuated P-value for Slope
ex_2 <- length(slope_south_d[slope_south_d >= slope_data[2,2]])
per_s.d.s_pval <- ex_2/dim(slope_south_d)[1] 
per_s.d.s_pval # p=0.21


#South Wet Permutation 

# row shuffling
for(i in 1:10000) {
  set.seed(i)
  rows <- sample(nrow(covmat_south_w),replace = FALSE,) # randomize row indices
  all.s.w.shuf <- covmat_south_w[rows, ] # shuffle rows
  peak.s.w.shuf <- all.s.w.shuf [1:30,] # make random pre dataset
  pre.s.w.shuf <- all.s.w.shuf [31:50,] # make random post dataset
  
  s_pre_w <- cov(pre.s.w.shuf[,1:2]) # get covariance from permuated loadings
  s_peak_w <- cov(peak.s.w.shuf[,1:2]) 
  #Area
  s_pre_w_area <- sum(s_pre_w[1,1],s_pre_w[2,2]) #sum permutated variance to get area
  s_peak_w_area <- sum(s_peak_w[1,1],s_peak_w[2,2])
  area_south_w[i,1] <- (s_peak_w_area - s_pre_w_area)
  #Slope
  s_w_slope <- (s_peak_w[1,2]) - (s_pre_w[1,2]) #calc permuted slope difference
  slope_south_w[i,1] <- s_w_slope
}


# Permuated P-value for Area
ex_3 <- length(area_south_w[area_south_w >= area_data[2,2]])
per_s.d.a.pval <- ex_3/dim(area_south_d)[1] 
per_s.d.a.pval # p=0.99


# Permuated P-value for Slope
ex_4 <- length(slope_south_w[slope_south_w >= slope_data[1,2]])
per_s.d.s_pval <- ex_4/dim(slope_south_w)[1] 
per_s.d.s_pval # p=0.82

#Center Dry Permutation 

# row shuffling
for(i in 1:10000) {
  set.seed(i)
  rows <- sample(nrow(covmat_center_d),replace = FALSE,) # randomize row indices
  all.c.d.shuf <- covmat_center_d[rows, ] # shuffle rows
  peak.c.d.shuf <- all.c.d.shuf [1:52,] # make random pre dataset
  pre.c.d.shuf <- all.c.d.shuf [53:93,] # make random post dataset
  
  c_pre_d <- cov(pre.c.d.shuf[,1:2]) # get covariance from permuated loadings
  c_peak_d <- cov(peak.c.d.shuf[,1:2]) 
  #Area
  c_pre_d_area <- sum(c_pre_d[1,1],c_pre_d[2,2]) #sum permutated variance to get area
  c_peak_d_area <- sum(c_peak_d[1,1],c_peak_d[2,2])
  area_center_d[i,1] <- (c_peak_d_area - c_pre_d_area)
  #Slope
  c_d_slope <- (c_peak_d[1,2]) - (c_pre_d[1,2]) #calc permuted slope difference
  slope_center_d[i,1] <- c_d_slope
}


# Permuated P-value for Area
ex_5 <- length(area_center_d[area_center_d >= area_data[3,2]])
per_c.d.a.pval <- ex_5/dim(area_center_d)[1] 
per_c.d.a.pval # p=0.5


# Permuated P-value for Slope
ex_6 <- length(slope_center_d[slope_center_d >= slope_data[4,2]])
per_s.d.s_pval <- ex_6/dim(slope_center_d)[1] 
per_s.d.s_pval # p=0.79






























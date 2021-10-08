####################################################################################################
# grabbing the area 
####################################################################################################
#Import libraries
library(tidyverse)

########## Read in data #############
s_peak_d <- read.csv("Data/south_peak_d_cov.csv")
s_pre_d <- read.csv("Data/south_pre_d_cov.csv")
s_peak_w <- read.csv("Data/south_peak_w_cov.csv")
s_pre_w <- read.csv("Data/south_pre_w_cov.csv")

c_peak_d<-read.csv("Data/centre_peak_d_cov.csv")
c_pre_d<-read.csv("Data/centre_pre_d_cov.csv")
c_peak_w<-read.csv("Data/centre_peak_w_cov.csv")
c_pre_w<-read.csv("Data/centre_pre_w_cov.csv")

n_peak_d<-read.csv("Data/north_peak_d_cov.csv")
n_pre_d<-read.csv("Data/north_pre_d_cov.csv")
n_peak_w<-read.csv("Data/north_peak_w_cov.csv")
n_pre_w<-read.csv("Data/north_pre_w_cov.csv")

###### Grab area for each region

s_peak_d_area <- sum(s_peak_d$V1[1],s_peak_d$V2[2])
s_pre_d_area <- sum(s_pre_d$V1[1],s_pre_d$V2[2])
s_peak_w_area <- sum(s_peak_w$V1[1],s_peak_w$V2[2])
s_pre_w_area <- sum(s_pre_w$V1[1],s_pre_w$V2[2])

c_peak_d_area <- sum(c_peak_d$V1[1],c_peak_d$V2[2])
c_pre_d_area <- sum(c_pre_d$V1[1],c_pre_d$V2[2])
c_peak_w_area <- sum(c_peak_w$V1[1],c_peak_w$V2[2])
c_pre_w_area <- sum(c_pre_w$V1[1],c_pre_w$V2[2])

n_peak_d_area <- sum(n_peak_d$V1[1],n_peak_d$V2[2])
n_pre_d_area <- sum(n_pre_d$V1[1],n_pre_d$V2[2])
n_peak_w_area <- sum(n_peak_w$V1[1],n_peak_w$V2[2])
n_pre_w_area <- sum(n_pre_w$V1[1],n_pre_w$V2[2])

data <- data.frame(rbind(s_pre_w_area, s_peak_w_area, s_pre_d_area, s_peak_d_area,
                         c_pre_w_area, c_peak_w_area, c_pre_d_area, c_peak_d_area,
                         n_pre_w_area, n_peak_w_area, n_pre_d_area, n_peak_d_area))

data$area <- data$rbind.s_pre_w_area..s_peak_w_area..s_pre_d_area..s_peak_d_area..
data$rbind.s_pre_w_area..s_peak_w_area..s_pre_d_area..s_peak_d_area.. <- NULL

#write.csv(data, "Data/area.csv")

#######grab the orientation change
s_d_slope <- (s_peak_d$V2[1]) - (s_pre_d$V2[1])
s_w_slope <- (s_peak_w$V2[1]) - (s_pre_w$V2[1])
c_d_slope <- (c_peak_d$V2[1]) - (c_pre_d$V2[1])
c_w_slope <- (c_peak_w$V2[1]) - (c_pre_w$V2[1])
n_d_slope <- (n_peak_d$V2[1]) - (n_pre_d$V2[1])
n_w_slope <- (n_peak_w$V2[1]) - (n_pre_w$V2[1])

data2 <- data.frame(rbind(s_w_slope, s_d_slope, c_w_slope, c_d_slope, n_w_slope, n_d_slope))
data2$slope <- data2$rbind.s_w_slope..s_d_slope..c_w_slope..c_d_slope..n_w_slope..                  
data2$rbind.s_w_slope..s_d_slope..c_w_slope..c_d_slope..n_w_slope..<-NULL

#write.csv(data2, "Data/slope.csv")



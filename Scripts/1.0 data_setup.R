####################################################################################################
# Setting up data
####################################################################################################
#Import libraries
library(tidyverse)
library(vegan)
library(devtools)
library("factoextra")
library(ggfortify)
library(ellipse)
library(ggplot2)

####################################################################################################
#Read in data
all <- read.csv("Data/all.csv")
all <- all %>% mutate(Region.period = paste(Region, Period, sep="_")) #Make Region.period variable
all <- all %>% separate(BlockDrought, c("Block", "Treatment"))



all <- rbind (all_dry, all_wet)

#filter by region, time, treatment
all_south_peak_d <- filter(all, Region=="3.South", Period == "Peak", Treatment=="D")
all_south_peak_w <- filter(all, Region=="3.South", Period == "Peak", Treatment=="W")

all_south_pre_d <- filter(all, Region=="3.South", Period=="Pre", Treatment=="D")
all_south_pre_w <- filter(all, Region=="3.South", Period=="Pre", Treatment=="W")

all_centre_peak_d <- filter(all, Region=="2.Center", Period == "Peak", Treatment=="D")
all_centre_peak_w <- filter(all, Region=="2.Center", Period == "Peak", Treatment=="W")

all_centre_pre_d <- filter(all, Region=="2.Center", Period=="Pre", Treatment=="D")
all_centre_pre_w <- filter(all, Region=="2.Center", Period=="Pre", Treatment=="W")

all_north_peak_d <- filter(all, Region=="1.North", Period == "Peak", Treatment=="D")
all_north_peak_w <- filter(all, Region=="1.North", Period == "Peak", Treatment=="W")

all_north_pre_d <- filter(all, Region=="1.North", Period=="Pre", Treatment=="D")
all_north_pre_w <- filter(all, Region=="1.North", Period=="Pre", Treatment=="W")

all <- rbind(all_south_peak_d, all_centre_peak_d, all_north_peak_d, all_south_pre_d,
             all_centre_pre_d, all_north_pre_d, all_south_peak_w, all_centre_peak_w,
             all_north_peak_w, all_south_pre_w, all_centre_pre_w, all_north_pre_w)

#write.csv(all, "Data/all_organized.csv")
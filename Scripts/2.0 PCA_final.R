####################################################################################################
# PCA of all regions
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
all <- read.csv("Data/all_organized.csv")

#PCA of all data
pc1_all <- prcomp(na.omit(all[,c("Experiment_Date","SLA","Water_Content",
                                 "Stomatal_Conductance","Assimilation")]), scale=T)



# matrix using PC1 and PC2 loadings as the two variables
#covmat <- matrix(c(pc1_all$x[,1], pc1_all$x[,2]), ncol=2)

#write.csv(covmat, "Data/covmat.csv")
covmat <- read.csv('Data/covmat.csv') 
covmat$X <- NULL 

# grab identifier columns and merge to covmat
covmat_all <- cbind(covmat, all)
write.csv(covmat_all, "Data/covmat_all.csv")

#organize the sections based on region, time, and treatment
covmat_south_peak_d <- filter(covmat_all, Region.period == "3.South_Peak" & Treatment == "D")
covmat_center_peak_d <- filter(covmat_all, Region.period == "2.Center_Peak" & Treatment == "D")
covmat_north_peak_d <- filter(covmat_all, Region.period == "1.North_Peak" & Treatment == "D")
covmat_south_pre_d <- filter(covmat_all, Region.period == "3.South_Pre" & Treatment == "D")
covmat_center_pre_d <- filter(covmat_all, Region.period == "2.Center_Pre" & Treatment == "D")
covmat_north_pre_d <- filter(covmat_all, Region.period == "1.North_Pre" & Treatment == "D")
covmat_south_peak_w <- filter(covmat_all, Region.period == "3.South_Peak" & Treatment == "W")
covmat_center_peak_w <- filter(covmat_all, Region.period == "2.Center_Peak" & Treatment == "W")
covmat_north_peak_w <- filter(covmat_all, Region.period == "1.North_Peak" & Treatment == "W")
covmat_south_pre_w <- filter(covmat_all, Region.period == "3.South_Pre" & Treatment == "W")
covmat_center_pre_w <- filter(covmat_all, Region.period == "2.Center_Pre" & Treatment == "W")
covmat_north_pre_w <- filter(covmat_all, Region.period == "1.North_Pre" & Treatment == "W")


south_peak_d <- cov(covmat_south_peak_d[,1:2]) 
center_peak_d <- cov(covmat_center_peak_d[,1:2])
north_peak_d <- cov(covmat_north_peak_d[,1:2])

south_pre_d <- cov(covmat_south_pre_d[,1:2])
center_pre_d <- cov(covmat_center_pre_d[,1:2])
north_pre_d <- cov(covmat_north_pre_d[,1:2])

south_peak_w <- cov(covmat_south_peak_w[,1:2])
center_peak_w <- cov(covmat_center_peak_w[,1:2])
north_peak_w <- cov(covmat_north_peak_w[,1:2])

south_pre_w <- cov(covmat_south_pre_w[,1:2])
center_pre_w <- cov(covmat_center_pre_w[,1:2])
north_pre_w <- cov(covmat_north_pre_w[,1:2])

# save all as dataframes 

#write.csv(south_peak_d, "Data/south_peak_d_cov.csv")
#write.csv(south_pre_d, "Data/south_pre_d_cov.csv")
#write.csv(south_peak_w, "Data/south_peak_w_cov.csv")
#write.csv(south_pre_w, "Data/south_pre_w_cov.csv")

#write.csv(center_peak_d, "Data/centre_peak_d_cov.csv")
#write.csv(center_pre_d, "Data/centre_pre_d_cov.csv")
#write.csv(center_peak_w, "Data/centre_peak_w_cov.csv")
#write.csv(center_pre_w, "Data/centre_pre_w_cov.csv")

#write.csv(north_peak_d, "Data/north_peak_d_cov.csv")
#write.csv(north_pre_d, "Data/north_pre_d_cov.csv")
#write.csv(north_peak_w, "Data/north_peak_w_cov.csv")
#write.csv(north_pre_w, "Data/north_pre_w_cov.csv")

# Plot all loadings
biplot(pc1_all, scale=0, col=c("black", "red"), xlab = "PC1 (55%)", ylab="PC2 (21%)")


## Add ellipse: region and have wet and dry, pre and peak on each graph (3 graphs) = 4 ellipse each 

#south
biplot(pc1_all, scale=0, col=c("black", "red"), xlab = "PC1 (55%)", ylab="PC2 (21%)")

ellipse_1 <- ellipse(south_pre_d/(max(abs(pc1_all$rotation))*100), centre=colMeans(covmat_south_pre_d[,1:2]), level=0.95)

polygon(ellipse(south_pre_d/(max(abs(pc1_all$rotation))*100), centre=colMeans(covmat_south_pre_d[,1:2]), level=0.95), col=adjustcolor("red", alpha.f=0.25), border="red")
polygon(ellipse(south_peak_d/(max(abs(pc1_all$rotation))*100), centre=colMeans(covmat_south_peak_d[,1:2]), level=0.95), col=adjustcolor("darkred", alpha.f=0.25), border="darkred")

polygon(ellipse(south_pre_w/(max(abs(pc1_all$rotation))*100), centre=colMeans(covmat_south_pre_w[,1:2]), level=0.95), col=adjustcolor("lightblue", alpha.f=0.25), border="lightblue")
polygon(ellipse(south_peak_w/(max(abs(pc1_all$rotation))*100), centre=colMeans(covmat_south_peak_w[,1:2]), level=0.95), col=adjustcolor("blue", alpha.f=0.25), border="blue")


#centre
biplot(pc1_all, scale=0, col=c("black", "red"), xlab = "PC1 (55%)", ylab="PC2 (21%)")

polygon(ellipse(center_pre_d/(max(abs(pc1_all$rotation))*100), centre=colMeans(covmat_center_pre_d[,1:2]), level=0.95), col=adjustcolor("red", alpha.f=0.25), border="red")
polygon(ellipse(center_peak_d/(max(abs(pc1_all$rotation))*100), centre=colMeans(covmat_center_peak_d[,1:2]), level=0.95), col=adjustcolor("darkred", alpha.f=0.25), border="darkred")

polygon(ellipse(center_pre_w/(max(abs(pc1_all$rotation))*100), centre=colMeans(covmat_center_pre_w[,1:2]), level=0.95), col=adjustcolor("lightblue", alpha.f=0.25), border="lightblue")
polygon(ellipse(center_peak_w/(max(abs(pc1_all$rotation))*100), centre=colMeans(covmat_center_peak_w[,1:2]), level=0.95), col=adjustcolor("blue", alpha.f=0.25), border="blue")



#north
biplot(pc1_all, scale=0, col=c("black", "red"), xlab = "PC1 (55%)", ylab="PC2 (21%)")

polygon(ellipse(north_pre_d/(max(abs(pc1_all$rotation))*100), centre=colMeans(covmat_north_pre_d[,1:2]), level=0.95), col=adjustcolor("red", alpha.f=0.25), border="red")
polygon(ellipse(north_peak_d/(max(abs(pc1_all$rotation))*100), centre=colMeans(covmat_north_peak_d[,1:2]), level=0.95), col=adjustcolor("darkred", alpha.f=0.25), border="darkred")

polygon(ellipse(north_pre_w/(max(abs(pc1_all$rotation))*100), centre=colMeans(covmat_north_pre_w[,1:2]), level=0.95), col=adjustcolor("lightblue", alpha.f=0.25), border="lightblue")
polygon(ellipse(north_peak_w/(max(abs(pc1_all$rotation))*100), centre=colMeans(covmat_north_peak_w[,1:2]), level=0.95), col=adjustcolor("blue", alpha.f=0.25), border="blue")




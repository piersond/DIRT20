######
# CONVERT THIS TO RMD
######

#####
#DIRT 20 SOM analysis

library(ggplot2)
library(tidyr)
library(reshape2)
library(dplyr)

### SETUP ###
#Set path to data folder
#setwd("C:/Users/Derek/Google Drive/OSU - PhD/Projects/DIRT/DIRT 2017 Soil Cores/_Master_Files_DIRT20/Analysis/DIRT20_pub_data_analysis/")

#Load raw data csv
data.raw <- read.csv("DIRT20_soil_master_raw.csv", as.is=T)
colnames(data.raw) <- as.character(data.raw[1,1:16])
data.raw <- data.raw[-1,]

#Convert numerics
for (i in 6:16) {data.raw[,i] <- as.numeric(data.raw[,i])}
str(data.raw)

### WARNING - MANUAL DATA CHANGE ### HARD CODED
### DELETE THIS CODE IF NOT UNDERSTOOD OR INTENDED

#Changing control values for 0-10 cm
  #C values are averages from original depth cores (repx4, minus one >20 outlier for plot 8)
  data.raw[8,6] <- 4.836
  data.raw[12,6] <- 6.407
  data.raw[14,6] <- 3.66

### Plot C by trt & depth
ggplot(data.raw, aes(x=TRT, y=bulk_percC, color=Depth)) +
  geom_boxplot(data=data.raw, aes(fill=Depth)) +
  geom_jitter(data=data.raw, aes(size=2), width=0.05, height=0.1) +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1")

######
# CONVERT THIS TO RMD
######

#####
#DIRT 20 SOM analysis

library(ggplot2)
library(tidyr)
library(reshape2)
library(dplyr)
library(ggthemes)

### SETUP ###
#Set path to data folder
setwd("C:/github/DIRT20")
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

  
# Get mean and standard dev, std err
soc_summary <- data.raw %>% group_by(TRT, Depth) %>% summarize(n = n(),
                                                      mean = mean(bulk_percC*10),
                                                      stdev = sd(bulk_percC*10),
                                                      sterr = sd(bulk_percC*10)/sqrt(3))

# Set plot discrete x-axis order
trt_order <- c('CTL', 'DL', 'DW', 'NL', 'NR', 'NI', 'NOA')

#Create labels for facet plots
depth.labs <- c("0-10 cm", "10-20 cm", "20-40 cm", "40-60 cm", "60-100 cm")
names(depth.labs) <- c("0-10", "10-20", "20-40", "40-60", "60-100")


### FIGURE 1 - ALL TRTS###
f1_all <- ggplot(soc_summary, aes(x=factor(TRT, levels = trt_order), y=mean, fill=TRT)) + 
            geom_histogram(stat = "identity", colour="black") + 
            facet_wrap( ~ Depth, ncol=2, scales = "free", labeller = labeller(Depth = depth.labs)) +
            ylim(0,80)+
            theme_minimal() +
            annotate("segment", x=0, xend=Inf, y=0, yend=0) +
            annotate("segment", x=0, xend=-Inf, y=0, yend=Inf) +
            ylab("Soil Carbon (mg C/g soil)") +
            xlab("") + 
            ggtitle("Detrital treatment effects on soil carbon by depth") +
            theme(panel.spacing = unit(2, "lines")) +
            scale_fill_manual(values = c("#999999", "#E69F00", "#009E73",
                                         "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) + 
            theme(legend.position = "none") +
            geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), width=.2,position=position_dodge(.9)) +
            theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
            theme(text = element_text(size=18))
f1_all
                  
ggsave(plot=f1_all, path = "ggplots", filename = "fig1_all_trts.png",
       width = 8, height = 16 , dpi = 300)


### FIGURE 1 - ADTN TRTS###
additions <- c("CTL", "DL", "DW")
adtns_only_data <- soc_summary %>% filter(TRT %in% additions)

f1_adtns <- ggplot(adtns_only_data, aes(x=factor(TRT, levels = trt_order), y=mean, fill=TRT)) + 
  geom_histogram(stat = "identity", colour="black") + 
  facet_wrap( ~ Depth, ncol=2, scales = "free", labeller = labeller(Depth = depth.labs)) +
  ylim(0,80)+
  theme_minimal() +
  annotate("segment", x=0, xend=Inf, y=0, yend=0) +
  annotate("segment", x=0, xend=-Inf, y=0, yend=Inf) +
  ylab("Soil Carbon (mg C/g soil)") +
  xlab("") + 
  ggtitle("Detrital treatment effects on soil carbon by depth") +
  theme(panel.spacing = unit(2, "lines")) +
  scale_fill_manual(values = c("#999999", "#E69F00", "#009E73",
                               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), width=.2,position=position_dodge(.9)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(text = element_text(size=18))
f1_adtns

ggsave(plot=f1_adtns, path = "ggplots", filename = "fig1_adtns_trts.png",
       width = 8, height = 16 , dpi = 300)

### FIGURE 1 - RMVL TRTS###
removals <- c("CTL", "NL", "NR", "NI", "NOA")
rmvl_only_data <- soc_summary %>% filter(TRT %in% removals)

f1_rmvls <- ggplot(rmvl_only_data, aes(x=factor(TRT, levels = trt_order), y=mean, fill=TRT)) + 
  geom_histogram(stat = "identity", colour="black") + 
  facet_wrap( ~ Depth, ncol=2, scales = "free", labeller = labeller(Depth = depth.labs)) +
  ylim(0,80)+
  theme_minimal() +
  annotate("segment", x=0, xend=Inf, y=0, yend=0) +
  annotate("segment", x=0, xend=-Inf, y=0, yend=Inf) +
  ylab("Soil Carbon (mg C/g soil)") +
  xlab("") + 
  ggtitle("Detrital treatment effects on soil carbon by depth") +
  theme(panel.spacing = unit(2, "lines")) +
  scale_fill_manual(values = c("#999999", "#E69F00", "#009E73",
                               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), width=.2,position=position_dodge(.9)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(text = element_text(size=18))
f1_rmvls

ggsave(plot=f1_rmvls, path = "ggplots", filename = "fig1_rmvls_trts.png",
       width = 8, height = 16 , dpi = 300)

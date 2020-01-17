library(ggplot2)
library(tidyr)
library(reshape2)
library(dplyr)
library(ggthemes)
library(patternplot)
library(png)

### SETUP ###
#Set path to data folder
#setwd("C:/github/DIRT20")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

#save summary
write.csv(soc_summary,"DIRT20_SOC by depth_treatment summary.csv")


# Set plot discrete x-axis order
trt_order <- c('CTL', 'DL', 'DW', 'NL', 'NR', 'NI', 'NOA')

#Create labels for facet plots
depth.labs <- c("0-10 cm", "10-20 cm", "20-40 cm", "40-60 cm", "60-100 cm")
names(depth.labs) <- c("0-10", "10-20", "20-40", "40-60", "60-100")


### FIGURE 1 - ALL TRTS###
f1_all <- ggplot(soc_summary, aes(x=factor(TRT, levels = trt_order), y=mean, fill=TRT)) + 
            geom_histogram(stat = "identity", colour="black") + 
            #facet_wrap( ~ Depth, ncol=2, scales = "free", labeller = labeller(Depth = depth.labs)) +
            ylim(0,80)+
            theme_minimal() +
            annotate("segment", x=0, xend=Inf, y=0, yend=0) +
            annotate("segment", x=0, xend=-Inf, y=0, yend=Inf) +
            ylab(expression("Soil carbon (mg C"~g^{-1}~"soil)")) +
            xlab("") + 
            ggtitle("Detrital treatment effects on soil carbon by depth") +
            theme(panel.spacing = unit(2, "lines")) +
            scale_fill_manual(values = c("#eeeeee", "#addd8e","#31a354","#d7301f", "#fdcc8a", "#b30000", "#fc8d59")) + 
            geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), width=.2,position=position_dodge(.9)) +
            theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
            theme(text = element_text(size=18))
f1_all
                  
ggsave(plot=f1_all, filename = "fig1_all_trts.jpeg",
       width = 8, height = 8 , dpi = 300)


### FIGURE 1 - ADTN TRTS###
additions <- c("CTL", "DL", "DW")
adtns_only_data <- soc_summary %>% filter(TRT %in% additions)

f1_adtns <- ggplot(adtns_only_data, aes(x=factor(TRT, levels = trt_order), y=mean, fill=TRT)) + 
  geom_histogram(stat = "identity", colour="black") + 
  facet_wrap( ~ Depth, ncol=3, scales = "free", labeller = labeller(Depth = depth.labs)) +
  ylim(0,80)+
  theme_minimal() +
  annotate("segment", x=0, xend=Inf, y=0, yend=0) +
  annotate("segment", x=0, xend=-Inf, y=0, yend=Inf) +
  ylab(expression("Soil carbon (mg C"~g^{-1}~"soil)")) +
  xlab("Treatment") + 
  theme(legend.title=element_blank()) +
  #ggtitle("Detrital treatment effects on soil carbon by depth") +
  theme(panel.spacing = unit(2, "lines")) +
  scale_fill_manual(labels = c("Control (CTL)", "Double Litter (DL)", "Double Wood (DW)"), values = c("#eeeeee", "#addd8e","#31a354")) + 
  theme(legend.position = "bottom") +
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), width=.2,position=position_dodge(.9)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(text = element_text(size=18))
f1_adtns

ggsave(plot=f1_adtns, filename = "fig1_adtns_trts.jpeg",
       width = 9, height = 8 , dpi = 300)


### FIGURE 1 - RMVL TRTS###
removals <- c("CTL", "NL", "NR", "NI", "NOA")
rmvl_only_data <- soc_summary %>% filter(TRT %in% removals)

f1_rmvls <- ggplot(rmvl_only_data, aes(x=factor(TRT, levels = trt_order), y=mean, fill=TRT)) + 
  geom_histogram(stat = "identity", colour="black") + 
  facet_wrap( ~ Depth, ncol=3, scales = "free", labeller = labeller(Depth = depth.labs)) +
  ylim(0,80)+
  theme_minimal() +
  annotate("segment", x=0, xend=Inf, y=0, yend=0) +
  annotate("segment", x=0, xend=-Inf, y=0, yend=Inf) +
  ylab(expression("Soil carbon (mg C"~g^{-1}~"soil)")) +
  xlab("Treatment") +  
  theme(legend.title=element_blank()) +
  theme(panel.spacing = unit(2, "lines")) +
  scale_fill_manual(labels = c("Control (CTL)", "No Input (NI)", "No Litter (NL)", "No O-A Horizon (NOA)", "No Root (NR)"),values = c("#eeeeee", "#d7301f", "#fdcc8a", "#b30000", "#fc8d59")) + 
  theme(legend.position = "bottom") +
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), width=.2,position=position_dodge(.9)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(text = element_text(size=18))
f1_rmvls

ggsave(plot=f1_rmvls, filename = "fig1_rmvls_trts.jpeg",
       width = 9, height = 8 , dpi = 300)

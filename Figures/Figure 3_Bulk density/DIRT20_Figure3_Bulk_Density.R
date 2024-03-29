library(ggplot2)
library(gridExtra)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Import compiled raw data file output from DIRT20_PVC Cores_data merge.R
data <- read.csv("DIRT20_PVC core data_compiled.csv")

################################
#CALCULATIONS
data$CoreVol <- data$CoreHt*(2.7*2.7)*3.14
data$RockVol <- data$RockWt/1.9  ### May need to add column to only adjust for rocks if greater than X rock wt
data$EffVol <- data$CoreVol-data$RockVol-data$WoodVol
data$moisture <- ((data$WetSampTinWt-data$TinWt)-(data$DryTinWt-data$TinWt))/(data$DryTinWt-data$TinWt)
data$BagWtChk <- data$BagSampWt-data$TotWetSampWt
data$TotDrySamplWt <- (data$TotWetSampWt/(1+data$moisture))-data$WoodWt-data$RockWt
data$Bulk.Density <- data$TotDrySamplWt/data$EffVol  
data$OM <- data$C*1.734
data$Mineral <- 100-data$OM

#Export data table
#write.csv(data, file = "Bulk density/DIRT20_bulk density_data.csv")

#Remove data with error flags
cln_data <- data[!(data$Flag=="X"),]

#Summarize data
sum_data <- cln_data %>% na.omit(Bulk.density) %>% group_by(Plot, Trt, Depth) %>% summarize(n=n(),
                                                            bd_mean = mean(Bulk.Density, na.rm = T),
                                                            bd_sterr = sd(Bulk.Density,na.rm = T)/sqrt(n()))

#Export to summarize manually in excel
#write.csv(sum_data, file = "Bulk density/mean bulk density by trt and depth.csv")


# Summarize across depths 0-10 cm
plot_bd_to_10cm <- sum_data %>% filter(Depth != 10) %>% group_by(Plot,Trt) %>% summarize(bulkden_mean = mean(bd_mean, na.rm = T),
                                                                                    bulkden_stderr = mean(bd_sterr, na.rm = T)) 

write.csv(plot_bd_to_10cm, "DIRT20_Bulkdensity_0-10cm_by plot.csv")

mean_bd_to_10cm <- plot_bd_to_10cm %>% group_by(Trt) %>% summarize(bulkden_mean = mean(bulkden_mean, na.rm = T),
                                                                                         bulkden_stderr = mean(bulkden_stderr, na.rm = T))
write.csv(mean_bd_to_10cm, "DIRT20_Bulkdensity_0-10cm_by trt.csv")


### Figure 3 ###
# Plot 0-10 cm bulk density by treatment
trt_order <- c('CTL', 'DL', 'DW', 'NL', 'NR', 'NI', 'NOA')

plot_bd_0_to_10 <- ggplot(mean_bd_to_10cm, aes(x=factor(Trt, levels = trt_order), y=bulkden_mean, fill=Trt)) + 
  geom_histogram(stat = "identity", colour="black") + 
  ylim(0,1)+
  theme_minimal() +
  annotate("segment", x=0, xend=Inf, y=0, yend=0) +
  annotate("segment", x=0, xend=-Inf, y=0, yend=Inf) +
  ylab(bquote('Bulk Density (g '*cm^-3~')')) +
  xlab("Treatment") + 
  #ggtitle("Soil Bulk Density, 0-10 cm") +
  scale_fill_manual(labels = c("Control (CTL)", "Double Litter (DL)", "Double Wood (DW)", "No Input (NI)", "No Litter (NL)", "No O-A Horizon (NOA)", "No Root (NR)"),
                    values = c("#eeeeee", "#addd8e","#31a354","#d7301f",
                               "#fdcc8a", "#b30000","#fc8d59")) + 
  #theme(legend.position = "none") +
  theme(legend.title=element_blank()) +
  geom_errorbar(aes(ymin=bulkden_mean-bulkden_stderr, ymax=bulkden_mean+bulkden_stderr), width=.2,position=position_dodge(.9)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(text = element_text(size=18))

plot_bd_0_to_10

#Figure 3 save
ggsave(plot=plot_bd_0_to_10, filename = "fig3_Bulk density by treatment_0-10cm.jpeg",
       width = 9, height = 6 , dpi = 300)




# # Summarize across depths 10-15 cm
# mean_bd_10_to_15cm <- sum_data %>% filter(Depth == 10) %>% group_by(Trt) %>% summarize(bulkden_mean = mean(bd_mean),
#                                                                                     bulkden_stderr = mean(bd_sterr)) 
# ### Figure 3 ###
# # Plot 0-10 cm bulk density by treatment
# trt_order <- c('CTL', 'DL', 'DW', 'NL', 'NR', 'NI', 'NOA')
# 
# plot_bd_10_to_15 <- ggplot(mean_bd_10_to_15cm, aes(x=factor(Trt, levels = trt_order), y=bulkden_mean, fill=Trt)) + 
#   geom_histogram(stat = "identity", colour="black") + 
#   ylim(0,1.2)+
#   theme_minimal() +
#   annotate("segment", x=0, xend=Inf, y=0, yend=0) +
#   annotate("segment", x=0, xend=-Inf, y=0, yend=Inf) +
#   ylab("Bulk Density (g cm-3") +
#   xlab("Treatment") + 
#   ggtitle("Soil Bulk Density, 10-15 cm") +
#   scale_fill_manual(values = c("#999999", "#E69F00", "#009E73",
#                                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) + 
#   theme(legend.position = "none") +
#   geom_errorbar(aes(ymin=bulkden_mean-bulkden_stderr, ymax=bulkden_mean+bulkden_stderr), width=.2,position=position_dodge(.9)) +
#   theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
#   theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
#   theme(text = element_text(size=18))
# 
# plot_bd_10_to_15

#Figure 3 save
#ggsave(plot=plot_bd_10_to_15, path = "ggplots", filename = "misc_Bulk density by treatment_10-15cm.png",
#       width = 5, height = 8 , dpi = 300)






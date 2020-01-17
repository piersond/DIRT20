library(ggplot2)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

master <- read.csv("HJA_DIRT20_Litter_weights.csv", as.is=T)

summary <- master %>% group_by(TRT, PLOT) %>% summarize(litter_wt = sum(litter_wt))


data <- summary %>% group_by(TRT) %>% summarize(lit_wt_mean = mean(litter_wt),
                                               lit_wt_sterr = sd(litter_wt)/sqrt(3),
                                               lit_wt_n = n())

data$rel_perc_mass <- (data$lit_wt_mean/data$lit_wt_mean[1]-1)*100
data$rel_perc_sterr <- (data$lit_wt_sterr/data$lit_wt_mean[1])*100


#plot

#gglot
trt_order <- c('CTL', 'DL', 'DW', 'NR')

litter_plot <- ggplot(data , aes(x=factor(TRT, levels = trt_order), y=data$rel_perc_mass, fill=TRT)) +
  geom_histogram(stat = "identity", colour="black") + 
  theme_minimal() +
  scale_y_continuous(breaks=seq(-40, 80, 20), limits=c(-40, 80)) +
  #annotate("segment", x=0, xend=Inf, y=0, yend=0) +
  annotate("segment", x=0, xend=-Inf, y=-40, yend=Inf) +
  ylab('Surface litter mass (% relative to control)') +
  xlab("Treatment") + 
  #ggtitle("HJA DIRT 20 YR: Fine Root Mass, 0-10 cm") +
  scale_fill_manual(labels = c("Control (CTL)", "Double Litter (DL)", "Double Wood (DW)", "No Roots (NR)"), values = c("#eeeeee", "#addd8e","#31a354","#fc8d59")) +
  #theme(legend.position = "none") +
  theme(legend.title=element_blank()) +
  geom_errorbar(aes(ymin=data$rel_perc_mass-data$rel_perc_sterr, ymax=data$rel_perc_mass+data$rel_perc_sterr), width=.2,position=position_dodge(.9)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(text = element_text(size=18))
litter_plot

ggsave(plot=litter_plot, filename = "fig5_relative litter mass.jpeg",
       width = 9, height = 6 , dpi = 300)



                        
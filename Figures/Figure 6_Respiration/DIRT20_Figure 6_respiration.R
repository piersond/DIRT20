library(ggplot2)
library(multcompView)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

master <- read.csv("DIRT_respiration_thruDay52_cumulative.csv", as.is=T)

data <- master %>% group_by(TRT,DAY) %>% filter(DAY < 24) %>% filter(DAY != 25) %>%
          summarize(co2_mean = mean(X12C, na.rm=T),
                    co2_sterr = sd(X12C, na.rm=T)/sqrt(n())) 

#plot
co2_plot <- ggplot(data, aes(x=DAY, y=co2_mean, colour=TRT, group=TRT)) + 
  theme_minimal() +
  geom_errorbar(aes(ymin=co2_mean-co2_sterr, ymax=co2_mean+co2_sterr), colour="grey30", width=.1, position=position_dodge(0.1)) +
  geom_line(position=pd, size=2) +
  geom_point(position=pd, size=2, shape=21, fill="white") + # 21 is filled circle
  scale_color_manual(values=c("#000000", "#addd8e","#31a354","#fc8d59","#d7301f")) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 800, by = 200), limits=c(0, 800)) +
  ylab(expression(mu*"g CO"[2]~g^{-1}~soil)) +
  xlab("Days") +
  theme(text = element_text(size = 24), legend.justification=c(1,0),legend.position=c(0.98,0.02), legend.key.size = unit(0.5, "cm")) + # Position legend in bottom right
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(legend.title = element_blank())
  #theme(legend.background = element_rect(fill='white', size=0.5, linetype="solid", colour ="grey20")) 

co2_plot
                        
ggsave(plot=co2_plot, filename = "fig6_respiration.png",
       width = 6, height = 6 , dpi = 300)
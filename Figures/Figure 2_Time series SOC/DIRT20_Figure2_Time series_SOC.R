library(dplyr)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Bring in master copy of DIRT C and N data from years 10, 15, 20
master <- read.csv("HJA_DIRT_CN_yrs10-20_DNP.csv") 

### Clean outliers from data
#Remove row if SOC > 10
df <- master %>% filter(percC < 10)

#Summarize by year, depth, and trt
df <- df %>% group_by(TX_Year, Trt, Depth) %>% summarize(n=n(),
                                                             soc_mean = mean(percC),
                                                             tn_mean = mean(percN),
                                                             soc_sterr = sd(percC)/sqrt(n()),
                                                             tn_sterr = sd(percN)/sqrt(n())) %>%
      mutate_if(is.numeric, ~round(., 3))

#Add a uniform starting SOC for the site in 1997
# ???

### Prep data for plot ###
#filter out depths past 0-10cm and remove the NOA trt
plot_df <- df %>% filter(Depth == "0-10cm") %>% filter(Trt != "NOA")


### Fig 2 ###
#point and line plot
f2_all <- ggplot(plot_df, aes(x=TX_Year, y=soc_mean, color=Trt)) + 
  geom_point(size=3) + geom_line(size=1) +
  ylim(0,8)

f2_all


#facet wrap histogram by trt
# Set facet order
trt_order <- c('CTL', 'DL', 'DW', 'NL', 'NR', 'NI', 'NOA')
plot_df_ordered <- arrange(transform(plot_df,
                           Trt=factor(Trt,levels=trt_order)),Trt)

#Set panel labels
Trt.labs <- c("Control (CTL)", "Double Litter (DL)", "Double Wood (DW)", "No Litter (NL)", "No Roots (NR)", "No Inputs (NI)")
names(Trt.labs) <- c('CTL', 'DL', 'DW', 'NL', 'NR', 'NI')


#Create vector for control reference bars
bars1 <- rep(c(6.04,5.64,4.97), 6)

plot_f2 <- ggplot(plot_df_ordered, aes(x=as.character(TX_Year), y=soc_mean, fill=Trt)) + geom_histogram(stat = "identity", colour="black") + 
  facet_wrap( ~ Trt, ncol=3, scales = "free", labeller = labeller(Trt = Trt.labs)) +
  ylim(0,10) +
  theme_minimal() +
  annotate("segment", x=0, xend=Inf, y=0, yend=0) +
  annotate("segment", x=0, xend=-Inf, y=0, yend=Inf) +
  ylab("Soil Carbon (mg C/g soil)") +
  xlab("Study Year") + 
  #ggtitle("Detrital treatment effects on soil carbon over time") +
  theme(panel.spacing = unit(3, "lines")) +
  scale_fill_manual(values = c("#eeeeee", "#addd8e","#31a354",
                               "#fdcc8a", "#fc8d59","#d7301f")) + 
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=soc_mean-soc_sterr, ymax=soc_mean+soc_sterr), width=.2,position=position_dodge(.9)) +
  geom_errorbar(aes(y = bars1, ymin = bars1, ymax = bars1), color="red",lty=1, size=1) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(text = element_text(size=18))
plot_f2

ggsave(plot=plot_f2, filename = "fig2_soc_by-trt_by-time.png",
       width = 9, height = 8 , dpi = 300)


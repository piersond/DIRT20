library(ggplot2)
library(gridExtra)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dnp <- read.csv("DIRT_DNP_key.csv")
colnames(dnp)[1] <- "DNP"

wts <- read.csv("DIRT20_DNP_root_rock_wts.csv")
colnames(wts)[1] <- "DNP"

data <- merge(dnp, wts, by="DNP", all.x=T, all.y=F)
write.csv(data, "DNP_rocks_roots.csv")

rep_smry <- data %>% filter(Depth < 10) %>% group_by(Trt, Plot, Rep) %>%
              summarise(root_sum = sum(RootWt, na.rm=TRUE))

plot_smry <- rep_smry %>% group_by(Trt, Plot) %>%
              summarise(root_mean = mean(root_sum, na.rm=TRUE),
                        root_sterr = sd(root_sum)/sqrt(3))

trt_smry <- plot_smry %>% group_by(Trt) %>%
              summarise(root_mn = mean(root_mean, na.rm=TRUE),
                        root_sterr = sd(root_mean)/sqrt(3))

core_vol <- 10*(2.7*2.7)*3.14*0.000001 # in m^3

core_area <- (2.7*2.7)*3.14*0.0001 # in m^2


plot_smry$core_vol <- core_vol
plot_smry$core_area <- core_area

plot_smry$root_mass_m2 <- plot_smry$root_mean/core_area
plot_smry$root_mass_sterr_m2 <- plot_smry$root_sterr/core_area

colnames(plot_smry)[3] <- 'root_mass_mean'
colnames(plot_smry)[4] <- 'root_mass_sterr'
write.csv(plot_smry, "DIRT20_root_plot_summary.csv")


trt_smry$rootm2 <- trt_smry$root_mn/core_area
trt_smry$rootsterr_m2 <- trt_smry$root_sterr/core_area
write.csv(trt_smry, "DIRT20_root_trt_summary.csv")


#gglot
trt_order <- c('CTL', 'DL', 'DW', 'NL', 'NR', 'NI', 'NOA')

root_plot <- ggplot(trt_smry, aes(x=factor(Trt, levels = trt_order), y=rootm2, fill=Trt)) +
  geom_histogram(stat = "identity", colour="black") + 
  theme_minimal() +
  annotate("segment", x=0, xend=Inf, y=0, yend=0) +
  annotate("segment", x=0, xend=-Inf, y=0, yend=Inf) +
  ylab(bquote('Fine root mass (g '*m^-2~')')) +
  xlab("Treatment") + 
  #ggtitle("HJA DIRT 20 YR: Fine Root Mass, 0-10 cm") +
  scale_fill_manual(labels = c("Control (CTL)", "Double Litter (DL)", "Double Wood (DW)", "No Input (NI)", "No Litter (NL)", "No O-A Horizon (NOA)", "No Root (NR)"),
                    values = c("#eeeeee", "#addd8e","#31a354","#d7301f",
                               "#fdcc8a", "#b30000","#fc8d59")) + 
  #theme(legend.position = "none") +
  theme(legend.title=element_blank()) +
  geom_errorbar(aes(ymin=rootm2-rootsterr_m2, ymax=rootm2+rootsterr_m2), width=.2,position=position_dodge(.9)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(text = element_text(size=18))
root_plot

ggsave(plot=root_plot, filename = "fig4_root mass.jpeg",
       width = 9, height = 6 , dpi = 300)

                        
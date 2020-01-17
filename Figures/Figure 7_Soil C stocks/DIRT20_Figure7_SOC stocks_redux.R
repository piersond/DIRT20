library(dplyr)
library(ggplot2)
library(scales)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#save data
df <- read.csv("DIRT20_SOC stocks.csv")

#0-100 cm sterr calc
err <- df %>% select(TRT, C_stock, C_stock_sterr) %>% group_by(TRT) %>% summarize(sum_stock = sum(C_stock),
                                                                                  sum_sterr = sum(C_stock_sterr))

err$low_lim <- err$sum_stock - err$sum_sterr
err$high_lim <- err$sum_stock + err$sum_sterr


#plot
trt_order <- c('CTL', 'DL', 'DW', 'NL', 'NR', 'NI', 'NOA')
fills <- c("#409f0c","#8aa024","#cfa03b","#8e651c","#522e00")
#cl <- c(err$low_lim,rep(0,28))
#cu <- c(err$high_lim,rep(0,28))

cl <- c(err$low_lim[1], rep(0,4),err$low_lim[2], rep(0,4), err$low_lim[3], rep(0,4), 
        err$low_lim[4], rep(0,4), err$low_lim[5], rep(0,4), err$low_lim[6], rep(0,4), 
        err$low_lim[7], rep(0,4))
cu <- c(err$high_lim[1], rep(0,4),err$high_lim[2], rep(0,4), err$high_lim[3], rep(0,4), 
        err$high_lim[4], rep(0,4), err$high_lim[5], rep(0,4), err$high_lim[6], rep(0,4), 
        err$high_lim[7], rep(0,4))

stack_plot <- ggplot(df, aes(x=factor(TRT, levels = trt_order), y=C_stock, fill=Depth)) + 
  geom_histogram(stat="identity", colour="black") +
  scale_fill_manual(values=fills, name = "Depth (cm)") +
  theme_minimal() +
  annotate("segment", x=0, xend=Inf, y=0, yend=0) +
  annotate("segment", x=0, xend=-Inf, y=0, yend=Inf) +
  ylab(expression("Soil C Stock (kg C"~m^{-2}~")")) + 
  xlab("Treatment") + 
  geom_errorbar(aes(x=factor(TRT, levels = trt_order),ymin=cl, ymax=cu), width=.5, position=position_dodge(0)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(text = element_text(size=18), legend.key.size = unit(1, "cm"))
stack_plot

ggsave(plot=stack_plot, filename = "fig7_C_stocks.png",
       width = 7, height = 6 , dpi = 300)  

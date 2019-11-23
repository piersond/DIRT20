library(dplyr)
library(ggplot2)
library(scales)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Bring in data
master <- read.csv("DIRT20_SOC by depth_treatment summary.csv", as.is=T)
deep_bulk_den <- read.csv("DIRT_deep_bulkden.csv", as.is=T)

#merge data
df <- merge(master,deep_bulk_den[,c(1,4)], by.x="Depth", by.y="depth", all.x=T)

#add surface bulk density values
df$avg_bulkden[which((df$Depth == "0-10" & df$TRT == "CTL"))] <- 0.606
df$avg_bulkden[which((df$Depth == "0-10" & df$TRT == "DL"))] <- 0.647
df$avg_bulkden[which((df$Depth == "0-10" & df$TRT == "DW"))] <- 0.495
df$avg_bulkden[which((df$Depth == "0-10" & df$TRT == "NL"))] <- 0.742
df$avg_bulkden[which((df$Depth == "0-10" & df$TRT == "NR"))] <- 0.776
df$avg_bulkden[which((df$Depth == "0-10" & df$TRT == "NI"))] <- 0.793
df$avg_bulkden[which((df$Depth == "0-10" & df$TRT == "NOA"))] <- 0.836

#add layer depth
df$lyr_height <- 10
df$lyr_height[which((df$Depth == "20-40"))] <- 20
df$lyr_height[which((df$Depth == "40-60"))] <- 20
df$lyr_height[which((df$Depth == "60-100"))] <- 40

df$C_stock <- (df$mean * df$avg_bulkden * df$lyr_height)/100  #converted to kg C/m2  
df$C_stock_sterr <- (df$sterr * df$avg_bulkden * df$lyr_height)/100  #converted to kg C/m2   

#0-100 cm sterr calc
err <- df %>% select(TRT, C_stock, C_stock_sterr) %>% group_by(TRT) %>% summarize(sum_stock = sum(C_stock),
                                                                        sum_sterr = sum(C_stock_sterr))

err$low_lim <- err$sum_stock - err$sum_sterr
err$high_lim <- err$sum_stock + err$sum_sterr

#save data
write.csv(df, "DIRT20_SOC stocks.csv")


#plot
trt_order <- c('CTL', 'DL', 'DW', 'NL', 'NR', 'NI', 'NOA')
fills <- c("#409f0c","#8aa024","#cfa03b","#8e651c","#522e00")
cl <- c(err$low_lim,rep(0,28))
cu <- c(err$high_lim,rep(0,28))

stack_plot <- ggplot(df, aes(x=factor(TRT, levels = trt_order), y=C_stock, fill=Depth)) + 
  geom_histogram(stat="identity", colour="black") +
  scale_fill_manual(values=fills, name = "Depth (cm)") +
  theme_minimal() +
  annotate("segment", x=0, xend=Inf, y=0, yend=0) +
  annotate("segment", x=0, xend=-Inf, y=0, yend=Inf) +
  ylab(expression("Soil C Stock (kg C"~g^{-1}~"soil)")) + 
  xlab("Treatment") + 
  geom_errorbar(aes(ymin=cl, ymax=cu), width=.5, position=position_dodge(0)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(text = element_text(size=18), legend.key.size = unit(1, "cm"))
stack_plot

ggsave(plot=stack_plot, filename = "fig7_C_stocks.png",
       width = 7, height = 6 , dpi = 300)  

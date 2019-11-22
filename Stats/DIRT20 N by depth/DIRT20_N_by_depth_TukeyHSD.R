# library
library(dplyr)
library(multcompView)
library(ggplot2)

setwd("C:/github/DIRT20/Stats/DIRT20 N by depth")

master <- read.csv("HJA_DIRT_CN_yrs10-20_DNP.csv") 

data <- master %>% filter(TX_Year == 20) %>% filter(Depth == "60-100cm")


# What is the effect of the treatment on the value ?
attach(data)
model=lm(percN ~ Trt )
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'Trt', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown", sub="60-100cm")

#Save TukeyHSD plot
jpeg('DIRT20_percN_TukeyHSD_60-100cm.jpg')
plot(TUKEY , las=1 , col="brown", sub="60-100cm")
dev.off()

#Save tukeyHSD table
write.csv(as.data.frame(TUKEY$Trt), "DIRT20_TukeyHSD_percN_60-100cm.csv")


#Quick ggplot 
#df <- data %>% group_by(Trt) %>% summarize(mean_N = mean(percN))
#ggplot(df, aes(x=Trt, y=mean_N, fill=Trt)) + geom_histogram(stat = "identity", colour="black")

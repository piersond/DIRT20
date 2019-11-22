# library
library(dplyr)
library(multcompView)

setwd("C:/github/DIRT20")

master <- read.csv("HJA_DIRT_CN_yrs10-20_DNP.csv") 

data <- master %>% filter(TX_Year == 20) %>% filter(Depth == "40-60cm")


# What is the effect of the treatment on the value ?
attach(data)
model=lm(percC ~ Trt )
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'Trt', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown", sub="40-60cm")

#Save TukeyHSD plot
jpeg('Stats/DIRT20 C by depth/DIRT20_percC_TukeyHSD_40-60cm.jpg')
plot(TUKEY , las=1 , col="brown", sub="40-60cm")
dev.off()

#Save tukeyHSD table
write.csv(as.data.frame(TUKEY$Trt), "Stats/DIRT20 C by depth/DIRT20_TukeyHSD_percC_40-60cm.csv")

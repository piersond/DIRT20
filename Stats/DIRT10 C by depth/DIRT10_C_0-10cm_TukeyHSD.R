# library
library(dplyr)
library(multcompView)

setwd("C:/github/DIRT20/Stats/DIRT10 C by depth")

master <- read.csv("HJA_DIRT_CN_yrs10-20_DNP.csv") 

data <- master %>% filter(TX_Year == 10)


# What is the effect of the treatment on the value ?
attach(data)
model=lm(percC ~ Trt )
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'Trt', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown", sub="0-10cm")

#Save TukeyHSD plot
jpeg('DIRT10_percC_TukeyHSD_0-10cm.jpg')
plot(TUKEY , las=1 , col="brown", sub="0-10cm")
dev.off()

#Save tukeyHSD table
write.csv(as.data.frame(TUKEY$Trt), "DIRT10_percC_TukeyHSD_0-10cm.csv")

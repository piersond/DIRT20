library(ggplot2)
library(multcompView)
library(dplyr)

setwd("C:/github/DIRT20/Stats/Respiration")

data <- read.csv("DIRT respiration_Day50.csv", as.is=T)

# What is the effect of the treatment on the value ?
attach(data)
model=lm(X12C ~ TRT )
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'TRT', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown", sub="0-10cm")

#Save TukeyHSD plot
jpeg('DIRT20_respiration_TukeyHSD_0-10cm.jpg')
plot(TUKEY , las=1 , col="brown", sub="0-10cm")
dev.off()

#Save tukeyHSD table
write.csv(as.data.frame(TUKEY$Trt), "DIRT20_respiration_TukeyHSD_0-10cm.csv")

                        
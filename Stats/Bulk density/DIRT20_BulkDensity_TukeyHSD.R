# library
library(dplyr)
library(multcompView)

setwd("C:/github/DIRT20/Stats/Bulk density")

data <- read.csv("DIRT20_Bulkdensity_0-10cm_by plot.csv") 

# What is the effect of the treatment on the value ?
attach(data)
model=lm(bulkden_mean ~ Trt )
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'Trt', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown", sub="0-10cm")

#Save TukeyHSD plot
jpeg('DIRT20_bulk density_TukeyHSD_0-10cm.jpg')
plot(TUKEY , las=1 , col="brown", sub="0-10cm")
dev.off()

#Save tukeyHSD table
write.csv(as.data.frame(TUKEY$Trt), "DIRT20_bulk density_TukeyHSD_0-10cm.csv")

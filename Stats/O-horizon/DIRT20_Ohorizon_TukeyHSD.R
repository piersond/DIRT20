library(ggplot2)
library(multcompView)
library(dplyr)

setwd("C:/github/DIRT20/Stats/O-horizon")

master <- read.csv("HJA_DIRT20_Litter_weights.csv", as.is=T)

data <- master %>% group_by(PLOT, TRT) %>% summarize(lit_wt = sum(litter_wt))


# What is the effect of the treatment on the value ?
attach(data)
model=lm(lit_wt ~ TRT )
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'TRT', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown", sub="O-horizon")

#Save TukeyHSD plot
jpeg('DIRT20_litter weight_TukeyHSD.jpg')
plot(TUKEY , las=1 , col="brown", sub="O-horizon")
dev.off()

#Save tukeyHSD table
write.csv(as.data.frame(TUKEY$Trt), "DIRT20_litter weight_TukeyHSD.csv")

                        
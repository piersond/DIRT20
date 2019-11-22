library(ggplot2)
library(multcompView)
library(dplyr)

setwd("C:/github/DIRT20/Stats/Roots")

master <- read.csv("DNP_rocks_roots.csv", as.is=T)

rep_smry <- master %>% filter(Depth < 10) %>% group_by(Trt, Plot, Rep) %>%
              summarise(root_sum = sum(RootWt, na.rm=TRUE))

data <- rep_smry %>% group_by(Trt, Plot) %>%
              summarise(root_mean = mean(root_sum, na.rm=TRUE))

write.csv(data, "DIRT20_roots_by plot.csv")


# What is the effect of the treatment on the value ?
attach(data)
model=lm(root_mean ~ Trt )
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'Trt', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown", sub="0-10cm")

#Save TukeyHSD plot
jpeg('DIRT20_roots_TukeyHSD_0-10cm.jpg')
plot(TUKEY , las=1 , col="brown", sub="0-10cm")
dev.off()

#Save tukeyHSD table
write.csv(as.data.frame(TUKEY$Trt), "DIRT20_roots_TukeyHSD_0-10cm.csv")

                        
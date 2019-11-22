# library
library(dplyr)
library(multcompView)

setwd("C:/github/DIRT20/Stats/DIRT20 C_0-10cm/by treatment type")

master <- read.csv("DIRT20_soil_percC_by trt type.csv") 

data <- master %>% filter(TX_Year == 20) %>% filter(Depth == "0-10cm")


# What is the effect of the treatment on the value ?
attach(data)
model=lm(percC ~ Type )
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'Type', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown", sub="0-10cm")



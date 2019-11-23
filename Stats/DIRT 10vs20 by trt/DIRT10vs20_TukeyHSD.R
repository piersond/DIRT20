# library
library(dplyr)
library(multcompView)

setwd("C:/github/DIRT20/Stats/DIRT 10vs20 by trt")

master <- read.csv("HJA_DIRT_CN_yrs10-20_DNP.csv", as.is=T) 
master$TX_Year <- as.character(master$TX_Year)
trts <- unique(master$Trt)

#confine analysis to 0-10 cm
data <- master %>% filter(Depth == "0-10cm")


for(i in 1:length(trts)){
  
df <- data %>% filter(Trt == trts[i])

# What is the effect of the treatment on the value ?
attach(df)
model=lm(percC ~ TX_Year)
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'TX_Year', conf.level=0.80)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown", sub=trts[i])

#Save TukeyHSD plot
jpeg(paste0('DIRT10vs20_percC_TukeyHSD_',trts[i],'conf_80.jpg'))
plot(TUKEY , las=1 , col="brown", sub=trts[i])
dev.off()

#Save tukeyHSD table
write.csv(as.data.frame(TUKEY$Trt), paste0('DIRT10vs20_percC_TukeyHSD_',trts[i],'conf_80.csv'))
}

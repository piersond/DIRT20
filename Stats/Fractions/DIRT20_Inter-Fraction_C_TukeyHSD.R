# library
library(dplyr)
library(multcompView)
library(ggplot)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

master <- read.csv("DIRT20_fracs_IF-HF combined.csv") 

data <- master %>% filter(Depth == "0-10") %>% filter(TRT != "NOA")


#ggplot
summary <- data %>% group_by(TRT) %>% summarize(n=n(),
                                                mean_C = mean(IF_percC),
                                                sterr_C = sd(IF_percC)/sqrt(3))

ggplot(summary, aes(x=TRT, y=mean_C, fill=TRT)) + 
  geom_histogram(stat = "identity", colour="black") + 
  theme_minimal() +
  geom_errorbar(aes(ymin=mean_C-sterr_C, ymax=mean_C+sterr_C), width=.2,position=position_dodge(.9)) +
  theme(text = element_text(size=24))


# What is the effect of the treatment on the value ?
attach(data)
model=lm(IF_percC ~ TRT )
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'TRT', conf.level=0.95)
tbl <- as.data.frame(TUKEY$TRT)
tbl

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown", sub="0-10cm")

#Save TukeyHSD plot
#jpeg('Stats/DIRT20 C by depth/DIRT20_percC_TukeyHSD_40-60cm.jpg')
#plot(TUKEY , las=1 , col="brown", sub="40-60cm")
#dev.off()

#Save tukeyHSD table
#write.csv(as.data.frame(TUKEY$TRT), "Stats/DIRT20 C by depth/DIRT20_TukeyHSD_percC_40-60cm.csv")

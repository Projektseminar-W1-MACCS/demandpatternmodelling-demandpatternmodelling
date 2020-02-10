#install.packages("RColorBrewer")
#install.packages("ggplot2")
library(RColorBrewer)
library(ggplot2)
library(dplyr)


#### INTI ####
# Comment for selecting 

Z = ABC_portfolio10  # 10 Activity cost pools
#Z = ABC_portfolio20 # 20 Activity cost pools


##### FUNCTIONS ######


Z$DELTA <- Z$DELTA*100
Z$FACTORCP <- as.factor(Z$CP)
Z$FACTORRUN <- as.factor(Z$RUN)
Z$ErrorLEVEL <- as.factor(Z$ErrorLEVEL)
Z$P_NUMB_FACTOR = as.factor(Z$P_NUMB)
Z = subset(Z, Z$DELTA > -100)


#### DENSITY ####
Z1 = subset(Z, Z$P_NUMB == 2)
mean(Z1$DELTA)
sd(Z1$DELTA)*2
summary(Z1)  # RANDOM ERRO

Z1densityProduct2 <- ggplot(Z1, aes(Z1$DELTA,  fill=Z1$ErrorLEVEL)) + xlim(-100,100) +  geom_density(alpha=0.4)  +  
  theme_minimal() + 
  geom_vline(xintercept=-13.06, linetype="dashed", color = "black") + 
  geom_vline(xintercept=38.97, linetype="dashed", color = "black") + 
  geom_vline(xintercept=12.954) + 
  # theme(legend.position="none") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Percentage error [%]", y ="Probability density function [pdf]", fill ="Error") + 
  theme(text = element_text(size=24))
Z1densityProduct2### new


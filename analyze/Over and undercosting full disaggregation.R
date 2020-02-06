###########  +5 -5 % ANALYSIS 

library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(reshape2)
library(scales)

### INIT ####
FontSize =20

Z = ABC_FullDis

#### VARIABLE DECLARATION AND FACTORIZATION

Z$DELTA = Z$DELTA * 100 
Z$CP = as.factor(Z$CP)


# Calculating +-5% after Anderson for botch datasets.
Z$OVERCOSTED = Z$DELTA > 0
Z$OVERCOSTED = as.numeric(Z$OVERCOSTED)
Z$UNDERCOSTED = Z$DELTA < 0
Z$UNDERCOSTED = as.numeric(Z$UNDERCOSTED)



####### COUNTING OVER AND UNDERCOSTING #########
#ABC
#45,000   Used for the Table in this section

tapply(Z$OVERCOSTED,  list(Z$CP), sum)
tapply(Z$UNDERCOSTED, list(Z$CP), sum)



zplot1 <- ggplot(Z, aes(x=Z$CP, y=Z$OVERCOSTED)) +
  theme_minimal() + 
  #coord_cartesian(ylim=c(lower.limit, upper.limit)) + 
  labs(x = "Product [#]", y ="Percentage Error [%]") + 
  ggtitle("")+
  theme(text = element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5)) +
 
zplot1




tapply(Z2$OVERCOSTED,  list(Z2$OH_SHARE, Z2$CP), sum)
tapply(Z2$UNDERCOSTED, list(Z2$OH_SHARE, Z2$CP), sum)

ABC_pc = tapply(Z2$OVERCOSTED,  list(Z2$OH_SHARE, Z2$CP), sum)
ABC_pc =tapply(Z2$UNDERCOSTED, list(Z2$OH_SHARE, Z2$CP), sum)


#### ######## DENSITY PLOTS FOR VISUALIZING THE REAMING VARIANCE  ######### 
#install.packages("RColorBrewer")
#install.packages("ggplot2")
library(RColorBrewer)
library(ggplot2)
library(dplyr)

##### FUNCTIONS 

# function for computing mean, DS, max and min values
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x)*2, mean(x), mean(x) + sd(x)*2, max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

#### PREPARING DATASET ####
DATAp <- read.csv("output/ProductCost_2020-02-18-1244.csv")
DATAa <- DATAp
DATAa$PRODUCT <- as.factor(DATAp$PRODUCT)
DATAa$PE <- DATAp$PE * 100

#### PLOTTING PORTFOLIO  ####

zplot1 <- ggplot(DATAa, aes(x=DATAa$PRODUCT, y=DATAa$PE)) +
  stat_summary(fun.data = min.mean.sd.max, geom = "boxplot", fill='lightgrey') + 
  geom_jitter(position=position_jitter(width=.2), size=3, alpha = 0) +
  ylim(-100,110) +
  theme_minimal() + 
  #coord_cartesian(ylim=c(lower.limit, upper.limit)) + 
  labs(x = "Product [#]", y ="Percentage Error (PE) [%]") + 
  ggtitle("")+
  theme(text = element_text(size=18)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept=-5,  color="lightgrey") + 
  geom_hline(yintercept=5,  color="lightgrey" )  
zplot1



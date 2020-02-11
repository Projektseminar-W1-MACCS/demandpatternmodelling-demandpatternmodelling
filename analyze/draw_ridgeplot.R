#install.packages("RColorBrewer")
#install.packages("ggplot2")
#install.packages("ggridges")
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(ggridges)


###### Ridge 1 GREY ####
DATAa <- read.csv("output/ProductCost_2020-02-11-1636.csv")
DATAa = (subset(DATAa, DATAa$PRODUCT<=25))
DATAa$PRODUCT <- as.factor(DATAa$PRODUCT)
DATAa$PE <- DATAa$PE * 100

ridgeplot1=ggplot(DATAa, aes(x = DATAa$PE, y = DATAa$PRODUCT))+
  #geom_density_ridges(scale=5, rel_min_height=0.01, alpha=0.5) +
  xlim(-100,100) +

  stat_density_ridges(quantile_lines = TRUE, quantiles = 2,rel_min_height=0.01,scale=5, alpha=1)+
  #coord_cartesian(ylim=c(lower.limit, upper.limit)) + 
  labs(x = "Percentage Error (PE) [%]", y ="Product [#]") + 
  ggtitle("")+
  theme(text = element_text(size=18)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_ridges( center_axis_labels = TRUE)

DATAb <- read.csv("output/ProductCost_2020-02-11-1632.csv")
DATAb = (subset(DATAb, DATAb$PRODUCT>25))
DATAb$PRODUCT <- as.factor(DATAb$PRODUCT)
DATAb$PE <- DATAb$PE * 100

ridgeplot2=ggplot(DATAb, aes(x = DATAb$PE, y = DATAb$PRODUCT))+
  #geom_density_ridges(scale=5, rel_min_height=0.01, alpha=0.5) +
  xlim(-100,100) +
  
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2,rel_min_height=0.01,scale=5, alpha=1)+
  #coord_cartesian(ylim=c(lower.limit, upper.limit)) + 
  labs(x = "Percentage Error (PE) [%]", y ="Product [#]") + 
  ggtitle("")+
  theme(text = element_text(size=18)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_ridges( center_axis_labels = TRUE)
#ridgeplot2

multiplot(ridgeplot1,ridgeplot2,cols=2)


###### Ridge 2 PROBABILITIES #######

DATAa <- read.csv("output/ProductCost_2020-02-11-1714.csv")

DATAa = (subset(DATAa, DATAa$PRODUCT<=25))
DATAa$PRODUCT <- as.factor(DATAa$PRODUCT)
DATAa$PE <- DATAa$PE * 100

ridgeplot1 <- ggplot(DATAa, aes(x = DATAa$PE, y = DATAa$PRODUCT, fill=factor(stat(quantile))))+
  xlim(-50,50) +
  #stat_density_ridges(quantile_lines = TRUE, quantiles = 2,rel_min_height=0.01,scale=5, alpha=1)+
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantile_lines = TRUE, quantiles = c(0.1587,0.5,0.8413),rel_min_height=0.01,scale=5,) +
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0","#A0A0A0A0", "#FF0000A0"),
    labels = c("(0, 0.1587]","(0.1587, Mean]", "(Mean, 0.8413]", "(0.8413, 1]")
  )+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = -5, linetype="dashed")+
  labs(x = "Percentage Error (PE) [%]", y ="") + 
  ggtitle("")+
  theme(text = element_text(size=18)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_ridges( center_axis_labels = TRUE)+
  theme(legend.position = "none")
ridgeplot1

DATAb <- read.csv("output/ProductCost_2020-02-11-1714.csv")

DATAb = (subset(DATAb, DATAb$PRODUCT>25))
DATAb$PRODUCT <- as.factor(DATAb$PRODUCT)
DATAb$PE <- DATAb$PE * 100

ridgeplot2 <- ggplot(DATAb, aes(x = DATAb$PE, y = DATAb$PRODUCT, fill=factor(stat(quantile))))+
  xlim(-50,50) +
  #stat_density_ridges(quantile_lines = TRUE, quantiles = 2,rel_min_height=0.01,scale=5, alpha=1)+
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantile_lines = TRUE, quantiles = c(0.1587,0.5,0.8413),rel_min_height=0.01,scale=5,) +
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0","#A0A0A0A0", "#FF0000A0"),
    labels = c("(0, 0.1587]","(0.1587, Mean]", "(Mean, 0.8413]", "(0.8413, 1]")
  )+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = -5, linetype="dashed")+
  labs(x = "Percentage Error (PE) [%]", y ="") + 
  ggtitle("")+
  theme(text = element_text(size=18)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_ridges( center_axis_labels = TRUE)
  ridgeplot2  


multiplot(ridgeplot1,ridgeplot2,cols=2)




###### Ridge 3 GRADIENT ######

DATAa <- read.csv("output/ProductCost_2020-02-06-1340.csv")
DATAa = (subset(DATAa, DATAa$PRODUCT<=25))
DATAa$PRODUCT <- as.factor(DATAa$PRODUCT)
DATAa$PE <- DATAa$PE * 100

ridgeplot1 <- ggplot(DATAa, aes(x = DATAa$PE, y = DATAa$PRODUCT, fill=stat(x)))+
  geom_vline(xintercept = 5)+
  geom_vline(xintercept = -5)+
  #stat_density_ridges(quantile_lines = TRUE, quantiles = 2,rel_min_height=0.01,scale=5, alpha=1)+
  geom_density_ridges_gradient(rel_min_height=0.01,scale=5) +
  scale_fill_viridis_c(name="Error [%]",option="C")+
  xlim(-70,70) +
  ggtitle("")+
  theme(text = element_text(size=18)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("")+
  theme(text = element_text(size=18)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_ridges( center_axis_labels = TRUE)
#ridgeplot1  


DATAb <- read.csv("output/ProductCost_2020-02-06-1340.csv")
DATAb = (subset(DATAb, DATAb$PRODUCT>25))
DATAb$PRODUCT <- as.factor(DATAb$PRODUCT)
DATAb$PE <- DATAb$PE * 100

ridgeplot2 <- ggplot(DATAb, aes(x = DATAb$PE, y = DATAb$PRODUCT, fill=stat(x)))+
  geom_vline(xintercept = 5)+
  geom_vline(xintercept = -5)+
  #stat_density_ridges(quantile_lines = TRUE, quantiles = 2,rel_min_height=0.01,scale=5, alpha=1)+
  geom_density_ridges_gradient(rel_min_height=0.01,scale=5) +
  scale_fill_viridis_c(name="Error [%]",option="C")+
  xlim(-70,70) +
  ggtitle("")+
  theme(text = element_text(size=18)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("")+
  theme(text = element_text(size=18)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_ridges( center_axis_labels = TRUE)
#ridgeplot2  
#####

multiplot(ridgeplot1,ridgeplot2,cols=2)

#####


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}   ##########

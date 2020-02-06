library(ggplot2)
library(reshape)

# function for computing mean, DS, max and min values
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x)*2, mean(x), mean(x) + sd(x)*2, max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}





#### INTI ####
# Comment for selecting 

Z = RegX_grouped  # 10 Activity cost pools


#RegX$FACTORINTRA_HET  <- cut(RegX$INTRA_HET , breaks = seq(-1, 1, by = 0.2))
#RegX$FACTORINTER_HET  <- cut(RegX$INTER_HET , breaks = seq(-1, 1, by = 0.2))




#### PREPARING DATASET ####

Z$FACTORSHARE <-cut(Z$`RegX$SHARE_UNIT`,quantile(Z$`RegX$SHARE_UNIT`,(0:4)/4))
Z = na.omit(Z)

##Z.m <- melt(Z,id.vars = 'FACTORSHARE' , measure.vars=c('mn','sd'))
##Z.m <- na.omit(Z.m)
#rm(Z.m)

#### PLOTTING PORTFOLIO  ####
Z$mnabs = abs(Z$mn)

zplot1 <- ggplot() +
  geom_boxplot(aes(x=Z$FACTORSHARE, y=Z$mnabs, fill=Z$FACTORSHARE), outlier.shape = NA) +
  #stat_summary(fun.y = mean, geom = "line",size=1.5) + 
  #stat_summary(fun.y = mean, geom = "point",size=4)+
  # stat_summary(fun.data = min.mean.sd.max, geom = "boxplot", alpha=0.1, fill='black') + 
  # geom_jitter(position=position_jitter(width=.2), size=3, alpha = 0) +
  ylim(0,100) +
  theme_minimal() + 
  #coord_cartesian(ylim=c(lower.limit, upper.limit)) + 
  labs(x = "COST_SHARE [%]", y ="Bias [%]") + 
  ggtitle("")+
  theme(text = element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept=0,  linetype="dashed")   +
  scale_fill_brewer(palette="Greys")   + 
  guides(fill=FALSE)
  #geom_hline(yintercept=5,  linetype="dashed" )  
zplot1


zplot2 <- ggplot() +
  geom_boxplot(aes(x=Z$FACTORSHARE, y=Z$sd, fill=Z$FACTORSHARE), outlier.shape = NA)+
  # stat_summary(fun.data = min.mean.sd.max, geom = "boxplot", alpha=0.1, fill='black') + 
  # geom_jitter(position=position_jitter(width=.2), size=3, alpha = 0) +
  ylim(0,25) +
  theme_minimal() + 
  #coord_cartesian(ylim=c(lower.limit, upper.limit)) + 
  labs(x = "COST_SHARE [%]", y ="Imprecision [%]") + 
  ggtitle("")+
  theme(text = element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=FALSE)+
  geom_hline(yintercept=0,  linetype="dashed")   +
  scale_fill_brewer(palette="Greys")   
  #geom_hline(yintercept=-5,  linetype="dashed") + 
  #geom_hline(yintercept=5,  linetype="dashed" )  
zplot2

multiplot(zplot1,zplot2,cols=2)


######## multiplot #######
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
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


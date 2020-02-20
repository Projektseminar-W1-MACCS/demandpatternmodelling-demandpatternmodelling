###### FULL DISAGGREGATION ######
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(reshape)
DATAa <-  read.csv("output/Imprecision/ProductCost_2020-02-19-1407.csv")
#summary(DATAp)

### ESTIMATE IMPRECISION AND BIAS  #############################################################################################################

DATAa$PE <- DATAa$PE*100

DATAa = subset(DATAa, DATAa$PE > -100)
DATAa = subset(DATAa, DATAa$PE < 250)



pre = group_by(DATAa, DATAa$PCb, DATAa$DENS, DATAa$Q_VAR, DATAa$RCC_VAR, DATAa$CP, DATAa$NUMB_Error)

DATAa_grouped =  (summarize(pre,n=n(), md=median(PE), mn=mean(PE),sd=sd(PE)*2)) ####
rm(pre)


LookUP = data.frame(DATAa_grouped$`DATAa$PCb`, DATAa_grouped$`DATAa$CP`, DATAa_grouped$`DATAa$NUMB_Error`, DATAa_grouped$mn, DATAa_grouped$sd) # Building data frame
colnames(LookUP) <- c("PCB", "cp", "ERROR" ,"BIAS","IMPRECISION") # rename columns. 
Base = data.frame(cbind(DATAa$PRODUCT, DATAa$PCb, DATAa$PCh, DATAa$NUMB_Error, 
                        DATAa$RCC_VAR, DATAa$Q_VAR, DATAa$DENS, DATAa$CP,
                        DATAa$PE))
colnames(Base) <- c("P_NUMB","PC_B", "PC_H", "ERROR",
                    "RCC_VAR", "Q_VAR","DENS","CP", 
                    "PE") # rename columns
#Left JOIN : keep ll rows in X even if there is no match  // all.x true = join y into x ; 
DATAap = merge(Base,LookUP , by.x = c("PC_B","CP","ERROR"), by.y =c("PCB","cp","ERROR"), all.x = TRUE)

rm(LookUP)
rm(Base)
rm(DATAa_grouped)







### FULL DISAGGREGATION PICTURE  ######

DATAap$ABSBIAS = abs(DATAap$BIAS)
DATAap$APE = abs(DATAap$PE)
DATAap.m <- melt(DATAap,id.vars = 'CP', measure.vars=c('APE','ABSBIAS','IMPRECISION'))
DATAap.m <- na.omit(DATAap.m)



plot2 <- ggplot(DATAap.m, aes(x=DATAap.m$CP, y=DATAap.m$value, group = factor(DATAap.m$variable, labels = c("APE","|BIAS|","IMPRECISION")))) +
  stat_summary(fun.y = mean, geom = "line",size=1.5) + 
  stat_summary( fun.y = mean, geom = "point",size=3)+
  (aes(linetype=factor(DATAap.m$variable, labels = c("APE","|BIAS|","IMPRECISION")) ,color=factor(DATAap.m$variable, labels = c("APE","|BIAS|","IMPRECISION")))) +
  theme_minimal() + 
  scale_colour_manual(values = c("red", "blue", "lightgreen")) + 
  #coord_cartesian(ylim=c(lower.limit, upper.limit)) + 
  labs(x = "Cost pools [#]", y ="Absolute Percentage Error [%]") + 
  theme(text = element_text(size=24))  +
  theme(legend.title = element_blank()) + 
  scale_y_continuous(breaks=seq(0,80,10))
 # theme(plot.title = element_text(hjust = 0.5))  
  #geom_hline(yintercept=-5,  linetype="dashed") + 
  #geom_hline(yintercept=5,  linetype="dashed" )  
plot2


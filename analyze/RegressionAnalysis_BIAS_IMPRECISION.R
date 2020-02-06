#install.packages('Metrics')
library(lsr)
library(dplyr)
library(QuantPsyc)
library(QuantPsyc)
library(car)
library(heplots)
library(Metrics)

ABC_11$CPH = 1 # RANDOM
ABC_21$CPH = 2 # CORREL SIZE
ABC_31$CPH = 3 # CORREL RANDOM
ABC_51$CPH = 4 # SIZE_RANDOM

RegX = rbind(ABC_11,ABC_21,ABC_31,ABC_51)

summary(RegX)


#### DATA WRANGLING AND SETTING ####  #####
RegX$DELTA <- RegX$DELTA * 100
RegX$SHARE_UNIT <- ((RegX$PC_B_UNIT)/RegX$PC_B)*100
RegX$DELTAABS <- abs(RegX$DELTA)

####GROUPING THE TOTAL DATASET (PIVOT) AND PUT THE ESTIMATE IN IT  ####  ##########

pre = group_by(RegX, RegX$VARSIZE_b_p_T, RegX$DENS, RegX$COR, RegX$Q_VAR, RegX$RC_VAR,
              RegX$INTER_HET, RegX$INTRA_HET, RegX$TQ,
              RegX$SHARE_UNIT, RegX$CPH,
              RegX$CP, RegX$ErrorLEVEL, RegX$ErrorNUMB)
              #RegX$VARSIZE_b_p_u, RegX$VARSIZE_b_p_T, RegX$VARSIZE_b_T, RegX$VARSIZE_b, ) # Grouping like Pivot tables. 
RegX_grouped =  (summarize(pre,n=n(), md=median(DELTA), mn=mean(DELTA),sd=sd(DELTA), var=var(DELTA),mse=mean(((PC_B-PC_H)/PC_B*100)^2)))#What is abot --- 
# var=var(DELTA),mse=mean(((PC_B-PC_H)/PC_B)^2
rm(pre)
gc()


#### REGRESSION APE #####

RegX$CPH_FACTOR = as.factor(RegX$CPH)

fit <- lm(RegX$DELTAABS ~ (RegX$VARSIZE_b_p_T + 
                        RegX$TQ+
                       
                        RegX$CP +
                        RegX$DENS +
                        RegX$Q_VAR +
                        RegX$COR + 
                        RegX$RC_VAR +
                        RegX$ErrorLEVEL       ))


summary(fit)
lm.beta <- lm.beta(fit)
lm.beta <- round(lm.beta, digits=2)
print(lm.beta)
vif(fit)

aov_all <- aov(RegX$DELTAABS ~ (RegX$VARSIZE_b_p_T +  
                                                  RegX$TQ + 
                                                  RegX$CP +
                                                  RegX$DENS +
                                                  RegX$Q_VAR +
                                                  RegX$COR + 
                                                  RegX$RC_VAR +       
                                                  RegX$ErrorLEVEL))
drop1(aov_all,~.,test="F") # type III SS and F Test
summary(aov_all)

eta = etasq(aov_all)
round(eta, digits=6)




#### REGRESSION WITH RegX_grouped #####
RegX_grouped$CPH_FACTOR = as.factor(RegX_grouped$`RegX$CPH`)
RegX_grouped$ABSBIAS = abs(RegX_grouped$mn)
RegX_grouped$IMPRECISION = RegX_grouped$sd





fit <- lm(RegX_grouped$IMPRECISION ~ (RegX_grouped$`RegX$VARSIZE_b_p_T` + RegX_grouped$`RegX$ErrorLEVEL` + 
                             RegX_grouped$`RegX$TQ`  + RegX_grouped$`RegX$CP` +
                             RegX_grouped$`RegX$DENS` + RegX_grouped$`RegX$Q_VAR` +  + RegX_grouped$`RegX$COR` + 
                             RegX_grouped$`RegX$RC_VAR`))



#+ RegX_grouped$CPH_FACTOR

summary(fit)
lm.beta <- lm.beta(fit)
lm.beta <- round(lm.beta, digits=2)
print(lm.beta)
vif(fit)


aov_all <- aov(RegX_grouped$IMPRECISION ~(RegX_grouped$`RegX$VARSIZE_b_p_T` + RegX_grouped$`RegX$ErrorLEVEL` + RegX_grouped$`RegX$TQ`  + RegX_grouped$`RegX$CP` + RegX_grouped$`RegX$ErrorLEVEL`+
                                 RegX_grouped$`RegX$DENS` + RegX_grouped$`RegX$Q_VAR` +  + RegX_grouped$`RegX$COR` + 
                                 RegX_grouped$`RegX$RC_VAR` ))
drop1(aov_all,~.,test="F") # type III SS and F Test
summary(aov_all)

eta = etasq(aov_all)
eta <- round(eta, digits=3)
print(eta)







#### CORRELATION ####
t=cor(RegX, method = c("pearson"))
t= round(t, digits=2) 












#### CLEARING ####
rm(RegX)
rm(X)
rm(fit)
rm(Z_grouped)
rm(Z)
rm(Zdensity)
rm(EtaSquare)
rm(TEST)
rm(Z1)
rm(zplot1)
rm(t1)
rm(t2)
rm(RegXt)
rm(LookUP)
rm(Base)
rm(RegX_grouped)
rm(fit)
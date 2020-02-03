#############################################################
# Analysis of the replication outpout and comparison with the original Model v1.0.0
#############################################################

####setup####
#options(java.parameters = "- Xmx2048m")
#install.packages("reshape2") #and 'xlsx',robustHD,lm.beta
library("xlsx")
library('ggplot2')
library('robustHD')
library('lm.beta')
library('reshape2')


####-------------Plotting the current DATA------------####


check = aggregate(DATA,list(DATA$CP),mean)
plot(check$MAPE,type ='p')
#print(check$MAPE)




sweep(FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp,1,)

rowMeans(FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp) == FIRM$COSTING_SYSTEM$ACT_CONS_PAT

FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp %*% FIRM$COSTING_SYSTEM$RCC == FIRM$COSTING_SYSTEM$ACT_CONS_PAT * FIRM$COSTING_SYSTEM$TC

sum(rowSums(FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp %*% FIRM$COSTING_SYSTEM$RCC)) == FIRM$COSTING_SYSTEM$ACT_CONS_PAT *FIRM$COSTING_SYSTEM$TC

FIRM$COSTING_SYSTEM$RCC == FIRM$COSTING_SYSTEM$ACP

rowSums(FIRM$COSTING_SYSTEM$ACT_CONS_PAT %*% FIRM$COSTING_SYSTEM$RCC) == FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp
colSums(FIRM$COSTING_SYSTEM$PCB) == rowSums(FIRM$COSTING_SYSTEM$ACT_CONS_PAT %*% FIRM$COSTING_SYSTEM$RCC)

rowMeans(FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp) == rowMeans(FIRM$COSTING_SYSTEM$ACT_CONS_PAT)

check = matrix(c(2,2,3,3,4,4,5,5),ncol = 4)

rowMeans(check)































#####Choice of heuristic to analyse####


heuristic = 1

####loading the heursitic output####
file_rep = paste0("C:/Users/cms9023/Documents/CostSystemDesignSim/output/Third Replication/P==",heuristic,"/REPLICATION ",heuristic,".xlsx")

replication_output = read.xlsx(file_rep,2)

##loading the outpout directly from R

#replication_output = DATA

##loading the original model output
file_anand = paste0("C:/Users/cms9023/Documents/CostSystemDesignSim/output/Third Replication/P==",heuristic,"/ANAND ",heuristic,".xlsx")

anand_output = read.xlsx(file_anand, 2)

# file_anand = paste0("C:/Users/cms9023/Documents/CostSystemDesignSim/output/Third Replication/P==",heuristic,"/ANAND ",heuristic," 3",".csv")
# 
# anand_output = read.csv(file_anand, sep =";")

#anand_output[] <- lapply(anand_output, function(x) as.numeric(as.character(x)))

# file_anand_gdrcc = paste0("C:/Users/cms9023/Documents/CostSystemDesignSim/output/Third Replication/P==",heuristic,"/ANAND ",heuristic," gdRCC"," 3",".csv")
# 
# anand_output_gdrcc = read.csv(file_anand_gdrcc,sep =";")

file_anand_gdrcc = paste0("C:/Users/cms9023/Documents/CostSystemDesignSim/output/Third Replication/P==",heuristic,"/ANAND ",heuristic," gdRCC",".xlsx")

anand_output_gdrcc = read.xlsx(file_anand_gdrcc,1)
#anand_output_gdrcc[] <- lapply(anand_output_gdrcc, function(x) as.numeric(as.character(x)))


##merging the two anand files 
anand_output = merge(anand_output,anand_output_gdrcc, by.x = 'FirmID', by.y ='FirmID')
##sorting to bring it in same order as replication output
anand_output = anand_output[order(anand_output$ACP),]


#####Plotting measurement error with standard deviation#####


boxplot_data_1 = data.frame("REPLICATION",replication_output$CP, replication_output$MAPE, replication_output$nn)
colnames(boxplot_data_1) = c('Model','CP','MAPE','run')

boxplot_data_2 = data.frame("ANAND",anand_output$ACP, anand_output$MPE, anand_output$FirmID)
colnames(boxplot_data_2) = c('Model','CP','MAPE','run')

boxplot_data = rbind(boxplot_data_1,boxplot_data_2)     

###boxplot
boxplot_data$CP = factor(boxplot_data$CP)

ggplot(boxplot_data, aes(x= CP,y=MAPE, fill=Model)) +
  geom_boxplot()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+
  theme_bw()+
  ggtitle('SIZE CORREL MISC')+                              #Adaption required each time heuristic is changes
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  ylim(0,1)





####descriptive statistics and Check #####

plot(hist(replication_output$CHECK_RCC02, breaks = 7, xlim = c(0.05,0.5)))
plot(hist(anand_output$CHECK_RCC02, breaks = 7,xlim = c(0.05,0.5)), add = TRUE)


ks.test(replication_output$CHECK_RCC02,'rnorm')

plot(sort(anand_output$RCC_0/1000000), type = 'l')
lines(sort(unique(replication_output$CHECK_RCC02)), type = 'l')

mean(replication_output$CHECK_RCC02)
mean(replication_output$CHECK_RCC10)
mean(replication_output$CHECK_RCC20)


anand_output$CHECK_RCC02 = as.numeric(anand_output$CHECK_RCC02)
mean(anand_output$CHECK_RCC02)
mean(anand_output$CHECK_RCC10)
mean(anand_output$CHECK_RCC20)

#####Regression Analysis####

print('CORRELATION BETWEEN REPLICATION AND ANAND MAPE CURVE OVER COST POOLS')
cor(anand_output$MPE,replication_output$MAPE)

scatter.smooth(replication_output$MAPE, replication_output$CHECK_RCC02)   #scatter plot to see if a linear regression would make sense --> makes sense

scatter.smooth(anand_output$MPE, anand_output$CHECK_RCC02)

###Standardized regression analysis###

linearReg_repl = lm(replication_output$MAPE ~ replication_output$CP 
               + replication_output$DISP2 + 
                 replication_output$DENS + 
                 replication_output$CHECK_RCC02)


linearReg_repl_beta = lm.beta(linearReg_repl)   #standardizes the regression coefficients (betas)

###Anand original model as comparison###

linearReg_anand = lm(anand_output$MPE ~ anand_output$ACP + anand_output$g + anand_output$d + anand_output$CHECK_RCC02)

linearReg_anand_beta = lm.beta(linearReg_anand)

print('replication')
summary(linearReg_repl_beta)
print('anand')
summary(linearReg_anand_beta)

coef(linearReg_repl_beta)
coef(linearReg_anand_beta)








#### Model breaking with Q-var variation####











##loading a saved file
file_q_var = "C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/Q_VAR variation/Zwischenpräsentation/CSD_2019-11-27-14191.csv"


rep_q_var_output = read.csv(file_q_var)




###reshaping###
rep_q_var_output = data.frame(rep_q_var_output$CP, rep_q_var_output$MAPE, rep_q_var_output$Q_VAR)
colnames(rep_q_var_output) = c('CP','MAPE','Q_VAR')
anand_q_var_output = data.frame(anand_output$ACP, anand_output$MPE, Q_VAR = ('ANAND'))
colnames(anand_q_var_output) = c('CP','MAPE', 'Q_VAR')

q_var_output = rbind(rep_q_var_output,anand_q_var_output)


###aggregated datae - mean over CP and Q_VAR ###
q_var_output_agg = aggregate(.~CP + Q_VAR, data = q_var_output, FUN = mean)


ggplot(q_var_output_agg, aes(x = CP, y = MAPE, color = Q_VAR, group = Q_VAR))+geom_line(size = 1)+
  ggtitle('SIZE CORREL MISC Q_VAR VARIATION')+theme_bw()+                              #Adaption required each time heuristic is changes
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'left')+
  ylim(0,1)+geom_line(data= q_var_output_agg[q_var_output_agg$Q_VAR == 'ANAND',], color = 'black', size = 1)



###detailed data -plot over all Q_VARs###

q_var = melt(q_var_output, id = c('CP','Q_VAR'))
q_var$CP = as.factor(q_var$CP)
q_var_output_agg$CP = as.factor(q_var_output_agg$CP)
ggplot(q_var, aes(x = CP, y = value, fill = Q_VAR))+geom_boxplot()+
  ggtitle('SIZE CORREL MISC Q_VAR VARIATION')+theme_bw()+                              #Adaption required each time heuristic is changes
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'left')+
  ylim(0,1)+geom_line(data = q_var_output_agg, aes(x =CP, y=MAPE, group = Q_VAR))




###detailed data - plot over Q_VAR = 0.4 and ANAND
q_var_direct = subset(q_var, Q_VAR == 2 | Q_VAR == 'ANAND')

ggplot(q_var_direct, aes(x = CP, y = value, fill = Q_VAR))+geom_boxplot()+
  ggtitle('SIZE CORREL MISC Q_VAR VARIATION')+theme_bw()+                              #Adaption required each time heuristic is changes
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'left')+
  ylim(0,1)




####loading the output directly####

DATA = DATA

DATA_agg = aggregate(.~CP, data = DATA, FUN = mean)

replication_output_agg = aggregate(.~CP, data = replication_output, FUN = mean)

DATA_agg_comb = data.frame(DATA_agg$CP, DATA_agg$MAPE, replication_output_agg$MAPE)
colnames(DATA_agg_comb) = c('CP','Breaking','Replication')

DATA_agg_comb = melt(DATA_agg_comb, id.vars = 'CP')

ggplot(DATA_agg_comb, aes(x = CP, y = value, linetype = variable))+geom_line(size = 1)+
  ggtitle('SIZE CORREL MISC FIXING COMPUTATIONAL ERRORS')+theme_bw()+                              #Adaption required each time heuristic is changes
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'left')+
  ylim(0,1)
       


#####SIM_NUMB_VARIATION####

file_path = 'C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/SIM_NUMB Variation/SIM_NUMB Variation 200 vs 1000 P==1 20191202.xlsx'

sim_numb_output = read.xlsx(file_path,3)

plot_data = melt(sim_numb_output, id.vars = 'CP')
plot_data = plot_data[order(plot_data$variable),]

ggplot(plot_data, aes(x = CP, y=value, linetype = variable, color = variable))+geom_line(size = 1.5)+xlim(c(0,20))+ylim(c(0.2,0.75))+ theme_bw()+
  ggtitle('EFFECT OF SIM_NUMB ON ACCURACY')+                              #Adaption required each time heuristic is changes
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+geom_point(aes(shape = variable), size = 3)


summary(sim_numb_output)






sim_numb_100 = DATA
sim_numb_100 = aggregate(.~CP,sim_numb_100, mean)

sim_numb_200 = DATA
sim_numb_200 = aggregate(.~CP,sim_numb_200, mean)

sim_numb_500 = DATA
sim_numb_500 = aggregate(.~CP,sim_numb_500, mean)

sim_numb_1000 = DATA
sim_numb_1000 = aggregate(.~CP,sim_numb_1000, mean)




plot_sim_numb = data.frame(sim_numb_100$CP,sim_numb_100$MAPE,sim_numb_200$MAPE,sim_numb_500$MAPE,sim_numb_1000$MAPE)
plot_sim_numb = melt(plot_sim_numb, id.vars = 'sim_numb_100.CP')

ggplot(plot_sim_numb, aes(x = sim_numb_100.CP, y= value, linetype = variable, color = variable))+geom_line(size = 1.5)










####-------------------------------------Over-and Undercosting---------------------------------------------------####

###----------------------Aggregated DATA--------------------- ####

DATA$RCC_VAR = as.numeric(DATA$RCC_VAR)
DATA$CP = as.factor(DATA$CP)
DATA$DENS = as.factor(DATA$DENS)
DATA_agg = aggregate(.~CP+NUMB_ME+ME_AD, data = DATA, FUN = mean)

ggplot(DATA_agg, aes(x = RCC_VAR, y = OC, color = CP))+geom_line()

mean(DATA$OC[DATA$NUMB_ME == 0.8])

###---------------------DENS VARIATION----------------------- ####
DATAx = DATA

DATAx$DENS = as.factor(DATAx$DENS)
DATAx$CP = as.factor(DATAx$CP)

boxplot_data_UC = data.frame(DATAx$DENS,DATAx$CP,DATAx$UC5, DATA$NUMB_ME)

colnames(boxplot_data_UC) = c('DENS','CP','UC', 'Error')

#boxplot = melt(boxplot_data, id.vars = 'DENS', value.name = 'Share_of_UC')

#boxplot = boxplot[order(boxplot$DENS),]


#ggplot(boxplot_data_UC, aes(x = DENS, y = UC, fill = CP))+geom_boxplot()+facet_wrap('Error')

DATA$DENS = as.factor(DATA$DENS)
DATA$CP = as.factor(DATA$CP)

boxplot_data_OC = data.frame(DATA$DENS,DATA$CP,DATA$OC5, DATA$NUMB_ME)

colnames(boxplot_data_OC) = c('DENS','CP','OC','Error')


#ggplot(boxplot_data_OC, aes(x = DENS, y = OC, fill = CP))+geom_boxplot()

ggplot(boxplot_data_OC, aes(x = DENS, y = OC, color = CP))+geom_boxplot()+scale_color_grey()+geom_boxplot(data =boxplot_data_UC, aes(x = DENS, y = UC, fill = CP))+theme_classic()+facet_wrap('Error')



###---------------------Q_VAR VARIATION----------------------- ####

DATAx = DATA

#DATA_agg = aggregate(.~CP, data = DATA, FUN = mean)

DATAx$Q_VAR = as.factor(DATAx$Q_VAR)
DATAx$CP = as.factor(DATAx$CP)

boxplot_data_UC = data.frame(DATAx$Q_VAR,DATAx$CP,DATAx$UC, DATAx$NUMB_ME)

colnames(boxplot_data_UC) = c('Q_VAR','CP','UC', 'NUMB_ERROR')

#boxplot = melt(boxplot_data, id.vars = 'Q_VAR', value.name = 'OC_UC')

#boxplot = boxplot[order(boxplot$Q_VAR),]


#ggplot(boxplot, aes(x = Q_VAR, y = OC_UC, fill = variable))+geom_boxplot()


DATAx$Q_VAR = as.factor(DATAx$Q_VAR)

boxplot_data_OC = data.frame(DATAx$Q_VAR,DATAx$CP,DATAx$OC,DATAx$NUMB_ME)

colnames(boxplot_data_OC) = c('Q_VAR','CP','OC','NUMB_ERROR')


ggplot(boxplot_data_OC, aes(x = Q_VAR, y = OC, color = CP))+geom_boxplot()+scale_color_grey()+geom_boxplot(data = boxplot_data_UC, aes(x = Q_VAR, y = UC, fill = CP))+theme_classic()+facet_wrap('NUMB_ERROR')


###---------------------RC_VAR VARIATION----------------------- ####
DATAx = DATA

DATAx$RCC_VAR = as.factor(DATAx$RCC_VAR)
DATAx$CP = as.factor(DATAx$CP)

boxplot_data_UC = data.frame(DATAx$RCC_VAR,DATAx$CP,DATAx$UC, DATA$ME_AD)

colnames(boxplot_data_UC) = c('RCC_VAR','CP','UC', 'Error')

#boxplot = melt(boxplot_data, id.vars = 'DENS', value.name = 'Share_of_UC')

#boxplot = boxplot[order(boxplot$DENS),]


ggplot(boxplot_data_UC, aes(x = RCC_VAR, y = UC, fill = CP))+geom_boxplot()+facet_wrap('Error')



boxplot_data_OC = data.frame(DATAx$RCC_VAR,DATAx$CP,DATAx$OC, DATAx$ME_AD)

colnames(boxplot_data_OC) = c('RCC_VAR','CP','OC','Error')


ggplot(boxplot_data_OC, aes(x = RCC_VAR, y = OC, fill = CP))+geom_boxplot()

ggplot(boxplot_data_OC, aes(x = RCC_VAR, y = OC, color = CP))+geom_boxplot()+scale_color_grey()+geom_boxplot(data =boxplot_data_UC, aes(x = RCC_VAR, y = UC, fill = CP))+theme_classic()+facet_wrap('Error')


###-----------------NUMB_ERROR Variation------------------ ####

DATAx = DATA

DATAx$NUMB_ME = as.factor(DATAx$NUMB_ME)
DATAx$CP = as.factor(DATAx$CP)

boxplot_data_UC = data.frame(DATAx$CP,DATAx$UC5, DATAx$NUMB_ME, DATAx$ME_AD)

colnames(boxplot_data_UC) = c('CP','UC', 'NUMB_ERROR','Error')

#boxplot = melt(boxplot_data, id.vars = 'DENS', value.name = 'Share_of_UC')

#boxplot = boxplot[order(boxplot$DENS),]


#ggplot(boxplot_data_UC, aes(x = RCC_VAR, y = UC, fill = CP))+geom_boxplot()+facet_wrap('Error')



boxplot_data_OC = data.frame(DATAx$CP,DATAx$OC5, DATAx$NUMB_ME, DATAx$ME_AD)

colnames(boxplot_data_OC) = c('CP','OC','NUMB_ERROR','Error')


#ggplot(boxplot_data_OC, aes(x = RCC_VAR, y = OC, fill = CP))+geom_boxplot()

ggplot(boxplot_data_OC, aes(x = NUMB_ERROR, y = OC, color = CP))+geom_boxplot()+scale_color_grey()+geom_boxplot(data =boxplot_data_UC, aes(x = NUMB_ERROR, y = UC, fill = CP))+theme_classic()+facet_wrap('Error')

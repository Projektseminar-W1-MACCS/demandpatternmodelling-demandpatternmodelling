#############################################################
# Analysis of the replication outpout and comparison with the original Model v1.0.0
#############################################################

####setup####
#options(java.parameters = "- Xmx2048m")
install.packages("reshape2") #and 'xlsx',robustHD,lm.beta
library("xlsx")
library('ggplot2')
library('robustHD')
library('lm.beta')
library('reshape2')

#####Choice of heuristic to analyse####


heuristic = 1

####loading the heursitic output####
file_rep = paste0("C:/Users/cms9023/Documents/CostSystemDesignSim/output/Third Replication/P==",heuristic,"/REPLICATION ",heuristic,".xlsx")

replication_output = read.xlsx(file_rep,2)

##loading the original model output
file_anand = paste0("C:/Users/cms9023/Documents/CostSystemDesignSim/output/Third Replication/P==",heuristic,"/ANAND ",heuristic,".xlsx")

anand_output = read.xlsx(file_anand, 2)

#anand_output[] <- lapply(anand_output, function(x) as.numeric(as.character(x)))

file_anand_gdrcc = paste0("C:/Users/cms9023/Documents/CostSystemDesignSim/output/Third Replication/P==",heuristic,"/ANAND ",heuristic," gdRCC",".xlsx")

anand_output_gdrcc = read.xlsx(file_anand_gdrcc, 1)

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
  ggtitle('SIZE CORREL CUT-OFF')+                              #Adaption required each time heuristic is changes
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  ylim(0,1)


# ###descriptive statistics#####
# CP = unique(anand_output$ACP)
# 
# 
# 
# ###Anand Standard deviation
# anand_sd = vector(mode ='numeric')
# for(i in CP) {
#   
#   
#   anand_sd[i] = sd(anand_output$MPE[anand_output$ACP == i])
#   
# }
# 
# ###Anand Mean
# anand_mean = vector(mode ='numeric')
# for(i in CP) {
#   
#   
#   anand_mean[i] = mean(anand_output$MPE[anand_output$ACP == i])
#   
# }
# 
# ###Replication Standard deviation
# replication_sd = vector(mode ='numeric')
# for(i in CP) {
#   
#   
#   replication_sd[i] = sd(replication_output$MAPE[replication_output$CP == i])
#   
# }
# 
# ###Replication Mean
# replication_mean = vector(mode ='numeric')
# for(i in CP) {
#   
#   
#   replication_mean[i] = mean(replication_output$MAPE[replication_output$CP == i])
#   
# }
# 
# 
# descriptive_stats = data.frame(anand_sd,anand_mean,replication_sd,replication_mean)
#####Regression Analysis####

print('CORRELATION BETWEEN REPLICATION AND ANAND MAPE CURVE OVER COST POOLS')
cor(anand_output$MPE,replication_output$MAPE)

scatter.smooth(replication_output$MAPE, replication_output$CHECK_RCC02)   #scatter plot to see if a linear regression would make sense --> makes sense



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


##loading
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

ggplot(q_var, aes(x = CP, y = value, fill = Q_VAR))+geom_boxplot()+
  ggtitle('SIZE CORREL MISC Q_VAR VARIATION')+theme_bw()+                              #Adaption required each time heuristic is changes
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'left')+
  ylim(0,1)




###detailed data - plot over Q_VAR = 0.4 and ANAND
q_var_direct = subset(q_var, Q_VAR == 2 | Q_VAR == 'ANAND')

ggplot(q_var_direct, aes(x = CP, y = value, fill = Q_VAR))+geom_boxplot()+
  ggtitle('SIZE CORREL MISC Q_VAR VARIATION')+theme_bw()+                              #Adaption required each time heuristic is changes
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'left')+
  ylim(0,1)
       
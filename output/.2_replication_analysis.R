############################################################
# Analysis of the replication outpout and comparison with the original Model v1.0.0
#############################################################

####setup####
#nstall.packages("extrafont")
library('extrafont')
#install.packages("tibble") #and 'xlsx',robustHD,lm.beta
library("xlsx")
library('ggplot2')
library('robustHD')
library('lm.beta')
library('reshape2')
library('apaTables')
library('MBESS')


####_________________________________REPLICATION TEST____________________####------------####

##Loading the Data 

##1.  REPLICATION MODEL###

loading_from_data = 0
if(loading_from_data == 0){
  file_link_replication = "C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/Replication/Third Replication/CSD_2020-02-07-1017.csv"
  replication_data = read.csv(file_link_replication, sep = ",")
}else{replication_data = DATA}


##2. ANAND MODEL###
##2.1. ANAND MODEL ERROR OUTPUT

file_link_anand_1 = "C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/Replication/Third Replication/ANAND_Model_20200212_200_SIM_NUMB.csv"

anand_data_1 = read.csv(file_link_anand_1, sep = ';')

##2.2. ANAND MODEL DISP2, DENS, RCC02 Output

file_link_anand_2 = "C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/Replication/Third Replication/ANAND_Model_ResCon_20200212_200_SIM_NUMB.csv"

anand_data_2 = read.csv(file_link_anand_2, sep = ';')


anand_data = merge(anand_data_1,anand_data_2, by.x = 'FirmID', by.y = 'FirmID')



##2.3. REPLACING INTEGER WITH NAMES##
##2.3.1. REPLICATION DATA
replication_data$CPH = as.character(replication_data$CPH)
replication_data$CDH = as.character(replication_data$CDH)

replication_data$CPH[replication_data$CPH == 'base'] = 'BASE'
replication_data$CDH[replication_data$CDH == 'base'] = 'BASE'

replication_data$CPH[replication_data$CPH == 0] = 'SIZE_MISC'
replication_data$CPH[replication_data$CPH == 1] = 'SIZE_CORREL_MISC'
replication_data$CPH[replication_data$CPH == 2] = 'SIZE_RANDOM_MISC'
replication_data$CPH[replication_data$CPH == 3] = 'SIZE_CORREL_MISC_CC'

replication_data$CDH[replication_data$CDH == 0] = 'BIGPOOL'

replication_data$CPH = as.factor(replication_data$CPH)
replication_data$CDH = as.factor(replication_data$CDH)

##2.3.2. ANAND DATA
#anand_data$PACP = as.character(anand_data$PACP)
#anand_data$PDR = as.character(anand_data$PDR)

anand_data$PACP[anand_data$PACP == 0] = 'SIZE_MISC'
anand_data$PACP[anand_data$PACP == 1] = 'SIZE_CORREL_MISC'
anand_data$PACP[anand_data$PACP == 2] = 'SIZE_RANDOM_MISC'
anand_data$PACP[anand_data$PACP == 3] = 'SIZE_CORREL_MISC_CC'

anand_data$PDR[anand_data$PDR == 0] == 'BIGPOOL'

anand_data$PACP = as.factor(anand_data$PACP)
anand_data$PDR = as.factor(anand_data$PDR)



##3. PLOTTING THE WHOLE DATAFRAME##

replication_data = subset(replication_data, CPH != 'BASE' & CDH != 'BASE')

replication_data_agg = aggregate(.~CP+CPH+CDH, data = replication_data, FUN = mean)

# ggplot(replication_data_agg, aes(x = CP, y = MAPE, shape = CPH))+labs(color = "Heuristik Kombinationen",shape = 'Heuristik Kombinationen')+
#   ggtitle('Überblick über alle Heuristiken')+ theme_bw()+                             
#   theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom', text = element_text((size = 20)), legend.direction = 'vertical')+
#   scale_y_continuous(labels = scales::percent, limits = c(0,1))+
#   geom_point(aes(shape = CPH), size = 3)+geom_line(aes(color = CPH))
#   guides(fill = guide_legend(override.aes = list(shape =NA)),
#          shape = FALSE)

loadfonts(device="win")
ggplot(replication_data_agg, aes(x = CP, y = MAPE, color = CPH))+
  geom_line(size=1)+
  labs(color = "Heuristik Kombinationen", shape = 'Heuristik Kombinationen')+
  ggtitle('Überblick über alle Heuristiken')+ 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 20,family = 'Times New Roman'), legend.position = 'bottom', 
        legend.text = element_text(family = 'Times New Roman',size = 16), legend.direction = 'vertical', 
        axis.title.x = element_text(family = 'Times New Roman',size = 16),
        axis.title.y = element_text(family = 'Times New Roman',size= 16),
        text = element_text(family = 'Times New Roman'))+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  geom_point(aes(shape = CPH),size = 4)+
  scale_color_grey(start = 0, end = .8)




##4. RESHAPING THE DATAFRAME FOR PLOTTING AND ANALYSIS OF SINGLE HEURISTIC COMBINATIONS##

##4.1. CHOOSING THE HEURISTIC (BASE, SIZE_MISC, SIZE_CORREL_MISC, SIZE_RANDOM_MISC, SIZE_CORREL_MISC_CC, BIGPOOL)

CP_HEURISTIC_B = 'BASE'
CD_HEURISTIC_B = 'BASE'

CP_HEURISTIC = 'SIZE_RANDOM_MISC'
CD_HEURISTIC = 'BIGPOOL'


replication_data_heuristic = subset(replication_data, CPH == CP_HEURISTIC & CDH == CD_HEURISTIC)

##Anand has no base heuristic (0,1,2,3)

anand_data_heuristic = subset(anand_data, PACP == CP_HEURISTIC)


boxplot_data_1 = data.frame("Replikations-Modell",replication_data_heuristic$CP, replication_data_heuristic$MAPE, replication_data_heuristic$nn)
colnames(boxplot_data_1) = c('Modell','CP','MAPE','run')

boxplot_data_2 = data.frame("Original-Modell",anand_data_heuristic$ACP, anand_data_heuristic$MPE, anand_data_heuristic$FirmID)
colnames(boxplot_data_2) = c('Modell','CP','MAPE','run')

boxplot_data = rbind(boxplot_data_1,boxplot_data_2)     

###boxplot
boxplot_data$CP = factor(boxplot_data$CP)

ggplot(boxplot_data, aes(x= CP,y=MAPE, fill=Modell)) +
  geom_boxplot()+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(family = 'Times New Roman',size =16),
        legend.text = element_text(size= 16),
        axis.title = element_text(size=16),
        title = element_text(size=18),
        axis.text.x = element_text(size = 12))+
  ggtitle(paste0(CP_HEURISTIC," & ",CD_HEURISTIC))+                              #Adaption required each time heuristic is changes
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'right')+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))


mean(replication_data_heuristic$CHECK_COR1)

##4.2. ANAND FEHLER 

CP_HEURISTIC = 'SIZE_CORREL_MISC'
CD_HEURISTIC = 'BIGPOOL'


replication_data_heuristic = subset(replication_data, CPH == CP_HEURISTIC & CDH == CD_HEURISTIC)

file_link_replication_correct = 'C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/Replication/Third Replication/ANAND Fehler/CSD_2020-02-18-1033.csv'

replication_data_correct = read.csv(file_link_replication_correct)

replication_data_correct$CPH = as.character(replication_data_correct$CPH)
replication_data_correct$CDH = as.character(replication_data_correct$CDH)

replication_data_correct$CPH[replication_data_correct$CPH == 7] = 'SIZE_CORREL_MISC (ohne CC)'
replication_data_correct$CDH[replication_data_correct$CDH == 0] = 'BIGPOOL'

replication_data_correct$CPH = as.factor(replication_data_correct$CPH)
replication_data_correct$CDH = as.factor(replication_data_correct$CDH)

replication_data_correct$COR1 = NULL
replication_data_correct$COR2 = NULL
replication_data_heuristic$COR = NULL

replication_data_comp = rbind(replication_data_heuristic,replication_data_correct)

replication_data_comp_agg = aggregate(.~ CP+CPH+CDH, data =replication_data_comp, FUN = mean)

ggplot(replication_data_comp_agg, aes(x = CP, y = MAPE, color = CPH))+geom_line(size=1)+
  labs(color = "Heuristik Kombinationen", shape = 'Heuristik Kombinationen')+
  ggtitle('Programmfehler des Original-Modells')+ 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 20,family = 'Times New Roman'), legend.position = 'bottom', 
        legend.text = element_text(family = 'Times New Roman',size = 16), legend.direction = 'vertical', 
        axis.title.x = element_text(family = 'Times New Roman',size = 16),
        axis.title.y = element_text(family = 'Times New Roman',size= 16),
        text = element_text(family = 'Times New Roman',size = 16))+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  geom_point(aes(shape = CPH),size = 4)+
  scale_color_grey(start = 0, end = .8)


##5. REGRESSION ANALYSIS
#Data building
CP_HEURISTIC = 'SIZE_CORREL_MISC_CC'
CD_HEURISTIC = 'BIGPOOL'
create_tables = 0
replication_data_heuristic = subset(replication_data, CPH == CP_HEURISTIC & CDH == CD_HEURISTIC)
anand_data_heuristic = subset(anand_data, PACP == CP_HEURISTIC)##Anand has no base heuristic (0,1,2,3)


##5.1. STANDARDIZING THE REQUIRED COEFFICIENTS FOR AN ANALYSIS


##Choosing the independent and dependent coefficients of each model
anand_model = MPE ~ ACP + g + d

replication_model = MAPE ~ CP + RC_VAR + DENS

##Standardizing the respective columns of the datasets

anand_data_heuristic = lapply(anand_data_heuristic[,all.vars(anand_model)], scale)

replication_data_heuristic = lapply(replication_data_heuristic[,all.vars(replication_model)], scale)

##calculating the regression model and coefficients

linearReg_anand_std = lm(MPE ~ACP+g+d, data = anand_data_heuristic)

linearReg_repl_std = lm(MAPE ~ CP+RC_VAR+DENS, data = replication_data_heuristic)


if(create_tables == 1){
  ###Creating the results
  #REPLICATION
  apa.reg.table(linearReg_repl_std,filename = paste0("reg_replication_",CP_HEURISTIC,".doc"), table.number = 1)
  apa.aov.table(linearReg_repl_std,filename = paste0("anova_replication_",CP_HEURISTIC,".doc"), table.number = 1)
  
  #ANAND
  apa.reg.table(linearReg_anand_std,filename = paste0("reg_anand_",CP_HEURISTIC,".doc"), table.number = 1)
  apa.aov.table(linearReg_anand_std,filename = paste0("anova_anand_",CP_HEURISTIC,".doc"), table.number = 1)
  
}


##5.2. KOLMOGOROV-SMIRNOV TEST
#ohne subset
r = c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48)
subset_replication_data = subset(replication_data_heuristic, CP ==r)
subset_anand_data = subset(anand_data_heuristic, ACP ==r)
ks.test(subset_replication_data$MAPE,subset_anand_data$MPE)

#mit subset für bestimmte CP
s = 40

subset_replication_data = subset(replication_data_heuristic, CP ==s)
subset_anand_data = subset(anand_data_heuristic, ACP ==s)
ks.test(subset_replication_data$MAPE,subset_anand_data$MPE)


#aggregated
replication_data_agg = aggregate(.~CP, data = replication_data_heuristic, FUN = mean)
anand_data_agg = aggregate(.~ACP, data = anand_data_heuristic, FUN = mean)

ks.test(replication_data_agg$MAPE,anand_data_agg$ACP)


####_________________________________Q_VAR_VARIATION_____________________########
##LOADING REPLICATION_DATA

loading_from_data = 0

if(loading_from_data==1){replication_data = DATA}else{
  file_link_q_var = "C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/Robustness Analysis/Q_VAR/CSD_2020-02-12-1653.csv"
  replication_data = read.csv(file_link_q_var,sep = ',')}

replication_data$CPH = as.character(replication_data$CPH)
replication_data$CDH = as.character(replication_data$CDH)

replication_data$CPH[replication_data$CPH == 'base'] = 'BASE'
replication_data$CDH[replication_data$CDH == 'base'] = 'BASE'

replication_data$CPH[replication_data$CPH == 0] = 'SIZE_MISC'
replication_data$CPH[replication_data$CPH == 1] = 'SIZE_CORREL_MISC'
replication_data$CPH[replication_data$CPH == 2] = 'SIZE_RANDOM_MISC'
replication_data$CPH[replication_data$CPH == 3] = 'SIZE_CORREL_MISC_CC'

replication_data$CDH[replication_data$CDH == 0] = 'BIGPOOL'

replication_data$CPH = as.factor(replication_data$CPH)
replication_data$CDH = as.factor(replication_data$CDH)

##ANAND als Vergleich:
file_link_anand_1 = "C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/Replication/Third Replication/ANAND_Model_20200212_200_SIM_NUMB.csv"
anand_data_1 = read.csv(file_link_anand_1, sep = ';')
##2.2. ANAND MODEL DISP2, DENS, RCC02 Output
file_link_anand_2 = "C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/Replication/Third Replication/ANAND_Model_ResCon_20200212_200_SIM_NUMB.csv"
anand_data_2 = read.csv(file_link_anand_2, sep = ';')
anand_data = merge(anand_data_1,anand_data_2, by.x = 'FirmID', by.y = 'FirmID')
##NAME REPLACEMENT
anand_data$PACP[anand_data$PACP == 0] = 'SIZE_MISC'
anand_data$PACP[anand_data$PACP == 1] = 'SIZE_CORREL_MISC'
anand_data$PACP[anand_data$PACP == 2] = 'SIZE_RANDOM_MISC'
anand_data$PACP[anand_data$PACP == 3] = 'SIZE_CORREL_MISC_CC'

anand_data$PDR[anand_data$PDR == 0] == 'BIGPOOL'

anand_data$PACP = as.factor(anand_data$PACP)
anand_data$PDR = as.factor(anand_data$PDR)



##bringing it down to one heuristic
CP_HEURISTIC = 'SIZE_RANDOM_MISC'
CD_HEURISTIC = 'BIGPOOL'
replication_data_heuristic = subset(replication_data, CPH == CP_HEURISTIC & CDH == CD_HEURISTIC)
anand_data_heuristic = subset(anand_data, PACP == CP_HEURISTIC)


###
rep_q_var_output = data.frame(replication_data_heuristic$CP, replication_data_heuristic$MAPE, replication_data_heuristic$Q_VAR)
colnames(rep_q_var_output) = c('CP','MAPE','Q_VAR')
anand_q_var_output = data.frame(anand_data_heuristic$ACP, anand_data_heuristic$MPE, Q_VAR = ('ANAND'))
colnames(anand_q_var_output) = c('CP','MAPE', 'Q_VAR')

q_var_output = rbind(rep_q_var_output,anand_q_var_output)
q_var_output$Q_VAR = as.factor(q_var_output$Q_VAR)

###aggregated datae - mean over CP and Q_VAR ###
q_var_output_agg = aggregate(.~CP + Q_VAR, data = q_var_output, FUN = mean)


ggplot(subset(q_var_output_agg,Q_VAR != 'ANAND'), aes(x = CP, y = MAPE, color = Q_VAR, group = Q_VAR))+
  geom_line(size = 1)+
  ggtitle('SIZE_RANDOM_MISC Q_VAR VARIATION')+
  theme_bw()+                             
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom', text = element_text(size = 16, family = 'Times New Roman'))+
  ylim(0,1)+
  geom_line(data= q_var_output_agg[q_var_output_agg$Q_VAR == 'ANAND',], color = 'black', size = 1)+
  scale_y_continuous(labels = scales::percent)



replication_data_heuristic_agg = aggregate(.~Q_VAR, data = replication_data_heuristic, FUN = mean)

##Q_VAR Regression

replication_model = MAPE ~ CP + Q_VAR
replication_data_heuristic = lapply(replication_data_heuristic[,all.vars(replication_model)], scale)
linearReg_repl_std = lm(MAPE ~ CP+Q_VAR, data = replication_data_heuristic)
apa.reg.table(linearReg_repl_std,filename = paste0("reg_Q_VAR_",CP_HEURISTIC,".doc"), table.number = 1)
apa.aov.table(linearReg_repl_std,filename = paste0("anova_Q_VAR_",CP_HEURISTIC,".doc"), table.number = 1)



####_________________________________RC_VAR_VARIATION_____________________####
##LOADING REPLICATION_DATA

loading_from_data = 0
mapping_RCC01_to_BASE = 1

if(loading_from_data==1){replication_data = DATA}else if(mapping_RCC01_to_BASE ==1){
  file_link_rc_var = "C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/Robustness Analysis/RC_VAR/CSD_2020-02-16-1601.csv"
  replication_data = read.csv(file_link_rc_var,sep = ',')
}else
{
  file_link_rc_var = "C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/Robustness Analysis/RC_VAR/CSD_2020-02-16-1617_no mapping to base.csv"
  replication_data = read.csv(file_link_rc_var,sep = ',')
}

replication_data$CPH = as.character(replication_data$CPH)
replication_data$CDH = as.character(replication_data$CDH)

replication_data$CPH[replication_data$CPH == 'base'] = 'BASE'
replication_data$CDH[replication_data$CDH == 'base'] = 'BASE'

replication_data$CPH[replication_data$CPH == 0] = 'SIZE_MISC'
replication_data$CPH[replication_data$CPH == 1] = 'SIZE_CORREL_MISC'
replication_data$CPH[replication_data$CPH == 2] = 'SIZE_RANDOM_MISC'
replication_data$CPH[replication_data$CPH == 3] = 'SIZE_CORREL_MISC_CC'

replication_data$CDH[replication_data$CDH == 0] = 'BIGPOOL'

replication_data$CPH = as.factor(replication_data$CPH)
replication_data$CDH = as.factor(replication_data$CDH)

##ANAND als Vergleich:
file_link_anand_1 = "C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/Replication/Third Replication/ANAND_Model_20200212_200_SIM_NUMB.csv"
anand_data_1 = read.csv(file_link_anand_1, sep = ';')
##2.2. ANAND MODEL DISP2, DENS, RCC02 Output
file_link_anand_2 = "C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/Replication/Third Replication/ANAND_Model_ResCon_20200212_200_SIM_NUMB.csv"
anand_data_2 = read.csv(file_link_anand_2, sep = ';')
anand_data = merge(anand_data_1,anand_data_2, by.x = 'FirmID', by.y = 'FirmID')
##NAME REPLACEMENT
anand_data$PACP[anand_data$PACP == 0] = 'SIZE_MISC'
anand_data$PACP[anand_data$PACP == 1] = 'SIZE_CORREL_MISC'
anand_data$PACP[anand_data$PACP == 2] = 'SIZE_RANDOM_MISC'
anand_data$PACP[anand_data$PACP == 3] = 'SIZE_CORREL_MISC_CC'

anand_data$PDR[anand_data$PDR == 0] == 'BIGPOOL'

anand_data$PACP = as.factor(anand_data$PACP)
anand_data$PDR = as.factor(anand_data$PDR)



##bringing it down to one heuristic
CP_HEURISTIC = 'SIZE_RANDOM_MISC'
CD_HEURISTIC = 'BIGPOOL'
replication_data_heuristic = subset(replication_data, CPH == CP_HEURISTIC & CDH == CD_HEURISTIC)
anand_data_heuristic = subset(anand_data, PACP == CP_HEURISTIC)


###
rep_rc_var_output = data.frame(replication_data_heuristic$CP, replication_data_heuristic$MAPE, replication_data_heuristic$RC_VAR)
colnames(rep_rc_var_output) = c('CP','MAPE','RC_VAR')
anand_rc_var_output = data.frame(anand_data_heuristic$ACP, anand_data_heuristic$MPE, RC_VAR = ('ANAND'))
colnames(anand_rc_var_output) = c('CP','MAPE', 'RC_VAR')

rc_var_output = rbind(rep_rc_var_output,anand_rc_var_output)
rc_var_output$RC_VAR = as.factor(rc_var_output$RC_VAR)

###aggregated datae - mean over CP and Q_VAR ###
rc_var_output_agg = aggregate(.~CP + RC_VAR, data = rc_var_output, FUN = mean)


ggplot(subset(rc_var_output_agg,RC_VAR != 'ANAND'), aes(x = CP, y = MAPE, color = RC_VAR, group = RC_VAR))+
  geom_line(size = 1)+
  ggtitle('SIZE_RANDOM_MISC RC_VAR VARIATION - ohne Sortierung')+
  theme_bw()+                             
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom', text = element_text(size = 16, family = 'Times New Roman'))+
  ylim(0,1)+
  geom_line(data= rc_var_output_agg[rc_var_output_agg$RC_VAR == 'ANAND',], color = 'black', size = 1)+
  scale_y_continuous(labels = scales::percent)


replication_data_heuristic_agg = aggregate(.~RC_VAR, data = replication_data_heuristic, FUN = mean)



##RC_VAR Regression

replication_model = MAPE ~ CP + RC_VAR
replication_data_heuristic = lapply(replication_data_heuristic[,all.vars(replication_model)], scale)
linearReg_repl_std = lm(MAPE ~ CP+RC_VAR, data = replication_data_heuristic)
apa.reg.table(linearReg_repl_std,filename = paste0("reg_RC_VAR_",CP_HEURISTIC,".doc"), table.number = 1)
apa.aov.table(linearReg_repl_std,filename = paste0("anova_RC_VAR_",CP_HEURISTIC,".doc"), table.number = 1)



####_________________________________COR_VARIATION_____________________########
##LOADING REPLICATION_DATA

loading_from_data = 1

if(loading_from_data==1){replication_data = DATA}else{
  file_link_cor_var = "C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/Robustness Analysis/COR_VAR/CSD_2020-02-16-1543.csv"
  replication_data = read.csv(file_link_cor_var,sep = ',')}

replication_data$CPH = as.character(replication_data$CPH)
replication_data$CDH = as.character(replication_data$CDH)

replication_data$CPH[replication_data$CPH == 'base'] = 'BASE'
replication_data$CDH[replication_data$CDH == 'base'] = 'BASE'

replication_data$CPH[replication_data$CPH == 0] = 'SIZE_MISC'
replication_data$CPH[replication_data$CPH == 1] = 'SIZE_CORREL_MISC'
replication_data$CPH[replication_data$CPH == 2] = 'SIZE_RANDOM_MISC'
replication_data$CPH[replication_data$CPH == 3] = 'SIZE_CORREL_MISC_CC'

replication_data$CDH[replication_data$CDH == 0] = 'BIGPOOL'

replication_data$CPH = as.factor(replication_data$CPH)
replication_data$CDH = as.factor(replication_data$CDH)

##ANAND als Vergleich:
file_link_anand_1 = "C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/Replication/Third Replication/ANAND_Model_20200212_200_SIM_NUMB.csv"
anand_data_1 = read.csv(file_link_anand_1, sep = ';')
##2.2. ANAND MODEL DISP2, DENS, RCC02 Output
file_link_anand_2 = "C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/Replication/Third Replication/ANAND_Model_ResCon_20200212_200_SIM_NUMB.csv"
anand_data_2 = read.csv(file_link_anand_2, sep = ';')
anand_data = merge(anand_data_1,anand_data_2, by.x = 'FirmID', by.y = 'FirmID')
##NAME REPLACEMENT
anand_data$PACP[anand_data$PACP == 0] = 'SIZE_MISC'
anand_data$PACP[anand_data$PACP == 1] = 'SIZE_CORREL_MISC'
anand_data$PACP[anand_data$PACP == 2] = 'SIZE_RANDOM_MISC'
anand_data$PACP[anand_data$PACP == 3] = 'SIZE_CORREL_MISC_CC'

anand_data$PDR[anand_data$PDR == 0] == 'BIGPOOL'

anand_data$PACP = as.factor(anand_data$PACP)
anand_data$PDR = as.factor(anand_data$PDR)



##bringing it down to one heuristic
CP_HEURISTIC = 'SIZE_RANDOM_MISC'
CD_HEURISTIC = 'BIGPOOL'
replication_data_heuristic = subset(replication_data, CPH == CP_HEURISTIC & CDH == CD_HEURISTIC)
anand_data_heuristic = subset(anand_data, PACP == CP_HEURISTIC)


###
rep_cor_var_output = data.frame(replication_data_heuristic$CP, replication_data_heuristic$MAPE, replication_data_heuristic$COR1,replication_data_heuristic$COR2)
colnames(rep_cor_var_output) = c('CP','MAPE','COR1','COR2')

anand_cor_var_output = data.frame(anand_data_heuristic$ACP, anand_data_heuristic$MPE)
colnames(anand_cor_var_output) = c('CP','MAPE')

#q_var_output = rbind(rep_q_var_output,anand_q_var_output)
rep_cor_var_output$COR1 = as.factor(rep_cor_var_output$COR1)
rep_cor_var_output$COR2 = as.factor(rep_cor_var_output$COR2)

###aggregated datae - mean over CP and Q_VAR ###
rep_cor_var_output_agg = aggregate(.~CP + COR1+COR2, data = rep_cor_var_output, FUN = mean)
anand_cor_var_output_agg = aggregate(.~CP, data = anand_cor_var_output,FUN = mean)

ggplot(rep_cor_var_output_agg, aes(x = CP, y = MAPE, color = COR1, group = COR1))+geom_line(size = 1)+
  ggtitle('SIZE_RANDOM_MISC COR1 Variation für jedes COR2')+
  theme_bw()+                              #Adaption required each time heuristic is changes
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom', text = element_text(size = 16, family = 'Times New Roman'))+
  ylim(0,1)+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap('COR2')+geom_line(data= anand_cor_var_output_agg, color = 'black', group = 'ANAND',size = 1)





replication_model = MAPE ~ CP + CHECK_COR1 + CHECK_COR2
replication_data_heuristic = lapply(replication_data_heuristic[,all.vars(replication_model)], scale)
linearReg_repl_std = lm(MAPE ~ CP+ CHECK_COR1+ CHECK_COR2, data = replication_data_heuristic)
apa.reg.table(linearReg_repl_std,filename = paste0("reg_COR_VAR_",CP_HEURISTIC,".doc"), table.number = 1)
apa.aov.table(linearReg_repl_std,filename = paste0("anova_COR_VAR_",CP_HEURISTIC,".doc"), table.number = 1)



####_________________________________DENS_VARIATION_____________________########


loading_from_data = 1

if(loading_from_data==1){replication_data = DATA}else{
  file_link_dens_var = "C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/Robustness Analysis/DENS_VAR/CSD_2020-02-15-1658.csv"
  replication_data = read.csv(file_link_dens_var,sep = ',')}

replication_data$CPH = as.character(replication_data$CPH)
replication_data$CDH = as.character(replication_data$CDH)

replication_data$CPH[replication_data$CPH == 'base'] = 'BASE'
replication_data$CDH[replication_data$CDH == 'base'] = 'BASE'

replication_data$CPH[replication_data$CPH == 0] = 'SIZE_MISC'
replication_data$CPH[replication_data$CPH == 1] = 'SIZE_CORREL_MISC'
replication_data$CPH[replication_data$CPH == 2] = 'SIZE_RANDOM_MISC'
replication_data$CPH[replication_data$CPH == 3] = 'SIZE_CORREL_MISC_CC'

replication_data$CDH[replication_data$CDH == 0] = 'BIGPOOL'

replication_data$CPH = as.factor(replication_data$CPH)
replication_data$CDH = as.factor(replication_data$CDH)

##ANAND als Vergleich:
file_link_anand_1 = "C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/Replication/Third Replication/ANAND_Model_20200212_200_SIM_NUMB.csv"
anand_data_1 = read.csv(file_link_anand_1, sep = ';')
##2.2. ANAND MODEL DISP2, DENS, RCC02 Output
file_link_anand_2 = "C:/Users/cms9023/Documents/CostSystemDesignSim PROJECT/MARK/Replication/Third Replication/ANAND_Model_ResCon_20200212_200_SIM_NUMB.csv"
anand_data_2 = read.csv(file_link_anand_2, sep = ';')
anand_data = merge(anand_data_1,anand_data_2, by.x = 'FirmID', by.y = 'FirmID')
##NAME REPLACEMENT
anand_data$PACP[anand_data$PACP == 0] = 'SIZE_MISC'
anand_data$PACP[anand_data$PACP == 1] = 'SIZE_CORREL_MISC'
anand_data$PACP[anand_data$PACP == 2] = 'SIZE_RANDOM_MISC'
anand_data$PACP[anand_data$PACP == 3] = 'SIZE_CORREL_MISC_CC'

anand_data$PDR[anand_data$PDR == 0] == 'BIGPOOL'

anand_data$PACP = as.factor(anand_data$PACP)
anand_data$PDR = as.factor(anand_data$PDR)



##bringing it down to one heuristic
CP_HEURISTIC = 'SIZE_RANDOM_MISC'
CD_HEURISTIC = 'BIGPOOL'
replication_data_heuristic = subset(replication_data, CPH == CP_HEURISTIC & CDH == CD_HEURISTIC)
anand_data_heuristic = subset(anand_data, PACP == CP_HEURISTIC)


###
rep_dens_var_output = data.frame(replication_data_heuristic$CP, replication_data_heuristic$MAPE, replication_data_heuristic$DENS)
colnames(rep_dens_var_output) = c('CP','MAPE','DENS')
anand_dens_var_output = data.frame(anand_data_heuristic$ACP, anand_data_heuristic$MPE, DENS = ('ANAND'))
colnames(anand_dens_var_output) = c('CP','MAPE', 'DENS')

dens_var_output = rbind(rep_dens_var_output,anand_dens_var_output)
dens_var_output$DENS = as.factor(dens_var_output$DENS)

###aggregated datae - mean over CP and Q_VAR ###
dens_var_output_agg = aggregate(.~CP + DENS, data = dens_var_output, FUN = mean)
dens_var_output_agg$DENS = as.factor(dens_var_output_agg$DENS)

ggplot(subset(dens_var_output_agg, DENS != 'ANAND'), aes(x = CP, y = MAPE, color = DENS, group = DENS))+
  geom_line(size = 1)+
  ggtitle('SIZE_RANDOM_MISC DENS VARIATION')+
  theme_bw()+                             
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom', text = element_text(size = 16, family = 'Times New Roman'))+
  ylim(0,1)+
  geom_line(data= dens_var_output_agg[dens_var_output_agg$DENS == 'ANAND',], color = 'black', size = 1)+
  scale_y_continuous(labels = scales::percent)



replication_model = MAPE ~ CP + DENS
replication_data_heuristic = lapply(replication_data_heuristic[,all.vars(replication_model)], scale)
linearReg_repl_std = lm(MAPE ~ CP+DENS, data = replication_data_heuristic)
apa.reg.table(linearReg_repl_std,filename = paste0("reg_DENS_VAR_",CP_HEURISTIC,".doc"), table.number = 1)
apa.aov.table(linearReg_repl_std,filename = paste0("anova_DENS_VAR_",CP_HEURISTIC,".doc"), table.number = 1)




####_________________________________OLD ANALYSIS_____________________########
replication_data_agg = aggregate(.~CP+DENS, data = replication_data,FUN = mean)
plot(replication_data_agg$MAPE)

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

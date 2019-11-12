#####################################
#### CostSystemDesignSim   // 2019-08-28    V 0.01
#####################################


###########
#### Test
###########


##############################
# 0 - Install librairies - Library
##############################

# install.packages(c(
#   "dplyr",
#   "tidyr",
#   "rmarkdown",
#   "ggplot2"
#   
# ))
# 
# Packages <- c("dplyr", "ggplot2", "rmarkdown", "tidyr")
# lapply(Packages, library, character.only = TRUE)

##############################
# 1 - Start
##############################

## SOURCE THIS FILE FOR EXECUTION
source('src/gen_ProductionEnvironment.R')
source('src/.gen_RES_CONS_PAT.R')
source('src/.gen_RC.R')
source('src/CP_Heuristics.R')
source('src/CD_Heuristics.R')
source('src/.gen_COST_CONS_PAT.R')
source('src/.gen_Q.R')
source('src/.datalogging.R')


source("1_INIT.R")

#test




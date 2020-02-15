##### Initizalization of the CostSystemDesignSim (CSDS)

## ======================================INPUT MASK============================================================
  FIRM = list()
  FIRM$PRODUCTION_ENVIRONMENT = list()
  FIRM$COSTING_SYSTEM = list()
  DATA = data.frame()
  DATAp = data.frame()
    
  NUMB_PRO =         50                     #INPUT independent Variable - Number of products 
  NUMB_RES  =        50                  #INPUT independent variable - Number of factors
  SIM_NUMB =         200                 #Control Variable - Number of Simulations for every single environment (standard: 30)     
  TC =               1000000             #Total costs




  ProductCostOutput= 0                      #Control Variable -  Zero = no tracking of the product level

  set_DEMAND_fix=   0                  #Control Variable -  Decide if always the same demand is used
  set_RES_CONS_PAT_fix = 0             #Control variable - Decide if the resource consumption is constant
  set_RCU_fix = 0                      #Control variable - Decide if the resource costs per unit are constant
  set_RCC_fix = 0                    # Control Variable - Decide if the reosurce costs vector is the same
  set_CSD_fix=  0                     #Control Variable -  Decide if CD_Heuristic always uses the same resources.

  
  CP = c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50)       #No. of Cost Pools
  COR = c(-1)                              #Correlation between big resources
  RC_VAR =  c(-1)                          #Resource cost variation --> base for DISP2
  Q_VAR = c(-1)                        #Demand variation
  Error = c(0)                              #Measurement error
  NUMB_Error = c(0)                         #Number of errornoues links
  DENS = c(-1)                              #Number of links between products and resources (sharing)
  CC = c(0.4)                               #Correlation Cutoff for correlative assignement in CP HEURISTICS
  MISCPOOLSIZE = c(0.25)                    #share of total costs that are supposed to go into the miscpool if there is a miscpool in the Costing System
  DISP1 = c(10)                             #No. of the biggest resources that have a DISP2 share of the total costs
  NUM = c(1)                                #No. of Resources used for indexed driver
  CP_HEURISTIC = c('base')                       #Which Heuristic for pooling resources? # 0-6
  CD_HEURISTIC = c('base')                       #which Heuristic for selecting a driver? #0-1



## ====================================== END OF INPUT MASK=====================================================                           

            set.seed(13) #Reproducability
            o=1 # First design point

## ======================================DESIGN OF EXPERIMENTS ==================================================
## EVIRONMENTAL FACTORS []
  for (ix_CP in seq_along(CP)) {
     for (ix_COR in seq_along(COR)) {
        for (ix_RC_VAR in seq_along(RC_VAR)) {
          for (ix_Q_VAR in seq_along(Q_VAR)) {
            for (ix_Error in seq_along(Error)) {
               for (ix_NUMB_Error in seq_along(NUMB_Error)) {
                 for (ix_DENS in seq_along(DENS)) {
                   for(ix_CC in seq_along(CC)){
                     for(ix_MISCPOOLSIZE in seq_along(MISCPOOLSIZE)){
                       for(ix_DISP1 in seq_along(DISP1)){
                         for(ix_CP_HEURISTIC in seq_along(CP_HEURISTIC)){
                           for(ix_CD_HEURISTIC in seq_along(CD_HEURISTIC)){


  ## ====================== PREDETERMINING AND PREALLOCATION  =========================

    FIRM$PRODUCTION_ENVIRONMENT$CP = CP[ix_CP]
    FIRM$PRODUCTION_ENVIRONMENT$DENS = DENS[ix_DENS]
    FIRM$PRODUCTION_ENVIRONMENT$COR  = COR[ix_COR]
    FIRM$PRODUCTION_ENVIRONMENT$Q_VAR= Q_VAR[ix_Q_VAR]
    FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO = NUMB_PRO
    FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES = NUMB_RES
    FIRM$PRODUCTION_ENVIRONMENT$DISP1 = DISP1
    FIRM$PRODUCTION_ENVIRONMENT$set_DEMAND_fix = set_DEMAND_fix
    FIRM$PRODUCTION_ENVIRONMENT$set_RES_CONS_PAT_fix = set_RES_CONS_PAT_fix
    FIRM$COSTING_SYSTEM$CP = CP[ix_CP]
    FIRM$COSTING_SYSTEM$RC_VAR = RC_VAR[ix_RC_VAR]
    FIRM$COSTING_SYSTEM$Error = Error[ix_Error]
    FIRM$COSTING_SYSTEM$NUMB_Error = NUMB_Error[ix_NUMB_Error]
    FIRM$COSTING_SYSTEM$TC = TC
    FIRM$COSTING_SYSTEM$CC = CC
    FIRM$COSTING_SYSTEM$MISCPOOLSIZE = MISCPOOLSIZE
    FIRM$COSTING_SYSTEM$set_CSD_fix = set_CSD_fix
    FIRM$COSTING_SYSTEM$set_RCC_fix = set_RCC_fix
    FIRM$COSTING_SYSTEM$set_RCU_fix = set_RCU_fix
    FIRM$COSTING_SYSTEM$NUM = NUM
    FIRM$COSTING_SYSTEM$CP_HEURISTIC = CP_HEURISTIC[ix_CP_HEURISTIC]
    FIRM$COSTING_SYSTEM$CD_HEURISTIC = CD_HEURISTIC[ix_CD_HEURISTIC]


   #### ============================== SIMULATION ======================================
   nn=1 # necessary for repeating the SIM_NUMB loop
   for (nn in 1:SIM_NUMB) {



    FIRM = gen_ProductionEnvironment(FIRM,set_PE_constant) #Generate Production Environment with RES_CONS_PAT

    ##Building the cost pools
    if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 'base'){FIRM = MAP_RES_CP_RANDOM(FIRM)}

    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 0){FIRM = MAP_RES_CP_SIZE_MISC(FIRM)}

    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 1){FIRM = MAP_RES_CP_SIZE_CORREL_MISC_ANAND(FIRM)}

    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 2){FIRM = MAP_RES_CP_SIZE_RANDOM_MISC(FIRM)}

    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 3){FIRM = MAP_RES_CP_SIZE_CORREL_CUTOFF_MISC_ANAND(FIRM)}

    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 4){FIRM = MAP_CP_CORREL_MISC(FIRM)}

    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 5){FIRM = MAP_RES_CP_SIZE_CORREL(FIRM)}

    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 6){FIRM = MAP_RES_CP_SIZE_RANDOM(FIRM)}

    else{print("error CP_Heuristic")}

    # Selecting the drivers of a cost pool
    if(FIRM$COSTING_SYSTEM$CD_HEURISTIC == 'base'){FIRM = MAP_CP_P_RANDOM(FIRM,Error,NUMB_Error)}

    else if(FIRM$COSTING_SYSTEM$CD_HEURISTIC == 0){FIRM = MAP_CP_P_BIGPOOL(FIRM,Error,NUMB_Error)}

    else if(FIRM$COSTING_SYSTEM$CD_HEURISTIC == 1){FIRM = MAP_CP_P_AVERAGE(FIRM,Error,NUMB_Error)}

    else if(FIRM$COSTING_SYSTEM$CD_HEURISTIC == 2){FIRM = MAP_CP_P_INDEXED(FIRM,Error,NUMB_Error)}

    else{print("error CD_Heuristic")}
    ## Calculating the estimated product costs

    FIRM$COSTING_SYSTEM$PCH =  FIRM$COSTING_SYSTEM$ACT_CONS_PAT %*% FIRM$COSTING_SYSTEM$ACP # CHECKED 2019/09/12

    ## ERROR MEASURES AFTER LABRO & VANHOUCKE 2007
    EUCD = round(sqrt(sum((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)^2)),digits=2)
    MAPE = round(mean(abs(FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)/FIRM$COSTING_SYSTEM$PCB),digits=4)
    MSE = round(mean(((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)^2)),digits=2);


    ## OVER AND UNDERCOSTING ##

    UC = sum((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)>0)/NUMB_PRO
    OC = sum((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)<=0)/NUMB_PRO

    UC5 = sum(((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)/FIRM$COSTING_SYSTEM$PCB)>0.05)/NUMB_PRO
    OC5 = sum(((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)/FIRM$COSTING_SYSTEM$PCB)<=-0.05)/NUMB_PRO


  #### ======== COLLECTING THE DATA FOR OUTPUT ==== ####

    ## DATA LOGGING
    DATA = .system_datalogging(o,nn,FIRM,DATA)
    if (ProductCostOutput==1){DATAp = .product_datalogging(o,nn,FIRM,DATAp,CP_HEURISTIC,CD_HEURISTIC)}


    print(o)
    print(FIRM$COSTING_SYSTEM$CP)
    print((MAPE))
    # print((EUCD))

    o=o+1 #Counting for the total number of runs
   }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

#### ====================================OUTPUT WRITING ===================================

#output data
output = paste("output/CSD_",format(Sys.time(),"%Y-%m-%d-%H%M"),".csv", sep = "")
write.csv(DATA, file = output)
print("Cost System Design FILE has been written")

if (ProductCostOutput==1)
{
  output = paste("output/ProductCost_",format(Sys.time(),"%Y-%m-%d-%H%M"), ".csv", sep = "")
  write.csv(DATAp, file = output)
  print("Product costs FILE has been written")
}


#### ====================================PlOT WRITING ===================================
replication_data = DATA
replication_data_agg = aggregate(.~CP+CPH+CDH, data = replication_data, FUN = mean)

ggplot(replication_data_agg, aes(x = CP, y = MAPE, color = interaction(CPH, CDH, sep = ' & ')))+
  geom_line(size = 1)+labs(color = "Heuristik Kombinationen")+
  theme_bw()+
  ggtitle(paste("CSD_",format(Sys.time(),"%Y-%m-%d-%H%M")))+                              
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  scale_y_continuous(labels = scales::percent)
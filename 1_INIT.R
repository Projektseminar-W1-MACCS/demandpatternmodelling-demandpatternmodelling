#############################################################
# Initizalization of the CostSystemDesignSim (CSDS)
#############################################################

## Kai ist cool ###
## PUSTKUCHEN ###

## ======================================INPUT MASK============================================================
  FIRM = list()                           
  FIRM$PRODUCTION_ENVIRONMENT = list()
  FIRM$COSTING_SYSTEM = list()
  DATA = data.frame()
  DATAp = data.frame()
  
  
  NUMB_PRO =         50                     #INPUT independent Variable - Number of products 
  NUMB_RES  =        50                     #INPUT independent variable - Number of factors

  SIM_NUMB =         200                  #Control Variable - Number of Simulations for every single environment (standard: 30)     

  TC =               1000000                #Total costs


  ProductCostOutput= 0                      #Control Variable -  Zero = no tracking of the product level
  set_PE_constant=   0                      #Control Variable -  Decide if genProduction environment is fixed: Using the same firm.

  dec_ERROR=         1                      #Control Variable - 

  #dec_DC=           0                      # = no direct costs 
  dec_CP=            1                      # =
  dec_CD=            1                      # =
  
  
  #CP = c(1)
  CP = c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50) #No. of Cost Pools
  COR = c(0.6)                              #Correlation between resources
  RC_VAR =  c(-1)                          #Resource cost variation --> base for DISP2 (ABL2019) (0.2)
  Q_VAR = c(0.4)                            #Demand variation
  Error = c(0)                              #Measurement error (BHL2011)
  NUMB_Error = c(0)                         #Number of errornoues links (LV2008)
  DENS = c(-1)                              #Number of links between products and resources (sharing)

  CP = c(10)       #No. of Cost Pools
  COR = c(0.6)                              #Correlation between resources
  RC_VAR =  c(0.5)                          #Resource cost variation --> base for DISP2
  Q_VAR = c(0.4)                            #Demand variation
  Error = c(0)                              #Measurement error
  NUMB_Error = c(1)                         #Number of errornoues links
  DENS = c(1)                              #Number of links between products and resources (sharing)

  CC = c(0.4)                               #Correlation Cutoff for correlative assignement in CP HEURISTICS
  MISCPOOLSIZE = c(0.25)                    #share of total costs that are supposed to go into the miscpool if there is a miscpool in the Costing System
  DISP1 = c(2)                             #No. of the biggest resources that have a DISP2 share of the total costs
  NUM = c(2)                                #No. of Resources used for indexed driver
  
  CP_HEURISTIC = c(1)                       #Which Heuristic for pooling resources? # 0-6
  CD_HEURISTIC = c(1)                   #which Heuristic for selecting a driver? #0-1
  
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
    
    FIRM$PRODUCTION_ENVIRONMENT$DENS = DENS[ix_DENS]   
    FIRM$PRODUCTION_ENVIRONMENT$COR  = COR[ix_COR]
    FIRM$PRODUCTION_ENVIRONMENT$Q_VAR= Q_VAR[ix_Q_VAR]
    FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO = NUMB_PRO
    FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES = NUMB_RES
    FIRM$PRODUCTION_ENVIRONMENT$DISP1 = DISP1
    FIRM$COSTING_SYSTEM$CP = CP[ix_CP]
    FIRM$COSTING_SYSTEM$RC_VAR = RC_VAR[ix_RC_VAR]
    FIRM$COSTING_SYSTEM$Error = Error[ix_Error]
    FIRM$COSTING_SYSTEM$NUMB_Error = NUMB_Error[ix_NUMB_Error]
    FIRM$COSTING_SYSTEM$TC = TC
    FIRM$COSTING_SYSTEM$CC = CC
    FIRM$COSTING_SYSTEM$MISCPOOLSIZE = MISCPOOLSIZE
    FIRM$COSTING_SYSTEM$NUM = NUM
    FIRM$COSTING_SYSTEM$CP_HEURISTIC = CP_HEURISTIC[ix_CP_HEURISTIC]
    FIRM$COSTING_SYSTEM$CD_HEURISTIC = CD_HEURISTIC[ix_CD_HEURISTIC]
    
                
    
  nn=1 # necessary for repeating the SIM_NUMB loop
  #### ============================== SIMULATION ======================================
  for (nn in 1:SIM_NUMB) {
    
    #print(FIRM$COSTING_SYSTEM$CP)  
    #print(FIRM$COSTING_SYSTEM$Error)  

       
    FIRM = gen_ProductionEnvironment(FIRM,set_PE_constant) #Generate Production Environment with RES_CONS_PAT
    
    ##Building the cost pools

    if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 0){FIRM = MAP_RES_CP_SIZE_MISC(FIRM)}
    
    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 1){FIRM = MAP_RES_CP_SIZE_CORREL_MISC_ANAND(FIRM)}
    
    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 2){FIRM = MAP_RES_CP_SIZE_RANDOM_MISC(FIRM)}
    
    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 3){FIRM = MAP_RES_CP_SIZE_CORREL_CUTOFF_MISC_ANAND(FIRM)}
    
    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 4){FIRM = MAP_CP_CORREL_MISC(FIRM)}
    
    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 5){FIRM = MAP_RES_CP_SIZE_CORREL_MISC_OWN(FIRM)}
    
    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 6){FIRM = MAP_RES_CP_SIZE_RANDOM(FIRM)}
   
      
      
      ## Selecting the drivers of a cost pool
    if(FIRM$COSTING_SYSTEM$CD_HEURISTIC == 0){FIRM = MAP_CP_P_BIGPOOL(FIRM,Error,NUMB_Error)}
    
    else if(FIRM$COSTING_SYSTEM$CD_HEURISTIC == 1){FIRM = MAP_CP_P_AVERAGE(FIRM,Error,NUMB_Error)}
    
    else if(FIRM$COSTING_SYSTEM$CD_HEURISTIC == 2){FIRM = MAP_CP_P_INDEXED(FIRM,Error,NUMB_Error)}
    
      
      
      
    ## Calculating the estimated product costs
    
    FIRM$COSTING_SYSTEM$PCH =  FIRM$COSTING_SYSTEM$ACT_CONS_PAT %*% FIRM$COSTING_SYSTEM$ACP # CHECKED 2019/09/12 
    #FIRM$COSTING_SYSTEM$PCH = rowSums(sweep(FIRM$COSTING_SYSTEM$ACT_CONS_PAT, MARGIN=1, FIRM$COSTING_SYSTEM$ACP, `*`))
  
    ## ERROR MEASURES AFTER LABRO & VANHOUCKE 2007
    EUCD = round(sqrt(sum((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)^2)),digits=2)
    MAPE = round(mean(abs(FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)/FIRM$COSTING_SYSTEM$PCB),digits=4)
    MSE = round(mean(((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)^2)),digits=2);
    
    
    UC = sum((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)>0)/NUMB_PRO
    OC = sum((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)<=0)/NUMB_PRO  
    
    UC5 = sum(((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)/FIRM$COSTING_SYSTEM$PCB)>0.05)/NUMB_PRO
    OC5 = sum(((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)/FIRM$COSTING_SYSTEM$PCB)<=-0.05)/NUMB_PRO  
       
    
    ## DATA LOGGING
    DATA = .system_datalogging(o,nn,FIRM,DATA)
    if (ProductCostOutput==1){DATAp = .product_datalogging(o,nn,FIRM,DATAp,CP_HEURISTIC,CD_HEURISTIC)}
    ## Print outputs;

    
    OC = sum((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)>0)/NUMB_PRO
    UC = sum((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)<=0)/NUMB_PRO  
    
    OC5 = sum(((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)/FIRM$COSTING_SYSTEM$PCB)>0.05)/NUMB_PRO
    UC5 = sum(((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)/FIRM$COSTING_SYSTEM$PCB)<=-0.05)/NUMB_PRO  
    
    
    
    
  #### ======== COLLECTING THE DATA FOR OUTPUT ==== ####
    preData = data.frame(o,
                         nn,
                         FIRM$COSTING_SYSTEM$CP,
                         FIRM$COSTING_SYSTEM$RC_VAR, 
                         FIRM$COSTING_SYSTEM$NUMB_Error, 
                         FIRM$COSTING_SYSTEM$Error,
                         FIRM$PRODUCTION_ENVIRONMENT$DENS, 
                         FIRM$PRODUCTION_ENVIRONMENT$COR, 
                         FIRM$PRODUCTION_ENVIRONMENT$Q_VAR, 
                         FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,
                         FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$RCC20,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$RCC10,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$RCC02,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q20,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q10,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q02,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$NonZeroConsumption,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$countNonZero,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$COR1,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$COR2,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$MISCPOOL,
                         EUCD,
                         MAPE,
                         MSE)
  
    #preData_p = .datalogging()
    colnames(preData) = c('o','nn','CP','RCC_VAR', 'NUMB_ME', 'ME_AD','DENS', 'COR', 'Q_VAR', 
                       'NUMB_PRO', 'NUMB_RES','CHECK_RCC20','CHECK_RCC10','CHECK_RCC02','CHECK_Q20',
                       'CHECK_Q10','CHECK_Q02','CHECK_NonZeroCons','CHECK_countNonZero','CHECK_COR1','CHECK_COR2',
                       'MISCPOOL','EUCD','MAPE','MSE')  
   
    #stacking the data with each run
    DATA = rbind(DATA,preData)
       
    # TRACKING THE PRODUCT LEVEL WHEN NEEDED
    if (ProductCostOutput==1){DATAp = .datalogging(o,nn,FIRM,DATAp)}
   
    #Print outputs;

    print(o)
    print(FIRM$COSTING_SYSTEM$CP)
    print((MAPE))
    
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

# check = aggregate(DATA,list(DATA$CP),mean)
# plot(check$MAPE,type ='l')
# print(check$MAPE)

if (ProductCostOutput==1)
{
  output = paste("output/ProductCost_",format(Sys.time(),"%Y-%m-%d-%H%M"), ".csv", sep = "")          
  write.csv(DATAp, file = output)
  print("Product costs FILE has been written")
}

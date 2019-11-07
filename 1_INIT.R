#############################################################
# Initizalization of the CostSystemDesignSim (CSDS)
#############################################################



## ======================================INPUT MASK============================================================
  FIRM = list()                           
  FIRM$PRODUCTION_ENVIRONMENT = list()
  FIRM$COSTING_SYSTEM = list()
  DATA = data.frame()
  
  
  NUMB_PRO =         50                     #INPUT independent Variable - Number of products 
  NUMB_RES  =        50                     #INPUT independent variable - Number of factors

  SIM_NUMB =         200                    #Control Variable - Number of Simulations for every single environment (standard: 30)     

  TC =               1000000                #Total costs

  ProductOutput=     1                      #Zero = no tracking
  set_pe_constant=   1                      #Control Variable -  Decide if Simulation is reproducible {1} or random {0}
  set_cs_constant=   0                      #Control Variable 
  vary_demand =      0                      #Control Variable
  
  dec_ERROR=         1                      #Control Variable - 
  seed=              13                     #Control Variable -
  
  #dec_DC=           0                      # = no direct costs 
  dec_CP=            1                      # =
  dec_CD=            1                      # =
  
  
  CP = c(1,2,4,6,8,10,12,14,16,18,20)       #No. of Cost Pools
  COR = c(0.6)                              #Correlation between resources
  RC_VAR =  c(-1)                           #Resource cost variation --> base for DISP2
  Q_VAR = c(1)                              #Demand variation
  Error = c(0)                              #Measurement error
  NUMB_Error = c(1)                         #Number of errornoues links
  DENS = c(-1)                              #Number of links between products and resources (sharing)
  CC = c(0.4)                               #Correlation Cutoff for correlative assignement in CP HEURISTICS
  MISCPOOLSIZE = c(0.25)                       #share of total costs that are supposed to go into the miscpool if there is a miscpool in the Costing System
  DISP1 = c(10)                                #No. of the biggest resources that have a DISP2 share of the total costs
  
## ======================================END OF INPUT MASK=====================================================                           

            set.seed(seed) #Reproducability
            o=1 # First design point
            
## ====================================== DESIGN OF EXPERIMENTS ================================================== 
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
    
                
    
  nn=1 # necessary for repeating the SIM_NUMB loop
  #### ============================== SIMULATION ======================================
  for (nn in 1:SIM_NUMB) {
    
    #print(FIRM$COSTING_SYSTEM$CP)  
    #print(FIRM$COSTING_SYSTEM$Error)  
    
    
    FIRM = gen_ProductionEnvironment(FIRM) #Generate Production Environment with RES_CONS_PAT

  
    FIRM = MAP_RES_CP_SIZE_CORREL_CUTOFF_MISC_ANAND2(FIRM) #Building the cost pools
  
  
    FIRM = MAP_CP_P_BIGPOOL(FIRM,Error) #Selecting the drivers of a cost pool
    
    
    
    
    ## Calculating the estimated product costs
    FIRM$COSTING_SYSTEM$PCH =  apply((FIRM$COSTING_SYSTEM$ACP) * t(FIRM$COSTING_SYSTEM$ACT_CONS_PAT),2,sum) # CHECKED 2019/09/12
  
      ## ERROR MEASURES AFTER LABRO & VANHOUCKE 2007 
    EUCD = round(sqrt(sum((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)^2)),digits=2)
    MAPE = round(mean(abs(FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)/FIRM$COSTING_SYSTEM$PCB),digits=4)
    MSE = round(mean(((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)^2)),digits=2);
  
    
    
  #### ======== COLLECTING THE DATA FOR OUTPUT ==== ####
    preData = data.frame(o,nn,FIRM$COSTING_SYSTEM$CP,FIRM$COSTING_SYSTEM$RC_VAR, FIRM$COSTING_SYSTEM$NUMB_Error, FIRM$COSTING_SYSTEM$Error,
                         FIRM$PRODUCTION_ENVIRONMENT$DENS, FIRM$PRODUCTION_ENVIRONMENT$COR, FIRM$PRODUCTION_ENVIRONMENT$Q_VAR, FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,
                         FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES,EUCD,MAPE,MSE)
  
    #preData_p = .datalogging()
    colnames(preData) = c('o','nn','CP','RCC_VAR', 'NUMB_ME', 'NUMB_ME_AD','DENS', 'COR', 'Q_VAR', 
                       'NUMB_PRO', 'NUMB_RES' ,'EUCD','MAPE','MSE')  
    
    #stacking the data with each run
    DATA = rbind(DATA,preData)
    #DATA = rbind(preData,preData)
    
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

#### ====================================== OUTPUT WRITING ===================================
            
output = paste("output/CSD_",format(Sys.time(),"%Y-%m-%d-%H%M"), ".csv", sep = "")          
write.csv(DATA, file = output)
print("Cost System Design FILE has been written")
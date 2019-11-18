#############################################################
# Initizalization of the CostSystemDesignSim (CSDS)
#############################################################



## ======================================INPUT MASK============================================================
  FIRM = list()                           
  FIRM$PRODUCTION_ENVIRONMENT = list()
  FIRM$COSTING_SYSTEM = list()
  DATA = data.frame()
  DATAp = data.frame()
  
  
  NUMB_PRO =         50                     #INPUT independent Variable - Number of products 
  NUMB_RES  =        50                     #INPUT independent variable - Number of factors

  SIM_NUMB =         200                   #Control Variable - Number of Simulations for every single environment (standard: 30)     

  TC =               1000000                #Total costs


  ProductCostOutput= 0                      #Control Variable -  Zero = no tracking of the product level
  set_PE_constant=   0                      #Control Variable -  Decide if genProduction environment is fixed: Using the same firm.

  dec_ERROR=         1                      #Control Variable - 

  #dec_DC=           0                      # = no direct costs 
  dec_CP=            1                      # =
  dec_CD=            1                      # =
  
  
  CP = c(1,2,4,6,8,10,12,14,16,18,20)       #No. of Cost Pools
  COR = c(0.6)                              #Correlation between resources
  RC_VAR =  c(-1)                           #Resource cost variation --> base for DISP2
  Q_VAR = c(0.4)                              #Demand variation
  Error = c(0)                            #Measurement error
  NUMB_Error = c(1)                         #Number of errornoues links
  DENS = c(-1)                              #Number of links between products and resources (sharing)
  CC = c(0.4)                               #Correlation Cutoff for correlative assignement in CP HEURISTICS
  MISCPOOLSIZE = c(0.25)                    #share of total costs that are supposed to go into the miscpool if there is a miscpool in the Costing System
  DISP1 = c(10)                             #No. of the biggest resources that have a DISP2 share of the total costs
  
## ======================================END OF INPUT MASK=====================================================                           

            set.seed(13) #Reproducability
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
    
    ####   !!!!! Normalerweise könnten wir Select-Case nutzen um die verschiedenen Heuristiken besser auszuwählen..  !!!! ####
    
    FIRM = gen_ProductionEnvironment(FIRM,set_PE_constant) #Generate Production Environment with RES_CONS_PAT

  
    FIRM = MAP_RES_CP_SIZE_CORREL_MISC_ANAND(FIRM) #Building the cost pools
  
  
    FIRM = MAP_CP_P_BIGPOOL(FIRM,Error) #Selecting the drivers of a cost pool
    
    ## Calculating the estimated product costs
    
    FIRM$COSTING_SYSTEM$PCH =  FIRM$COSTING_SYSTEM$ACT_CONS_PAT %*% FIRM$COSTING_SYSTEM$ACP # CHECKED 2019/09/12
  
    ## ERROR MEASURES AFTER LABRO & VANHOUCKE 2007 
    EUCD = round(sqrt(sum((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)^2)),digits=2)
    MAPE = round(mean(abs(FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)/FIRM$COSTING_SYSTEM$PCB),digits=4)
    MSE = round(mean(((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)^2)),digits=2);
   
    
    
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
                         EUCD,
                         MAPE,
                         MSE)
  
    #preData_p = .datalogging()
    colnames(preData) = c('o','nn','CP','RCC_VAR', 'NUMB_ME', 'NUMB_ME_AD','DENS', 'COR', 'Q_VAR', 
                       'NUMB_PRO', 'NUMB_RES','CHECK_RCC20','CHECK_RCC10','CHECK_RCC02','CHECK_Q20',
                       'CHECK_Q10','CHECK_Q02','CHECK_NonZeroCons','CHECK_countNonZero','CHECK_COR1','CHECK_COR2'
                       ,'EUCD','MAPE','MSE')  
   
    #stacking the data with each run
    DATA = rbind(DATA,preData)
       
    # TRACKING THE PRODUCT LEVEL WHEN NEEDED
    if (ProductCostOutput==1){DATAp = .datalogging(o,nn,FIRM,DATAp)}
   
    #Print outputs;
    print(o)
    print(FIRM$COSTING_SYSTEM$CP)
    print((EUCD))
    
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

check = aggregate(DATA,list(DATA$CP),mean)
plot(check$MAPE,type ='l')
print(mean(check$CHECK_RCC20))
print(mean(check$CHECK_RCC10))
print(mean(check$CHECK_RCC02))
if (ProductCostOutput==1)
{
  output = paste("output/ProductCost_",format(Sys.time(),"%Y-%m-%d-%H%M"), ".csv", sep = "")          
  write.csv(DATAp, file = output)
  print("Product costs FILE has been written")
}

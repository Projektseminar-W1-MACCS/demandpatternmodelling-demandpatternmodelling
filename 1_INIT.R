#############################################################
# Initizalization of the CostSystemDesignSim (CSDS)
#############################################################


## ======================================INPUT MASK============================================================
  FIRM = list()
  FIRM$PRODUCTION_ENVIRONMENT = list()
  FIRM$COSTING_SYSTEM = list()
  DATA = data.frame()
  
  
  NUMB_PRO =         50      #INPUT independent Variable - Number of products 
  NUMB_RES  =        50    #INPUT independent variable - Number of factors
  SIM_NUMB =         10       #Control Variable - Number of Simulations for every single environment (standard: 30)     
  tt   =             1       #Periods
  TC =               1000000  #Total costs

  ProductOutput=     1       #Zero = no tracking
  set_pe_constant=   1       #Control Variable -  Decide if Simulation is reproducible {1} or random {0}
  set_cs_constant=   0       #Control Variable 
  vary_demand =      0       #Control Variable
  
  
  dec_ERROR=         1       #Control Variable - 
  seed=              13      #Control Variable -
  
 #dec_DC=            0       # = no direct costs 
  dec_CP=            1       # =
  dec_CD=            1       # =
  
  
  
  
  
  CP = c(1,5,10,15,20,25,30)
  COR = c(0)
  RC_VAR =  c(0.55)
  Q_VAR = c(0.5,1,1.5)
  Error = c(0.1,0.3,0.5)
  NUMB_Error = c(1)
  DENS = c(0,35,0.6,0.85)
  
## ======================================END OF INPUT MASK=====================================================                           

            set.seed(seed)
            o=1
## ====================================== DESIGN OF EXPERIMENTS ================================================== 
## EVIRONMENTAL FACTORS [] 
  for (ix_CP in seq_along(CP)) {
     for (ix_COR in seq_along(COR)) {
       for (ix_RC_VAR in seq_along(RC_VAR)) {
         for (ix_Q_VAR in seq_along(Q_VAR)) {
           for (ix_Error in seq_along(Error)) {
             for (ix_NUMB_Error in seq_along(NUMB_Error)) {
               for (ix_DENS in seq_along(DENS)) {
                 
                 
## ===================================== DETERMINE PRODUCTION ENVIRONMENT AND COSTING SYSTEM =======================             
  
                 
  FIRM$PRODUCTION_ENVIRONMENT$DENS = DENS[ix_DENS]   
  FIRM$PRODUCTION_ENVIRONMENT$COR  = COR[ix_COR]
  FIRM$PRODUCTION_ENVIRONMENT$Q_VAR= Q_VAR[ix_Q_VAR]
  FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO = NUMB_PRO
  FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES = NUMB_RES
  FIRM$COSTING_SYSTEM$CP = CP[ix_CP]
  FIRM$COSTING_SYSTEM$RC_VAR = RC_VAR[ix_RC_VAR]
  FIRM$COSTING_SYSTEM$Error = Error[ix_Error]
  FIRM$COSTING_SYSTEM$NUMB_Error = NUMB_Error[ix_NUMB_Error]
  FIRM$COSTING_SYSTEM$TC = TC 

#if ( dec_CP==1) {
#} else if ( dec_CP==2) {
#   #statement2
# } else {
#   printf("Blub")}
              
nn=1 # necessary for repeating the SIM_NUMB loop
## SIMULATION ROUTINE ===
for (nn in 1:SIM_NUMB) {
  
  
  #print(FIRM$COSTING_SYSTEM$CP)  
  #print(FIRM$COSTING_SYSTEM$Error)  
  
  FIRM = gen_ProductionEnvironment(FIRM)
  
  FIRM = MAP_RES_CP_RANDOM(FIRM)
  FIRM = MAP_CP_PRO(FIRM,method= "BIG-POOL",Error)
  
  FIRM$COSTING_SYSTEM$PCH =  apply((FIRM$COSTING_SYSTEM$ACP) * t(FIRM$COSTING_SYSTEM$ACT_CONS_PAT),2,sum) # CHECKED 2019/09/12
  #                 EUCD = sqrt(sum(((PC_B-PC_H).^2)));  
  #                 MSE = (mean((PC_B - PC_H).^2));
  #                 MPE = sum((abs(PC_B-PC_H)./PC_B))./NUMB_PRO;
  
  
  
  ## ERROR MEASURES AFTER LABRO & VANHOUCKE 2007 
  EUCD<-round(sqrt(sum((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)^2)),digits=2)
  MPE <- round(mean(abs(FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)/FIRM$COSTING_SYSTEM$PCB),digits=4)
   MSE = round(mean(((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)^2)),digits=2);

  #COLLECTING THE DATA FOR OUTPUT;
preData = data.frame(o,nn,FIRM$COSTING_SYSTEM$CP,FIRM$COSTING_SYSTEM$RC_VAR, FIRM$COSTING_SYSTEM$NUMB_Error, FIRM$COSTING_SYSTEM$Error,
                       FIRM$PRODUCTION_ENVIRONMENT$DENS, FIRM$PRODUCTION_ENVIRONMENT$COR, FIRM$PRODUCTION_ENVIRONMENT$Q_VAR, FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,
                       FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES,EUCD,MPE,MSE)
colnames(DATA) = c('o','nn','CP','RCC_VAR', 'NUMB_ME', 'NUMB_ME_AD','DENS', 'COR', 'Q_VAR', 'NUMB_PRO', 'NUMB_RES' ,'EUCD','MPE','MSE')  
  # Stacking the data with each run
DATA = rbind(DATA,preData) 
  
##  DATA COLLECTION END  
  
  
  
  
  
  print(o)
  print((nn))
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

## ====================================== OUTPUT WRITING ===================================
            
            
output = paste("output/CDSD_",format(Sys.time(),"%Y-%m-%d-%H%M"), ".csv", sep = "")          
write.csv(DATA, file = output)
print("Cost System Design FILE has been written")









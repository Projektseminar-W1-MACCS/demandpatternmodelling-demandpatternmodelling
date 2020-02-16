################################################
# TRACKING THE DATA
################################################


.system_datalogging <- function(o,nn,FIRM,DATA){
  
  #### ======== COLLECTING THE DATA FOR OUTPUT ==== ####
  preData = data.frame(o,
                       nn,
                       FIRM$COSTING_SYSTEM$CP_HEURISTIC,
                       FIRM$COSTING_SYSTEM$CD_HEURISTIC,
                       FIRM$COSTING_SYSTEM$CP,
                       FIRM$COSTING_SYSTEM$RC_VAR,
                       FIRM$COSTING_SYSTEM$NUMB_Error,
                       FIRM$COSTING_SYSTEM$Error,
                       FIRM$PRODUCTION_ENVIRONMENT$DENS,
                       FIRM$PRODUCTION_ENVIRONMENT$COR1,
                       FIRM$PRODUCTION_ENVIRONMENT$COR2,
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
                       OC,
                       UC,
                       OC5,
                       UC5,
                       EUCD,
                       MAPE,
                       MSE)

  #preData_p = .datalogging()
  colnames(preData) = c('o','nn','CPH','CDH','CP','RC_VAR', 'NUMB_ME', 'ME_AD','DENS', 'COR1','COR2', 'Q_VAR',
                        'NUMB_PRO', 'NUMB_RES','CHECK_RCC20','CHECK_RCC10','CHECK_RCC02','CHECK_Q20',
                        'CHECK_Q10','CHECK_Q02','CHECK_NonZeroCons','CHECK_countNonZero','CHECK_COR1','CHECK_COR2',
                        'MISCPOOL','OC','UC','OC5','UC5','EUCD','MAPE','MSE')

  
  #stacking the data with each run
  DATA = rbind(DATA,preData)
  
  return(DATA)
  
}


.product_datalogging<-function(o,nn,FIRM,DATAp,CP_HEURISTIC,CD_HEURISTIC){

  NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO

  CPH <- vector()
  CDH <- vector()
  PRODUCT <- vector()
  DENS <- vector()
  RCC_VAR <- vector()
  CP <- vector()
  Error <-vector()
  NUMB_Error <-vector()
  CC <- vector()
  MISCPOOLSIZE <- vector()
  RUN <- vector()
  DESIGN <- vector ()
  PCb <- vector()
  PCh <- vector()
  Q <- vector()
  
  
  
  
  PRODUCT <- c(PRODUCT, 1:NUMB_PRO) #How many products per run 
  
  CPH[PRODUCT]= FIRM$COSTING_SYSTEM$CP_HEURISTIC
  CDH[PRODUCT] =FIRM$COSTING_SYSTEM$CD_HEURISTIC
  DENS[PRODUCT] = FIRM$PRODUCTION_ENVIRONMENT$DENS #Scaling firm parameter to products.
  Q_VAR[PRODUCT] = FIRM$PRODUCTION_ENVIRONMENT$Q_VAR #Scaling firm parameter to products.
  RCC_VAR[PRODUCT] = FIRM$COSTING_SYSTEM$RC_VAR #Scaling firm parameter to products.
  CP[PRODUCT] = FIRM$COSTING_SYSTEM$CP #Scaling firm parameter to products.
  Error[PRODUCT] = FIRM$COSTING_SYSTEM$Error #Scaling firm parameter to products.
  NUMB_Error[PRODUCT] = FIRM$COSTING_SYSTEM$NUMB_Error #Scaling firm parameter to products.
  CC[PRODUCT] = FIRM$COSTING_SYSTEM$CC
  MISCPOOLSIZE[PRODUCT] = FIRM$COSTING_SYSTEM$MISCPOOLSIZE
  RUN[PRODUCT] = o #run, repetition
  DESIGN[PRODUCT] = nn #which kind of design? 
  
  
  PE = (FIRM$COSTING_SYSTEM$PCH - FIRM$COSTING_SYSTEM$PCB)/FIRM$COSTING_SYSTEM$PCB
  APE = abs((FIRM$COSTING_SYSTEM$PCH - FIRM$COSTING_SYSTEM$PCB))/FIRM$COSTING_SYSTEM$PCB
  PCb[PRODUCT] = FIRM$COSTING_SYSTEM$PCB
  PCh[PRODUCT] = FIRM$COSTING_SYSTEM$PCH
  Q[PRODUCT] = FIRM$PRODUCTION_ENVIRONMENT$DEMAND
  
  DATApre = data.frame(o,nn,PRODUCT,PCb,PCh,Q,PE,APE,DENS,Q_VAR,RCC_VAR,CP,Error,NUMB_Error,CC,MISCPOOLSIZE,CPH,CDH) # construct the dataframe 
  
  DATAp = rbind(DATAp,DATApre) #put it together
  
  return(DATAp)
}

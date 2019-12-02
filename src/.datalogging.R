################################################
# TRACKING THE DATA
################################################


.system_datalogging <- function(o,nn,FIRM,DATA,CP_HEURISTIC,CD_HEURISTIC){
  
  #### ======== COLLECTING THE DATA FOR OUTPUT ==== ####
  preData = data.frame(o,
                       nn,
                       CP_HEURISTIC,
                       CD_HEURISTIC,
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
  colnames(preData) = c('o','nn','CPH','CDH','CP','RCC_VAR', 'NUMB_ME', 'ME_AD','DENS', 'COR', 'Q_VAR', 
                        'NUMB_PRO', 'NUMB_RES','CHECK_RCC20','CHECK_RCC10','CHECK_RCC02','CHECK_Q20',
                        'CHECK_Q10','CHECK_Q02','CHECK_NonZeroCons','CHECK_countNonZero','CHECK_COR1','CHECK_COR2',
                        'MISCPOOL','EUCD','MAPE','MSE')  
  
  #stacking the data with each run
  DATA = rbind(DATA,preData)
  
  return(DATA)
  
}


.product_datalogging<-function(o,nn,FIRM,DATAp){
  
  NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  
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
  
  DATApre = data.frame(o,nn,PRODUCT,PCb,PCh,Q,PE,APE,DENS,Q_VAR,RCC_VAR,CP,Error,NUMB_Error,CC,MISCPOOLSIZE) # construct the dataframe 
  
  DATAp = rbind(DATAp,DATApre) #put it together
  
  return(DATAp)
}

.input_datalogging <- function(FIRM,Input_DATA){
  
  
  Input_DATA = data.frame(FIRM$PRODUCTION_ENVIRONMENT$DENS,
                          FIRM$PRODUCTION_ENVIRONMENT$DENS_MIN,
                          FIRM$PRODUCTION_ENVIRONMENT$DENS_MAX,
                          FIRM$PRODUCTION_ENVIRONMENT$COR,
                          FIRM$PRODUCTION_ENVIRONMENT$Q_VAR,
                          FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,
                          FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES,
                          FIRM$PRODUCTION_ENVIRONMENT$DISP1,
                          FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE_MIN,
                          FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE_MAX,
                          FIRM$COSTING_SYSTEM$RC_VAR_MIN,
                          FIRM$COSTING_SYSTEM$RC_VAR_MAX,
                          FIRM$COSTING_SYSTEM$Error,
                          FIRM$COSTING_SYSTEM$NUMB_Error,
                          FIRM$COSTING_SYSTEM$CC,
                          FIRM$COSTING_SYSTEM$MISCPOOLSIZE,
                          FIRM$COSTING_SYSTEM$CP_HEURISTIC,
                          FIRM$COSTING_SYSTEM$CD_HEURISTIC)
    
   colnames(Input_DATA) = c('DENS','DENS_MIN','DENS_MAX','COR','Q_VAR','NUMB_PRO','NUMB_RES','DISP1','UNITLEVEL_ACT_SHARE_MIN',
                            'UNITLEVEL_ACT_SHARE_MAX','RC_VAR_MIN','RC_VAR_MAX','ERROR','NUMB_ERROR','CC','MISCPOOLSIZE','CP_HEURISTIC','CD_HEURISTIC')
   
   Input_DATA = t(Input_DATA)
   
   return(Input_DATA)
    
}




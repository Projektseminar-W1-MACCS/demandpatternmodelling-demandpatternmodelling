################################################
# TRACKS THE PRODUCT LEVEL
################################################

.datalogging<-function(o,nn,FIRM,DATAp)
{
  
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
  
  
  
  browser()
  DATApre = data.frame(o,nn,PRODUCT,PCb,PCh,PE,APE,DENS,Q_VAR,RCC_VAR,CP,Error,NUMB_Error,CC,MISCPOOLSIZE) # construct the dataframe 
  
  DATAp = rbind(DATAp,DATApre) #put it together
  
  return(DATAp)
}




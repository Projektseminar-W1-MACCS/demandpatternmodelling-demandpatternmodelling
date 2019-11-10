################################################
# TRACKS THE PRODUCT LEVEL
################################################

.datalogging<-function(o,nn,FIRM,DATAp)
{
  
  NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  
  PRODUCT <- vector()
  RUN <- vector()
  DESIGN <- vector ()
  
  
  
  PRODUCT <- c(PRODUCT, 1:NUMB_PRO) #How many products per run 
  DENS[PRODUCT] = FIRM$PRODUCTION_ENVIRONMENT$DENS #Scaling firm parameter to products.
  RUN[PRODUCT] = o #run, repetition
  DESIGN[PRODUCT] = nn #which kind of design? 
  
  
  DATApre = data.frame(o,nn,DENS) # construct the dataframe 
  
  DATAp = rbind(DATAp,DATApre) #put it together
  
  return(DATAp)
}




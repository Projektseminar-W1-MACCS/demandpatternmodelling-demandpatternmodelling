#####################################################
# BUILDING A RESOURCE COST VECTOR
#####################################################

.gen_RCC <- function(PRODUCTION_ENVIRONMENT,COSTING_SYSTEM,unitsize,nonunitsize) {

# 
  RC_VAR_MIN = 0.4
  RC_VAR_MAX = 0.7
  RC_VAR = RC_VAR_MIN + (RC_VAR_MAX-RC_VAR_MIN)*runif(1)
  
# NON-UNIT-LEVEL COST SHARE 
  RES_BATCH_COST_MIN = 0.2;
  RES_BATCH_COST_MAX = 0.5;
  PER_BATCH= RES_BATCH_COST_MIN + (RES_BATCH_COST_MAX-RES_BATCH_COST_MIN)*runif(1); # Uniform distribution (0,1)
  TC=COSTING_SYSTEM$TC
# COST SHAR
  TC_BATCH = round(TC*PER_BATCH)
  TC_UNIT  = TC - TC_BATCH;
  
  p_UNIT = rlnorm(unitsize, meanlog = 0, sdlog = COSTING_SYSTEM$RC_VAR) #%DRAW RANDOM NUMBER 
  p_BATCH = rlnorm(nonunitsize, meanlog=0, sdlog= COSTING_SYSTEM$RC_VAR); #%DRAW RANDOM NUMBER 
  
  RC_UNIT = (p_UNIT/sum(p_UNIT))*TC_UNIT
  RC_BATCH =(p_BATCH/sum(p_BATCH))*TC_BATCH
  RC = c(RC_UNIT,RC_BATCH)
  
  
  
  return(RC)
  
  
}

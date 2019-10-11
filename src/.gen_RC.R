#####################################################
# BUILDING A RESOURCE COST VECTOR
#####################################################

.gen_RCC <- function(FIRM,unitsize,nonunitsize) {

# INIT 
  RC_VAR_MIN = 0.4
  RC_VAR_MAX = 0.7
  RC_VAR = RC_VAR_MIN + (RC_VAR_MAX-RC_VAR_MIN)*runif(1)
  
# NON-UNIT-LEVEL COST SHARE DETERMINED Ittner et al. 1997
  RES_BATCH_COST_MIN = 0.2
  RES_BATCH_COST_MAX = 0.5
  PER_BATCH= RES_BATCH_COST_MIN + (RES_BATCH_COST_MAX-RES_BATCH_COST_MIN)*runif(1) # Uniform distribution (0,1)
  TC=FIRM$COSTING_SYSTEM$TC
  
# COST SHARE
  TC_BATCH = round(TC*PER_BATCH)  #resources that are measured in batches
  TC_UNIT  = TC - TC_BATCH;       #resources whose consumption is proportional to the number of units made
  
# Draw from a lognornmal Function
  p_UNIT = rlnorm(unitsize, meanlog = 0, sdlog = FIRM$COSTING_SYSTEM$RC_VAR) #%DRAW RANDOM NUMBER as costs per one unit of each resource
  p_BATCH = rlnorm(nonunitsize, meanlog=0, sdlog= FIRM$COSTING_SYSTEM$RC_VAR); #%DRAW RANDOM NUMBER 

  
# 
  RC_UNIT = (p_UNIT/sum(p_UNIT))*TC_UNIT #share of every unit resource costs multiplied with total unit costs
  RC_BATCH =(p_BATCH/sum(p_BATCH))*TC_BATCH #share of every batch resource costs multiplied with total unit costs
  RCC = c(RC_UNIT,RC_BATCH)  #same order as RES_CONS_PAT
  
  
  
  return(RCC)
  
  
}

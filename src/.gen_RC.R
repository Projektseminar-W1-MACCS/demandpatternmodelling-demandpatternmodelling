## BUILDING A RESOURCE COST VECTOR ##



.gen_RCC<- function(FIRM, unitsize, nonunitsize) {
  # INIT
  NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES
  TC = FIRM$COSTING_SYSTEM$TC
  RC_VAR = FIRM$COSTING_SYSTEM$RC_VAR
  
  if (RC_VAR == -1)
  {
    RC_VAR_MIN = 0.4
    RC_VAR_MAX = 0.7
    RC_VAR = runif(1, RC_VAR_MIN, RC_VAR_MAX)
  }
  FIRM$COSTING_SYSTEM$RC_VAR_draw = RC_VAR

  preRCC = rlnorm(NUMB_RES, meanlog = 1, sdlog = RC_VAR)
  #preRCC = rbeta(NUMB_RES, 0.025,1)
  
  RCC = (preRCC/sum(preRCC))*TC #normalizing it #ceiled realized demand for each product
  
  
  # Move the biggest resource to the front
  largest_RC <-
     sort(RCC, decreasing = TRUE, index.return = TRUE)$ix[1]
  RCC <- c(RCC[largest_RC], RCC[-largest_RC])

  ###CHECK###
  RCCs = sort(RCC, decreasing = TRUE)
  
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$RCC20 = sum(RCCs[1:(0.2 * length(RCC))])/TC     #size of 20% biggest resources
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$RCC10 = sum(RCCs[1:(0.1 * length(RCC))])/TC     #size of 10% biggest resources
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$RCC02 = sum(RCCs[1:(0.02 * length(RCC))])/TC    #size of 2% biggest resources
  
  #plot(sort(RCC))
  #### sourcing
  FIRM$COSTING_SYSTEM$RCU = RCC/FIRM$PRODUCTION_ENVIRONMENT$TRU
  FIRM$COSTING_SYSTEM$RCC = RCC
  return(FIRM)
  
}

# Building variable / fix costs Mertens (2020)
.gen_RCC_unit <- function(FIRM, unitsize, nonunitsize) {
  # INIT
  
  unitsize = FIRM$PRODUCTION_ENVIRONMENT$UNITSIZE
  nonunitsize =FIRM$PRODUCTION_ENVIRONMENT$NONUNITSIZE
  RC_VAR = FIRM$COSTING_SYSTEM$RC_VAR
  
  
  if (FIRM$COSTING_SYSTEM$RC_VAR == -1)
  {
    RC_VAR_MIN = 0.4
    RC_VAR_MAX = 0.7
    RC_VAR = runif(1, RC_VAR_MIN, RC_VAR_MAX)
  }
  FIRM$COSTING_SYSTEM$RC_VAR_draw = RC_VAR
  
  
  
  # NON-UNIT-LEVEL COST SHARE DETERMINED Ittner et al. (1997)
  RES_BATCH_COST_MIN = 0.2
  RES_BATCH_COST_MAX = 0.5
  PER_BATCH = runif(1, RES_BATCH_COST_MIN,RES_BATCH_COST_MAX) # Uniform distribution (U[RES_BATCH_COST_MIN;RES_BATCH_COST_MAX])
  TC = FIRM$COSTING_SYSTEM$TC
  
  # COST SHARE
  TC_BATCH = round(TC * PER_BATCH)  #resources that are measured in batches
  TC_UNIT  = TC - TC_BATCH
  #resources whose consumption is proportional to the number of units made
  
  # Draw from a lognornmal Function
  p_UNIT = abs(rlnorm(
    unitsize,
    meanlog = 0,
    sdlog = (FIRM$COSTING_SYSTEM$RC_VAR)
  )) #%DRAW RANDOM NUMBER as costs per one unit of each resource
  
  
  
  
  p_BATCH = abs(rlnorm(
    nonunitsize,
    meanlog = 0,
    sdlog = (FIRM$COSTING_SYSTEM$RC_VAR)                
  ))
  #%DRAW RANDOM NUMBER
  
  
  #
  RC_UNIT = (p_UNIT / sum(p_UNIT)) * TC_UNIT #share of every unit resource costs multiplied with total unit costs
  RC_BATCH = (p_BATCH / sum(p_BATCH)) * TC_BATCH #share of every batch resource costs multiplied with total unit costs
  RCC = c(RC_UNIT, RC_BATCH)  #put the vectors together
  
  
 
  ## Move the biggest resource to the front
  largest_RC <-
    sort(RCC, decreasing = TRUE, index.return = TRUE)$ix[1]
  RCC <- c(RCC[largest_RC], RCC[-largest_RC])
  
  
  
  ###CHECK###
  RCCs = sort(RCC, decreasing = TRUE)
  
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$RCC20 = sum(RCCs[1:(0.2 * length(RCC))])/TC     #size of 20% biggest resources
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$RCC10 = sum(RCCs[1:(0.1 * length(RCC))])/TC     #size of 10% biggest resources
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$RCC02 = sum(RCCs[1:(0.02 * length(RCC))])/TC    #size of 2% biggest resources
  
  
  
  
  #### sourcing
  FIRM$COSTING_SYSTEM$RCU = RCC/FIRM$PRODUCTION_ENVIRONMENT$TRU
  FIRM$COSTING_SYSTEM$RCC = RCC
  
  return(FIRM)
  
}

.gen_RCC_Anand <- function(FIRM, unitsize, nonunitsize) {

  RC_VAR = FIRM$COSTING_SYSTEM$RC_VAR
  #RC_VAR =-1
  if (RC_VAR == -1)
  {
    DISP2_MIN = 0.4               #if bounds between 0.5 and 0.8 this could be an equivilent to per_BATCH costs in gen_RCC_unit
    DISP2_MAX = 0.7
    DISP2 = runif(1, DISP2_MIN, DISP2_MAX)
    FIRM$COSTING_SYSTEM$RC_VAR_MIN = DISP2_MIN
    FIRM$COSTING_SYSTEM$RC_VAR_MAX = DISP2_MAX
  }else{
    DISP2 = FIRM$COSTING_SYSTEM$RC_VAR
  }
  
  FIRM$COSTING_SYSTEM$RC_VAR_draw = DISP2
  
  #FIRM$PRODUCTION_ENVIRONMENT$CHECK$DISP2 = RC_VAR
  
  DISP1 = FIRM$PRODUCTION_ENVIRONMENT$DISP1
  TC = FIRM$COSTING_SYSTEM$TC
  NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES
  
  # Step 1
  r_MIN <- ((1 - DISP2) * TC) / (NUMB_RES - DISP1)
  
  #Step 2
  r1_MAX <- (DISP2 * TC) - ((DISP1 - 1) * r_MIN)
  
  # Step 3
  r_MIN <- r_MIN + (r1_MAX - r_MIN) * 0.025   #0.025?
  
  ## Step 4
  #Initalize Values
  RCC <- vector(mode = "numeric")
  r_MAX <- vector(mode = "numeric")
  temp1_ADD <- vector(mode = "numeric", length = DISP1 - 1)
  temp1_ADD[1] <- 0
  
  
  for (i in 1:(DISP1 - 1)) {
    r_MAX[i] <- (DISP2 * TC - sum(temp1_ADD)) - (DISP1 - i) * r_MIN
    
    RCC[i] <- runif(1, min = r_MIN, max = r_MAX[i])
    temp1_ADD[i] <- RCC[i]
    
    
  }
  
  ## The final element is computed to ensure that the total rescource cost is exactly DISP2*TC
  RCC <- c(RCC, DISP2 * TC - sum(temp1_ADD))
  
  ## Move the biggest resource to the front
  largest_RC <-
    sort(RCC, decreasing = TRUE, index.return = TRUE)$ix[1]
  RCC <- c(RCC[largest_RC], RCC[-largest_RC])
  
  
  #### Generate Small Rescources ####
  
  RC_small <-
    runif(length((length(RCC) + 1):NUMB_RES), min = 0.05, max =
            0.95)
  RC_small <- RC_small / sum(RC_small) #normalize
  RC_small <- RC_small * (1 - DISP2) * TC
  
  
  ## Some Checks ##
  # Sum of first DISP1 resources not correct.
  # if(min(RC)> ((1-DISP2)*TC)/(NUMB_RES-DISP1)){
  
  while (max(RC_small) - min(RCC) > 1.0) {
    RC_small <- sort(RC_small, decreasing = TRUE)
    min_bigRes <- min(RCC)
    for (i in 1:(length(RC_small))) {
      overage <- max(c(RC_small[i] - min_bigRes , 0))
      RC_small[i] <- RC_small[i] - overage
      RC_small[length(RC_small) - i + 1] <-
        RC_small[length(RC_small) - i + 1] + overage
    }
  }
  
  
  # Step 6 Schuffle small rescources
  RC_small <- RC_small[sample(length(RC_small))]
  RCC <- c(RCC, RC_small)
  
  # sum(RC)
  RCCs <- sort(RCC, decreasing = TRUE, index.return = TRUE)
  
  RCC <-
    list(
      RCC = RCC,
      CHECK = list(
        cost_largestRCP = RCCs$x[1] / RCCs$x[NUMB_RES],
        cost_topTEN = sum(RCCs$x[1:10]) / TC,
        DISP1 = DISP1,
        DISP2 = DISP2,
        RC_VAR = RC_VAR
      )
    )
  
  RCC = RCC$RCC
  
  
  ###CHECK###
  RCCs = sort(RCC, decreasing = TRUE)
  
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$RCC20 = sum(RCCs[1:(0.2 * length(RCC))])/TC     #size of 20% biggest resources (10)
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$RCC10 = sum(RCCs[1:(0.1 * length(RCC))])/TC     #size of 10% biggest resources (5)
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$RCC02 = sum(RCCs[1:(0.02 * length(RCC))])/TC    #size of 2% biggest resources (1)
  
  
  #### sourcing
  FIRM$COSTING_SYSTEM$RCC = RCC
  
  
  
  
  # '''' checked and compared with Anand et al. 2019 - 30/10/2019
  
  return(FIRM)
  
}



###IF RCU IS SET FIX#####

.gen_RCC_variation <- function(FIRM, unitsize, nonunitsize) {
  
  FIRM$COSTING_SYSTEM$RC_VAR = RC_VAR
  PRE_TC = 1000000 #former TC but helps to level our resource cost drivers. 
  #RC_VAR =-1
  if (RC_VAR == -1)
  {
    DISP2_MIN = 0.4               #if bounds between 0.5 and 0.8 this could be an equivilent to per_BATCH costs in gen_RCC_unit
    DISP2_MAX = 0.7
    DISP2 = runif(1, DISP2_MIN, DISP2_MAX)
    FIRM$COSTING_SYSTEM$RC_VAR_MIN = DISP2_MIN
    FIRM$COSTING_SYSTEM$RC_VAR_MAX = DISP2_MAX
  }else{
    DISP2 = FIRM$COSTING_SYSTEM$RC_VAR
  }
  
  FIRM$COSTING_SYSTEM$RC_VAR = DISP2
  #FIRM$PRODUCTION_ENVIRONMENT$CHECK$DISP2 = RC_VAR
  
  DISP1 = FIRM$PRODUCTION_ENVIRONMENT$DISP1

  NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES
  
  # Step 1
  r_MIN <- ((1 - DISP2) * PRE_TC) / (NUMB_RES - DISP1)
  
  #Step 2
  r1_MAX <- (DISP2 * PRE_TC) - ((DISP1 - 1) * r_MIN)
  
  # Step 3
  r_MIN <- r_MIN + (r1_MAX - r_MIN) * 0.025   #0.025?
  
  ## Step 4
  #Initalize Values
  PRE_RCC <- vector(mode = "numeric")
  r_MAX <- vector(mode = "numeric")
  temp1_ADD <- vector(mode = "numeric", length = DISP1 - 1)
  temp1_ADD[1] <- 0
  
  
  for (i in 1:(DISP1 - 1)) {
    r_MAX[i] <- (DISP2 * PRE_TC - sum(temp1_ADD)) - (DISP1 - i) * r_MIN
    
    PRE_RCC[i] <- runif(1, min = r_MIN, max = r_MAX[i])
    temp1_ADD[i] <- PRE_RCC[i]
    
    
  }
  
  ## The final element is computed to ensure that the total rescource cost is exactly DISP2*TC
  PRE_RCC <- c(PRE_RCC, DISP2 * PRE_TC - sum(temp1_ADD))
  
  ## Move the biggest resource to the front
  largest_RC <-
    sort(PRE_RCC, decreasing = TRUE, index.return = TRUE)$ix[1]
  PRE_RCC <- c(PRE_RCC[largest_RC], PRE_RCC[-largest_RC])
  
  
  #### Generate Small Rescources ####
  
  RC_small <-
    runif(length((length(PRE_RCC) + 1):NUMB_RES), min = 0.05, max =
            0.95)
  RC_small <- RC_small / sum(RC_small) #normalize
  RC_small <- RC_small * (1 - DISP2) * PRE_TC
  
  
  ## Some Checks ##
  # Sum of first DISP1 resources not correct.
  # if(min(RC)> ((1-DISP2)*TC)/(NUMB_RES-DISP1)){
  
  while (max(RC_small) - min(PRE_RCC) > 1.0) {
    RC_small <- sort(RC_small, decreasing = TRUE)
    min_bigRes <- min(PRE_RCC)
    for (i in 1:(length(RC_small))) {
      overage <- max(c(RC_small[i] - min_bigRes , 0))
      RC_small[i] <- RC_small[i] - overage
      RC_small[length(RC_small) - i + 1] <-
        RC_small[length(RC_small) - i + 1] + overage
    }
  }
  
  
  # Step 6 Schuffle small rescources
  RC_small <- RC_small[sample(length(RC_small))]
  PRE_RCC <- c(PRE_RCC, RC_small)
  
  # sum(RC)
  PRE_RCCs <- sort(PRE_RCC, decreasing = TRUE, index.return = TRUE)
  
  PRE_RCC <-
    list(
      PRE_RCC = PRE_RCC,
      CHECK = list(
        cost_largestRCP = PRE_RCCs$x[1] / PRE_RCCs$x[NUMB_RES],
        cost_topTEN = sum(PRE_RCCs$x[1:10]) / PRE_TC,
        DISP1 = DISP1,
        DISP2 = DISP2,
        RC_VAR = RC_VAR
      )
    )
  
  PRE_RCC = PRE_RCC$PRE_RCC
  
  
  ##GENERATING RCU--> Copied from gen_Q. to set the RCU fix
  
  PRE_RCU = runif(NUMB_RES,100,400)
  
  RCU = as.vector(PRE_RCC/PRE_RCU)
  
  RCU = runif(NUMB_RES,100,400)
  #### sourcing
  FIRM$COSTING_SYSTEM$RCU = RCU
  
  return(FIRM)
  
}
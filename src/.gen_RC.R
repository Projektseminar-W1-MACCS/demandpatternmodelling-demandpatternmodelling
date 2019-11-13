## BUILDING A RESOURCE COST VECTOR ##


# Building variable / fix costs Mertens (2020)
.gen_RCC_KGM <- function(FIRM, unitsize, nonunitsize) {
  # INIT
  if (RC_VAR == -1)
  {
    RC_VAR_MIN = 0.4
    RC_VAR_MAX = 0.7
    RC_VAR = runif(1, RC_VAR_MIN, RC_VAR_MAX)
    
    FIRM$COSTING_SYSTEM$RC_VAR = RC_VAR
  }
  
  
  # NON-UNIT-LEVEL COST SHARE DETERMINED Ittner et al. (1997)
  RES_BATCH_COST_MIN = 0.2
  RES_BATCH_COST_MAX = 0.5
  PER_BATCH = RES_BATCH_COST_MIN + (RES_BATCH_COST_MAX - RES_BATCH_COST_MIN) *
    runif(1) # Uniform distribution (0,1)
  TC = FIRM$COSTING_SYSTEM$TC
  
  # COST SHARE
  TC_BATCH = round(TC * PER_BATCH)  #resources that are measured in batches
  TC_UNIT  = TC - TC_BATCH
  #resources whose consumption is proportional to the number of units made
  
  # Draw from a lognornmal Function
  p_UNIT = abs(rlnorm(
    unitsize,
    meanlog = 0,
    sdlog = FIRM$COSTING_SYSTEM$RC_VAR
  )) #%DRAW RANDOM NUMBER as costs per one unit of each resource
  p_BATCH = abs(rlnorm(
    nonunitsize,
    meanlog = 0,
    sdlog = FIRM$COSTING_SYSTEM$RC_VAR
  ))
  #%DRAW RANDOM NUMBER
  
  
  #
  RC_UNIT = (p_UNIT / sum(p_UNIT)) * TC_UNIT #share of every unit resource costs multiplied with total unit costs
  RC_BATCH = (p_BATCH / sum(p_BATCH)) * TC_BATCH #share of every batch resource costs multiplied with total unit costs
  RCC = c(RC_UNIT, RC_BATCH)  #put the vectors together
  
  #### sourcing
  FIRM$COSTING_SYSTEM$RCC = RCC
  
  return(FIRM)
  
}

.gen_RCC_Anand <- function(FIRM, unitsize, nonunitsize) {
  if (RC_VAR == -1)
  {
    DISP2_MIN = 0.4
    DISP2_MAX = 0.7
    DISP2 = runif(1, DISP2_MIN, DISP2_MAX)
    #DISP2 = RC_VAR
    # FIRM$COSTING_SYSTEM$RC_VAR = RC_VAR
  }
  
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
  #### sourcing
  FIRM$COSTING_SYSTEM$RCC = RCC
  
  
  
  
  
  # '''' checked and compared with Anand et al. 2019 - 30/10/2019
  
  return(FIRM)
  
}
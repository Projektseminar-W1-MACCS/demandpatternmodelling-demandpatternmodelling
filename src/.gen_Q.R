## BUILDING A REALIZED DEMAND VECTOR ##


.gen_Demand_Pattern <- function(FIRM){
  ####Implementing Demand Pattern into the Production Simulation####
  #...
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###CHECK###
  
  Qs = sort(DEMAND, decreasing = TRUE)
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q20 = sum(Qs[1:(0.2 * NUMB_PRO)])/units        #no. of units of 20% biggest products
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q10 = sum(Qs[1:(0.1 * NUMB_PRO)])/units        #no. of units of 10% biggest products
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q02 = sum(Qs[1:(0.02 * NUMB_PRO)])/units       ##no. of units of 2% biggest products
  
  
  ##Sourcing##
  FIRM$PRODUCTION_ENVIRONMENT$DEMAND = DEMAND
  
  return(FIRM)
}




####----current gen_Demand functions----####


.gen_Demand_Anand <- function(FIRM){
  
  NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  
  units = 10^3

  preDemand = as.integer(runif(NUMB_PRO,10,40))
  
  DEMAND = ceiling((preDemand/sum(preDemand))*units) #normalizing it #ceiled realized demand for each product
  
  FIRM$PRODUCTION_ENVIRONMENT$DEMAND = as.vector(DEMAND)
  
  FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND)
  
  ###CHECK###
  
  Qs = sort(DEMAND, decreasing = TRUE)
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q20 = sum(Qs[1:(0.2 * NUMB_PRO)])/units        #no. of units of 20% biggest products
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q10 = sum(Qs[1:(0.1 * NUMB_PRO)])/units        #no. of units of 10% biggest products
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q02 = sum(Qs[1:(0.02 * NUMB_PRO)])/units       ##no. of units of 2% biggest products

  
  # it is possible that the sum(units) >= units !
  
  return(FIRM)
}

.gen_Demand <- function(FIRM){
  # This has been used in the Mertens (2020) for modeling dispersed realized demand
  Q_VAR = FIRM$PRODUCTION_ENVIRONMENT$Q_VAR
  NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  units = 10^3
  preDemand = rlnorm(NUMB_PRO, meanlog = 1, sdlog = Q_VAR) #preDemand is buildup as a -> LogNormal Distribution 
  FIRM$PRODUCTION_ENVIRONMENT$DEMAND = ceiling((preDemand/sum(preDemand))*units)
  
  FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = Q_VAR
  #FIRM$PRODUCTION_ENVIRONMENT$Q_VAR = sd(FIRM$PRODUCTION_ENVIRONMENT$DEMAND)/mean(FIRM$PRODUCTION_ENVIRONMENT$DEMAND)
  ###CHECK###
  
  Qs = sort(FIRM$PRODUCTION_ENVIRONMENT$DEMAND, decreasing = TRUE)
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q20 = sum(Qs[1:(0.2 * NUMB_PRO)])/units        #no. of units of 20% biggest products
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q10 = sum(Qs[1:(0.1 * NUMB_PRO)])/units        #no. of units of 10% biggest products
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q02 = sum(Qs[1:(0.02 * NUMB_PRO)])/units       ##no. of units of 2% biggest products
  
  
  # rlnorm = disperion = 0.5 
  
  
  return(FIRM)
}
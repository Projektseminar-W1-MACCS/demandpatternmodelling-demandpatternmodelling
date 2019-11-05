####

####

.gen_Demand_Anand <- function(FIRM){
  
  NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  
  units = 10^3

  preDemand = as.integer(runif(NUMB_PRO,10,40))
  
  DEMAND = ceiling((preDemand/sum(preDemand))*units) #ceiled realized demand for each product
  
  FIRM$PRODUCTION_ENVIRONMENT$DEMAND = DEMAND
  
  
  # it is possible that the sum(units) >= units !
  
  return(FIRM)
}


.gen_Demand <- function(FIRM){
  
  Q_VAR = FIRM$PRODUCTION_ENVIRONMENT$Q_VAR
  
  units = 10^3
  preDemand = rlnorm(NUMB_PRO, meanlog = 1, sdlog = Q_VAR) #pre Demand is buildup as a log normal distribution 
  FIRM$PRODUCTION_ENVIRONMENT$DEMAND = ceiling((preDemand/sum(preDemand))*units)

  # rlnorm = disperion = 0.5 
  
  
  return(FIRM)
}
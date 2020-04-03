## BUILDING A REALIZED DEMAND VECTOR ##


.gen_Demand_Pattern <- function(FIRM){
  ####Implementing Demand Pattern into the Production Simulation####
  #hello
  
  
  
  
  ### 1. VERESUCH UNIFORM DISTRIBUTION (ANAND) ###
  
  
  # NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  # 
  # units = 10^3
  # 
  # preDemand = as.integer(runif(NUMB_PRO,1,150))
  # 
  # DEMAND = ceiling((preDemand/sum(preDemand))*units) #normalizing it #ceiled realized demand for each product
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$DEMAND = as.vector(DEMAND)
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND)
  
  
  
  
  ### 2. VERSUCH MERTENS LN ###
  
  
  # Q_VAR = FIRM$PRODUCTION_ENVIRONMENT$Q_VAR
  # NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  # units = 10^3
  # preDemand = rlnorm(NUMB_PRO, meanlog = 1, sdlog = Q_VAR) #preDemand is buildup as a -> LogNormal Distribution
  # 
  # DEMAND = ceiling((preDemand/sum(preDemand))*units)
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$DEMAND = DEMAND
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND)
  
  
  
  
  
  
  
  
  ### 3. VERSUCH NORMALVERTEILUNG ###
  
  # NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  # Q_VAR = FIRM$PRODUCTION_ENVIRONMENT$Q_VAR
  # 
  # units = 10^3
  # 
  # preDemand = as.integer(rnorm(NUMB_PRO, mean =20, sd = Q_VAR))
  # 
  # DEMAND = ceiling((preDemand/sum(preDemand))*units) #normalizing it #ceiled realized demand for each product
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$DEMAND = as.vector(DEMAND)
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND)
  
  
  
  
  
  
  
  
  
  ### 3.1. 2 MAL NORMALVERTEILUNG MIT UNTERSCHIEDLICHER SD ### 
  
  # NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  # Q_VAR = FIRM$PRODUCTION_ENVIRONMENT$Q_VAR
  # 
  # units = 10^3
  # 
  # preDemand1 = as.integer(rnorm((NUMB_PRO / 2), mean =20, sd = 1))
  # 
  # predemand2 = as.integer(rnorm((NUMB_PRO / 2), mean =20, sd = 4))
  # 
  # preDemand = c(preDemand1, predemand2)
  # 
  # DEMAND = ceiling((preDemand/sum(preDemand))*units) #normalizing it #ceiled realized demand for each product
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$DEMAND = as.vector(DEMAND)
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND)
  
  
  
  
  
  
  
  
  
  ### 4. VERSUCH HOHE NACHFRAGEDIFFERENZ ###
  # 
  #   NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  # 
  #   units = 10^3
  # 
  #   preDemand1 = 0
  #   for (i in 1:(NUMB_PRO*0.1)) { # Problem: Ungenauigkeit, falls NumB_Pro * 0.1 nicht Ganzzahlig ist
  #     if (preDemand1[1] == 0){
  #       preDemand1 = 100          # Idee: 100 normalverteil ziehen und so Varianz einbringen.
  #     }else{
  #       preDemand1 = c(preDemand1,100 )
  #     }
  #   }
  # 
  #   a = NUMB_PRO - length(preDemand1)
  # 
  #   preDemand2 = 0
  #   for(i in 1:a){
  # 
  #     if (preDemand2[1] == 0){
  #       preDemand2 = c(10)
  #     }else{
  #       preDemand2 = c(preDemand2, 10)
  #     }
  #   }
  # 
  #   preDemand = c(preDemand1, preDemand2)
  # 
  #   DEMAND = ceiling((preDemand/sum(preDemand))*units) #normalizing it #ceiled realized demand for each product
  # 
  #   FIRM$PRODUCTION_ENVIRONMENT$DEMAND = as.vector(DEMAND)
  # 
  #   
  #   FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND)
  
  
  
  ### 5. GELEICHE NACHFRAGE FÜR JEDES PRODUKT ###
  
  
  NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  
  units = 10^3
  
  a = 20
  
  preDemand = a
  
  for (i in 1: (NUMB_PRO-1)) {
    preDemand = c(preDemand, a) 
  }
  
  DEMAND = ceiling((preDemand/sum(preDemand))*units) #normalizing it #ceiled realized demand for each product
  
  FIRM$PRODUCTION_ENVIRONMENT$DEMAND = as.vector(DEMAND)
  
  FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
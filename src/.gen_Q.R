## BUILDING A REALIZED DEMAND VECTOR ##


.gen_Demand_Pattern <- function(FIRM){
  ####Implementing Demand Pattern into the Production Simulation####
  #hello
  
  
  
  
  ### 1. VERESUCH UNIFORM DISTRIBUTION (ANAND) ###
  
  
  # NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  # 
  # units = 10^3
  # 
  # preDemand = as.integer(runif(NUMB_PRO,10,40))
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
  
  
  
  
  
  
  
  
  
  ### 4. VERSUCH HOHE NACHFRAGEDIFFERENZ (5 mal 100 und 45 mal 10) ###
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
  
  
  
  ### 5. GELEICHE NACHFRAGE FÜR JEDES PRODUKT (konstant 20)###
  
  
  # NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  # 
  # units = 10^3
  # 
  # a = 20
  # 
  # preDemand = a
  # 
  # for (i in 1: (NUMB_PRO-1)) {
  #   preDemand = c(preDemand, a) 
  # }
  # 
  # DEMAND = ceiling((preDemand/sum(preDemand))*units) #normalizing it #ceiled realized demand for each product
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$DEMAND = as.vector(DEMAND)
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND)
  
  
  
  
  
  
  
  
  
  ### 6. Actual Demand von Apple (nach Produktgruppen jährlich 2013 bis 2019) ###
  
  # in diesem Pattern wird eine Maxrix aus den jährlichen Umsätzen nach Produktgruppen von Apple erstellt #
  # der tatsächliche Demand jedes Produkts wird anschließend gleichverteilt aus jeweiligen Umsatzdaten gezogen #
  # so wird in den Experimenten die tatsächliche Verteilung abgebildet, wobei die Simulation mit 6 Produkten durchgeführt wird #
  
  
  units = 10^3
  
  Jahre = c(2013, 2014, 2015, 2016, 2017, 2018, 2019)
  
  iPhone =    c(91.283031,
                102.0024,
                155.049848,
                136.694196,
                141.320295,
                164.88448,
                142.3620341)
  
  Software =  c(16.048449,
                18.06064,
                19.912944,
                24.345756,
                29.983284,
                39.76032,
                46.2748143)
  
  MAC =       c(21.483387,
                24.07476,
                25.47548,
                22.836276,
                25.857144,
                25.20544,
                25.7255713)
  
  Werables =  c(5.708394,
                6.08724,
                10.073332,
                11.127024,
                12.859803,
                17.37024,
                24.4770097)
  
  iPad =      c(31.977261,
                30.28996,
                23.231768,
                20.636748,
                19.232397,
                18.37952,
                21.2775706)
  
  iPod =      c(4.409478,
                2.285,0.001,0.001,0.001,0.001,0.001)
  
  MATRIX = cbind(Jahre,iPhone, Software, MAC, Werables, iPad, iPod) # Matrix aus den vorher eingegebenen Vektoren wir erstellt
  
  preDemand = 0
  for (i in 2:7) {      # Alle Spalten der Matrix werden durchgegangen (Außnahme ist die erste Spalte, in der die Jahreszahl steht)
    Jahr = runif(1, 1,7)      # Jahr des verwendeten Wertes wird gleichverteilt gezogen
    
    if (preDemand[1] == 0) {
      preDemand = MATRIX[Jahr,i] 
    }else{
      preDemand = c(preDemand, MATRIX[Jahr,i])  # Demand der Produktgruppe wird aus Feld in der Matrix entnommen und zum Demandvektor hinzugefügt
    }
  }
  
  DEMAND = ceiling((preDemand/sum(preDemand))*units) # Anteile der produktgruppen werden auf Gesamtdemand von 1000 heruntergebrochen
  
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

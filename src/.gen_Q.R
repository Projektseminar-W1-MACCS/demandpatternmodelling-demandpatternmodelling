## BUILDING A REALIZED DEMAND VECTOR ##


.gen_Demand_Pattern <- function(FIRM){
  #========== Implementing Demand Pattern into the Production Simulation ==========#
  
  # Im folgenden Abschnitt sind die Demand Pattern aufgefuehrt,
  # welche waehrend der Projektarbeit den unterschiedlichen Gruppen 
  # zugeordnet wurden. Eine genauere Erlaeuterung befindet sich 
  # in der Projektarbeit. Um Redundanzen zu vermeiden werden 
  # Programmabschnitte immer da erklaert, wo diese das erste 
  # Mal verwendet wurden.
  
  
  #=== Gruppe 1 ===#
  
  
  ### GLEICHVERTEILUNG (ANAND) ###
  
  # NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO # Anzahl der Produkte (in Init angegeben) wird uebertragen - in der Projektarbeit = 6
  # 
  # units = 10^3 # Gesamtzahl herzustellender Einheiten (Summe der Demands aller Produkte)
  # 
  # PreDemand = runif(NUMB_PRO,10,40) # Predemand fuer jedes Produkt wird aus Verteilungsfunktion (hier Gleichverteilung) gezogen
  # 
  # DEMAND = ceiling((PreDemand/sum(PreDemand))*units) # Demandvektor wird erstellt
  #         # relativer Anteil jedes Predemands am gesamten Predemand mal Anzahl der Einheiten, wird durch ceiling aufgerundet
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$DEMAND = DEMAND # Ermittelter Demandvektor wird global ins Modell ?uebertragen (Sourcing)
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND) # Variationskoeffizient wird zur Auswertung ermittelt
  
  
  
  
  ### GAMMAVERTEILUNG ###
  
  # NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  # 
  # units = 10^3
  # 
  # PreDemand = rgamma(NUMB_PRO,2,1) # Verwendung der Gammaverteilung (Parameter Form und Gewicht)
  # 
  # DEMAND = ceiling((PreDemand/sum(PreDemand))*units)
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$DEMAND = DEMAND
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND)
  
  
  
  
  ### TRIANGULAR- / DREIECKSVERTEILUNG  ###
  
  # library(triangle) # Bibliothek muss geladen werden, da die Verteilung in der Standardversion nicht vorhanden ist
  # 
  # NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  # 
  # units = 10^3
  # 
  # PreDemand =  rtriangle(NUMB_PRO,10,40,35) # Verwendung der Dreiecksverteilung (Parameter Minimum, Maximum und Modalwert)
  # 
  # DEMAND = ceiling((PreDemand/sum(PreDemand))*units)
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$DEMAND = DEMAND
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND)
  
  
  
  
  
  
  
  
  #=== Gruppe 2 ===#
  
  
  ### Logarithmische Normalverteilung ###
  
  # Q_VAR = FIRM$PRODUCTION_ENVIRONMENT$Q_VAR # Variationsparameter Q_Var wird aus Init übernommen
  # 
  # NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  # 
  # units = 10^3
  # 
  # PreDemand = rlnorm(NUMB_PRO, meanlog = 1, sdlog = Q_VAR) # Verwendung der Log. Normalverteilung (Parameter Mittelwert und Standardabweichung)
  # 
  # DEMAND = ceiling((PreDemand/sum(PreDemand))*units)
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$DEMAND = DEMAND
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND)
  
  
  
  
  
  
  
  #=== Gruppe 3 ===#
  
  
  ### Nestle (anhand von Nomalverteilungen) ###
  
  # units = 10^3
  # 
  #           # fuer jede Produktkategorie wird durch den Kolmogorov-Smirnov-Test die Normalverteilung nachgewiesen und daraufhin
  #           # Mittelwert und Standardabweichung ermittelt. So wird der PreDemand fuer jedes Produkt aus der jeweiligen
  #           # Normalverteilung gezogen
  # 
  # PreDemandGetraenke = rnorm(1, mean = 20.163, sd = 1.5158)
  # 
  # PreDemandMilchprodukte = rnorm(1, mean =15.360, sd = 1.9162)
  # 
  # PreDemandFertiggerichte = rnorm(1, mean =13.189, sd = 1.1250)
  # 
  # PreDemandNutrition = rnorm(1, mean =13.226, sd = 2.4055)
  # 
  # PreDemandHeimtiere = rnorm(1, mean =11.620, sd = 1.1331)
  # 
  # PreDemandSuesswaren = rnorm(1, mean =9.165, sd = 0.8656)
  # 
  #         # Der gesamte PreDemandvektor wird aus den einzelnen PreDemands der Produkte zusammengesetzt
  # 
  # PreDemand = c(PreDemandGetraenke, PreDemandMilchprodukte , PreDemandFertiggerichte, PreDemandNutrition, PreDemandHeimtiere, PreDemandSuesswaren)
  # 
  #         # Da die Gefahr besteht, dass gezogenen Werte in den negavtiven Bereich reichen, wird der PreDemandvektor
  #         # ueberprüft und negativen Werte auf 0.001 gesetzt, was einem Demand von 1 entspricht
  # 
  # for (i in 1:6) {
  #   if (PreDemand [i] <= 0){
  #     PreDemand [i] = 0.001
  #   }
  # }
  # 
  # DEMAND = ceiling((PreDemand/sum(PreDemand))*units)
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$DEMAND = DEMAND
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND)
  
  
  
  
  
  
  
  
  #=== Gruppe 4 ===#
  
  
  ###  Apple (anhand von Nomalverteilungen) ###
  
  # units = 10^3
  # 
  #       # Ziehen der PreDemands aus den jeweiligen Verteilungen
  # 
  # PreDemandiPhone = rnorm(1, mean = 133.371, sd = 26.999)
  # 
  # PreDemandSoftware = rnorm(1, mean =27.769, sd = 11.520)
  # 
  # PreDemandMAC = rnorm(1, mean =24.380, sd = 1.670)
  # 
  # PreDemandWerables = rnorm(1, mean =12.529, sd = 6.612)
  # 
  # PreDemandiPad = rnorm(1, mean =23.575, sd = 5.409)
  # 
  # PreDemandiPod = rnorm(1, mean =0.956, sd = 1.745)
  # 
  # 
  # PreDemand = c(PreDemandiPhone, PreDemandSoftware, PreDemandMAC, PreDemandWerables, PreDemandiPad, PreDemandiPod)
  # 
  #     # Ausschluss negativer Werte
  # 
  # for (i in 1:6) {
  #   if (PreDemand [i] <= 0){
  #     PreDemand [i] = 0.001
  #   }
  # }
  # 
  # DEMAND = ceiling((PreDemand/sum(PreDemand))*units)
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$DEMAND = DEMAND
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND)
  
  
  
  
  
  
  
  #=== Zusaetzliche, in der Arbeit verwendete Demand Pattern ===#
  
  
  
  
  ### NORMALVERTEILUNG ###
  
  Q_VAR = FIRM$PRODUCTION_ENVIRONMENT$Q_VAR
  
  NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  
  units = 10^3
  
  PreDemand = rnorm(NUMB_PRO, mean =20, sd = Q_VAR)
  
  # Ausschluss negativer Werte
  
  for (i in 1:NUMB_PRO) {
    if (PreDemand [i] <= 0){
      PreDemand [i] = 0.001
    }
  }
  
  DEMAND = ceiling((PreDemand/sum(PreDemand))*units)
  
  FIRM$PRODUCTION_ENVIRONMENT$DEMAND = DEMAND
  
  FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND)
  
  
  
  
  
  
  
  
  ### GLEICHE NACHFRAGE FUER JEDES PRODUKT (KONSTANT 20)###
  
  
  # NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  # 
  # units = 10^3
  # 
  # a = 20
  # 
  #       # Der PreDemand wird zunächst auf 20 festgelegt und im Anschluss fuer NUMB_Pro -1 Produkte 
  #       # jeweils um 20 erweitert, sodass für jedes Produkt ein PreDemand von 20 entsteht
  # 
  # PreDemand = a
  # 
  # for (i in 1: (NUMB_PRO-1)) {
  #  PreDemand = c(PreDemand, a)
  # }
  # 
  # DEMAND = ceiling((PreDemand/sum(PreDemand))*units) 
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$DEMAND = DEMAND
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND)
  
  
  
  
  ### Tatsaechlicher Demand von Apple (nach Produktgruppen jaehrlich 2013 bis 2019) ###
  
  # # in dieser Demand Pattern wird eine Maxrix aus den jaehrlichen Umsaetzen nach Produktgruppen von Apple erstellt. 
  # # Der PreDemand jedes Produkts wird anschliessend gleichverteilt aus jeweiligen Umsatzdaten gezogen. 
  # # so wird in den Experimenten die tatsaechliche Verteilung abgebildet
  # 
  # 
  # units = 10^3
  # 
  # Jahre = c(2013, 2014, 2015, 2016, 2017, 2018, 2019) # Vektor mit den Jahreszahlen wird gebildet
  # 
  # # Fuer jede Produktgruppe wird ein Vektor aus dessen Umsatzdaten erstellt
  # 
  # iPhone =    c(91.283031,
  #               102.0024,
  #               155.049848,
  #               136.694196,
  #               141.320295,
  #               164.88448,
  #               142.3620341)
  # 
  # Software =  c(16.048449,
  #               18.06064,
  #               19.912944,
  #               24.345756,
  #               29.983284,
  #               39.76032,
  #               46.2748143)
  # 
  # MAC =       c(21.483387,
  #               24.07476,
  #               25.47548,
  #               22.836276,
  #               25.857144,
  #               25.20544,
  #               25.7255713)
  # 
  # Werables =  c(5.708394,
  #               6.08724,
  #               10.073332,
  #               11.127024,
  #               12.859803,
  #               17.37024,
  #               24.4770097)
  # 
  # iPad =      c(31.977261,
  #               30.28996,
  #               23.231768,
  #               20.636748,
  #               19.232397,
  #               18.37952,
  #               21.2775706)
  # 
  # iPod =      c(4.409478,
  #               2.285,
  #               0.001,
  #               0.001,
  #               0.001,
  #               0.001,
  #               0.001)
  # 
  # MATRIX = cbind(Jahre,iPhone, Software, MAC, Werables, iPad, iPod) # Die erstellten Vektoren werden zu einer Matrix zusammengefasst 
  # 
  # PreDemand = 0
  # for (i in 2:7) {      # Alle Spalten der Matrix werden durchlaufen ( mit Ausnahme der ersten Spalte, in der die Jahreszahl steht)
  #   Jahr = runif(1, 1,7)      # Jahr der Produktgruppe, welche verwendet werden soll, wird gleichverteilt gezogen
  # 
  #   if (PreDemand[1] == 0) {
  #     PreDemand = MATRIX[Jahr,i]
  #   }else{
  #     PreDemand = c(PreDemand, MATRIX[Jahr,i])  # Demand der Produktgruppe wird aus Feld in der Matrix entnommen und zum Demandvektor hinzugefuegt
  #   }
  # }
  # 
  # DEMAND = ceiling((PreDemand/sum(PreDemand))*units) # Gesamtdemand wird auf 1000 Einheiten skaliert 
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$DEMAND = DEMAND
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND)
  
  
  
  
  


  
  #=== Zusaetzliche, in der Arbeit  NICHT verwendete Demand Pattern ===#
  
  
  
  ### 2 MAL NORMALVERTEILUNG MIT UNTERSCHIEDLICHER SD ### 
  
  # NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO # nur gerade Zahlen experimentierbar 
  # 
  # Q_VAR = FIRM$PRODUCTION_ENVIRONMENT$Q_VAR
  # 
  # units = 10^3
  # 
  # PreDemand1 = rnorm((NUMB_PRO / 2), mean =20, sd = 1) 
  # 
  # PreDemand2 = rnorm((NUMB_PRO / 2), mean =20, sd = 4)
  # 
  # PreDemand = c(PreDemand1, PreDemand2)
  # 
  # DEMAND = ceiling((PreDemand/sum(PreDemand))*units) 
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$DEMAND = sample(DEMAND) # Durchmischung der PreDemand1 und PreDemand2 Werte des Demandvektors 
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND)
  
  
  
  
  
  
  ### HOHE NACHFRAGEDIFFERENZ (5 mal 100, 45 mal 10) ###
  
  # NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  # 
  # units = 10^3
  # 
  #     # Fuer 10 % der Produkte wird ein PreDemand von 100 festgelegt
  # 
  # PreDemand1 = 0
  # for (i in 1:(NUMB_PRO*0.1)) {
  #   if (PreDemand1[1] == 0){ # 0 Dient als Platzhalter, damit PreDemand zu Beginn nicht unbeschrieben ist und erweitert werden kann
  #     PreDemand1 = 100
  #   }else{
  #     PreDemand1 = c(PreDemand1,100 )
  #   }
  # }
  # 
  #     # Die Produkte ohne Demand werden im PreDemand 2 zusammengefasst und ihnen jeweils der Wert 10 zugordnet
  # 
  # a = NUMB_PRO - length(PreDemand1)
  # 
  # PreDemand2 = 0
  # for(i in 1:a){
  # 
  #   if (PreDemand2[1] == 0){
  #     PreDemand2 = c(10)
  #   }else{
  #     PreDemand2 = c(PreDemand2, 10)
  #   }
  # }
  # 
  # PreDemand = c(PreDemand1, PreDemand2) # Gesamter PreDemand wird zusammengesetzt
  # 
  # DEMAND = ceiling((PreDemand/sum(PreDemand))*units) 
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$DEMAND = sample(DEMAND) # Durchmischung des Demandvektors
  # 
  # FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw = sd(DEMAND)/mean(DEMAND)
  # 
  
  
  
  
  
  ###CHECK###
  
  Qs = sort(DEMAND, decreasing = TRUE)
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q01 = sum(Qs[1:(1/6 * NUMB_PRO)])/units        #no. of units of the biggest product
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q5u = sum(Qs[1:(0.5 * NUMB_PRO)])/units        #no. of units of 50% biggest products
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q5d = sum(Qs[(0.5 * NUMB_PRO):NUMB_PRO])/units #no. of units of 50% lowest products
  
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
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q01 = sum(Qs[1:(0.2 * NUMB_PRO)])/units        #no. of units of 20% biggest products
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q5u = sum(Qs[1:(0.1 * NUMB_PRO)])/units        #no. of units of 10% biggest products
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q5d = sum(Qs[1:(0.02 * NUMB_PRO)])/units       ##no. of units of 2% biggest products

  
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

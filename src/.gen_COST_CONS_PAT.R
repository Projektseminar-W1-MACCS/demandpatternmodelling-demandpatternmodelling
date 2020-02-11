###########
# Cost structure modeling
############


.gen_COST_CONS_PAT<-function(FIRM,COST_APPROACH="ANAND"){
  
  if(COST_APPROACH=="KGM"){
    # # case 1 # ADDING THE COSTS TO THE RESOURCE_CONSUMPTION_MATRIX ORIGINAL  KGM MM
    # # SEPARATING UNIT FROM NON-UNIT;
    # TRU_UNIT = sum(RES_CONS_PAT[,1:ProductionEnvironment[['UnitSize']]]*ProductionEnvironment[['DEMAND']]) # UNIT WILL BE MULTIPLIED BY TQ. [total quantity]
    # TRU_BATCH = sum(RES_CONS_PAT[,((ProductionEnvironment[['UnitSize']]+1):ProductionEnvironment[['NUMB_RES']])]) # NON-UNIT
    # TRU =  c(TRU_UNIT,TRU_BATCH) # TOTAL RESOURCE UNITS
    # cd<-data.frame(UNIT=RCC$RC_UNIT,BATCH=RCC$RC_BATCH)/t(TRU)
    #
    #
    # # BUILD THE BENCHMARK PRODUCT COSTS
    # RES_CONS_PAT_UNIT_pre <- RES_CONS_PAT[,1:ProductionEnvironment[['UnitSize']]]*ProductionEnvironment[['DEMAND']]
    # RES_CONS_PAT_BATCH_pre <- RES_CONS_PAT[,((ProductionEnvironment[['UnitSize']]+1):ProductionEnvironment[['NUMB_RES']])]
    # RES_CONS_PAT_t <- cbind(RES_CONS_PAT_UNIT_pre,RES_CONS_PAT_BATCH_pre)
    # CostSystem.PC_B <- rowSums(RES_CONS_PAT_t*t(cd)) # (BENCHMARK PRODUCT COSTS TOTAL)
    #
    # % Assess the cost structure
    # COST_CONS_PAT = RES_CONS_PAT_t.*cd';
    # CHECK.PC_B_UNIT = sum(COST_CONS_PAT(:,1:ProductionEnvironment.UnitSize),2);
    # CHECK.PC_B_BATCH = sum(COST_CONS_PAT(:,ProductionEnvironment.UnitSize+1:ProductionEnvironment.NUMB_RES),2);
    # % check = PC_B_UNIT + PC_B_BATCH; 20180502 checked
    # RES_CONS_PATp = RES_CONS_PAT_t./sum(RES_CONS_PAT_t);
    
  }
  
  else if(COST_APPROACH=="ANAND"){
    ## ADDING THE COSTS TO THE RESOURCE_CONSUMPTION_MATRIX ORIGINAL in ANAND
    if(FIRM$COSTING_SYSTEM$set_RCU_fix == 1){
      
      RCU = FIRM$COSTING_SYSTEM$RCU
      
      RCC = FIRM$PRODUCTION_ENVIRONMENT$TRU * FIRM$COSTING_SYSTEM$RCU
      
      FIRM$COSTING_SYSTEM$RCC = RCC
      ###CHECK###
      RCCs = sort(RCC, decreasing = TRUE)
      
      
      FIRM$PRODUCTION_ENVIRONMENT$CHECK$RCC20 = sum(RCCs[1:(0.2 * length(RCC))])/TC     #size of 20% biggest resources (10)
      FIRM$PRODUCTION_ENVIRONMENT$CHECK$RCC10 = sum(RCCs[1:(0.1 * length(RCC))])/TC     #size of 10% biggest resources (5)
      FIRM$PRODUCTION_ENVIRONMENT$CHECK$RCC02 = sum(RCCs[1:(0.02 * length(RCC))])/TC    #size of 2% biggest resources (1)
    }  #if this is set fix, the RCC vector is adapted to the acutal Demand and resource consumption so that the sum is alwas equal to TC
    else if(FIRM$COSTING_SYSTEM$set_RCU_fix == 0){
      # Total Resource Units -> Amount Needed to produce mix QT = ProductionEnvironment[['DEMAND']]
      TRU = FIRM$PRODUCTION_ENVIRONMENT$TRU
      
      # RES_CONS_PAT Anand et al. 2019
      
      FIRM$COSTING_SYSTEM$RCU<- FIRM$COSTING_SYSTEM$RCC/TRU # BUILDING RESOURCE COST DRIVERS (Unit Resource Costs) BY DIVIDING RCC THROUGH THE TOTAL RESOURCE UNITS (TRU)
      
    }
   

    
    ###TOTAL COST CONS PAT###
    COST_CONS_PAT_TOTAL = sweep(FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PAT_TOTAL, MARGIN=2, FIRM$COSTING_SYSTEM$RCC, `*`)

    TCU = colSums(COST_CONS_PAT_TOTAL)

    COST_CONS_PATp = sweep((COST_CONS_PAT_TOTAL),2,TCU,"/")
    
    FIRM$PRODUCTION_ENVIRONMENT$COST_CONS_PATp = COST_CONS_PATp
    
    
    
    
    

    # Benchmark product costs
    FIRM$COSTING_SYSTEM$PCB <- FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PAT%*%FIRM$COSTING_SYSTEM$RCU * FIRM$PRODUCTION_ENVIRONMENT$DEMAND #BENCHMARK PRODUCT COSTS (TOTAL)
    
    #FIRM$COSTING_SYSTEM$PCB = rowSums(COST_CONS_PAT)
    
    #FIRM$COSTING_SYSTEM$PCB = rowMeans(FIRM$PRODUCTION_ENVIRONMENT$COST_CONS_PATp) * FIRM$COSTING_SYSTEM$TC
    #FIRM$COSTING_SYSTEM$PCB = FIRM$PRODUCTION_ENVIRONMENT$COST_CONS_PATp %*% FIRM$COSTING_SYSTEM$RCC * FIRM$COSTING_SYSTEM$RCP
    
    #FIRM$COSTING_SYSTEM$PCB = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PAT %*% FIRM$COSTING_SYSTEM$RCU
    #FIRM$COSTING_SYSTEM$PCB = rowMeans(FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp)*FIRM$COSTING_SYSTEM$TC
    #FIRM$COSTING_SYSTEM$PCB <- FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp %*% FIRM$COSTING_SYSTEM$RCC
    #PCB = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PAT%*%(RCC/TRU)*FIRM$PRODUCTION_ENVIRONMENT$DEMAND
    
    #PCB = PCB*FIRM$PRODUCTION_ENVIRONMENT$DEMAND
    
    #PCB = vector(mode ='numeric')
    
    
    #PCB = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp%*%FIRM$COSTING_SYSTEM$RCC
    
    #FIRM$COSTING_SYSTEM$PCB = PCB
    #check1 = sum(PC_B); % sum must be 10^6
    # RES_CONS_PATp is essential for further cost allocation.
    
    #RES_CONS_PATp = as.data.frame(scale(RES_CONS_PAT, center=FALSE, scale=colSums(RES_CONS_PAT)))
    
    
  }
  
  else if(COST_APPROACH=="BALA"){
    
    
    
    RES_CONS_PATp<-as.data.frame(scale(RES_CONS_PAT, center=FALSE, scale=colSums(RES_CONS_PAT)))
    # RES_CONS_PAT is already normalized
    PC_B = RES_CONS_PAT%*%RCC
    
    CostSystem[['PC_B']] = PC_B
    
  }
  
  # res<-vector(mode="numeric")
  # for (i in 1:NCOL(RES_CONS_PATp)) {
  #   res[i]<-cor.test(RES_CONS_PATp[,i],ProductionEnvironment[['DEMAND']])$estimate
  # }
  #
  #
  # CHECK[['COR_UNIT_AND_OUTPUT']] <-res
  
  
  #FIRM<-list(PRODUCTION_ENVIRONMENT,COSTING_SYSTEM)
  return(FIRM)
}

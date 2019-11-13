###########
# Cost structure modeling
############


.genCOST_CONS_PAT<-function(FIRM,COST_APPROACH="ANAND"){
  
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
    
  }else if(COST_APPROACH=="ANAND"){
    ## ADDING THE COSTS TO THE RESOURCE_CONSUMPTION_MATRIX ORIGINAL in ANAND
    
        # Total Resource Units -> Amount Needed to produce mix QT = ProductionEnvironment[['DEMAND']]
    TRU = FIRM$PRODUCTION_ENVIRONMENT$TRU
    
    # RES_CONS_PAT Anand et al. 2019
    
    RCU<- FIRM$COSTING_SYSTEM$RCC/TRU # BUILDING RESOURCE COST DRIVERS (Unit Resource Costs) BY DIVIDING RCC THROUGH THE TOTAL RESOURCE UNITS (TRU)
   # browser()
    # Benchmark product costs
    FIRM$COSTING_SYSTEM$PCB <- FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PAT%*%RCU*FIRM$PRODUCTION_ENVIRONMENT$DEMAND #BENCHMARK PRODUCT COSTS (TOTAL)
    
    
    #PCB = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PAT%*%(RCC/TRU)*FIRM$PRODUCTION_ENVIRONMENT$DEMAND
    
    #PCB = PCB*FIRM$PRODUCTION_ENVIRONMENT$DEMAND
    
    #PCB = vector(mode ='numeric')
    
    #PCB = RES_CONS_PATp%*%RCC
    
    #FIRM$COSTING_SYSTEM$PCB = PCB
    #check1 = sum(PC_B); % sum must be 10^6
    # RES_CONS_PATp is essential for further cost allocation.
    
    #RES_CONS_PATp = as.data.frame(scale(RES_CONS_PAT, center=FALSE, scale=colSums(RES_CONS_PAT)))
    
    
  }else if(COST_APPROACH=="BALA"){
    
    
    
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

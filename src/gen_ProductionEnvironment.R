## PRODUCTION ENVIRONMENT GENERATION V 1.2

gen_ProductionEnvironment <- function(FIRM,set_PE_constant) {


## ====================== Set constant or vary =================
   

##SET DEMAND FIX
   
if (FIRM$PRODUCTION_ENVIRONMENT$set_DEMAND_fix ==1) {set.seed(13)} 
  
FIRM = .gen_Demand_Anand(FIRM) #gen_Demand_Anand and gen_Demand

if (FIRM$PRODUCTION_ENVIRONMENT$set_DEMAND_fix==1) {set.seed(NULL)} # This removes the seed of the firm allowing random cost system design


if (FIRM$PRODUCTION_ENVIRONMENT$set_RES_CONS_PAT_fix ==1) {set.seed(13)} 

FIRM = .gen_RES_CONS_PAT_Anand(FIRM) ## Building Demand, RES_CONS_PAT, RCC and PCB 

if (FIRM$PRODUCTION_ENVIRONMENT$set_RES_CONS_PAT_fix ==1) {set.seed(NULL)}

##SET THE RESOURCE COSTS FIX

if (FIRM$COSTING_SYSTEM$set_RCU_fix==1) {
   
   set.seed(13)
  
    FIRM = .gen_RCC_variation(FIRM) #based on Ananads RC vector algorythm, but only creates RCU
   set.seed(NULL)
   }

if(FIRM$COSTING_SYSTEM$set_RCU_fix==0){
if(FIRM$COSTING_SYSTEM$set_RCC_fix==0){ 
   FIRM = .gen_RCC_Anand(FIRM) 
   
}
   if(FIRM$COSTING_SYSTEM$set_RCC_fix==1){
      set.seed(13)
      FIRM = .gen_RCC_Anand(FIRM) 
      set.seed(NULL)
      
   }
   
}

FIRM = .gen_COST_CONS_PAT(FIRM,COST_APPROACH = "ANAND")


return(FIRM)

 } # Function end


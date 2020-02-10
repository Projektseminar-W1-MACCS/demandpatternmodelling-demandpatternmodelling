## PRODUCTION ENVIRONMENT GENERATION V 1.01


gen_ProductionEnvironment <- function(FIRM,set_PE_constant) {

  
## ====================== Set constant or vary =================
   
##SET DEMAND CONSTANT
if (set_DEMAND_constant==1) {set.seed(13)} 
  
FIRM = .gen_Demand_Anand(FIRM) #gen_Demand_Anand and gen_Demand

if (set_DEMAND_constant==1) {set.seed(NULL)} # This removes the seed of the firm allowing random cost system design



##SET THE RESOURCE CONSUMPTION CONSTANT

if (set_RES_CONS_PAT_constant==1) {set.seed(13)} 

FIRM = .gen_RES_CONS_PAT_Anand(FIRM) ## Building Demand, RES_CONS_PAT, RCC and PCB 

if (set_RES_CONS_PAT_constant==1) {set.seed(NULL)}


##SET THE RESOURCE COSTS CONSTANT

if (set_RC_constant==1) {set.seed(13)}

FIRM = .gen_RCC_Anand(FIRM) 

if (set_RC_constant==1) {set.seed(NULL)}




FIRM = .genCOST_CONS_PAT(FIRM,COST_APPROACH = "ANAND")





return(FIRM)




 } # Function end


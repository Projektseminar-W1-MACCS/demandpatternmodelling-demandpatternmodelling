## PRODUCTION ENVIRONMENT GENERATION V 1.1

gen_ProductionEnvironment <- function(FIRM,set_PE_constant) {

  
## ====================== Set constant or vary =================
   

##SET DEMAND CONSTANT
if (set_DEMAND_constant==1) {set.seed(13)} 
  
  FIRM = .gen_Demand_Anand(FIRM) #gen_Demand_Anand and gen_Demand
  
## ====================== STEP 1.a  Determining the activities =========================
if (set_DEMAND_constant==1) {set.seed(NULL)} # This removes the seed of the firm allowing random cost system design

  ## ====================== STEP 1.b Determining the amount of cost categories =================
##SET THE RESOURCE CONSUMPTION CONSTANT

if (set_RES_CONS_PAT_constant==1) {set.seed(13)} 

FIRM = .gen_RES_CONS_PAT_Anand(FIRM) ## Building Demand, RES_CONS_PAT, RCC and PCB 

if (set_RES_CONS_PAT_constant==1) {set.seed(NULL)}

##SET THE RESOURCE COSTS CONSTANT


if (set_RC_constant==1) {set.seed(13)}

FIRM = .gen_RCC_Anand(FIRM) 

if (set_PE_constant==1) {set.seed(NULL)} # This removes the seed of the firm allowing random cost system design








return(FIRM)


 } # Function end


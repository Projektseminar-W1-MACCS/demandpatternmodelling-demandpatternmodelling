## PRODUCTION ENVIRONMENT GENERATION V 1.1

gen_ProductionEnvironment <- function(FIRM,set_PE_constant) {


## ====================== Set constant or vary =========
if (set_PE_constant==1) {set.seed(13)} 
   
   
## ====================== STEP 1.a  Determining the activities =========================

UNITLEVEL_ACT_SHARE_MIN = 0.2    #0.2 is the size of DISP1 =10
UNITLEVEL_ACT_SHARE_MAX = 0.2
FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE = runif(1, UNITLEVEL_ACT_SHARE_MIN, UNITLEVEL_ACT_SHARE_MAX) #random activity share between lower and upper bounds

FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE_MIN = UNITLEVEL_ACT_SHARE_MIN
FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE_MAX = UNITLEVEL_ACT_SHARE_MAX
## ====================== STEP 1.b Determining the amount of cost categories =================

unitsize = floor(FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE*FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)
nonunitsize = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES-unitsize

FIRM$PRODUCTION_ENVIRONMENT$UNITSIZE = unitsize
FIRM$PRODUCTION_ENVIRONMENT$NONUNITSIZE = nonunitsize


## ====================== STEP 1.c Determining the density (DENS)  =========================

#Randomization and setting clear design points. 
if(DENS[1] == -1)
{
DENS_MIN = 0.4;
DENS_MAX = 0.7;
DENS = runif(1, DENS_MIN, DENS_MAX);
FIRM$PRODUCTION_ENVIRONMENT$DENS_MIN = DENS_MIN
FIRM$PRODUCTION_ENVIRONMENT$DENS_MAX = DENS_MAX
FIRM$PRODUCTION_ENVIRONMENT$DENS = DENS
}
FIRM$PRODUCTION_ENVIRONMENT$DENS = DENS



## ===================== STEP 2 Building Demand, RES_CONS_PAT, RCC and PCB '' =========================

FIRM = .gen_Demand_Anand(FIRM) #gen_Demand_Anand and gen_Demand

FIRM = .gen_RES_CONS_PAT_Anand(FIRM)

FIRM = .gen_RCC_Anand(FIRM)

FIRM = .genCOST_CONS_PAT(FIRM,COST_APPROACH = "ANAND")


if (set_PE_constant==1) {set.seed(NULL)} # This removes the seed of the firm allowing random cost system design




return(FIRM)


 } # Function end


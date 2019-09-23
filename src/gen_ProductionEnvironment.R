#############################################################
# PRODUCTION ENVIRONMENT GENERATION V 0.01
# 
#############################################################

gen_ProductionEnvironment <- function(FIRM) {

  #  if vary_demand==0 
 #  DEMAND_BASE = lognrnd(1,VOL_VAR,NUMB_PRO,1);
 #  else
 #  rng('shuffle') 


## ====================== STEP 1 REALIZED DEMAND GENERATION ========================= 

units = 10^3
preDemand = rlnorm(NUMB_PRO, meanlog = 0, sdlog = 0.1)
FIRM$PRODUCTION_ENVIRONMENT$DEMAND = ceiling(preDemand/sum(preDemand)*units)


## ====================== STEP 2  Determining the ACTIVITY STRUCTURE =========================

UNITLEVEL_ACT_SHARE_MIN = 0.3
UNITLEVEL_ACT_SHARE_MAX = 0.7
FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE = UNITLEVEL_ACT_SHARE_MIN + (UNITLEVEL_ACT_SHARE_MAX - UNITLEVEL_ACT_SHARE_MIN)*runif(1)


## =================== STEP 2.1 Determining the amount of cost categories =================

unitsize = floor(FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE*FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)
nonunitsize = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES-unitsize

## ====================== STEP 2.b Determining the density (DENS)  =========================

#Randomization and setting clear design points. 
DENS_MIN = 0.4;
DENS_MAX = 0.7;
DENS = DENS_MIN + (DENS_MAX-DENS_MIN)*runif(1);
FIRM$PRODUCTION_ENVIRONMENT$DENS = DENS
## ====================== STEP 2.b Determining the density (DENS)  =========================
source('src/.gen_RES_CONS_PAT.R')
source('src/.gen_RC.R')

FIRM = .gen_RES_CONS_PAT(FIRM);

#RES_CONS_PAT,CHECK] = genRES_CONS_PAT2(ProductionEnvironment,DENS_RUN,COR); % generate res_cons_pat

FIRM$COSTING_SYSTEM$RCC = .gen_RCC(FIRM, unitsize, nonunitsize);



FIRM = .genCOST_CONS_PAT(FIRM,COST_APPROACH = "ANAND")






#COSTING_SYSTEM$PCB = COSTING_SYSTEM$RCC*PRODUCTION_ENVIRONMENT$
 #  [RES_CONS_PATp,CostSystem,CHECK] = genCOST_CONS_PAT(ProductionEnvironment,CHECK,RC,RES_CONS_PAT,DENS_RUN,COR);
   
 #  %% COMPUTING DESCRIPTIVE VALUES 
 #  % Computing Resource cost percentage: How many percentage are in the
 #  RC_sort = sort(RC,'descend');
 #  RC_20p = sum(RC_sort(1:(NUMB_RES*0.2)));
 #  CHECK.RC_20p = RC_20p./TC*100;
 #  
 #  % Computing Output distribution percentage; How many percentage are in the % 20% values?
 #    DEMAND_sort = sort(TQ,'descend');
 #  DEMAND_20p = sum(DEMAND_sort(1:(NUMB_PRO*0.2))); 
 #  CHECK.DEMAND_20p = DEMAND_20p./sum(TQ)*100;
 #  
 #  % Computing heterogeneity 
 #  [CHECK] = measure_heterogeneity(RES_CONS_PATp,ProductionEnvironment,CHECK);
 #  
 #  % Averange range between lowest and highest consumption.
 #  CORAP_pre=max(RES_CONS_PATp)-min(RES_CONS_PATp);
 #  CHECK.CORAP=mean(CORAP_pre)*100;  
 #  
return(FIRM)


  } # Function end

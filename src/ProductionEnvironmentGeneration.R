#############################################################
# PRODUCTION ENVIRONMENT GENERATION V 0.01
# 
#############################################################

gen_ProductionEnvironment <- function(PRODUCTION_ENVIRONMENT) {

  #  if vary_demand==0 
 #  DEMAND_BASE = lognrnd(1,VOL_VAR,NUMB_PRO,1);
 #  else
 #  rng('shuffle') 


## ====================== STEP 1 REALIZED DEMAND GENERATION ========================= 

units = 10^3
preDemand = rlnorm(NUMB_PRO, meanlog = 0, sdlog = 0.1)
DEMAND = ceiling(preDemand/sum(preDemand)*units)

barplot(sort(DEMAND))


## ====================== STEP 2  Determining the ACTIVITY STRUCTURE =========================
    blub
ProductionEnvironment.NUMB_RES = NUMB_RES; #Amount of processes
ProductionEnvironment.NUMB_PRO = NUMB_PRO; #Amount of products
  
 # ## ====================== STEP 2 Determining the amount of cost categories (fix vs. variable costs) =========================
    
 #  if VOL_SHARE_RES == -1
 #  VOL_SHARE_RES_MIN = 0.3; 
 #  VOL_SHARE_RES_MAX = 0.7;
 #  VOL_SHARE_RES = VOL_SHARE_RES_MIN + (VOL_SHARE_RES_MAX-VOL_SHARE_RES_MIN).*rand(1,1);
 #  else
 #  end 
 #  
 #  ProductionEnvironment.UnitSize=floor(VOL_SHARE_RES*ProductionEnvironment.NUMB_RES);
 #  ProductionEnvironment.BatchSize=ProductionEnvironment.NUMB_RES-ProductionEnvironment.UnitSize;
 #  
 # ## ====================== STEP 2.b Determining a DMM (RES_CONS_PAT);  =========================
 #    
 #    %% Randomization and setting clear design points. 
 #  if DENS == -1
 #  DENS_MIN = 0.4; 
 #  DENS_MAX = 0.7;
 #  DENS_RUN = DENS_MIN + (DENS_MAX-DENS_MIN).*rand(1,1);
 #  else
 #    DENS_MIN = DENS-0.1;
 #  DENS_MAX = DENS;
 #  DENS_RUN = DENS_MIN + (DENS_MAX-DENS_MIN).*rand(1,1);
 #  end 
 #  
 #  
 #  
 #  %DENS_RUN = 1 ; 
 #  
 #  
 #  [RES_CONS_PAT,CHECK] = genRES_CONS_PAT(ProductionEnvironment,DENS_RUN,COR); % generate res_cons_pat
 #  %[RES_CONS_PAT,CHECK] = genRES_CONS_PAT2(ProductionEnvironment,DENS_RUN,COR); % generate res_cons_pat
 #  
 #  ## ====================== STEP 2.b Determining a DMM (RES_CONS_PAT) ===========================
 #    
 #    RC = genRC(ProductionEnvironment,VOL_SHARE_RES,RC_VAR,TC);
 #  [RES_CONS_PATp,CostSystem,CHECK] = genCOST_CONS_PAT(ProductionEnvironment,CHECK,RC,RES_CONS_PAT,DENS_RUN,COR);
 #  
 #  
 #  
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
return(PRODUCTION_ENVIRONMENT)
  } # Function end

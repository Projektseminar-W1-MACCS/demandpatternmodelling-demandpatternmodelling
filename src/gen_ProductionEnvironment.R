## PRODUCTION ENVIRONMENT GENERATION V 1.01


gen_ProductionEnvironment <- function(FIRM,set_PE_constant) {

  
## ====================== Set constant or vary =================
   
##SET DEMAND FIX
   
if (FIRM$PRODUCTION_ENVIRONMENT$set_DEMAND_fix ==1) {set.seed(13)} 
  
FIRM = .gen_Demand_Anand(FIRM) #gen_Demand_Anand and gen_Demand

if (FIRM$PRODUCTION_ENVIRONMENT$set_DEMAND_fix==1) {set.seed(NULL)} # This removes the seed of the firm allowing random cost system design



##SET THE RESOURCE CONSUMPTION FIX

if (FIRM$PRODUCTION_ENVIRONMENT$set_RES_CONS_PAT_fix ==1) {set.seed(13)} 

FIRM = .gen_RES_CONS_PAT_Anand(FIRM) ## Building Demand, RES_CONS_PAT, RCC and PCB 

if (FIRM$PRODUCTION_ENVIRONMENT$set_RES_CONS_PAT_fix ==1) {set.seed(NULL)}


##SET THE RESOURCE COSTS FIX

if (FIRM$PRODUCTION_ENVIRONMENT$set_RCU_fix==1) {
   
   set.seed(13)
   
   
   FIRM = .gen_RCC_variation(FIRM) #based on Ananads RC vector algorythm, but only creates RCU
   
   
   set.seed(NULL)
   }


if(FIRM$PRODUCTION_ENVIRONMENT$set_RCU_fix==0){
   
   FIRM = .gen_RCC_Anand(FIRM) 
   
}




FIRM = .gen_COST_CONS_PAT(FIRM,COST_APPROACH = "ANAND")





return(FIRM)


#RES_CONS_PAT,CHECK] = genRES_CONS_PAT2(ProductionEnvironment,DENS_RUN,COR); % generate res_cons_pat


#FIRM = .gen_RCC_KGM(FIRM, unitsize, nonunitsize);



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





 


 } # Function end


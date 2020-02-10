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


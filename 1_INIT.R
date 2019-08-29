#############################################################
# Initizalization of the CostSystemDesignSim (CSDS)
#############################################################


## ======================================INPUT MASK============================================================
  PRODUCTION_ENVIRONMENT = list()
  COSTING_SYSTEM = list()
  
  
  
  PRODUCTION_ENVIRONMENT$NUMB_PRO =         50;      #INPUT independent Variable - Number of products 
  COSTING_SYSTEM$NUMB_RES          =         50;      #INPUT independent variable - Number of factors
  SIM_NUMB =         5;       #Control Variable - Number of Simulations for every single environment (standard: 30)     
  tt   =             1;       #Periods


  ProductOutput=     1;       #Zero = no tracking
  set_pe_constant=   1;       #Control Variable -  Decide if Simulation is reproducible {1} or random {0}
  set_cs_constant=   0;       #Control Variable 
  vary_demand =      0;       #Control Variable
  
  
  dec_ERROR=         1;       #Control Variable - 
  seed=              13;      #Control Variable -
  
 #dec_DC=            0;       # = no direct costs 
  dec_CP=            1;       # =
  dec_CD=            1;       # =
  
  
  
  COSTING_SYSTEM$CP = c(1,2,3,4,5,6,7,8,9,10)
  PRODUCTION_ENVIRONMENT$COR = c(0)
  COSTING_SYSTEM$RC_VAR =  c(0.55)
  PRODUCTION_ENVIRONMENT$Q_VAR = c(1)
  COSTING_SYSTEM$Error = c(0.1,0.3,0.5)
  COSTING_SYSTEM$NUMB_Error = c(1)
  
## ======================================END OF INPUT MASK=====================================================                           

    
  
  
  
  
            o=1
            nn=1
            #source('./src/ProductionEnvironmentGeneration.R')              
            #source('./src/.RES_CONS_PAT.R')
            # initialize global variables #
            data_log<-list()    # List for Logging Data
              
              
              
## ======================================DESIGN OF EXPERIMENTS===================================================== 
## EVIRONMENTAL FACTORS [FULLDESIGN: PER CP = *SIM_NUMB] [3k Design - 324 DP*SIM_NUMB]
              
  for (ix_CP in seq_along(COSTING_SYSTEM$CP)) {
     for (ix_COR in seq_along(PRODUCTION_ENVIRONMENT$COR)) {
       for (ix_RC_VAR in seq_along(COSTING_SYSTEM$RC_VAR)) {
         for (ix_Q_VAR in seq_along(PRODUCTION_ENVIRONMENT$Q_VAR)) {
           for (ix_Error in seq_along(COSTING_SYSTEM$Error)) {
             for (ix_NUMB_Error in seq_along(COSTING_SYSTEM$NUMB_Error)) {
     
              
               
#if ( dec_CP==1) {
#} else if ( dec_CP==2) {
#   #statement2
# } else {
#   printf("Blub")}
              
            
  
## ====================================== SIMULATION ROUTINE   =====================================================    
for (nn in 1:SIM_NUMB) {

  
print(COSTING_SYSTEM$CP[ix_CP])  
  print(COSTING_SYSTEM$Error[ix_Error])  
}
             }
           }
         }
       }
     }
   }
  

  
#   switch dec_CP
#   case 1% 
#   [APC,index] = MAP_RES_CP_RANDOM(ProductionEnvironment,CostSystem,CP);
#   case 2% 
#   [APC,index] = MAP_RES_CP_CORRSIZE(ProductionEnvironment,CostSystem,CP,set_cs_constant,seed);
#   case 3%
#   [APC,index] = MAP_RES_CP_CORRRANDOM(ProductionEnvironment,CostSystem,CP,set_cs_constant);
#   case 4 
#   [APC,index] = MAP_RES_CP_SIZEMISC(ProductionEnvironment,CostSystem,CP);
#   case 5 
#   [APC,index] = MAP_RES_CP_SIZERANDOM(ProductionEnvironment,CostSystem,CP,set_cs_constant,seed);
#   case 6 
#   [APC,index] = MAP_RES_CP_UNITSIZERANDOM(ProductionEnvironment,CostSystem,CP,set_cs_constant,seed);
#   end
#   
#   
#   switch dec_CD      %switch between the Resource Allocation Heuristics (depending on choosen INPUT Variable )
#   case 1
#   [ACT_CONS_PAT]=MAP_CP_PRO_BigPool(ProductionEnvironment,CostSystem,index,dec_ERROR,Error);
#   case 2
#   [ACT_CONS_PAT]=MAP_CP_PRO_Average(ProductionEnvironment,CostSystem,index,dec_ERROR,Error);
#   case 3 
#   [ACT_CONS_PAT]=MAP_CP_PRO_Division(ProductionEnvironment,CostSystem,index,dec_ERROR,Error);
#   case 4   
#   [ACT_CONS_PAT,CHECK]=MAP_CP_PRO_BigPool_Error(ProductionEnvironment,CostSystem,index,dec_ERROR,Error,ErrNUMB,CHECK);
#   case 5
#   [ACT_CONS_PAT]= MAP_CP_PRO_UnitActivityMeasure(ProductionEnvironment,CostSystem,index,dec_ERROR,Error);
#   case 6
#   [ACT_CONS_PAT]= MAP_CP_PRO_DirectLaborHours(ProductionEnvironment,CostSystem,index,dec_ERROR,Error);
#   case 7 
#   [ACT_CONS_PAT]= MAP_CP_PRO_DirectMaterial(ProductionEnvironment,CostSystem,index,dec_ERROR,Error);
#   case 8 
#   [ACT_CONS_PAT]= MAP_CP_PRO_NonUnitActivityMeasure(ProductionEnvironment,CostSystem,index,dec_ERROR,Error);
#   end
# 
              
              
              
              
              
              
              
              
              
              
              
              
#   %% BENCHMARK VS. REPORTED 
#   
#   PC_B = CostSystem.PC_B + CostSystem.PC_B_DC ;  % Product cost from Benchmark system
#   PC_H = sum((APC.*ACT_CONS_PAT'),2) + CostSystem.PC_B_DC; %Product cost from the Heuristic system
#                     
#                CostSystem.PC_H = PC_H;  % Put  value in  structure
#                CostSystem.PC_B = PC_B;
#       
# %% DATA OUTPUT
#                 % Generating EUCD as the dependent variable TOTAL       
#                 EUCD = sqrt(sum(((PC_B-PC_H).^2)));  
#                 MSE = (mean((PC_B - PC_H).^2));
#                 MPE = sum((abs(PC_B-PC_H)./PC_B))./NUMB_PRO;
#                 
#                 % Gathering the factors from the production environment. 
#                 ProductionEnvironment.RC_VAR = RC_VAR;
#                 ProductionEnvironment.VOL_VAR = Q_VAR;
#                 ProductionEnvironment.ErrorLevel = Error;
#                 ProductionEnvironment.ErrorNUMB = ErrNUMB;
#                 ProductionEnvironment.VOL_SHARE = VOL_SHARE_PERC;
#                 ProductionEnvironment.COR = COR;
#              
#                 % CostSystem 
#                 CostSystem.dec_CP = dec_CP;
#                 CostSystem.dec_CD = dec_CD;
#                 CostSystem.DC_SHARE = OH_SHARE;
#                 
#                 %% DATA LOGGING
#                 DATA = f_DataLog(DATA,o,nn,tt,ProductionEnvironment,CostSystem,CP,EUCD,MSE,MPE,CHECK);
#                                              
#                 if track_p_level==1 
#                 DATAp = f_DataLog_p(DATAp,o,nn,tt,ProductionEnvironment,CostSystem,CP,CHECK);
#                 end
# %%COUNTER FOR PERIODS
#  tt=tt+1;
# 
# %% Counter
#  nn=nn+1;
# %% Display command.
#  disp(['RUN',num2str(nn),' CP',num2str(CP),' DENS',num2str(DENS),' VOL_VAR',num2str(Q_VAR)])
# end %% SIM_NUMB
#  o=o+1;
# end % COR
# end % RC_VAR
# end % DENS
# end % Error
# end % Error_Numb
# end % VOL_VAR
# end % DC_RATIO
# end % VOLSHARE
# end % SIM NUM LOOP
# 
# 
# % %% ======================================FINAL DATA LOGGING & FILE WRITING =====================================================    
# % %% Aggregated system file 
# try
# t = datestr(now,'yyyymmdd-HHMM');
# filename = ['SimOutV2_',num2str(dec_CP), num2str(dec_CD),'_',  t, '.xlsx'];
# writetable(DATA,filename, 'WriteVariableNames', true);
# 
# disp('============================')
# disp('SYSTEM OUTPUT is written')
# disp('============================')
# catch 
# end 
# % %% Productlevel file
# try
#    filename = ['Productcosts2_', num2str(dec_CP), num2str(dec_CD), '_', t, '.xlsx'];
#    writetable(DATAp,filename);
# disp('============================')
# disp('PORTFOLIO OUTPUT is written')
# disp('============================')
#      
# catch
#     try  
#     filename = ['Productcosts2_', num2str(dec_CP), num2str(dec_CD), '_', t, '.txt'];
#     writetable(DATAp,filename);
#     disp('============================')
#     disp('PORTFOLIO OUTPUT is written')
#     disp('============================')
#      
#     catch 
#     disp('=====================================')
#     disp('PORTFOLIO OUTPUT has not been written')
#     disp('=====================================')  
#     end 
#    
# end    
# 
# 
# disp(['END  ', datestr(now,'yyyymmdd-HHMM')]); 
#      
# end
#   
#   
#  
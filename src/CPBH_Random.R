#############################################################
# COST POOL BUILDING HEURISTIC - RANDOM
# The algorithm orientates to Balakrishnan, Hansen, Labro 2011
#############################################################

MAP_RES_CP_RANDOM<-function(PRODUCTION_ENVIRONMENT,COSTING_SYSTEM,CP){
  
 RCC=PRODUCTION_ENVIRONMENT$RCC
 RCCn = length(RCC)

 ## r
   repeat{
     
     ACP_SIZE<-rmultinom(n = 1, size = RCCn, prob = rep(1/CP, CP))    # validate
     
     # break if every ACP has at least one resource 
     if(any(ACP_SIZE==0)==FALSE){
       break
     }
   }
   
   RC_to_ACP<-split(sample(c(1:RCCn),RCCn),rep(1:CP,ACP_SIZE)) #Assign Resources (RC) to ACP
   
   ACP<-vector(mode="numeric")
   for (i in 1:length(RC_to_ACP)) {
     
     ACP[i]<-sum(RCC[RC_to_ACP[[i]]])
     
   }
   
   COSTING_SYSTEM$RC_ACP = RC_to_ACP
  
   return(COSTING_SYSTEM)
 }
 
 
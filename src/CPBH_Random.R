#############################################################
# COST POOL BUILDING HEURISTIC - RANDOM
# The algorithm orientates to Balakrishnan, Hansen, Labro 2011
#############################################################

MAP_RES_CP_RANDOM<-function(FIRM){
  
 CP = FIRM$COSTING_SYSTEM$CP
 RCC= FIRM$COSTING_SYSTEM$RCC
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
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
  
   return(FIRM)
 }
 
 
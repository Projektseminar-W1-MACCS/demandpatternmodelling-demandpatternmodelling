#############################################################
# COST POOL BUILDING HEURISTICS
# The algorithm orientates to 
# Balakrishnan, Hansen, Labro 2011
# Anand, Balakrishnan, Labro 2019
#############################################################
MAP_RES_CP_RANDOM<-function(FIRM){
#### RANDOM ALLOCATION OF RESOURCES TO COST POOLS #### 
   
   
 CP = FIRM$COSTING_SYSTEM$CP
 RCC= FIRM$COSTING_SYSTEM$RCC
 RCCn = length(RCC)

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

 
MAP_RES_CP_SIZERANDOM<-function(FIRM){
   #### SIZE-BASED RANDOM ALLOCATION OF RESOURCES TO COST POOLS ####    
   CP = FIRM$COSTING_SYSTEM$CP
   RCC= FIRM$COSTING_SYSTEM$RCC
   RCCn = length(RCC)
   
  
   ####---- pre allocation of largest resorces ----####
   ACP_pre1<-vector(mode="numeric")
   RCCs_random_index <- vector(mode = "list", CP)
   RC_to_ACP_pre1 = list()

   RCCs2=list()
   RCCs2$ix = vector(mode="numeric")
   RCCs2$x =  vector(mode="numeric")
  
   
   
   #### Find the largest RCC (resource costs) for one cost pool each####
      RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sort resource costs 
   
   for (i in 1:CP){               # assign the biggest RCC each to one cost pool
      
      ACP_pre1[i]<-RCCs$x[i]
      RC_to_ACP_pre1[[i]]<-RCCs$ix[i]
    
   }
   
  RCCs2$x = RCCs$x[! RCCs$x %in% RCCs$x[1:CP]]  ## Can be improved in speed;
  RCCs2$ix = RCCs$ix[! RCCs$ix %in% RCCs$ix[1:CP]]
 
    ####---- Assign other RC randomly ----####
   

   ACP_SIZE<-rmultinom(n = 1, size = length(RCCs2$x), prob = rep(1/CP, CP))    #Validate
      

   
   RCCs_to_CPs_random_draw<-split(sample(c(1:length(RCCs2$ix)),length(RCCs2$ix)),rep(1:CP,ACP_SIZE)) #Assign Resources (RC) to ACP
   
   
   
   
   ########  PROBLEM #########
  
   for (i in 1:length(RCCs_to_CPs_random_draw))
   {
     idx = as.numeric(names(RCCs_to_CPs_random_draw[i]))
     RCCs_random_index[[idx]]= RCCs_to_CPs_random_draw[[i]]
   }
   

   ACP_pre2<-vector(mode="numeric")
   for (i in 1:length(RCCs_random_index)) {
   
   ACP_pre2[i] = sum(RCCs2$x[RCCs_random_index[[i]]]) 
      
   }
   
   # SUMS ARE CHECKED 23/09/2019 
   
   
   ACP<-ACP_pre1+ACP_pre2
   #RC_to_ACP = vector(mode="numeric")
   
   
   # Bringing the pre index vectors RC_to_ACP_pre together
   RC_to_ACP = list()
  
   
    for (i in 1:CP) {
       RC_to_ACP[[i]]<-c(RC_to_ACP_pre1[[i]],RCCs2$ix[RCCs_random_index[[i]]])
    }
 
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
}






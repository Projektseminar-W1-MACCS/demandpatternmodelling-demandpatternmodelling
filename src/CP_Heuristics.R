################################################
# COST POOL BUILDING HEURISTICS
# The algorithms orientate to 
# Balakrishnan, Hansen, Labro 2011
# Anand, Balakrishnan, Labro 2019
################################################

### BALAKRISHNAN et al. 2011

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
   CP = FIRM$COSTING_SYSTEM$CP                  #
   RCC= FIRM$COSTING_SYSTEM$RCC                 #
   RCCn = length(RCC)                           #number of resources that need to be allocated to cost pools
   
  
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
   
  RCCs2$x = RCCs$x[! RCCs$x %in% RCCs$x[1:CP]]  ## Can be improved in speed; the remaining resources that are not yet allocated to a CP
  RCCs2$ix = RCCs$ix[! RCCs$ix %in% RCCs$ix[1:CP]]
 
    ####---- Assign other RC randomly ----####
   

   ACP_SIZE<-rmultinom(n = 1, size = length(RCCs2$x), prob = rep(1/CP, CP))    #Validate
      

   
   RCCs_to_CPs_random_draw<-split(sample(c(1:length(RCCs2$ix)),length(RCCs2$ix)),rep(1:CP,ACP_SIZE)) #Assign  remaining Resources (RC) to ACP
   
   
   
   
  
   if(length(RCCs_to_CPs_random_draw)>0)                       #if there are remaining, not assigned resources, if CP = Resources, these steps are not necessary
   {
   
   for (i in 1:length(RCCs_to_CPs_random_draw))                   #for every cost pools that gets at least one of the remaining resources
   {
     idx = as.numeric(names(RCCs_to_CPs_random_draw[i]))          # set idx (index) as the cost pool that gets the i. resource that is remaining 
     RCCs_random_index[[idx]]= RCCs_to_CPs_random_draw[[i]]       
   }
   

   ACP_pre2<-vector(mode="numeric")
   for (i in 1:length(RCCs_random_index)) {
   
   ACP_pre2[i] = sum(RCCs2$x[RCCs_random_index[[i]]]) 
      
   }
   
   
   # SUMS ARE CHECKED 23/09/2019 
   
   
   ACP<-ACP_pre1+ACP_pre2
   }
   
   ACP<-ACP_pre1
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

MAP_RES_CP_SIZECORREL<-function(FIRM){
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

MAP_RES_CP_RANDOMCORREL<-function(FIRM){
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

MAP_RES_CP_SIZEMISC<-function(FIRM){
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

################################################

#### ANAND et sl. 2019 -> NOT ADAPATED TO NEW NAMES; 

MAP_CP_CORREL_MISC<-function(ProductionEnvironment,CostSystem,CP,MISCPOOLSIZE,CC=0.4){
   
   
   RC = CostSystem$RCC
   RCn= length(RC)
   PEARSONCORR<-ProductionEnvironment$PEARSONCORR
   
   RCs<-sort(RC,decreasing = TRUE,index.return=TRUE)   # sorted Resource cost vector
   
   if(CP>1){
      ####---- pre allocation (one pool left open) ----####
      RC_to_ACP<-list()
      ACP_pre1<-rep(0,(CP-1))
      for (i in 1:(CP-1)){               # assign the biggest Resource -1  each to one activity pool
         
         ACP_pre1[i]<-RCs$x[i]
         RC_to_ACP[[i]]<-RCs$ix[i]
         
      }
      
      ####---- Correlation Based Assigned ----####
      already_assigned<-unlist(RC_to_ACP)
      
      # Initialize vector for allocation of ACP-1 correlation and MISC
      ACP_pre2<-rep(0,CP)
      
      
      ## compute correlation between unassigned resources and assigned
      RC_correl<-PEARSONCORR[already_assigned,]
      
      if(CP==2){
         RC_correl<-t(as.matrix(RC_correl))
      }
      
      colnames(RC_correl)<-1:RCn
      rownames(RC_correl)<-already_assigned
      RC_correl<-RC_correl[,-already_assigned]
      
      if(CP==2){
         RC_correl<-t(as.matrix(RC_correl))
      }else if( CP==NCOL(PEARSONCORR)){
         RC_correl<-as.matrix(RC_correl)
         colnames(RC_correl)<-c(1:RCn)[!1:RCn %in% already_assigned]
      }
      
      #for each resource find the ACP-1 pool with the highest correlation
      # miscRes is a list returning the correlation and the index of ACP-1 cost pool
      miscRes<-apply(RC_correl,2,function(x){list(cor=max(x),pool=which(x==max(x)))})
      
      miscRes<-sapply(miscRes,function(x){
         c(cor=x$cor,
           pool=as.integer(x$pool[1]))
      })
      miscRes<-as.matrix(t(miscRes))
      miscRes<-cbind(res=as.integer(rownames(miscRes)),miscRes)
      
      # miscRes is now a ordered matrix where the first column represents the unassigned res
      # the second the correlation and the third the ACP pool
      miscRes<-miscRes[order(miscRes[,2],decreasing = TRUE),]
      
      if(CP==NCOL(PEARSONCORR)){
         miscRes<-t(as.matrix(miscRes))
      }
      
      
      ## Start assigning the rescources until MISCPOOLSIZE is reached
      cutoff_Reached<-sum(RC[-already_assigned])/sum(RC) < MISCPOOLSIZE
      
      for (i in 1:NROW(miscRes)) {
         
         if(cutoff_Reached==FALSE & miscRes[i,2]>=CC){
            acp_pool<-miscRes[i,3]
            res<-miscRes[i,1]
            ACP_pre2[acp_pool]<-ACP_pre2[acp_pool]+RC[res]
            RC_to_ACP[[acp_pool]]<-c(RC_to_ACP[[acp_pool]],res)
            already_assigned<-unlist( RC_to_ACP)
            cutoff_Reached<-sum(RC[-already_assigned])/sum(RC) < MISCPOOLSIZE
         }else{
            break
         }
         
      }
      
      # ## add remaining rescources to the misc pool
      RC_to_ACP[[CP]]<-as.integer(miscRes[i:NROW(miscRes),1])
      ACP_pre2[CP]<-sum(RC[RC_to_ACP[[CP]]])
      
      
      ACP<-c(ACP_pre1,0)+ACP_pre2
      
      
   }else{
      
      
      CS<-MAP_RES_CP_RANDOM(CostSystem,CP)
      ACP<-CS$ACP
      RC_to_ACP<-CS$assign
      
   }
   
   
   
   return(list(ACP=ACP,assign=RC_to_ACP))
   
}

MAP_CP_SIZE_RANDOM_MISC<-function(ProductionEnvironment,CostSystem,CP,MISCPOOLSIZE){
   
   
   
   RC = CostSystem$RCC
   RCn= length(RC)
   
   
   RCs<-sort(RC,decreasing = TRUE,index.return=TRUE)   # sorted Resource cost vector
   
   if(CP>1){
      ####---- pre allocation (one pool left open) ----####
      RC_to_ACP<-list()
      ACP_pre1<-rep(0,(CP-1))
      for (i in 1:(CP-1)){               # assign the biggest Resource -1  each to one activity pool
         
         ACP_pre1[i]<-RCs$x[i]
         RC_to_ACP[[i]]<-RCs$ix[i]
         
      }
      already_assigned<-unlist(RC_to_ACP)
      unassigned_RES<-c(1:RCn)[-already_assigned]
      
      ####---- Random Assignment ----####
      ## assign randomly to pool until misc size is reaced
      ACP_pre2<-rep(0,CP)
      miscPoolPrct<-sum(RC[-already_assigned])/sum(RC)
      
      while (miscPoolPrct>MISCPOOLSIZE &
             sum(RC[-ifelse(length(already_assigned[-1])==0,already_assigned,already_assigned[-1])])>0) {
         ## choose random ACP-1 pool
         pool<-sample(1:(CP-1),1)
         res<-unassigned_RES[1]
         ACP_pre2[pool]<-ACP_pre2[pool]+RC[res]
         RC_to_ACP[[pool]]<-c(RC_to_ACP[[pool]],res)
         
         already_assigned<-unlist(RC_to_ACP)
         unassigned_RES<-c(1:RCn)[-already_assigned]
         miscPoolPrct<-sum(RC[-already_assigned])/sum(RC)
         
         
      }
      
      ## add remaining rescources to the misc pool
      RC_to_ACP[[CP]]<-unassigned_RES
      ACP_pre2[CP]<-sum(RC[unassigned_RES])
      
      ACP<-c(ACP_pre1,0)+ACP_pre2
      
      
   }else{
      
      
      CS<-MAP_RES_CP_RANDOM(CostSystem,CP)
      ACP<-CS$ACP
      RC_to_ACP<-CS$assign
      
   }
   
   
   return(list(ACP=ACP,assign=RC_to_ACP))
   
}

MAP_CP_CORREL_CUTOFF<-function(ProductionEnvironment,CostSystem,CP,MISCPOOLSIZE,CC=0.4){
   
   
   RC = CostSystem$RCC
   RCn= length(RC)
   PEARSONCORR<-ProductionEnvironment$PEARSONCORR
   
   
   # RCs<-sort(RC,decreasing = TRUE,index.return=TRUE)   # sorted Resource cost vector
   
   if(CP>1){
      ####---- pre allocation (first pool -> larges res) ----####
      RC_to_ACP<-list()
      ACP_pre1<-rep(0,(CP-1))
      ACP_pre1[1]<-RCs$x[1]
      RC_to_ACP[[1]]<-RCs$ix[1]
      
      already_assigned<-unlist(RC_to_ACP)
      unassigned_RES<-c(1:RCn)[-already_assigned]
      
      ACP_pre2<-rep(0,CP)
      RCs<-sort(RC[-already_assigned],decreasing = TRUE,index.return=TRUE)
      
      for (pool in 1:(CP-1)) {
         
         # choose random resource
         seedResource<-sample(unassigned_RES,1)
         poolsToBeFilled<-CP-pool
         
         correlations<-PEARSONCORR[seedResource,]
         correlations<-sort(correlations,decreasing = TRUE,index.return=TRUE)
         correlations$x<-correlations$x[correlations$ix!=seedResource & correlations$ix %in% unassigned_RES]
         correlations$ix<-correlations$ix[correlations$ix!=seedResource & correlations$ix %in% unassigned_RES]
         
         cond1<-correlations$x[1]>CC
         cond2<-length(unassigned_RES) > poolsToBeFilled
         
         miscPoolPrct<-sum(RC[-already_assigned])/sum(RC)
         cond3<-miscPoolPrct > MISCPOOLSIZE
         
         while (cond1 & cond2 & cond3) {
            
            # Find the index of the resource with the maximum correlation with the seed resource
            maxCorr <- correlations$x[1]
            maxCorrIndx <- correlations$ix[1]
            
            # Add it to the current pool
            if(length(RC_to_ACP)<pool){
               RC_to_ACP[[pool]]<-maxCorrIndx
            }else{
               RC_to_ACP[[pool]]<-c(RC_to_ACP[[pool]],maxCorrIndx)
            }
            
            ACP_pre2[pool]<-ACP_pre2[pool]+RC[maxCorrIndx]
            
            # Remove it from the remainingResources list
            already_assigned<-unlist(RC_to_ACP)
            unassigned_RES<-c(1:RCn)[-already_assigned]
            
            correlations<-PEARSONCORR[seedResource,]
            correlations<-sort(correlations,decreasing = TRUE,index.return=TRUE)
            correlations$x<-correlations$x[correlations$ix!=seedResource & correlations$ix %in% unassigned_RES]
            correlations$ix<-correlations$ix[correlations$ix!=seedResource & correlations$ix %in% unassigned_RES]
            
            cond1<-correlations$x[1]>CC
            cond2<-length(unassigned_RES) > poolsToBeFilled
            
            miscPoolPrct<-sum(RC[-already_assigned])/sum(RC)
            cond3<-miscPoolPrct > MISCPOOLSIZE
            
            
         }
         
      }
      
      ## add remaining rescources to the misc pool
      RC_to_ACP[[CP]]<-unassigned_RES
      ACP_pre2[CP]<-sum(RC[unassigned_RES])
      
      ACP<-c(ACP_pre1,0)+ACP_pre2
      
      
      
   }else{
      
      
      CS<-MAP_RES_CP_RANDOM(CostSystem,CP)
      ACP<-CS$ACP
      RC_to_ACP<-CS$assign
      
   }
   
   
   return(list(ACP=ACP,assign=RC_to_ACP))
   
   
}


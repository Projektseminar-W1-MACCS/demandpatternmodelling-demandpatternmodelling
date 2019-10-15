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
     ACP_SIZE<-rmultinom(n = 1, size = RCCn, prob = rep(1/CP, CP))    # validate, define size (#of RC) of each ACP
     
     # break if every ACP has at least one resource 
     if(any(ACP_SIZE==0)==FALSE){
       break
     }
   }
   
   RC_to_ACP<-split(sample(c(1:RCCn),RCCn),rep(1:CP,ACP_SIZE)) #Assign Resources (RC) to ACP, by splitting the # RC into each ACP and taking into account the ACP_Size
   
   ACP<-vector(mode="numeric") #empty vector for all ACPs volume
   for (i in 1:length(RC_to_ACP)) {
     
     ACP[i]<-sum(RCC[RC_to_ACP[[i]]]) # assign to every row in ACP the sum of resource costs for evey ACP in to RC_to_ACP
     
   }
 
   FIRM$COSTING_SYSTEM$ACP = ACP             #class ACP as a variable of the firms costing system
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP    #class RC_to_ACP as a variable of the firms costing system 
  
   return(FIRM)
 } #fully implemented

MAP_RES_CP_SIZE_RANDOM<-function(FIRM){
   #### SIZE-BASED RANDOM ALLOCATION OF RESOURCES TO COST POOLS ####    
   CP = FIRM$COSTING_SYSTEM$CP                  #
   RCC= FIRM$COSTING_SYSTEM$RCC                 #
   RCCn = length(RCC)                           #number of resources that need to be allocated to cost pools
   
  
   ####---- pre allocation of largest resorces ----####
   ACP_pre1<-vector(mode="numeric")                      #empty vector for ACP-biggest resource assignment
   RCCs_random_index <- vector(mode = "list", CP)        #
   RC_to_ACP_pre1 = list()

   RCCs2=list()
   RCCs2$ix = vector(mode="numeric")
   RCCs2$x =  vector(mode="numeric")
  
   
   
   #### Find the largest RCC (resource costs) for one cost pool each####
      RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sort resource costs 
   
   for (i in 1:CP){                    #assign the biggest RCC each to one cost pool
      
      ACP_pre1[i]<-RCCs$x[i]           #Volume for each of the biggest Resources
      RC_to_ACP_pre1[[i]]<-RCCs$ix[i]  #Resource itself (index)
    
   }

  RCCs2$x = RCCs$x[! RCCs$x %in% RCCs$x[1:CP]]  ## Can be improved in speed; the remaining resources that are not yet allocated to a CP
  RCCs2$ix = RCCs$ix[! RCCs$ix %in% RCCs$ix[1:CP]]
 
    ####---- Assign other RC randomly ----####
   

   ACP_SIZE<-rmultinom(n = 1, size = length(RCCs2$x), prob = rep(1/CP, CP))    #Validate; defines sizes of remaining cost pools (No. of RC)
    
   
   ###these steps are only necessary if after the size assignment step there are still remaining resources that are not yet assigned to an ACP###

   if(NUMB_RES>CP){                                                #if there are more resources than cost pools

   
      RCCs_to_CPs_random_draw<-split(sample(c(1:length(RCCs2$ix)),length(RCCs2$ix)),rep(1:CP,ACP_SIZE)) #Assign  remaining Resources (RC) to ACP   
   
   
      for (i in 1:length(RCCs_to_CPs_random_draw))                   #for every cost pools that gets at least one of the remaining resources
      {
     idx = as.numeric(names(RCCs_to_CPs_random_draw[i]))          #set idx (index) as the cost pool that gets the i. resource that is remaining 
     RCCs_random_index[[idx]]= RCCs_to_CPs_random_draw[[i]]       
      }
   

      ACP_pre2<-vector(mode="numeric")                               #empty vector for the assignment of the remaining resources to ACPs
      for (i in 1:length(RCCs_random_index)) {
   
      ACP_pre2[i] = sum(RCCs2$x[RCCs_random_index[[i]]])             # sum up the volume of the now assigned resources togeher that are in one ACP
      
      }
   
   
   # SUMS ARE CHECKED 23/09/2019 
   
   
   ACP<-ACP_pre1+ACP_pre2                                         #The total volume of each ACP is the sum of all resources together in one ACP
   
   } else{ 
      ACP <- ACP_pre1                                             #if there was no second assignment of remaining resources (No. of RC = No. of ACP) all RC are in ACP_pre1
   }
   
   #RC_to_ACP = vector(mode="numeric")
   
   
   # Bringing the pre index vectors RC_to_ACP_pre together
   RC_to_ACP = list()
  
   
    for (i in 1:CP) {
       RC_to_ACP[[i]]<-c(RC_to_ACP_pre1[[i]],RCCs2$ix[RCCs_random_index[[i]]])   #states which resources are in each ACP
    }
 
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
}#fully implemented

MAP_RES_CP_SIZE_CORREL<-function(FIRM){
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
   
   
   ACP_SIZE<-rmultinom(n = 1, size = length(RCCs2$x), prob = rep(1/CP, CP))    #Validate  
   
   
   
   if(NUMB_RES> CP)                                         #if there are more resources than cost pools
   {
      
      ###---correlative assignment of remaining resources---###
      
       
      
      
      
      ACP<-ACP_pre1+ACP_pre2                                         #The total volume of each ACP is the sum of all resources together in one ACP
   
   } else{
      
         ACP <- ACP_pre1                                             #if there was no second assignment of remaining resources (No. of RC = No. of ACP) all RC are in ACP_pre1
   }
   
   
   # Bringing the pre index vectors RC_to_ACP_pre together
   RC_to_ACP = list()
   
   
   for (i in 1:CP) {
      RC_to_ACP[[i]]<-c(RC_to_ACP_pre1[[i]],RCCs2$ix[RCCs_random_index[[i]]])
   }
   
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
}

MAP_RES_CP_RANDOM_CORREL<-function(FIRM){
   #### Random Allocation of Resources to Cost Pools and then correlative allocation ####    
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
   
   
   
   #### Random assignment of one resource to each Cost Pool####
   RCCr = list()
   RCCr$x = sample(RCC)          #randomize resources
   RCCr$ix = match(RCCr$x, RCC)  #keep same index of RCC in RCCr
   
   
   for (i in 1:CP){               # assign one random RCC to one cost pool each
      
      ACP_pre1[i]<-RCCr$x[i]
      RC_to_ACP_pre1[[i]]<-RCCr$ix[i]
      
   }
   
   RCCs2$x = RCCr$x[! RCCr$x %in% RCCr$x[1:CP]]  ## Can be improved in speed;
   RCCs2$ix = RCCr$ix[! RCCr$ix %in% RCCr$ix[1:CP]]
   
   
   ACP_SIZE<-rmultinom(n = 1, size = length(RCCs2$x), prob = rep(1/CP, CP))    #Validate
   
   
   
   
   if(NUMB_RES > CP)
   {
      
      
      ###---correlative assignment of remaining resources---###
   
   
   
   ACP<-ACP_pre1+ACP_pre2
   #RC_to_ACP = vector(mode="numeric")
   
   } else{
      
      ACP = ACP_pre1
   }
   
   # Bringing the pre index vectors RC_to_ACP_pre together
   RC_to_ACP = list()
   
   
   for (i in 1:CP) {
      RC_to_ACP[[i]]<-c(RC_to_ACP_pre1[[i]],RCCs2$ix[RCCs_random_index[[i]]])
   }
   
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
}

MAP_RES_CP_SIZE_MISC<-function(FIRM){
   #### SIZE-BASED RANDOM ALLOCATION OF RESOURCES TO COST POOLS ####    
   CP = FIRM$COSTING_SYSTEM$CP
   RCC= FIRM$COSTING_SYSTEM$RCC
   RCCn = length(RCC)
   
   
   ####---- pre allocation of largest resorces ----####
   ACP_pre1<-vector(mode="numeric",length = (CP-1))
   RCCs_random_index <- vector(mode = "list", CP)
   RC_to_ACP_pre1 = list()
   
   RCCs2=list()
   RCCs2$ix = vector(mode="numeric")
   RCCs2$x =  vector(mode="numeric")
   
   
   
   #### Find the largest RCC (resource costs) for one cost pool each####
   RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sort resource costs 
   
   
   for (i in 1:(CP-1)){               # assign the biggest RCC each to one cost pool
      
      ACP_pre1[i]<-RCCs$x[i]
      RC_to_ACP_pre1[[i]]<-RCCs$ix[i]
      
   }
   
  
   
   ####---- Assign remaining RC to Misc Pool ----####
   
   RCCs2$x = RCCs$x[! RCCs$x %in% RCCs$x[1:(CP-1)]]  ## Can be improved in speed;
   RCCs2$ix = RCCs$ix[! RCCs$ix %in% RCCs$ix[1:(CP-1)]]
   
   
   ACP_misc = sum(RCCs2$x)
   RC_to_ACP_misc = list(RCCs2$ix)
   
   
   ACP = vector(mode='numeric', length = 30)
   ACP<-append(ACP_pre1, ACP_misc, after = (CP-1))
   ACP
      #RC_to_ACP = vector(mode="numeric")
   
   # Bringing the pre index vectors RC_to_ACP_pre together
   RC_to_ACP = list()
   
   RC_to_ACP = append(RC_to_ACP_pre1, RC_to_ACP_misc)
   
   
   
   
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
}  #fully implemented

################################################

#### ANAND et al. 2019 -> NOT ADAPATED TO NEW NAMES; 

MAP_RES_CP_SIZE_CORREL_MISC<-function(FIRM){
   
   ##INIT##
   
   RCC = FIRM$CostSystem$RCC
   RCCn= length(RCC)
   PEARSONCORR<-FIRM$PRODUCTION_ENVIRONMENT$COR        # COR
   
   RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sorted Resource cost vector
   
   
   ####SIZE RULE####
   if(CP>1){
      ####---- pre allocation (one pool left open) ----####
      RC_to_ACP<-list()
      ACP_pre1<-vector(mode ='numeric', length = (CP-1))    #rep(0,(CP-1))
      for (i in 1:(CP-1)){               # assign the biggest Resource -1  each to one activity pool
         
         ACP_pre1[i]<-RCCs$x[i]
         RC_to_ACP[[i]]<-RCCs$ix[i]
         
      }
      
   ####---- Correlation Based Assigned ----####
      already_assigned<-unlist(RC_to_ACP)          #transforms the list into a vector with all resources that are already assigned
      not_assigned <- RCCs$ix[CP:length(RCC)]
      
      
      # Initialize vector for allocation of ACP_pre1 correlation and MISC
      ACP_pre2<-vector(mode='numeric', length = CP)
      
      
      ## compute correlation between unassigned resources and assigned
      
      RES_CONS_PAT = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PAT
      MISCPOOLSIZE = 0.25 
      
      
      
      ####BUILDIUNG OF CORRELATION MATRIX####
      
      ##Create empty matrix that shows correlation between assigned and unassigned resources
      RC_Correl = matrix(nrow = length(already_assigned), ncol = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)#empty matrix for correlations between assigned and not assigned resources
      #RC_Correl = matrix(nrow = 3, ncol = 3)
      
      ##fill empty matrix with correlations
      for (i in 1:length(already_assigned)){
         for (j in 1:ncol(RES_CONS_PAT)){
            
            RC_Correl[i,j] = cor(RES_CONS_PAT[,already_assigned[i]],RES_CONS_PAT[,j])
            
         }
      }
      
      
      colnames(RC_Correl) = paste(c(1:50))   #changing the column names to the resource number
      RC_Correl = RC_Correl[,-already_assigned] #delete resources that are already assigned from Correlation Matrix, so they dont get assigned twice
      
      #Assign resources to ACPs based on the correlation as long as there are more resources unassigned than the Miscpoolsize
      #Sorting the RC_Correl Matrix by high correlations
      
      
      ####SORTING THE CORRELATION MATRIX BY BIGGEST CORRELATIONS####
      
      new_order = list()
      for (i in 1:ncol(RC_Correl)){
         
       new_order$x[i]<- max(RC_Correl[,i])
       new_order$ix[i]<- which.max(RC_Correl[i,])
       
      }
      
      
      which.max(RC_Correl)
      which.max(RC_Correl[,1])
      
      RC_Correl<- RC_Correl[,order(new_order$x,decreasing = TRUE), drop = F]
      
      
      
      ###ASSIGNMENT OF CORRELATIVE RESOURECS TO COST POOLS####
      
      
      While(length(not_assigned)>(floor(MISCPOOLSIZE*FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)))
      
      {
         
      for (i in 1:(length(not_assigned)-FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES*MISCPOOLSIZE)){
         
         RC_to_ACP[which.max(RC_Correl[,i])] <- unlist(c(RC_to_ACP[[i]],not_assigned[i]))
         not_assigned = not_assigned[]
      }
      }
      
      
   
      
      
      
      
      RC_correl<-PEARSONCORR[already_assigned,]
      
      if(CP==2){
         RC_correl<-t(as.matrix(RC_correl))
      }
      
      colnames(RC_correl)<-1:RCCn
      rownames(RC_correl)<-already_assigned
      RC_correl<-RC_correl[,-already_assigned]
      
      if(CP==2){
         RC_correl<-t(as.matrix(RC_correl))
      }else if( CP==NCOL(PEARSONCORR)){
         RC_correl<-as.matrix(RC_correl)
         colnames(RC_correl)<-c(1:RCCn)[!1:RCCn %in% already_assigned]
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
      cutoff_Reached<-sum(RCC[-already_assigned])/sum(RCC) < MISCPOOLSIZE
      
      for (i in 1:NROW(miscRes)) {
         
         if(cutoff_Reached==FALSE & miscRes[i,2]>=CC){
            acp_pool<-miscRes[i,3]
            res<-miscRes[i,1]
            ACP_pre2[acp_pool]<-ACP_pre2[acp_pool]+RCC[res]
            RC_to_ACP[[acp_pool]]<-c(RC_to_ACP[[acp_pool]],res)
            already_assigned<-unlist(RC_to_ACP)
            cutoff_Reached<-sum(RC[-already_assigned])/sum(RCC) < MISCPOOLSIZE
         }else{
            break
         }
         
      }
      
      # ## add remaining rescources to the misc pool
      RC_to_ACP[[CP]]<-as.integer(miscRes[i:NROW(miscRes),1])
      ACP_pre2[CP]<-sum(RCC[RC_to_ACP[[CP]]])
      
      
      ACP<-c(ACP_pre1,0)+ACP_pre2
      
      
   }else{
      
      
      CS<-MAP_RES_CP_RANDOM(CostSystem,CP)
      ACP<-CS$ACP
      RC_to_ACP<-CS$assign
      
   }
   
   
   
   return(list(ACP=ACP,assign=RC_to_ACP))
   
}               #size correl misc

MAP_RES_CP_SIZE_RANDOM_MISC<-function(ProductionEnvironment,CostSystem,CP,MISCPOOLSIZE){
   
   stop("Not fully implemented yet")
   
   
   RCC = CostSystem$RCC
   RCCn= length(RCC)
   
   
   RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sorted Resource cost vector
   
   if(CP>1){
      ####---- pre allocation (one pool left open) ----####
      RC_to_ACP<-list()
      ACP_pre1<-rep(0,(CP-1))
      for (i in 1:(CP-1)){               # assign the biggest Resource -1  each to one activity pool
         
         ACP_pre1[i]<-RCCs$x[i]
         RC_to_ACP[[i]]<-RCCs$ix[i]
         
      }
      already_assigned<-unlist(RC_to_ACP)
      unassigned_RES<-c(1:RCCn)[-already_assigned]
      
      ####---- Random Assignment ----####
      ## assign randomly to pool until misc size is reaced
      ACP_pre2<-rep(0,CP)
      miscPoolPrct<-sum(RCC[-already_assigned])/sum(RCC)
      
      while (miscPoolPrct>MISCPOOLSIZE &
             sum(RCC[-ifelse(length(already_assigned[-1])==0,already_assigned,already_assigned[-1])])>0) {
         ## choose random ACP-1 pool
         pool<-sample(1:(CP-1),1)
         res<-unassigned_RES[1]
         ACP_pre2[pool]<-ACP_pre2[pool]+RCC[res]
         RC_to_ACP[[pool]]<-c(RC_to_ACP[[pool]],res)
         
         already_assigned<-unlist(RC_to_ACP)
         unassigned_RES<-c(1:RCCn)[-already_assigned]
         miscPoolPrct<-sum(RCC[-already_assigned])/sum(RCC)
         
         
      }
      
      ## add remaining rescources to the misc pool
      RC_to_ACP[[CP]]<-unassigned_RES
      ACP_pre2[CP]<-sum(RCC[unassigned_RES])
      
      ACP<-c(ACP_pre1,0)+ACP_pre2
      
      
   }else{
      
      
      CS<-MAP_RES_CP_RANDOM(CostSystem,CP)
      ACP<-CS$ACP
      RC_to_ACP<-CS$assign
      
   }
   
   
   return(list(ACP=ACP,assign=RC_to_ACP))
   
}       #size random misc

MAP_RES_CP_SIZE_CORREL_CUTOFF<-function(ProductionEnvironment,CostSystem,CP,MISCPOOLSIZE,CC=0.4){
   
   stop("Not fully implemented yet")
   
   RCC = CostSystem$RCC
   RCCn= length(RCC)
   PEARSONCORR<-ProductionEnvironment$PEARSONCORR              #Pearsoncorr = Init COR?
   
   
   # RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sorted Resource cost vector
   
   if(CP>1){
      ####---- pre allocation (first pool -> larges res) ----####
      RC_to_ACP<-list()
      ACP_pre1<-rep(0,(CP-1))
      ACP_pre1[1]<-RCCs$x[1]
      RC_to_ACP[[1]]<-RCCs$ix[1]
      
      already_assigned<-unlist(RC_to_ACP)
      unassigned_RES<-c(1:RCn)[-already_assigned]
      
      ACP_pre2<-rep(0,CP)
      RCCs<-sort(RCC[-already_assigned],decreasing = TRUE,index.return=TRUE)
      
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
         
         miscPoolPrct<-sum(RCC[-already_assigned])/sum(RCC)
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
            
            ACP_pre2[pool]<-ACP_pre2[pool]+RCC[maxCorrIndx]
            
            # Remove it from the remainingResources list
            already_assigned<-unlist(RC_to_ACP)
            unassigned_RES<-c(1:RCCn)[-already_assigned]
            
            correlations<-PEARSONCORR[seedResource,]
            correlations<-sort(correlations,decreasing = TRUE,index.return=TRUE)
            correlations$x<-correlations$x[correlations$ix!=seedResource & correlations$ix %in% unassigned_RES]
            correlations$ix<-correlations$ix[correlations$ix!=seedResource & correlations$ix %in% unassigned_RES]
            
            cond1<-correlations$x[1]>CC
            cond2<-length(unassigned_RES) > poolsToBeFilled
            
            miscPoolPrct<-sum(RCC[-already_assigned])/sum(RCC)
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
   
   
}   #size correl cutoff





####### other CP Mapping Heuristics#####

#MAP_RES_CP_SIZE_CORREL_MISC
#Correlation not one by one, but by maximizing the overall correlation
#So it becomes possible, that an ACP, which has already a resource assigned(by size), gets more than one remaining resource assigned based on correlation
#Instead of looking at the highest correlation between one not assigned resource and an ACP it compares every correlation and is therefore capable of assigning more resources to one ACP


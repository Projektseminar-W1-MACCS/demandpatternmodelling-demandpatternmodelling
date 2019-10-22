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
   RC_to_ACP = list()
   
   #### Find the largest RCC (resource costs) for one cost pool each####
   RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sort resource costs 
   
   for (i in 1:CP){               # assign the biggest RCC each to one cost pool
      
      ACP_pre1[i]<-RCCs$x[i]
      RC_to_ACP[[i]]<-RCCs$ix[i]
      
   }
   
   
   
   already_assigned<-unlist(RC_to_ACP)          #transforms the list into a vector with all resources that are already assigned
   not_assigned <- setdiff(c(1:RCCn),already_assigned)
   
   if(NUMB_RES> CP)                                         #if there are more resources than cost pools
   {
      
      ####CORRELATION RULE####
      #### BUILDIUNG OF CORRELATION MATRIX---------------
      
      ##Create empty matrix that shows correlation between assigned and unassigned resources
      RC_Correl = matrix(nrow = length(already_assigned), ncol = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)#empty matrix for correlations between assigned and not assigned resources
      
      ##fill empty matrix with correlations
      for (i in 1:length(already_assigned)){
         for (j in 1:ncol(RES_CONS_PAT)){
            
            RC_Correl[i,j] = cor(RES_CONS_PAT[,already_assigned[i]],RES_CONS_PAT[,j])
            
         }
      }
      
      
      colnames(RC_Correl) = paste(c(1:ncol(RES_CONS_PAT)))#changing the column names to the resource number
      RC_Correl = matrix(RC_Correl[,-already_assigned], ncol = length(not_assigned)) #delete resources that are already assigned from Correlation Matrix, so they dont get assigned twice
      colnames(RC_Correl) = paste(not_assigned)
      
      #### CREATING A LIST THAT SHOWS THE ALLOCATION OF RESOURCES TO COST POOLS---------------------
      
      #Assign resources to ACPs based on the correlation as long as there are more resources unassigned than the Miscpoolsize
      #Sorting the RC_Correl Matrix by high correlations
      
      
      RC_to_ACP_cor <- which(RC_Correl>=sort(RC_Correl, decreasing = T)[ncol(RC_Correl)*nrow(RC_Correl)], arr.ind = T)#list of length of size of RC_Correl
      RC_to_ACP_cor = data.frame(RC_to_ACP_cor) #transform it into a dataframe
      
      RC_Correl_V = as.vector(RC_Correl)  #transform correlations into vector
      
      RC_to_ACP_cor$cor = RC_Correl_V     #append vector to correl dataframe, so each correlation for every CP/Res combination gets a row
      
      RC_to_ACP_cor = RC_to_ACP_cor[order(RC_to_ACP_cor$cor, decreasing = TRUE),]   #sort the df by dreasing correlations
      
      RC_to_ACP_cor = RC_to_ACP_cor[!duplicated(RC_to_ACP_cor$col, fromLast = FALSE),] #drop all duplicate resources, so you have the highest correlations left
      
      RC_to_ACP_cor = RC_to_ACP_cor[order(RC_to_ACP_cor$col, decreasing = FALSE),]     # sort it by decreasing resource so resource numbers can get changed into the not assigned ones
      
      
      for (i in RC_to_ACP_cor$col){
         
         RC_to_ACP_cor$col[i] =  colnames(RC_Correl)[i]           #change the resource names, according to the ones that are not yet assigned
      }
      
      RC_to_ACP_cor = RC_to_ACP_cor[order(RC_to_ACP_cor$cor, decreasing = TRUE),] #sort it again by decreasing correlation so the biggest correlations are assigned first
      
      
      
      #### ALLOCATING RESOURCES TO COST POOLS ------------------
      ##It is possible and allowed that more than one resource is assigned to one cost pool
      ACP_pre2<-vector(mode='numeric', length = CP)
      
     for (i in 1:length(not_assigned)) {
         
         RC_to_ACP[[RC_to_ACP_cor$row[i]]] = c(RC_to_ACP[[RC_to_ACP_cor$row[i]]],as.integer(RC_to_ACP_cor$col[i]))
         ACP_pre2[[RC_to_ACP_cor$row[i]]] = sum(ACP_pre2[[RC_to_ACP_cor$row[i]]],RCC[as.integer(RC_to_ACP_cor$col[i])])
         not_assigned = as.integer(RC_to_ACP_cor$col[i+1:(length(RC_to_ACP_cor$col)-i)])
      }
      
      
      
      ACP<-ACP_pre1+ACP_pre2                                         #The total volume of each ACP is the sum of all resources together in one ACP
   
      
      
   } else{
      
         ACP <- ACP_pre1                                             #if there was no second assignment of remaining resources (No. of RC = No. of ACP) all RC are in ACP_pre1
   }
   
   
   
   
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   
   return(FIRM)
}#fully impemented

MAP_RES_CP_RANDOM_CORREL<-function(FIRM){
   #### Random Allocation of Resources to Cost Pools and then correlative allocation ####    
   CP = FIRM$COSTING_SYSTEM$CP
   RCC= FIRM$COSTING_SYSTEM$RCC
   RCCn = length(RCC)
   
   
   ####---- pre allocation of largest resorces ----####
   ACP_pre1<-vector(mode="numeric")
   RCCs_random_index <- vector(mode = "list", CP)
   RC_to_ACP = list()
   
   
   #### Random assignment of one resource to each Cost Pool####
   RCCr = list()
   RCCr$x = sample(RCC)          #randomize resources
   RCCr$ix = match(RCCr$x, RCC)  #keep same index of RCC in RCCr
   
   
   for (i in 1:CP){               # assign one random RCC to one cost pool each
      
      ACP_pre1[i]<-RCCr$x[i]
      RC_to_ACP[[i]]<-RCCr$ix[i]
      
   }
   
   
   
   already_assigned<-unlist(RC_to_ACP)          #transforms the list into a vector with all resources that are already assigned
   not_assigned <- setdiff(c(1:RCCn),already_assigned)
   
   
   if(NUMB_RES > CP)
   {
      ####CORRELATION RULE####
      #### BUILDIUNG OF CORRELATION MATRIX---------------
      
      ##Create empty matrix that shows correlation between assigned and unassigned resources
      RC_Correl = matrix(nrow = length(already_assigned), ncol = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)#empty matrix for correlations between assigned and not assigned resources
      
      ##fill empty matrix with correlations
      for (i in 1:length(already_assigned)){
         for (j in 1:ncol(RES_CONS_PAT)){
            
            RC_Correl[i,j] = cor(RES_CONS_PAT[,already_assigned[i]],RES_CONS_PAT[,j])
            
         }
      }
      
      
      colnames(RC_Correl) = paste(c(1:ncol(RES_CONS_PAT)))#changing the column names to the resource number
      RC_Correl = matrix(RC_Correl[,-already_assigned], ncol = length(not_assigned)) #delete resources that are already assigned from Correlation Matrix, so they dont get assigned twice
      colnames(RC_Correl) = paste(not_assigned) #change resources names back 
      
      
      #### CREATING A LIST THAT SHOWS THE ALLOCATION OF RESOURCES TO COST POOLS---------------------
      
      #Assign resources to ACPs based on the correlation as long as there are more resources unassigned than the Miscpoolsize
      #Sorting the RC_Correl Matrix by high correlations
      
      
      RC_to_ACP_cor <- which(RC_Correl>=sort(RC_Correl, decreasing = T)[ncol(RC_Correl)*nrow(RC_Correl)], arr.ind = T)#list of length of size of RC_Correl
      RC_to_ACP_cor = data.frame(RC_to_ACP_cor) #transform it into a dataframe
      
      RC_Correl_V = as.vector(RC_Correl)  #transform correlations into vector
      
      RC_to_ACP_cor$cor = RC_Correl_V     #append vector to correl dataframe, so each correlation for every CP/Res combination gets a row
      
      RC_to_ACP_cor = RC_to_ACP_cor[order(RC_to_ACP_cor$cor, decreasing = TRUE),]   #sort the df by dreasing correlations
      
      RC_to_ACP_cor = RC_to_ACP_cor[!duplicated(RC_to_ACP_cor$col, fromLast = FALSE),] #drop all duplicate resources, so you have the highest correlations left
      
      RC_to_ACP_cor = RC_to_ACP_cor[order(RC_to_ACP_cor$col, decreasing = FALSE),]     # sort it by decreasing resource so resource numbers can get changed into the not assigned ones
      
      
      for (i in RC_to_ACP_cor$col){
         
         RC_to_ACP_cor$col[i] =  colnames(RC_Correl)[i]           #change the resource names, according to the ones that are not yet assigned
      }
      
      RC_to_ACP_cor = RC_to_ACP_cor[order(RC_to_ACP_cor$cor, decreasing = TRUE),] #sort it again by decreasing correlation so the biggest correlations are assigned first
      
      
      
      #### ALLOCATING RESOURCES TO COST POOLS AND TAKING INTO ACCOUNT THE MISCPOOLSIZE------------------
      ##It is possible and allowed that more than one resource is assigned to one cost pool
      ACP_pre2<-vector(mode='numeric', length = CP)
      
      for (i in 1:length(not_assigned)) {
         
         RC_to_ACP[[RC_to_ACP_cor$row[i]]] = c(RC_to_ACP[[RC_to_ACP_cor$row[i]]],as.integer(RC_to_ACP_cor$col[i]))
         ACP_pre2[[RC_to_ACP_cor$row[i]]] = sum(ACP_pre2[[RC_to_ACP_cor$row[i]]],RCC[as.integer(RC_to_ACP_cor$col[i])])
         not_assigned = as.integer(RC_to_ACP_cor$col[i+1:(length(RC_to_ACP_cor$col)-i)])
      }
      
      
      
   
   
   
   ACP<-ACP_pre1+ACP_pre2
   
   } else{
      
      ACP = ACP_pre1
   }
   
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
}#fully implemented

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
   
   
   if (CP > 1){
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
   
   
   ACP = vector(mode='numeric', length = CP)
   ACP<-append(ACP_pre1, ACP_misc, after = (CP-1))
   ACP
      #RC_to_ACP = vector(mode="numeric")
   
   # Bringing the pre index vectors RC_to_ACP_pre together
   RC_to_ACP = list()
   
   RC_to_ACP = append(RC_to_ACP_pre1, RC_to_ACP_misc)
   
   }else if (CP ==1){
      ACP = sum(RCC)
      RC_to_ACP = list(c(1:NUMB_RES))
   }
   
   
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
   
} #fully implemented


#### ANAND et al. 2019; 

MAP_RES_CP_SIZE_CORREL_MISC<-function(FIRM){
   
   ##INIT##
   
   CP = FIRM$COSTING_SYSTEM$CP
   RCC= FIRM$COSTING_SYSTEM$RCC
   RES_CONS_PAT = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp
   MISCPOOLSIZE = 0.25 * TC
   
   
   RCCn= length(RCC)
####SIZE RULE####
      ####pre allocation, one pool left open
   if (CP > 1){
      RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sorted Resource cost vector
      RC_to_ACP<-list()
      ACP_pre1<-vector(mode ='numeric', length = (CP-1))    #rep(0,(CP-1))
      for (i in 1:(CP-1)){               # assign the biggest Resource -1  each to one activity pool
         
         ACP_pre1[i]<-RCCs$x[i]
         RC_to_ACP[[i]]<-RCCs$ix[i]
         
      }
      already_assigned<-unlist(RC_to_ACP)          #transforms the list into a vector with all resources that are already assigned
      not_assigned <- setdiff(c(1:RCCn),already_assigned)
      #correlative assignment only if there are more than one resource in not_assigned
          if (NUMB_RES > CP){
####CORRELATION RULE####
      
      #### BUILDIUNG OF CORRELATION MATRIX---------------
      
      ##Create empty matrix that shows correlation between assigned and unassigned resources
      RC_Correl = matrix(nrow = length(already_assigned), ncol = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)#empty matrix for correlations between assigned and not assigned resources
      
      ##fill empty matrix with correlations
      for (i in 1:length(already_assigned)){
         for (j in 1:ncol(RES_CONS_PAT)){
            
            RC_Correl[i,j] = cor(RES_CONS_PAT[,already_assigned[i]],RES_CONS_PAT[,j])
            
         }
      }
      
      
      colnames(RC_Correl) = paste(c(1:ncol(RES_CONS_PAT)))#changing the column names to the resource number
      RC_Correl = matrix(RC_Correl[,-already_assigned], ncol = length(not_assigned)) #delete resources that are already assigned from Correlation Matrix, so they dont get assigned twice
      colnames(RC_Correl) = paste(not_assigned) #change resources names back 
      
      
      
      #### CREATING A LIST THAT SHOWS THE ALLOCATION OF RESOURCES TO COST POOLS---------------------
      
      #Assign resources to ACPs based on the correlation as long as there are more resources unassigned than the Miscpoolsize
      #Sorting the RC_Correl Matrix by high correlations
      
      RC_to_ACP_cor <- which(RC_Correl>=sort(RC_Correl, decreasing = T)[ncol(RC_Correl)*nrow(RC_Correl)], arr.ind = T)#list of length of size of RC_Correl
      RC_to_ACP_cor = data.frame(RC_to_ACP_cor) #transform it into a dataframe
      RC_Correl_V = as.vector(RC_Correl)  #transform correlations into vector
      RC_to_ACP_cor$cor = RC_Correl_V     #append vector to correl dataframe, so each correlation for every CP/Res combination gets a row
      RC_to_ACP_cor = RC_to_ACP_cor[order(RC_to_ACP_cor$cor, decreasing = TRUE),]   #sort the df by dreasing correlations
      RC_to_ACP_cor = RC_to_ACP_cor[!duplicated(RC_to_ACP_cor$col, fromLast = FALSE),] #drop all duplicate resources, so you have the highest correlations left
      
      RC_to_ACP_cor = RC_to_ACP_cor[order(RC_to_ACP_cor$col, decreasing = FALSE),]     # sort it by decreasing resource so resource numbers can get changed into the not assigned ones
      
      
      for (i in RC_to_ACP_cor$col){
         
         RC_to_ACP_cor$col[i] =  colnames(RC_Correl)[i]           #change the resource names, according to the ones that are not yet assigned
      }
      
      RC_to_ACP_cor = RC_to_ACP_cor[order(RC_to_ACP_cor$cor, decreasing = TRUE),] #sort it again by decreasing correlation so the biggest correlations are assigned first
   
     
        
     
      #### ALLOCATING RESOURCES TO COST POOLS AND TAKING INTO ACCOUNT THE MISCPOOLSIZE------------------
      ##It is possible and allowed that more than one resource is assigned to one cost pool
      ACP_pre2<-vector(mode='numeric', length = CP-1)
      i=1
      # while until the misc pool has a cost share of MISCPOOLSIZE
      while (sum(RCC)-sum(RCC[already_assigned])-sum(RCC[as.numeric(RC_to_ACP_cor$col[c(1:i)])])> MISCPOOLSIZE) {
         
         RC_to_ACP[[RC_to_ACP_cor$row[i]]] = c(RC_to_ACP[[RC_to_ACP_cor$row[i]]],as.integer(RC_to_ACP_cor$col[i]))
         ACP_pre2[[RC_to_ACP_cor$row[i]]] = sum(ACP_pre2[[RC_to_ACP_cor$row[i]]],RCC[as.integer(RC_to_ACP_cor$col[i])])
         not_assigned = as.integer(RC_to_ACP_cor$col[i+1:(length(RC_to_ACP_cor$col)-i)])
         i = i+1
      }
   

###MISCPOOL RULE####
      
      #Appending 
      RC_to_ACP_misc =list(not_assigned)
      RC_to_ACP = append(RC_to_ACP,RC_to_ACP_misc)
      
      
      #Adding the misc pool value to ACP
      ACP_misc = sum(RCC[not_assigned])
      ACP = append((ACP_pre1 + ACP_pre2),ACP_misc)
      
      
    
 } else{
        RC_to_ACP_misc =list(not_assigned)
        RC_to_ACP = append(RC_to_ACP,RC_to_ACP_misc)
        
        
        #Adding the misc pool value to ACP
        ACP_misc = sum(RCC[not_assigned])
        ACP = append(ACP_pre1,ACP_misc)
        
     }
      
   }else if (CP == 1){
      
      ACP = sum(RCC)
      RC_to_ACP = list(c(1:FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES))
   }
   
   
   #### SOURCING ####  
      
      FIRM$COSTING_SYSTEM$ACP = ACP
      FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
      
      return(FIRM)
} #fully implemented

MAP_RES_CP_SIZE_RANDOM_MISC<-function(FIRM){
   #### SIZE-BASED RANDOM ALLOCATION OF RESOURCES TO COST POOLS ####    
   CP = FIRM$COSTING_SYSTEM$CP                  #
   RCC= FIRM$COSTING_SYSTEM$RCC                 #
   RCCn = length(RCC)                           #number of resources that need to be allocated to cost pools
   MISCPOOLSIZE = 0.25 * FIRM$COSTING_SYSTEM$TC
   
   
   ####---- pre allocation of largest resorces ----####
   ACP_pre1<-vector(mode="numeric")                      #empty vector for ACP-biggest resource assignment
   RCCs_random_index <- vector(mode = "list", CP-1)        #
   RC_to_ACP = list()
  
   
   #### Find the largest RCC (resource costs) for one cost pool each####
   RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sort resource costs 
   
   for (i in 1:(CP-1)){                    #assign the biggest RCC each to one cost pool
      
      ACP_pre1[i]<-RCCs$x[i]           #Volume for each of the biggest Resources
      RC_to_ACP[[i]]<-RCCs$ix[i]  #Resource itself (index)
      
   }
   
   
   already_assigned<-unlist(RC_to_ACP)          #transforms the list into a vector with all resources that are already assigned
   not_assigned <- setdiff(c(1:RCCn),already_assigned)
   
   
   ####Create randomized misc pool####
   miscpool = vector(mode = 'numeric', length(not_assigned))
   not_assigned_shuffled = sample(not_assigned,length(not_assigned),replace = FALSE)
   i=1 
   while (MISCPOOLSIZE-sum(RCC[miscpool[1:i]])>max(RCC[not_assigned])){
      miscpool[[i]] = not_assigned_shuffled[i]
      i= i+1
   }
   
   
   ACP_misc = sum(RCC[miscpool])
   
   not_assigned = setdiff(not_assigned, miscpool)
  
   
   
   ###these steps are only necessary if after the size assignment step there are still remaining resources that are not yet assigned to an ACP###
   
   if(NUMB_RES>CP){                                                #if there are more resources than cost pools
      
      ACP_SIZE<-rmultinom(n = 1, size = length(not_assigned), prob = rep(1/(length(not_assigned)), (length(not_assigned)-1)))
      
      
      #Validate; defines sizes of remaining cost pools (No. of RC)
      RCCs_to_CPs_random_draw<-split(sample(c(1:length(not_assigned)),length(not_assigned)),rep(1:(length(not_assigned)-1),ACP_SIZE)) #Assign  remaining Resources (RC) to ACP   
      
      
      for (i in 1:length(RCCs_to_CPs_random_draw))                   #for every cost pools that gets at least one of the remaining resources
      {
         idx = as.numeric(names(RCCs_to_CPs_random_draw[i]))          #set idx (index) as the cost pool that gets the i. resource that is remaining 
         RCCs_random_index[[idx]]= RCCs_to_CPs_random_draw[[i]]       
      }
      
      # Bringing the pre index vectors RC_to_ACP_pre together
      for (i in 1:(CP-1)) {
         RC_to_ACP[[i]]<-c(RC_to_ACP[[i]],RCCs2$ix[RCCs_random_index[[i]]])   #states which resources are in each ACP
      
      }
            
      ACP_pre2<-vector(mode="numeric")                               #empty vector for the assignment of the remaining resources to ACPs
      
      for (i in 1:length(RCCs_random_index)) {
         
         ACP_pre2[i] = sum(RCC[RCCs_random_index[[i]]])             # sum up the volume of the now assigned resources togeher that are in one ACP
         
      }
      
      
      # SUMS ARE CHECKED 23/09/2019 
      
      
      ACP<-ACP_pre1+ACP_pre2                                         #The total volume of each ACP is the sum of all resources together in one ACP
      
   } else{ 
      ACP <- ACP_pre1                                             #if there was no second assignment of remaining resources (No. of RC = No. of ACP) all RC are in ACP_pre1
   }
   
   
   

   
   
   
   
###SOURCING
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
  
   
} #fully implemented

MAP_RES_CP_SIZE_CORREL_CUTOFF<-function(FIRM){
   
   
   ##INIT##
   
   CP = FIRM$COSTING_SYSTEM$CP
   RCC= FIRM$COSTING_SYSTEM$RCC
   RES_CONS_PAT = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp
   MISCPOOLSIZE = 0.25 * TC
   CC = 0.4 #as in Anand et al. 2019
   
   
   RCCn= length(RCC)
   ####SIZE RULE####
   ####pre allocation, one pool left open
   if (CP > 1){
      RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sorted Resource cost vector
      RC_to_ACP<-list()
      ACP_pre1<-vector(mode ='numeric', length = (CP-1))    #rep(0,(CP-1))
      for (i in 1:(CP-1)){               # assign the biggest Resource -1  each to one activity pool
         
         ACP_pre1[i]<-RCCs$x[i]
         RC_to_ACP[[i]]<-RCCs$ix[i]
         
      }
      already_assigned<-unlist(RC_to_ACP)          #transforms the list into a vector with all resources that are already assigned
      not_assigned <- setdiff(c(1:RCCn),already_assigned)
      
      
      #correlative assignment only if there are more than one resource in not_assigned
      if (NUMB_RES > CP){
         ####CORRELATION RULE####
         #### BUILDIUNG OF CORRELATION MATRIX---------------
         
         ##Create empty matrix that shows correlation between assigned and unassigned resources
         RC_Correl = matrix(nrow = length(already_assigned), ncol = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)#empty matrix for correlations between assigned and not assigned resources
         
         ##fill empty matrix with correlations
         for (i in 1:length(already_assigned)){
            for (j in 1:ncol(RES_CONS_PAT)){
               
               RC_Correl[i,j] = cor(RES_CONS_PAT[,already_assigned[i]],RES_CONS_PAT[,j])
               
            }
         }
         
         
         colnames(RC_Correl) = paste(c(1:ncol(RES_CONS_PAT)))#changing the column names to the resource number
         RC_Correl = matrix(RC_Correl[,-already_assigned], ncol = length(not_assigned)) #delete resources that are already assigned from Correlation Matrix, so they dont get assigned twice
         colnames(RC_Correl) = paste(not_assigned) #change resources names back 
         
         
         #### CREATING A LIST THAT SHOWS THE ALLOCATION OF RESOURCES TO COST POOLS---------------------
         
         #Assign resources to ACPs based on the correlation as long as there are more resources unassigned than the Miscpoolsize
         #Sorting the RC_Correl Matrix by high correlations
         
         
         RC_to_ACP_cor <- which(RC_Correl>=sort(RC_Correl, decreasing = T)[ncol(RC_Correl)*nrow(RC_Correl)], arr.ind = T)#list of length of size of RC_Correl
         RC_to_ACP_cor = data.frame(RC_to_ACP_cor) #transform it into a dataframe
         
         RC_Correl_V = as.vector(RC_Correl)  #transform correlations into vector
         
         RC_to_ACP_cor$cor = RC_Correl_V     #append vector to correl dataframe, so each correlation for every CP/Res combination gets a row
         
         RC_to_ACP_cor = RC_to_ACP_cor[order(RC_to_ACP_cor$cor, decreasing = TRUE),]   #sort the df by dreasing correlations
         
         RC_to_ACP_cor = RC_to_ACP_cor[!duplicated(RC_to_ACP_cor$col, fromLast = FALSE),] #drop all duplicate resources, so you have the highest correlations left
         
         RC_to_ACP_cor = RC_to_ACP_cor[order(RC_to_ACP_cor$col, decreasing = FALSE),]     # sort it by decreasing resource so resource numbers can get changed into the not assigned ones
         
         
         for (i in RC_to_ACP_cor$col){
            
            RC_to_ACP_cor$col[i] =  colnames(RC_Correl)[i]           #change the resource names, according to the ones that are not yet assigned
         }
         
         RC_to_ACP_cor = RC_to_ACP_cor[order(RC_to_ACP_cor$cor, decreasing = TRUE),] #sort it again by decreasing correlation so the biggest correlations are assigned first
         
         
         
         #### ALLOCATING RESOURCES TO COST POOLS AND TAKING INTO ACCOUNT THE MISCPOOLSIZE------------------
         ##It is possible and allowed that more than one resource is assigned to one cost pool
         ACP_pre2<-vector(mode='numeric', length = CP-1)
         i=1
         while (sum(RCC)-sum(RCC[already_assigned])-sum(RCC[as.numeric(RC_to_ACP_cor$col[c(1:i)])])> MISCPOOLSIZE | RC_to_ACP_cor$cor[i] > CC) {
            
            RC_to_ACP[[RC_to_ACP_cor$row[i]]] = c(RC_to_ACP[[RC_to_ACP_cor$row[i]]],as.integer(RC_to_ACP_cor$col[i]))
            ACP_pre2[[RC_to_ACP_cor$row[i]]] = sum(ACP_pre2[[RC_to_ACP_cor$row[i]]],RCC[as.integer(RC_to_ACP_cor$col[i])])
            not_assigned = as.integer(RC_to_ACP_cor$col[i+1:(length(RC_to_ACP_cor$col)-i)])
            i = i+1
         }
         
         
         
         ###MISCPOOL RULE####
         
         #Appending 
         RC_to_ACP_misc =list(not_assigned)
         RC_to_ACP = append(RC_to_ACP,RC_to_ACP_misc)
         
         
         #Adding the misc pool value to ACP
         ACP_misc = sum(RCC[not_assigned])
         ACP = append((ACP_pre1 + ACP_pre2),ACP_misc)
         
         
      } else{
         RC_to_ACP_misc =list(not_assigned)
         RC_to_ACP = append(RC_to_ACP,RC_to_ACP_misc)
         
         
         #Adding the misc pool value to ACP
         ACP_misc = sum(RCC[not_assigned])
         ACP = append(ACP_pre1,ACP_misc)
         
      }
      
   }else if (CP == 1){
      
      ACP = sum(RCC)
      RC_to_ACP = list(c(1:FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES))
   }
   ###SOURCING####  
   
   
   
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
   
   
   
} #fully implemented


####### other CP Mapping Heuristics#####

#MAP_RES_CP_SIZE_CORREL_MISC
#Correlation not one by one, but by maximizing the overall correlation
#So it becomes possible, that an ACP, which has already a resource assigned(by size), gets more than one remaining resource assigned based on correlation
#Instead of looking at the highest correlation between one not assigned resource and an ACP it compares every correlation and is therefore capable of assigning more resources to one ACP



#creating the RES_CONS_PAT Matrix based on the values for one unit and not for all units, thus lowers the correlations and changes the possible assignment of resources
#depends on how companies view a resource consumption (based on one unit, or all units)

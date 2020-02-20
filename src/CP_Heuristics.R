## INTRO ##
# COST POOL BUILDING HEURISTICS
# The algorithms orientate to 
# Balakrishnan, Hansen, Labro 2011
# Anand, Balakrishnan, Labro 2019

## BALAKRISHNAN et al. 2011

MAP_RES_CP_RANDOM<-function(FIRM){
#### RANDOM ALLOCATION OF RESOURCES TO COST POOLS #### 
   
 if (FIRM$COSTING_SYSTEM$set_CSD_fix==1) {set.seed(13)} 
 CP = FIRM$COSTING_SYSTEM$CP
 RCC= FIRM$COSTING_SYSTEM$RCC
 RCCn = length(RCC)
 NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES
 
 if(CP > 1){
    
    not_assigned = sample(c(1:NUMB_RES),NUMB_RES)
    RC_to_ACP = list()
    
    for(i in 1:CP){
       
       RC_to_ACP[i] = not_assigned[i]
    }
    
    not_assigned = setdiff(c(1:NUMB_RES),unlist(RC_to_ACP))
    x = length(unlist(RC_to_ACP))
    
    while(x < NUMB_RES){
       not_assigned = setdiff(c(1:NUMB_RES),unlist(RC_to_ACP))
       random_cp = sample(c(1:CP),1)
       RC_to_ACP[[random_cp]] = c(RC_to_ACP[[random_cp]],sample(not_assigned,1))
       not_assigned = setdiff(c(1:NUMB_RES),unlist(RC_to_ACP))
       x = length(unlist(RC_to_ACP))
    }
    
    
    
    ACP<-vector(mode="numeric") #empty vector for all ACPs volume
    for (i in 1:length(RC_to_ACP)) {
       
       ACP[i]<-sum(RCC[RC_to_ACP[[i]]]) # assign to every row in ACP the sum of resource costs for evey ACP in to RC_to_ACP
       
    }
    
 }

 if(CP == 1){
    ACP = sum(RCC)
    RC_to_ACP = list(c(1:NUMB_RES)) 
 } 
   
   FIRM$PRODUCTION_ENVIRONMENT$CHECK$MISCPOOL = sum(RCC[unlist(RC_to_ACP[CP])])/FIRM$COSTING_SYSTEM$TC
 
   FIRM$COSTING_SYSTEM$ACP = ACP             #class ACP as a variable of the firms costing system
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP             #class RC_to_ACP as a variable of the firms costing system 
  
   return(FIRM)
 } #

MAP_RES_CP_SIZE_RANDOM<-function(FIRM){
  
    if (FIRM$COSTING_SYSTEM$set_CSD_fix==1) {set.seed(13)} 
   CP = FIRM$COSTING_SYSTEM$CP                  #
   RCC= FIRM$COSTING_SYSTEM$RCC                 #
   RES_CONS_PATp = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp
   NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES
   FIRM$PRODUCTION_ENVIRONMENT$CHECK$MISCPOOL = 0
   RCCn = length(RCC)                           #number of resources that need to be allocated to cost pools
  
  
   ####---- pre allocation of largest resorces ----####
   ACP_pre1<-vector(mode="numeric")                      #empty vector for ACP-biggest resource assignment
   RCCs_random_index <- vector(mode = "list", CP)        #
   RC_to_ACP_pre1 = list()

   RCCs2=list()
   RCCs2$ix = vector(mode="numeric")
   RCCs2$x =  vector(mode="numeric")
  
   
   
   ####---- Find the largest RCC (resource costs) for one cost pool each####
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
}#

MAP_RES_CP_SIZE_CORREL<-function(FIRM){
   #### SIZE-BASED RANDOM ALLOCATION OF RESOURCES TO COST POOLS ####    
   if (FIRM$COSTING_SYSTEM$set_CSD_fix==1) {set.seed(13)} 
   FIRM$PRODUCTION_ENVIRONMENT$CHECK$MISCPOOL = 0
   CP = FIRM$COSTING_SYSTEM$CP
   RCC= FIRM$COSTING_SYSTEM$RCC
   RES_CONS_PATp = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp
   RCCn = length(RCC)
   NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES
   
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
         for (j in 1:ncol(RES_CONS_PATp)){
            
            RC_Correl[i,j] = cor(RES_CONS_PATp[,already_assigned[i]],RES_CONS_PATp[,j])
            
         }
      }
      
      
      colnames(RC_Correl) = paste(c(1:ncol(RES_CONS_PATp)))#changing the column names to the resource number
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
}#

MAP_RES_CP_RANDOM_CORREL<-function(FIRM){
   #### Random Allocation of Resources to Cost Pools and then correlative allocation ####    
   if (FIRM$COSTING_SYSTEM$set_CSD_fix==1) {set.seed(13)} 
   CP = FIRM$COSTING_SYSTEM$CP
   RCC= FIRM$COSTING_SYSTEM$RCC
   RCCn = length(RCC)
   NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES
   
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
         for (j in 1:ncol(RES_CONS_PATp)){
            
            RC_Correl[i,j] = cor(RES_CONS_PAT[,already_assigned[i]],RES_CONS_PAT[,j])
            
         }
      }
      
      
      colnames(RC_Correl) = paste(c(1:ncol(RES_CONS_PATp)))#changing the column names to the resource number
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
}#


## ANAND et al. 2019;
MAP_RES_CP_SIZE_MISC<-function(FIRM){
   
   #### SIZE-BASED RANDOM ALLOCATION OF RESOURCES TO COST POOLS ####    
   CP = FIRM$COSTING_SYSTEM$CP
   RCC= FIRM$COSTING_SYSTEM$RCC
   RCCn = length(RCC)
   NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES
   if (FIRM$COSTING_SYSTEM$set_CSD_fix==1) {set.seed(13)} 
   ####---- pre allocation of largest resorces ----####
   ACP_pre1<-vector(mode="numeric",length = (CP-1))
   RC_to_ACP = list()
   
   
   if (CP > 1){
      #### Find the largest RCC (resource costs) for one cost pool each####
      RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sort resource costs 
      
      
      for (i in 1:(CP-1)){               # assign the biggest RCC each to one cost pool
         
         ACP_pre1[i]<-RCCs$x[i]
         RC_to_ACP[[i]]<-RCCs$ix[i]
         
      }
      
      
      
      ####---- Assign remaining RC to Misc Pool ----####
      
      not_assigned = vector(mode = 'numeric')
      not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))
      
      
      ACP_misc = sum(RCC[not_assigned])
      RC_to_ACP_misc = list(not_assigned)
      
      
      ACP = vector(mode='numeric')
      ACP<-append(ACP_pre1, ACP_misc)
      #RC_to_ACP = vector(mode="numeric")
      
      # Bringing the pre index vectors RC_to_ACP_pre together
      
      RC_to_ACP = append(RC_to_ACP, RC_to_ACP_misc)
      
   }else if (CP ==1){
      ACP = sum(RCC)
      RC_to_ACP = list(c(1:NUMB_RES))
   }
   
   
   ###CHECK####
   FIRM$PRODUCTION_ENVIRONMENT$CHECK$MISCPOOL = sum(RCC[unlist(RC_to_ACP[CP])])/FIRM$COSTING_SYSTEM$TC
   
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
   
} # ANAND p==0

MAP_RES_CP_SIZE_CORREL_MISC_ANAND<-function(FIRM){
   
   #### SOURCE ####
   CP = FIRM$COSTING_SYSTEM$CP
   RCC= FIRM$COSTING_SYSTEM$RCC     #RCC
   NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES
   RES_CONS_PATp = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp #taking the p
   MISCPOOLSIZE = FIRM$COSTING_SYSTEM$MISCPOOLSIZE * FIRM$COSTING_SYSTEM$TC
   CC = FIRM$COSTING_SYSTEM$CC #0.4 as in Anand et al. 2019
   RCCn= length(RCC)
   if (FIRM$COSTING_SYSTEM$set_CSD_fix==1) {set.seed(13)} 
   
   
   ####SIZE RULE####
   ####pre allocation, one pool left open
   if (CP > 1){
      RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sorted Resource cost vector
      RC_to_ACP<-list()
      ACP_pre1<-vector(mode ='numeric', length = (CP-1))    #rep(0,(CP-1))
      for (i in 1:(CP-1)){ # assign the biggest Resource -1  each to one activity pool #last is msic
         
         ACP_pre1[i]<-RCCs$x[i] 
         RC_to_ACP[[i]]<-RCCs$ix[i]
         
      }
      already_assigned<-unlist(RC_to_ACP)          #transforms the list into a vector with all resources that are already assigned
      not_assigned <- setdiff(c(1:RCCn),already_assigned)
      #correlative assignment only if there are more than one resource in not_assigned
      ####CORRELATION RULE####
      if (NUMB_RES > CP){
   
         
         #### BUILDIUNG OF CORRELATION MATRIX ####
         
         ##Create empty matrix that shows correlation between assigned and unassigned resources
         #RC_Correl = matrix(nrow = length(already_assigned), ncol = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)#empty matrix for correlations between assigned and not assigned resources
         
         ##fill empty matrix with correlations
         
         RC_Correl = cor(RES_CONS_PATp,RES_CONS_PATp)
         
         
         if(CP==2){
            RC_Correl = t(RC_Correl[already_assigned,])
         }else{
            RC_Correl = RC_Correl[already_assigned,]
         }
         colnames(RC_Correl) = paste(c(1:ncol(RC_Correl)))#changing the column names to the resource number
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
         while (sum(RCC)-sum(RCC[already_assigned])-sum(RCC[as.numeric(RC_to_ACP_cor$col[c(1:i)])])> (MISCPOOLSIZE-RCC[as.integer(RC_to_ACP_cor$col[i])]) && RC_to_ACP_cor$cor[i]> CC) {
            
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
   
   
   
   ###CHECK####
   FIRM$PRODUCTION_ENVIRONMENT$CHECK$MISCPOOL = sum(RCC[unlist(RC_to_ACP[CP])])/FIRM$COSTING_SYSTEM$TC
   
   #### SOURCING ####  
   
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
} # ANAND p==1 / with misc pool and both conditions (MISCpool AND CC)

MAP_RES_CP_SIZE_RANDOM_MISC<-function(FIRM){
    
   #### SIZE-BASED RANDOM ALLOCATION OF RESOURCES TO COST POOLS ####
   if (FIRM$COSTING_SYSTEM$set_CSD_fix==1) {set.seed(13)} 
   CP = FIRM$COSTING_SYSTEM$CP                  #
   RCC= FIRM$COSTING_SYSTEM$RCC
   NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES#
   RCCn = length(RCC)                           #number of resources that need to be allocated to cost pools
   MISCPOOLSIZE = FIRM$COSTING_SYSTEM$MISCPOOLSIZE * FIRM$COSTING_SYSTEM$TC
   
   
   
   if (CP > 1){
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
      
      
      
      if(NUMB_RES > CP){
         
         ####Random Assignment####
         
         CP_ohne_misc = CP-1
         x = sum(RCC[unlist(RC_to_ACP)])
         y = length(not_assigned)
         
         while(x < FIRM$COSTING_SYSTEM$TC-MISCPOOLSIZE & y > 1){
            not_assigned = setdiff(c(1:NUMB_RES),unlist(RC_to_ACP))
            random_cp = sample(length(c(1:CP_ohne_misc)),1)
            RC_to_ACP[[random_cp]] = c(RC_to_ACP[[random_cp]],sample(not_assigned,1))
            not_assigned = setdiff(c(1:NUMB_RES),unlist(RC_to_ACP))
            x = sum(RCC[unlist(RC_to_ACP)])
            y = length(not_assigned)
         }
         
         
         
         
         
         
         # not_assigned = sample(not_assigned)
         # ACP_pre2 = vector(mode = 'numeric', length = CP-1)
         # 
         # 
         # x = sample(length(RC_to_ACP),length(not_assigned), replace = TRUE)
         # RCC_not_assigned = RCC[not_assigned]
         # random_assign = data.frame(x,not_assigned, RCC_not_assigned)
         # 
         # i=1
         # while (sum(RCC)-sum(RCC[already_assigned])-sum(random_assign$RCC_not_assigned[c(1:i)])> MISCPOOLSIZE-random_assign$RCC_not_assigned[i]){
         #    
         #    RC_to_ACP[[random_assign$x[i]]] = c(RC_to_ACP[[random_assign$x[i]]],as.integer(random_assign$not_assigned[i]))
         #    ACP_pre2[[random_assign$x[i]]] = sum(ACP_pre2[[random_assign$x[i]]],random_assign$RCC_not_assigned[i])
         #    not_assigned = as.integer(random_assign$not_assigned[i+1:(length(random_assign$not_assigned)-i)])
         #    i = i+1
         # }
         # 
         # 
         
         ###Miscpool building
         
         
         
         RC_to_ACP_misc =list(not_assigned)
         RC_to_ACP = append(RC_to_ACP,RC_to_ACP_misc)
         
         
         # #Adding the misc pool value to ACP
         # ACP_misc = sum(RCC[not_assigned])
         # ACP = append((ACP_pre1 + ACP_pre2),ACP_misc)
         ACP = vector(mode = 'numeric')
         
         for (i in 1:CP){
            ACP[i] = sum(RCC[unlist(RC_to_ACP[i])])
         }
         
      } else{
         
         RC_to_ACP_misc =list(not_assigned)
         RC_to_ACP = append(RC_to_ACP,RC_to_ACP_misc)
         
         
         #Adding the misc pool value to ACP
         # ACP_misc = sum(RCC[not_assigned])
         # ACP = append(ACP_pre1,ACP_misc)                                      #if there was no second assignment of remaining resources (No. of RC = No. of ACP) all RC are in ACP_pre1
         
         ACP = vector(mode = 'numeric')
         
         for (i in 1:CP){
            ACP[i] = sum(RCC[unlist(RC_to_ACP[i])])
         }
      }
      
      
      
   }else if(CP ==1){
      
      ACP = sum(RCC)
      RC_to_ACP = list(c(1:FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES))
      
      
   }
   
   ###CHECK####
   FIRM$PRODUCTION_ENVIRONMENT$CHECK$MISCPOOL = sum(RCC[unlist(RC_to_ACP[CP])])/FIRM$COSTING_SYSTEM$TC
   
   
   ###SOURCING
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
   
   
} #ANAND p==2

MAP_RES_CP_SIZE_CORREL_CUTOFF_MISC_ANAND<-function(FIRM){
  
   ##INIT##
   if (FIRM$COSTING_SYSTEM$set_CSD_fix==1) {set.seed(13)} 
   CP = FIRM$COSTING_SYSTEM$CP
   RCC= FIRM$COSTING_SYSTEM$RCC
   NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES
   RES_CONS_PATp = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp
   MISCPOOLSIZE = FIRM$COSTING_SYSTEM$MISCPOOLSIZE * FIRM$COSTING_SYSTEM$TC
   CC = FIRM$COSTING_SYSTEM$CC #0.4 as in Anand et al. 2019
  
   if (CP > 1){
      
      RCCn =length(RCC)
      already_assigned<- vector(mode = 'numeric', length = RCCn)          #transforms the list into a vector with all resources that are already assigned
      not_assigned <- setdiff(c(1:RCCn),already_assigned)
      
      if(NUMB_RES>CP){
         
         
         RC_to_ACP<-list()
         
         
         
         for (i in 1:(CP-1)){               # assign the biggest Resource to an activity pool
         
            #if(length(not_assigned)> length(i:(CP-1)) && sum(RCC[not_assigned])>MISCPOOLSIZE){
            
            #ACP[i]<-max(RCC[not_assigned])
            RC_to_ACP[[i]]<- not_assigned[which.max(RCC[not_assigned])]  #may not work if some of the resources have exactly the same values
            
            #already_assigned[i] = RC_to_ACP[i]
            not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))
            
            RC_Correl = cor(RES_CONS_PATp[,RC_to_ACP[[i]]],RES_CONS_PATp)
            colnames(RC_Correl) = paste(c(1:ncol(RES_CONS_PATp)))#changing the column names to the resource number
            RC_Correl = matrix(RC_Correl[,not_assigned],ncol = length(not_assigned))
            #delete resources that are already assigned from Correlation Matrix, so they dont get assigned twice
            colnames(RC_Correl) = paste(not_assigned)
            RC_Correl = data.frame(sort(RC_Correl,decreasing = TRUE, index.return = TRUE))
            
            
            x =1
            
            while(RC_Correl$x[x] >= CC && NUMB_RES - length(unlist(RC_to_ACP))> (CP-i) && sum(RCC[unlist(RC_to_ACP)])< (FIRM$COSTING_SYSTEM$TC- MISCPOOLSIZE)){
               
               #NUMB_RES - length(unlist(RC_to_ACP))> length(i:CP) yields a different result, since the constraint is reached one resource to early, which causes that
               #the then not assigned resource is assigned later (if not even in the misc pool) and the correlation is then lower --> higher error
              
               
               RC_to_ACP[[i]] = c(RC_to_ACP[[i]],not_assigned[RC_Correl$ix[x]])
               #not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))
               #ACP[i] = sum(ACP[i],RCC[RC_Correl$ix[x]])
               #already_assigned[RC_Correl$ix[x]] = c(already_assigned[RC_Correl$ix[x]],RC_Correl$ix[x])
               x=x+1
               
            }
            
            #print(RC_to_ACP[i])
            not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))
            
            #}
            
            if(length(not_assigned) == length(i:(CP))){
               
               RC_to_ACP[[i]] =c(RC_to_ACP[[i]],not_assigned[i])
               
            }
            
            not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))   
            
            
         }
         
         RC_to_ACP_misc = list(not_assigned)
         RC_to_ACP = append(RC_to_ACP, RC_to_ACP_misc)
         
         ACP_misc = sum(RCC[not_assigned])
         
         ACP<-vector(mode ='numeric', length = (CP-1))
         
         for (i in 1:(CP-1)){
            
            ACP[i] = sum(RCC[unlist(RC_to_ACP[i])])
            
         }
         
         ACP = append(ACP,ACP_misc)
         
      }
      
      
      if(NUMB_RES == CP){
         
         
         
         RC_to_ACP = list()
         ACP = vector(mode='numeric', length = CP)
         
         for (i in 1:length(not_assigned)){
            
            RC_to_ACP[i] = not_assigned[i]
            ACP[i] = RCC[not_assigned[i]]
         }
         
         
      }
      
   }else if (CP == 1){
      
      ACP = sum(RCC)
      RC_to_ACP = list(c(1:FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES))
      
      ###SOURCING####  
      
   }  
   
   
   
   ###CHECK####
   FIRM$PRODUCTION_ENVIRONMENT$CHECK$MISCPOOL = sum(RCC[unlist(RC_to_ACP[CP])])/FIRM$COSTING_SYSTEM$TC
   
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
   
}# ANAND P==3 like anands currel cutoff ( not one by one) w/ miscpool

MAP_CP_CORREL_MISC<-function(FIRM){
   if (FIRM$COSTING_SYSTEM$set_CSD_fix==1) {set.seed(13)} 
   MISCPOOLSIZE = FIRM$COSTING_SYSTEM$MISCPOOLSIZE
   CC = FIRM$COSTING_SYSTEM$CC #0.4 as in Anand et al. 2019
   CP = FIRM$COSTING_SYSTEM$CP
   RCC = FIRM$COSTING_SYSTEM$RCC
   RCCn= length(RCC)
   PEARSONCORR<-cor(FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp,FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp)
   
   RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sorted Resource cost vector
   
   
   if(CP>1){
      ####---- pre allocation (one pool left open) ----####
      RC_to_ACP<-list()
      ACP_pre1<-rep(0,(CP-1))
      for (i in 1:(CP-1)){               # assign the biggest Resource -1  each to one activity pool
         
         ACP_pre1[i]<-RCCs$x[i]
         RC_to_ACP[[i]]<-RCCs$ix[i]
         
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
      cutoff_Reached<-sum(RCC[-already_assigned])/sum(RCC) < MISCPOOLSIZE     #makes the miscpool always too big
      
      for (i in 1:NROW(miscRes)) {
         
         if(cutoff_Reached==FALSE & miscRes[i,2]>=CC){
            acp_pool<-miscRes[i,3]
            res<-miscRes[i,1]
            ACP_pre2[acp_pool]<-ACP_pre2[acp_pool]+RCC[res]
            RC_to_ACP[[acp_pool]]<-c(RC_to_ACP[[acp_pool]],res)
            already_assigned<-unlist( RC_to_ACP)
            cutoff_Reached<-sum(RCC[-already_assigned])/sum(RCC) < MISCPOOLSIZE
         }else{
            break
         }
         
      }
      
      # ## add remaining rescources to the misc pool
      RC_to_ACP[[CP]]<-as.integer(miscRes[i:NROW(miscRes),1])
      ACP_pre2[CP]<-sum(RCC[RC_to_ACP[[CP]]])
      
      
      ACP<-c(ACP_pre1,0)+ACP_pre2
      
      
   }else if(CP==1){
      
      
      ACP = sum(RCC)
      RC_to_ACP = list(c(1:FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES))
      
      
   }
   
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
   
} # ANAND p==1 / OLE



### Other Heuristics
MAP_RES_CP_SIZE_CORREL_MISC_OWN<-function(FIRM){
   if (FIRM$COSTING_SYSTEM$set_CSD_fix==1) {set.seed(13)} 
   
   
   #### SOURCE ####
   CP = FIRM$COSTING_SYSTEM$CP
   RCC= FIRM$COSTING_SYSTEM$RCC
   NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES
   RES_CONS_PATp = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp #taking the p
   MISCPOOLSIZE = FIRM$COSTING_SYSTEM$MISCPOOLSIZE * FIRM$COSTING_SYSTEM$TC
   CC = FIRM$COSTING_SYSTEM$CC #0.4 as in Anand et al. 2019
   RCCn= length(RCC)
   
   
   ####SIZE RULE####
   ####pre allocation, one pool left open
   if (CP > 1){
      RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sorted Resource cost vector
      RC_to_ACP<-list()
      ACP_pre1<-vector(mode ='numeric', length = (CP-1))    #rep(0,(CP-1))
      for (i in 1:(CP-1)){ # assign the biggest Resource -1  each to one activity pool #last is msic
         
         ACP_pre1[i]<-RCCs$x[i] 
         RC_to_ACP[[i]]<-RCCs$ix[i]
         
      }
      already_assigned<-unlist(RC_to_ACP)          #transforms the list into a vector with all resources that are already assigned
      not_assigned <- setdiff(c(1:RCCn),already_assigned)
      #correlative assignment only if there are more than one resource in not_assigned
      ####CORRELATION RULE####
      if (NUMB_RES > CP){
         
         
         #### BUILDIUNG OF CORRELATION MATRIX ####
         
         ##Create empty matrix that shows correlation between assigned and unassigned resources
         #RC_Correl = matrix(nrow = length(already_assigned), ncol = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)#empty matrix for correlations between assigned and not assigned resources
         
         ##fill empty matrix with correlations
         
         RC_Correl = cor(RES_CONS_PATp,RES_CONS_PATp)
         
         
         if(CP==2){
            RC_Correl = t(RC_Correl[already_assigned,])
         }else{
            RC_Correl = RC_Correl[already_assigned,]
         }
         colnames(RC_Correl) = paste(c(1:ncol(RC_Correl)))#changing the column names to the resource number
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
         while (sum(RCC)-sum(RCC[already_assigned])-sum(RCC[as.numeric(RC_to_ACP_cor$col[c(1:i)])])> (MISCPOOLSIZE-RCC[as.integer(RC_to_ACP_cor$col[i])])) {
            
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
   
   
   
   ###CHECK####
   FIRM$PRODUCTION_ENVIRONMENT$CHECK$MISCPOOL = sum(RCC[unlist(RC_to_ACP[CP])])/FIRM$COSTING_SYSTEM$TC
   
   #### SOURCING ####  
   
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
} # ANAND p==1 / without CC condition

MAP_RES_CP_SIZE_CORREL_MISC<-function(FIRM){
   if (FIRM$COSTING_SYSTEM$set_CSD_fix==1) {set.seed(13)} 
   
  
#### SOURCE ####
   CP = FIRM$COSTING_SYSTEM$CP
   RCC= FIRM$COSTING_SYSTEM$RCC
   NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES
   RES_CONS_PATp = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp #taking the p
   MISCPOOLSIZE = FIRM$COSTING_SYSTEM$MISCPOOLSIZE * FIRM$COSTING_SYSTEM$TC
   RCCn= length(RCC)

####SIZE RULE####
      ####pre allocation, one pool left open
   if (CP > 1){
      RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sorted Resource cost vector
      RC_to_ACP<-list()
      ACP_pre1<-vector(mode ='numeric', length = (CP-1))    #rep(0,(CP-1))
      for (i in 1:(CP-1)){ # assign the biggest Resource -1  each to one activity pool #last is msic
         
         ACP_pre1[i]<-RCCs$x[i] 
         RC_to_ACP[[i]]<-RCCs$ix[i]
         
      }
      already_assigned<-unlist(RC_to_ACP)          #transforms the list into a vector with all resources that are already assigned
      not_assigned <- setdiff(c(1:RCCn),already_assigned)
      #correlative assignment only if there are more than one resource in not_assigned
          if (NUMB_RES > CP){
####CORRELATION RULE####
      
      #### BUILDIUNG OF CORRELATION MATRIX ####
      
      ##Create empty matrix that shows correlation between assigned and unassigned resources
      RC_Correl = cor(RES_CONS_PATp,RES_CONS_PATp)
     
             
      if(CP==2){
         RC_Correl = t(RC_Correl[already_assigned,])
      }else{
         RC_Correl = RC_Correl[already_assigned,]
      }
      colnames(RC_Correl) = paste(c(1:ncol(RC_Correl)))#changing the column names to the resource number
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
   
   
   
   ###CHECK####
   FIRM$PRODUCTION_ENVIRONMENT$CHECK$MISCPOOL = sum(RCC[unlist(RC_to_ACP[CP])])/FIRM$COSTING_SYSTEM$TC
   #### SOURCING ####  
      
      FIRM$COSTING_SYSTEM$ACP = ACP
      FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
      return(FIRM)
} #fully implemented

MAP_RES_CP_SIZE_CORREL_CUTOFF<-function(FIRM){
   if (FIRM$COSTING_SYSTEM$set_CSD_fix==1) {set.seed(13)} 
   
   
   
   ##INIT##
   
   CP = FIRM$COSTING_SYSTEM$CP
   RCC= FIRM$COSTING_SYSTEM$RCC
   NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES
   RES_CONS_PATp = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp
   MISCPOOLSIZE = FIRM$COSTING_SYSTEM$MISCPOOLSIZE * FIRM$COSTING_SYSTEM$TC
   CC = FIRM$COSTING_SYSTEM$CC #0.4 as in Anand et al. 2019
   
   
   RCCn= length(RCC)
   ####SIZE RULE####
   ####pre allocation, one pool left open
   if (CP > 1){
      RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sorted Resource cost vector
      RC_to_ACP<-list()
      ACP_pre1<-vector(mode ='numeric', length = (CP-1))    #rep(0,(CP-1))
      for (i in 1:(CP)){               # assign the biggest Resource -1  each to one activity pool
         
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
         RC_Correl = cor(RES_CONS_PATp,RES_CONS_PATp)
         
         RC_Correl = RC_Correl[already_assigned,]
         colnames(RC_Correl) = paste(c(1:ncol(RC_Correl)))#changing the column names to the resource number
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
         
         i=1
         
         while (sum(RCC)-sum(RCC[already_assigned])-sum(RCC[as.numeric(RC_to_ACP_cor$col[c(1:i)])])> MISCPOOLSIZE && RC_to_ACP_cor$cor[i] > CC) {
            
            RC_to_ACP[[RC_to_ACP_cor$row[i]]] = c(RC_to_ACP[[RC_to_ACP_cor$row[i]]],as.integer(RC_to_ACP_cor$col[i]))
            ACP_pre2[[RC_to_ACP_cor$row[i]]] = sum(ACP_pre2[[RC_to_ACP_cor$row[i]]],RCC[as.integer(RC_to_ACP_cor$col[i])])
            not_assigned = as.integer(RC_to_ACP_cor$col[i+1:(length(RC_to_ACP_cor$col)-i)])
            i = i+1
         }
         
         # while (sum(RCC)-sum(RCC[already_assigned])-sum(RCC[as.numeric(RC_to_ACP_cor$col[c(1:i)])])> MISCPOOLSIZE && RC_to_ACP_cor$cor[i] > CC) {
         #    
         #    RC_to_ACP[[RC_to_ACP_cor$row[i]]] = c(RC_to_ACP[[RC_to_ACP_cor$row[i]]],as.integer(RC_to_ACP_cor$col[i]))
         #    ACP_pre2[[RC_to_ACP_cor$row[i]]] = sum(ACP_pre2[[RC_to_ACP_cor$row[i]]],RCC[as.integer(RC_to_ACP_cor$col[i])])
         #    not_assigned = as.integer(RC_to_ACP_cor$col[i+1:(length(RC_to_ACP_cor$col)-i)])
         #    i = i+1
         # }
         
         
         
         ###MISCPOOL RULE####
         
         #Appending
         
         for (i in 1:length(not_assigned)){
            
            RC_to_ACP[[CP]] = c(RC_to_ACP[[CP]],not_assigned[i])
         
         }
         
         #Adding the misc pool value to ACP
        
         ACP_pre2[CP] = ACP_pre2[CP] + sum(RCC[not_assigned])
         ACP = (ACP_pre1 + ACP_pre2)
      
      
         
      } else{
         
         
         for (i in 1:length(not_assigned)){
            
            RC_to_ACP[[CP]] = c(RC_to_ACP[[CP]],not_assigned[i])
            
         }
         
         
         #Adding the misc pool value to ACP
         ACP_pre1[CP] = ACP_pre1[CP] + sum(RCC[not_assigned])
         ACP = ACP_pre1
         
      }
      
   }else if (CP == 1){
      
      ACP = sum(RCC)
      RC_to_ACP = list(c(1:FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES))
   }
   ###SOURCING####  
   
   
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
   
   
   
}#like correl misc without miscpool

MAP_RES_CP_SIZE_CORREL_CUTOFF_ANAND<-function(FIRM){
   if (FIRM$COSTING_SYSTEM$set_CSD_fix==1) {set.seed(13)} 
   
   
   
   ##INIT##
   
   CP = FIRM$COSTING_SYSTEM$CP
   RCC= FIRM$COSTING_SYSTEM$RCC
   NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES
   RES_CONS_PATp = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PAT
   MISCPOOLSIZE = FIRM$COSTING_SYSTEM$MISCPOOLSIZE * FIRM$COSTING_SYSTEM$TC
   CC = FIRM$COSTING_SYSTEM$CC #0.4 as in Anand et al. 2019
   
   if (CP > 1){
      
      if(NUMB_RES>CP){
         
         
         
         RCCn =length(RCC)
         already_assigned<- vector(mode = 'numeric', length = RCCn)          #transforms the list into a vector with all resources that are already assigned
         not_assigned <- setdiff(c(1:RCCn),already_assigned)
         
         RC_to_ACP<-list()
         
         
         
         for (i in 1:CP){               # assign the biggest Resource to an activity pool
            
            #if(length(not_assigned)> length(i:(CP-1)) && sum(RCC[not_assigned])>MISCPOOLSIZE){
            
            #ACP[i]<-max(RCC[not_assigned])
            RC_to_ACP[[i]]<- not_assigned[which.max(RCC[not_assigned])]  #may not work if some of the resources have exactly the same values
            
            #already_assigned[i] = RC_to_ACP[i]
            not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))
            
            RC_Correl = cor(RES_CONS_PATp[,RC_to_ACP[[i]]],RES_CONS_PATp)
            colnames(RC_Correl) = paste(c(1:ncol(RES_CONS_PATp)))#changing the column names to the resource number
            RC_Correl = matrix(RC_Correl[,not_assigned],ncol = length(not_assigned))
            #delete resources that are already assigned from Correlation Matrix, so they dont get assigned twice
            colnames(RC_Correl) = paste(not_assigned)
            RC_Correl = data.frame(sort(RC_Correl,decreasing = TRUE, index.return = TRUE))
            
            
            x =1
            
            while(RC_Correl$x[x] >= CC && NUMB_RES - length(unlist(RC_to_ACP))> length(i:CP) && sum(RCC[not_assigned])>MISCPOOLSIZE){
               
               RC_to_ACP[[i]] = c(RC_to_ACP[[i]],not_assigned[RC_Correl$ix[x]])
               #ACP[i] = sum(ACP[i],RCC[RC_Correl$ix[x]])
               #not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))
               #already_assigned[RC_Correl$ix[x]] = c(already_assigned[RC_Correl$ix[x]],RC_Correl$ix[x])
               x=x+1
               
            }
            
            #print(RC_to_ACP[i])
            not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))
            
            #}
            
            if(length(not_assigned)== length(i:CP)){
               
               RC_to_ACP[[i]] =c(not_assigned[i])
               
            }
            
            not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))   

            if(i == CP) {

               RC_to_ACP[[CP]] =c(RC_to_ACP[[CP]],not_assigned)

            }
            
         }   
         
         
               

         
         
         ACP<-vector(mode ='numeric', length = CP)
         
         for (i in 1:length(RC_to_ACP)){
            
            ACP[i] = sum(RCC[unlist(RC_to_ACP[i])])
            
         }
         

      
      }else{
            
            RC_to_ACP = list()
            ACP = vector(mode='numeric', length = CP)
            
            for (i in 1:length(not_assigned)){
               
               RC_to_ACP[i] = not_assigned[i]
               ACP[i] = RCC[not_assigned[i]]
         }
            
            
            #Adding the misc pool value to ACP
            
            ACP = ACP_pre1
            
      }
      
   }else if (CP == 1){
      
      ACP = sum(RCC)
      RC_to_ACP = list(c(1:FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES))
   
   ###SOURCING####  
   
   }  
   
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
   
   
   
}# like anands currel cutoff (not one by one) w/o miscpool # produces NAs for some firms

MAP_RES_CP_SIZE_CORREL_CUTOFF_ANAND2<-function(FIRM){
   if (FIRM$COSTING_SYSTEM$set_CSD_fix==1) {set.seed(13)} 
   
   
   
   ##INIT##
   
   CP = FIRM$COSTING_SYSTEM$CP
   RCC= FIRM$COSTING_SYSTEM$RCC
   NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES
   RES_CONS_PATp = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp
   MISCPOOLSIZE = FIRM$COSTING_SYSTEM$MISCPOOLSIZE * FIRM$COSTING_SYSTEM$TC
   CC = FIRM$COSTING_SYSTEM$CC #0.4 as in Anand et al. 2019
   
   
   if (CP > 1){
      
      if(NUMB_RES>CP){
         
         
         
         RCCn =length(RCC)
         already_assigned<- vector(mode = 'numeric', length = RCCn)          #transforms the list into a vector with all resources that are already assigned
         not_assigned <- setdiff(c(1:RCCn),already_assigned)
         
         RC_to_ACP<-list()
         
         
         
         for (i in 1:CP){               # assign the biggest Resource to an activity pool
            
            if(length(not_assigned)> length(i:CP)){
               
               #ACP[i]<-max(RCC[not_assigned])
               RC_to_ACP[[i]]<- not_assigned[which.max(RCC[not_assigned])]  #may not work if some of the resources have exactly the same values
               
               #already_assigned[i] = RC_to_ACP[i]
               not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))
               
               RC_Correl = cor(RES_CONS_PATp[,RC_to_ACP[[i]]],RES_CONS_PATp)
               colnames(RC_Correl) = paste(c(1:ncol(RES_CONS_PATp)))#changing the column names to the resource number
               RC_Correl = matrix(RC_Correl[,not_assigned],ncol = length(not_assigned))
               #delete resources that are already assigned from Correlation Matrix, so they dont get assigned twice
               colnames(RC_Correl) = paste(not_assigned)
               RC_Correl = data.frame(sort(RC_Correl,decreasing = TRUE, index.return = TRUE))
               
               #x =1
               
               if(RC_Correl$x[1] >= CC && length(not_assigned)> length(i:CP) && sum(RCC[not_assigned])>MISCPOOLSIZE){
                  
                  RC_to_ACP[[i]] = c(RC_to_ACP[[i]],not_assigned[RC_Correl$ix[1]])
                  #ACP[i] = sum(ACP[i],RCC[RC_Correl$ix[x]])
                  not_assigned = not_assigned[-RC_Correl$ix[1]]
                  #already_assigned[RC_Correl$ix[x]] = c(already_assigned[RC_Correl$ix[x]],RC_Correl$ix[x])
                  #x=x+1
                  
               }
               
               #print(RC_to_ACP[i])
               not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))
               
            }else if(length(not_assigned)== length(1:CP)){
               
               RC_to_ACP[[i]] =c(not_assigned[i])
               
            }
            
            not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))   
            
            if(i == CP) {
               
               RC_to_ACP[[i]] =c(RC_to_ACP[[i]],not_assigned)
               
            }
            
         }
         
         
         ACP<-vector(mode ='numeric', length = CP)
         
         for (i in 1:length(RC_to_ACP)){
            
            ACP[i] = sum(RCC[unlist(RC_to_ACP[i])])
            
         }
         
         
         
      }else{
         
         RC_to_ACP = list()
         ACP = vector(mode='numeric', length = CP)
         
         for (i in 1:length(not_assigned)){
            
            RC_to_ACP[i] = not_assigned[i]
            ACP[i] = RCC[not_assigned[i]]
         }
         
         
         #Adding the misc pool value to ACP
         
         ACP = ACP_pre1
         
      }
      
   }else if (CP == 1){
      
      ACP = sum(RCC)
      RC_to_ACP = list(c(1:FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES))
      
      ###SOURCING####  
      
   }  
   
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
   
   
   
}# like anands currel cutoff (one by one in correl assignment)

MAP_RES_CP_SIZE_CORREL_RAND_MISC<-function(FIRM){
   if (FIRM$COSTING_SYSTEM$set_CSD_fix==1) {set.seed(13)} 
   
   
   #### SOURCE ####
   CP = FIRM$COSTING_SYSTEM$CP
   RCC= FIRM$COSTING_SYSTEM$RCC
   NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES
   RES_CONS_PATp = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp #taking the p
   MISCPOOLSIZE = FIRM$COSTING_SYSTEM$MISCPOOLSIZE * FIRM$COSTING_SYSTEM$TC
   CC = FIRM$COSTING_SYSTEM$CC #0.4 as in Anand et al. 2019
   RCCn= length(RCC)
   
   ####SIZE RULE####
   ####pre allocation, one pool left open
   if (CP > 1){
      RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sorted Resource cost vector
      RC_to_ACP<-list()
      ACP_pre1<-vector(mode ='numeric', length = (CP-1))    #rep(0,(CP-1))
      for (i in 1:(CP-1)){ # assign the biggest Resource -1  each to one activity pool #last is msic
         
         ACP_pre1[i]<-RCCs$x[i] 
         RC_to_ACP[[i]]<-RCCs$ix[i]
         
      }
      already_assigned<-unlist(RC_to_ACP)          #transforms the list into a vector with all resources that are already assigned
      not_assigned <- setdiff(c(1:RCCn),already_assigned)
      #correlative assignment only if there are more than one resource in not_assigned
      if (NUMB_RES > CP){
         ####CORRELATION RULE####
         
         #### BUILDIUNG OF CORRELATION MATRIX ####
         
         ##Create empty matrix that shows correlation between assigned and unassigned resources
         #RC_Correl = matrix(nrow = length(already_assigned), ncol = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)#empty matrix for correlations between assigned and not assigned resources
         
         ##fill empty matrix with correlations
         
         RC_Correl = cor(RES_CONS_PATp,RES_CONS_PATp)
         
        
         
         if(CP==2){
            RC_Correl = t(RC_Correl[already_assigned,])
         }else{
            RC_Correl = RC_Correl[already_assigned,]
         }
         colnames(RC_Correl) = paste(c(1:ncol(RC_Correl)))#changing the column names to the resource number
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
         while (sum(RCC)-sum(RCC[already_assigned])-sum(RCC[as.numeric(RC_to_ACP_cor$col[c(1:i)])])> (MISCPOOLSIZE-RCC[as.integer(RC_to_ACP_cor$col[i])]) && RC_to_ACP_cor$cor[i] > CC) {
            
            RC_to_ACP[[RC_to_ACP_cor$row[i]]] = c(RC_to_ACP[[RC_to_ACP_cor$row[i]]],as.integer(RC_to_ACP_cor$col[i]))
            ACP_pre2[[RC_to_ACP_cor$row[i]]] = sum(ACP_pre2[[RC_to_ACP_cor$row[i]]],RCC[as.integer(RC_to_ACP_cor$col[i])])
            not_assigned = as.integer(RC_to_ACP_cor$col[i+1:(length(RC_to_ACP_cor$col)-i)])
            i = i+1
         }
         
         
         not_assigned=sample(not_assigned)
         
         x = sample(length(RC_to_ACP),length(not_assigned), replace = TRUE)
         RCC_not_assigned = RCC[not_assigned]
         random_assign = data.frame(x,not_assigned, RCC_not_assigned)
         ACP_pre3 = vector(mode = 'numeric', length = CP-1)
         
         i=1
         while (sum(RCC)-sum(RCC[already_assigned])-sum(random_assign$RCC_not_assigned[c(1:i)])> MISCPOOLSIZE-random_assign$RCC_not_assigned[i]){
            
            RC_to_ACP[[random_assign$x[i]]] = c(RC_to_ACP[[random_assign$x[i]]],as.integer(random_assign$not_assigned[i]))
            ACP_pre3[[random_assign$x[i]]] = sum(ACP_pre2[[random_assign$x[i]]],random_assign$RCC_not_assigned[i])
            not_assigned = as.integer(random_assign$not_assigned[i+1:(length(random_assign$not_assigned)-i)])
            i = i+1
         }
         
         
         ###MISCPOOL RULE####
         
         #Appending 
         RC_to_ACP_misc =list(not_assigned)
         RC_to_ACP = append(RC_to_ACP,RC_to_ACP_misc)
         
         
         #Adding the misc pool value to ACP
         ACP_misc = sum(RCC[not_assigned])
         ACP = append((ACP_pre1 + ACP_pre2 + ACP_pre3),ACP_misc)
         
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
} #with miscpool and trying to minimize it by random assignment

MAP_RES_CP_SIZE_CORREL_CUTOFF_MISC_ANAND2<-function(FIRM){
   if (FIRM$COSTING_SYSTEM$set_CSD_fix==1) {set.seed(13)} 
   
   
   
   ##INIT##
   
   CP = FIRM$COSTING_SYSTEM$CP
   RCC= FIRM$COSTING_SYSTEM$RCC
   NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES
   RES_CONS_PATp = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp
   MISCPOOLSIZE = FIRM$COSTING_SYSTEM$MISCPOOLSIZE * FIRM$COSTING_SYSTEM$TC
   CC = FIRM$COSTING_SYSTEM$CC #0.4 as in Anand et al. 2019
   
   if (CP > 1){
      
      if(NUMB_RES>CP){
         
         
         
         RCCn =length(RCC)
         already_assigned<- vector(mode = 'numeric', length = RCCn)          #transforms the list into a vector with all resources that are already assigned
         not_assigned <- setdiff(c(1:RCCn),already_assigned)
         
         RC_to_ACP<-list()
         
         
         
         for (i in 1:(CP-1)){               # assign the biggest Resource to an activity pool
            
            #if(length(not_assigned)> length(i:(CP-1)) && sum(RCC[not_assigned])>MISCPOOLSIZE){
               
               #ACP[i]<-max(RCC[not_assigned])
               RC_to_ACP[[i]]<- not_assigned[which.max(RCC[not_assigned])]  #may not work if some of the resources have exactly the same values
               
               #already_assigned[i] = RC_to_ACP[i]
               not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))
               
               RC_Correl = cor(RES_CONS_PATp[,RC_to_ACP[[i]]],RES_CONS_PATp)
               colnames(RC_Correl) = paste(c(1:ncol(RES_CONS_PATp)))#changing the column names to the resource number
               RC_Correl = matrix(RC_Correl[,not_assigned],ncol = length(not_assigned))
               #delete resources that are already assigned from Correlation Matrix, so they dont get assigned twice
               colnames(RC_Correl) = paste(not_assigned)
               RC_Correl = data.frame(sort(RC_Correl,decreasing = TRUE, index.return = TRUE))
               
               #x =1
               
               if(RC_Correl$x[1] >= CC && length(not_assigned)> length(i:(CP-1)) && sum(RCC[not_assigned])>MISCPOOLSIZE){
                  
                  RC_to_ACP[[i]] = c(RC_to_ACP[[i]],not_assigned[RC_Correl$ix[1]])
                  #ACP[i] = sum(ACP[i],RCC[RC_Correl$ix[x]])
                  #not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))
                  #already_assigned[RC_Correl$ix[x]] = c(already_assigned[RC_Correl$ix[x]],RC_Correl$ix[x])
                  #x=x+1
                  
               }
               
               #print(RC_to_ACP[i])
               not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))
               
            #}
            
            if(length(not_assigned)== length(1:(CP-1))){
               
               RC_to_ACP[[i]] =c(not_assigned[i])
               
            }
            
            not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))   
            
            # if(i == (CP-i)) {
            #    
            #    RC_to_ACP[[CP]] =c(RC_to_ACP[[CP]],not_assigned)
            #    
            # }
            
         }
         
         RC_to_ACP_misc = list(not_assigned)
         RC_to_ACP = append(RC_to_ACP, RC_to_ACP_misc)
         
         ACP_misc = sum(RCC[not_assigned])
         
         ACP<-vector(mode ='numeric', length = (CP-1))
         
         for (i in 1:(CP-1)){
            
            ACP[i] = sum(RCC[unlist(RC_to_ACP[i])])
            
         }
         
         ACP = append(ACP,ACP_misc)
         
      }else{
         
         RC_to_ACP = list()
         ACP = vector(mode='numeric', length = CP)
         
         for (i in 1:length(not_assigned)){
            
            RC_to_ACP[i] = not_assigned[i]
            ACP[i] = RCC[not_assigned[i]]
         }
         
         
         #Adding the misc pool value to ACP
         
         ACP = ACP_pre1
         
      }
      
   }else if (CP == 1){
      
      ACP = sum(RCC)
      RC_to_ACP = list(c(1:FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES))
      
      ###SOURCING####  
      
   }  
   
   FIRM$COSTING_SYSTEM$ACP = ACP
   FIRM$COSTING_SYSTEM$RC_ACP = RC_to_ACP
   
   return(FIRM)
   
   
   
}# like anands currel cutoff with misc pool (one by one in correl assignment)

## other CP Mapping Heuristics

#MAP_RES_CP_SIZE_CORREL_MISC
#Correlation not one by one, but by maximizing the overall correlation
#So it becomes possible, that an ACP, which has already a resource assigned(by size), gets more than one remaining resource assigned based on correlation
#Instead of looking at the highest correlation between one not assigned resource and an ACP it compares every correlation and is therefore capable of assigning more resources to one ACP



#creating the RES_CONS_PAT Matrix based on the values for one unit and not for all units, thus lowers the correlations and changes the possible assignment of resources
#depends on how companies view a resource consumption (based on one unit, or all units)

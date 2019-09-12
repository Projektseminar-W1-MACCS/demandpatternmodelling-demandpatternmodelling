########
## Mapping the cost pools to products
########

MAP_CP_PRO<-function(FIRM,method="BIG-POOL",ME_AD=NULL,ME_NUM=NULL){

ACP_index_choosen<-vector(mode="numeric")
# normalize RES_CONS_PAT
RES_CONS_PAT<-FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp
ME_AD = FIRM$COSTING_SYSTEM$Error
RCC<-FIRM$COSTING_SYSTEM$RCC
RC_ACP_index<-FIRM$COSTING_SYSTEM$RC_ACP

# preallocation
ACT_CONS_PAT<-matrix(0,nrow = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,ncol = length(RC_ACP_index))


if(method=="BIG-POOL"){
    for (i in 1:length(RC_ACP_index)){
    
    ## exception handler if ACP[i] has only one RCP
    # in original version not needed, this is due to basic implementation of Rs function rowSums
    if(length(RC_ACP_index[[i]])==1){
      ACT_CONS_PAT[,i]<-RES_CONS_PAT[,RC_ACP_index[[i]]]
    }else{
     
    # 1. Order ACP_index in decreasing order of resource size
      RC_order<-sort(RCC, decreasing=TRUE)
      
      RC_ACP_index[[i]]<-RC_ACP_index[[i]][order(match(RC_ACP_index[[i]],RC_order))]
      
      RES_CONS_PAT_temp<-RES_CONS_PAT[,RC_ACP_index[[i]]] # subsetting for resources used in this ACP and ordering
      # ACs<-sort(colSums(RES_CONS_PAT_temp),decreasing = TRUE,index.return=TRUE)
      ACT_CONS_PAT[,i]<-RES_CONS_PAT_temp[,1] #use the largest Rescource as a driver
    }
    
    ACP_index_choosen[i]<-RC_ACP_index[[i]][1]
  }
  
  
  
}else if(method=="INDEXED"){
  stopifnot(!is.null(NUM))
  
  # coppied from Anand et al. (2017) still need to translate
  # // Indexed drivers.
  # else if (this.r == 1) {
  #   // First, go through the lists in D and remove resources for which there
  #   // is zero usage
  #   D_prime =
  #     D_prime.Select(list => list.Where(resource => TRU_F[resource] != 0.0).ToList()).ToList();
  #
  #   // If any list in D_prime is empty, find a resource in the corresponding
  #   // list in B_prime that has non-zero resource usage, and add that one resource
  #   // to the empty list in D_prime.
  #   for (int i = 0; i < D_prime.Count; ++i) {
  #     if (D_prime[i].Count == 0) {
  #       D_prime[i].Add(possibleDrivers2[i]);
  #     }
  #   }
  # }
}else if(method=="AVERAGE"){
  
  stop("Not fully implemented yet")
  for (i in 1:length(ACP_index)){
    
    ## exception handler if ACP[i] has only one RCP
    # in original version not needed, this is due to basic implemantion of Rs function rowMeans
    if(length(ACP_index[[i]])==1){
      ACT_CONS_PAT[,i]<-RES_CONS_PATp[,ACP_index[[i]]]
    }else{
      ACT_CONS_PAT[,i]<-rowMeans(RES_CONS_PATp[,ACP_index[[i]]]) #resource allocation for each product
    }
  }
}

## percentage value of how many costs of each activity pool are used

# ACT_CONS_PAT<- as.data.frame(scale(ACT_CONS_PAT, center=FALSE, scale=colSums(ACT_CONS_PAT)))

if (!is.null(ME_AD)) {
  if(length(RC_ACP_index)==1){
    ACT_CONS_PAT<-ACT_CONS_PAT*runif(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,min=(1-ME_AD),max=(1+ME_AD))
  }else{
    err_MAT<-matrix(runif(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO*length(RC_ACP_index),min=(1-ME_AD),max=(1+ME_AD)),ncol=length(RC_ACP_index))
    ACT_CONS_PAT<-ACT_CONS_PAT*err_MAT
    ACT_CONS_PAT = ACT_CONS_PAT/colSums(ACT_CONS_PAT)
  }
}

FIRM$COSTING_SYSTEM$ACT_CONS_PAT<-as.matrix(ACT_CONS_PAT)
FIRM$COSTING_SYSTEM$ACP_index_choosen


return(FIRM)

}

################################################
# COST DRIVER SELECTION HEURISTICS
# The algorithms orientates to 
# Balakrishnan, Hansen, Labro 2011
################################################

## Balakrishnan, Hansen, Labro 2011
MAP_CP_P_BIGPOOL <-function(FIRM,ME_AD=NULL,ME_NUM=NULL){

ACP_index_choosen<-vector(mode="numeric")
# normalize RES_CONS_PAT
RES_CONS_PAT<-FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp
ME_AD = FIRM$COSTING_SYSTEM$Error
RCC<-FIRM$COSTING_SYSTEM$RCC
RC_ACP_index<-FIRM$COSTING_SYSTEM$RC_ACP

# preallocation
ACT_CONS_PAT<-matrix(0,nrow = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,ncol = length(RC_ACP_index))


for (i in 1:length(RC_ACP_index)){
    
## exception handler if there is only one resource in ACP[[i]] take this resource as the driver
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
}
 if (!is.null(ME_AD)) {
  if(length(RC_ACP_index)==1){
    ACT_CONS_PAT<-ACT_CONS_PAT*runif(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,min=(1-ME_AD),max=(1+ME_AD))
  }else{
    err_MAT<-matrix(runif(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO*length(RC_ACP_index),min=(1-ME_AD),max=(1+ME_AD)),ncol=length(RC_ACP_index))
    ACT_CONS_PAT<-ACT_CONS_PAT*err_MAT
    ACT_CONS_PAT = ACT_CONS_PAT/colSums(ACT_CONS_PAT)
  }
ACP_index_choosen[i]<-RC_ACP_index[[i]][1]
 }


FIRM$COSTING_SYSTEM$ACT_CONS_PAT<-as.matrix(ACT_CONS_PAT)
FIRM$COSTING_SYSTEM$ACP_index_choosen

return(FIRM)
}

MAP_CP_P_AVERAGE <-function(FIRM,ME_AD=NULL,ME_NUM=NULL){
  
  ACP_index_choosen<-vector(mode="numeric")
  # normalize RES_CONS_PAT
  RES_CONS_PAT<-FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp
  ME_AD = FIRM$COSTING_SYSTEM$Error
  RCC<-FIRM$COSTING_SYSTEM$RCC
  RC_ACP_index<-FIRM$COSTING_SYSTEM$RC_ACP
  
  # preallocation
  ACT_CONS_PAT<-matrix(0,nrow = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,ncol = length(RC_ACP_index))
  
  
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
  
  if (!is.null(ME_AD)) {
    if(length(RC_ACP_index)==1){
      ACT_CONS_PAT<-ACT_CONS_PAT*runif(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,min=(1-ME_AD),max=(1+ME_AD))
    }else{
      err_MAT<-matrix(runif(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO*length(RC_ACP_index),min=(1-ME_AD),max=(1+ME_AD)),ncol=length(RC_ACP_index))
      ACT_CONS_PAT<-ACT_CONS_PAT*err_MAT
      ACT_CONS_PAT = ACT_CONS_PAT/colSums(ACT_CONS_PAT)
    }
  
  
  FIRM$COSTING_SYSTEM$ACT_CONS_PAT<-as.matrix(ACT_CONS_PAT)
  FIRM$COSTING_SYSTEM$ACP_index_choosen
  
  
  return(FIRM)
  
}
}

MAP_CP_P_INDEXED <-function(FIRM,ME_AD=NULL,ME_NUM=NULL){
  
  stop("Not fully implemented yet")
  ACP_index_choosen<-vector(mode="numeric")
  # normalize RES_CONS_PAT
  RES_CONS_PAT<-FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp
  ME_AD = FIRM$COSTING_SYSTEM$Error
  RCC<-FIRM$COSTING_SYSTEM$RCC
  RC_ACP_index<-FIRM$COSTING_SYSTEM$RC_ACP
  
  # preallocation
  ACT_CONS_PAT<-matrix(0,nrow = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,ncol = length(RC_ACP_index))
  
  
    
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


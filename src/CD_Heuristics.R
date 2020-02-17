# COST DRIVER SELECTION HEURISTICS
# The algorithms orientates to 
# Balakrishnan, Hansen, Labro 2011


## Balakrishnan, Hansen, Labro 2011
MAP_CP_P_RANDOM <-function(FIRM,ME_AD=NULL,ME_NUM=NULL){
  #set.seed(NULL)
  ACP_index_choosen<-vector(mode="numeric")
  # normalize RES_CONS_PAT
  RES_CONS_PAT<-FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp
  ME_AD = FIRM$COSTING_SYSTEM$Error
  ME_AD_NUMB = FIRM$COSTING_SYSTEM$NUMB_Error
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
      ACT_CONS_PAT[,i]<-RES_CONS_PAT_temp[,sample(c(1:ncol(RES_CONS_PAT_temp)))[1]] #uses a random resource as the driver
    }
  }
  
  
  if (is.null(ME_AD_NUMB)){     #if the numb_error is zero, the measurement error is applied to all driver links in each driver in the act const pat
    if (!is.null(ME_AD)) {
      if(length(RC_ACP_index)==1){        #if there is only one cost pool
        ACT_CONS_PAT<-ACT_CONS_PAT*runif(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,min=(1-ME_AD),max=(1+ME_AD))
        
       # ACT_CONS_PAT = sweep((ACT_CONS_PAT),2,colSums(ACT_CONS_PAT),"/") ####durch AC_CONS_PAT teilen?
        
      }else{                              # when therer are more than one cost pools, every cost pool is weighed with the measurement error
        err_MAT<-matrix(runif(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO*(length(RC_ACP_index)),min=(1-ME_AD),max=(1+ME_AD)),ncol=length(RC_ACP_index))
        ACT_CONS_PAT<-ACT_CONS_PAT*err_MAT
        
      #  ACT_CONS_PAT = sweep((ACT_CONS_PAT),2,colSums(ACT_CONS_PAT),"/")
      }
      ACP_index_choosen[i]<-RC_ACP_index[[i]][1]
    }
    
    # Measurement error 
    # waive seed for implementing random measurement errors
  }else{
    if (!is.null(ME_AD)) {
      if(length(RC_ACP_index)==1){        #if there is only one cost pool
        error_links = round(runif(ceiling(ME_AD_NUMB* FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO),1,FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO),0)
        ACT_CONS_PAT[error_links] = ACT_CONS_PAT[error_links]*runif(length(error_links),min=(1-ME_AD),max=(1+ME_AD))
        
        ACT_CONS_PAT = sweep((ACT_CONS_PAT),2,colSums(ACT_CONS_PAT),"/") ####durch AC_CONS_PAT teilen?
        
      }else{                              # when therer are more than one cost pools, every cost pool is weighed with the measurement error
        
        err_MAT<-matrix(1,nrow = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,ncol=length(RC_ACP_index))
        
        for (i in 1:length(RC_ACP_index)){
          error_links = round(runif(ceiling(ME_AD_NUMB* FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO),1,FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO),0)
          err_MAT[error_links,i] = runif(length(error_links),min=(1-ME_AD),max=(1+ME_AD))
        }
        
        ACT_CONS_PAT<-ACT_CONS_PAT*err_MAT
        ACT_CONS_PAT = sweep((ACT_CONS_PAT),2,colSums(ACT_CONS_PAT),"/")
      }
      ACP_index_choosen[i]<-RC_ACP_index[[i]][1]}
    
    
    FIRM$COSTING_SYSTEM$ACT_CONS_PAT<-as.matrix(ACT_CONS_PAT)
    FIRM$COSTING_SYSTEM$ACP_index_choosen
    
    
    
    return(FIRM)
  }
}

MAP_CP_P_BIGPOOL <-function(FIRM,ME_AD=NULL,ME_NUM=NULL){
set.seed(NULL)
ACP_index_choosen<-vector(mode="numeric")
# normalize RES_CONS_PAT
RES_CONS_PAT<-FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp
ME_AD = FIRM$COSTING_SYSTEM$Error
ME_AD_NUMB = FIRM$COSTING_SYSTEM$NUMB_Error
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


if (is.null(ME_AD_NUMB)){     #if the numb_error is zero, the measurement error is applied to all driver links in each driver in the act const pat
  if (!is.null(ME_AD)) {
    if(length(RC_ACP_index)==1){        #if there is only one cost pool
      ACT_CONS_PAT<-ACT_CONS_PAT*runif(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,min=(1-ME_AD),max=(1+ME_AD))
      
     # ACT_CONS_PAT = sweep((ACT_CONS_PAT),2,colSums(ACT_CONS_PAT),"/") ####durch AC_CONS_PAT teilen?
      
    }else{                              # when therer are more than one cost pools, every cost pool is weighed with the measurement error
      err_MAT<-matrix(runif(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO*(length(RC_ACP_index)),min=(1-ME_AD),max=(1+ME_AD)),ncol=length(RC_ACP_index))
      ACT_CONS_PAT<-ACT_CONS_PAT*err_MAT
      #ACT_CONS_PAT = sweep((ACT_CONS_PAT),2,colSums(ACT_CONS_PAT),"/")
    }
    ACP_index_choosen[i]<-RC_ACP_index[[i]][1]
  }
  
  # Measurement error 
  # waive seed for implementing random measurement errors
}else{
  if (!is.null(ME_AD)) {
    if(length(RC_ACP_index)==1){        #if there is only one cost pool
      error_links = round(runif(ceiling(ME_AD_NUMB* FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO),1,FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO),0)
      ACT_CONS_PAT[error_links] = ACT_CONS_PAT[error_links]*runif(length(error_links),min=(1-ME_AD),max=(1+ME_AD))
      
      #ACT_CONS_PAT = sweep((ACT_CONS_PAT),2,colSums(ACT_CONS_PAT),"/") ####durch AC_CONS_PAT teilen?
      
    }else{                              # when therer are more than one cost pools, every cost pool is weighed with the measurement error
      
      err_MAT<-matrix(1,nrow = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,ncol=length(RC_ACP_index))
      
      for (i in 1:length(RC_ACP_index)){
        error_links = round(runif(ceiling(ME_AD_NUMB* FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO),1,FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO),0)
        err_MAT[error_links,i] = runif(length(error_links),min=(1-ME_AD),max=(1+ME_AD))
      }
       
      ACT_CONS_PAT<-ACT_CONS_PAT*err_MAT
      #ACT_CONS_PAT = sweep((ACT_CONS_PAT),2,colSums(ACT_CONS_PAT),"/")
    }
    ACP_index_choosen[i]<-RC_ACP_index[[i]][1]}


FIRM$COSTING_SYSTEM$ACT_CONS_PAT<-as.matrix(ACT_CONS_PAT)
FIRM$COSTING_SYSTEM$ACP_index_choosen



return(FIRM)
}
}

MAP_CP_P_AVERAGE <-function(FIRM,ME_AD=NULL,ME_NUM=NULL){
  
  stop("Average driver is still producing an MAPE >0.")
  #in discussion ; 
  
  
  
  #FIRM$COSTING_SYSTEM$CD_HEURISTIC = 'MAP_CP_P_AVERAGE, == 1'
  ACP_index_choosen<-vector(mode="numeric")
  # normalize RES_CONS_PAT
  COST_CONS_PAT<-FIRM$PRODUCTION_ENVIRONMENT$COST_CONS_PATp
  ME_AD = FIRM$COSTING_SYSTEM$Error
  ME_AD_NUMB = FIRM$COSTING_SYSTEM$NUMB_Error
  RCC<-FIRM$COSTING_SYSTEM$RCC
  RC_ACP_index<-FIRM$COSTING_SYSTEM$RC_ACP
  
  # preallocation
  ACT_CONS_PAT<-matrix(0,nrow = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,ncol = length(RC_ACP_index))
  
  
  for (i in 1:length(RC_ACP_index)){
    
    ## exception handler if there is only one resource in ACP[[i]] take this resource as the driver
    # in original version not needed, this is due to basic implementation of Rs function rowSums
    if(length(RC_ACP_index[[i]])==1){
      ACT_CONS_PAT[,i]<-COST_CONS_PAT[,RC_ACP_index[[i]]]
    }else{
      
      # 1. Order ACP_index in decreasing order of resource size
      #RC_order<-sort(RCC, decreasing=TRUE)
      
      #RC_ACP_index[[i]]<-RC_ACP_index[[i]][order(match(RC_ACP_index[[i]],RC_order))]
      COST_CONS_PAT_temp<-COST_CONS_PAT[,RC_ACP_index[[i]]] # subsetting for resources used in this ACP and ordering
      # ACs<-sort(colSums(RES_CONS_PAT_temp),decreasing = TRUE,index.return=TRUE
        
      ACT_CONS_PAT[,i] = rowMeans(COST_CONS_PAT_temp) #use the average of all resources in that CP as the driver
      
      
    }
  } 

  if (is.null(ME_AD_NUMB)){     #if the numb_error is zero, the measurement error is applied to all driver links in each driver in the act const pat
    if (!is.null(ME_AD)) {
      if(length(RC_ACP_index)==1){        #if there is only one cost pool
        ACT_CONS_PAT<-ACT_CONS_PAT*runif(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,min=(1-ME_AD),max=(1+ME_AD))

        ACT_CONS_PAT = sweep((ACT_CONS_PAT),2,colSums(ACT_CONS_PAT),"/") ####durch AC_CONS_PAT teilen?

      }else{                              # when therer are more than one cost pools, every cost pool is weighed with the measurement error
        err_MAT<-matrix(runif(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO*(length(RC_ACP_index)),min=(1-ME_AD),max=(1+ME_AD)),ncol=length(RC_ACP_index))
        ACT_CONS_PAT<-ACT_CONS_PAT*err_MAT
        ACT_CONS_PAT = sweep((ACT_CONS_PAT),2,colSums(ACT_CONS_PAT),"/")
      }
      ACP_index_choosen[i]<-RC_ACP_index[[i]][1]
    }



  ##if numb_error is implemented###
  }else{
    if (!is.null(ME_AD)) {
      if(length(RC_ACP_index)==1){        #if there is only one cost pool
        error_links = round(runif(ceiling(ME_AD_NUMB* FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO),1,FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO),0)
        ACT_CONS_PAT[error_links] = ACT_CONS_PAT[error_links]*runif(length(error_links),min=(1-ME_AD),max=(1+ME_AD))

        ACT_CONS_PAT = sweep((ACT_CONS_PAT),2,colSums(ACT_CONS_PAT),"/") ####durch AC_CONS_PAT teilen?

      }else{                              # when therer are more than one cost pools, every cost pool is weighed with the measurement error

        err_MAT<-matrix(1,nrow = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,ncol=length(RC_ACP_index))

        for (i in 1:length(RC_ACP_index)){
          error_links = round(runif(ceiling(ME_AD_NUMB* FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO),1,FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO),0)
          err_MAT[error_links,i] = runif(length(error_links),min=(1-ME_AD),max=(1+ME_AD))
        }

        ACT_CONS_PAT<-ACT_CONS_PAT*err_MAT
        ACT_CONS_PAT = sweep((ACT_CONS_PAT),2,colSums(ACT_CONS_PAT),"/")
      }
      ACP_index_choosen[i]<-RC_ACP_index[[i]][1]
    }

  }


  
  
  FIRM$COSTING_SYSTEM$ACT_CONS_PAT<-ACT_CONS_PAT
  FIRM$COSTING_SYSTEM$ACP_index_choosen
  
  
  
  
  return(FIRM)
  
}

MAP_CP_P_INDEXED <-function(FIRM,ME_AD=NULL,ME_NUM=NULL){
  
  stop("Based on avergae driver, which is not properly working")
  
  #sort the resources in each cost pool ascending by size
  #if the length of a cp is larger than NUM
    #then chose the average of NUM largest resources as the driver
  #if the length of a cp is smaller then NUM
    #then chose the average of all resources as the driver
  #FIRM$COSTING_SYSTEM$CD_HEURISTIC = 'MAP_CP_P_INDEXED, == 2'
  RC_ACP_index = FIRM$COSTING_SYSTEM$RC_ACP
  RES_CONS_PAT = FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp
  ACP_index_choosen<-vector(mode="numeric")
  NUM = FIRM$COSTING_SYSTEM$NUM
  
  ME_AD = FIRM$COSTING_SYSTEM$Error   
  ME_AD_NUMB = FIRM$COSTING_SYSTEM$NUMB_Error
  RCC<-FIRM$COSTING_SYSTEM$RCC
  
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
      
      if(ncol(RES_CONS_PAT_temp)> NUM){
        ACT_CONS_PAT[,i]<-rowMeans(RES_CONS_PAT_temp[,1:2]) #use the average of the larges NUM resources as driver
      }else{
        ACT_CONS_PAT[,i]<-rowMeans(RES_CONS_PAT_temp)
      }
      
    }
  }
  
  ##ERROR HANDLING
  
  
  if (is.null(ME_AD_NUMB)){     #if the numb_error is zero, the measurement error is applied to all driver links in each driver in the act const pat
    if (!is.null(ME_AD)) {
      if(length(RC_ACP_index)==1){        #if there is only one cost pool
        ACT_CONS_PAT<-ACT_CONS_PAT*runif(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,min=(1-ME_AD),max=(1+ME_AD))
        
        ACT_CONS_PAT = sweep((ACT_CONS_PAT),2,colSums(ACT_CONS_PAT),"/") ####durch AC_CONS_PAT teilen?
        
      }else{                              # when therer are more than one cost pools, every cost pool is weighed with the measurement error
        err_MAT<-matrix(runif(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO*(length(RC_ACP_index)),min=(1-ME_AD),max=(1+ME_AD)),ncol=length(RC_ACP_index))
        ACT_CONS_PAT<-ACT_CONS_PAT*err_MAT
        ACT_CONS_PAT = sweep((ACT_CONS_PAT),2,colSums(ACT_CONS_PAT),"/")
      }
      ACP_index_choosen[i]<-RC_ACP_index[[i]][1]
    }
    
    
    
    ###if numb_error is implemented###
  }else{
    if (!is.null(ME_AD)) {
      if(length(RC_ACP_index)==1){        #if there is only one cost pool
        error_links = round(runif(ceiling(ME_AD_NUMB* FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO),1,FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO),0)
        ACT_CONS_PAT[error_links] = ACT_CONS_PAT[error_links]*runif(length(error_links),min=(1-ME_AD),max=(1+ME_AD))
        
        ACT_CONS_PAT = sweep((ACT_CONS_PAT),2,colSums(ACT_CONS_PAT),"/") ####durch AC_CONS_PAT teilen?
        
      }else{                              # when therer are more than one cost pools, every cost pool is weighed with the measurement error
        
        err_MAT<-matrix(1,nrow = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,ncol=length(RC_ACP_index))
        
        for (i in 1:length(RC_ACP_index)){
          error_links = round(runif(ceiling(ME_AD_NUMB* FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO),1,FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO),0)
          err_MAT[error_links,i] = runif(length(error_links),min=(1-ME_AD),max=(1+ME_AD))
        }
        
        ACT_CONS_PAT<-ACT_CONS_PAT*err_MAT
        ACT_CONS_PAT = sweep((ACT_CONS_PAT),2,colSums(ACT_CONS_PAT),"/")
      }
      ACP_index_choosen[i]<-RC_ACP_index[[i]][1]
    }
    
  }
  
  FIRM$COSTING_SYSTEM$ACT_CONS_PAT<-as.matrix(ACT_CONS_PAT)
  FIRM$COSTING_SYSTEM$ACP_index_choosen
  
  
  
  
  return(FIRM)
} 

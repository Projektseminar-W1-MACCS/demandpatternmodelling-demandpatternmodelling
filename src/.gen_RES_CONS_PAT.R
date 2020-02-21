#BUILDING THE ACT_CONS_PAT  /  RES_CONS_PAT 

.gen_RES_CONS_PAT <- function(FIRM) {

  ## ====================== STEP 1 Determining the activities =========================
  UNITLEVEL_ACT_SHARE_MIN = 0.2    #0.2 is the size of DISP1 =10
  UNITLEVEL_ACT_SHARE_MAX = 0.2
  FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE = runif(1, UNITLEVEL_ACT_SHARE_MIN, UNITLEVEL_ACT_SHARE_MAX) #random activity share between lower and upper bounds
  
  FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE_MIN = UNITLEVEL_ACT_SHARE_MIN
  FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE_MAX = UNITLEVEL_ACT_SHARE_MAX
  ## ====================== STEP 1 Determining the amount of cost categories =================
  
  unitsize = floor(FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE*FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)
  nonunitsize = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES-unitsize
  
  FIRM$PRODUCTION_ENVIRONMENT$UNITSIZE = unitsize
  FIRM$PRODUCTION_ENVIRONMENT$NONUNITSIZE = nonunitsize
  DENS = FIRM$PRODUCTION_ENVIRONMENT$DENS #Necessary transition for check.
  ## ====================== STEP 0.b Determining the density (DENS)  =========================

  #Randomization and setting clear design points. 
  
  if(DENS == -1)
  {
    DENS_MIN = 0.4;
    DENS_MAX = 0.7;
    DENS = runif(1, DENS_MIN, DENS_MAX);
  }
  FIRM$PRODUCTION_ENVIRONMENT$DENS_raw = DENS

  

  
## ====================== STEP 1 BASELINE NORM ========================= 


repeat    {
    
BASE = rnorm(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO) #creates for every CO (product) a random number
  
RES_CONS_PATpre = matrix(rnorm(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO*FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES,mean=0,sd=1), 
                         FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO, FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)                            #random pre matrix, as Baseline

RES_CONS_PAT = matrix(0, nrow = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO, ncol = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES, byrow = TRUE) #empy matrix, that is going to be filled 


## ====================== STEP 1.a CORRELATION ========================= 
# Products and Resource are transposed in constrast to Anand 2019 but there is no issue in the model
# Rows Products Colums Resources

FIRM$PRODUCTION_ENVIRONMENT$COR1 = COR1
FIRM$PRODUCTION_ENVIRONMENT$COR2 = COR2

# Correlation of the top [DISP1] resources
if(COR1 == -1){
  COR1 <- runif(1, -0.2, 0.8)
  FIRM$PRODUCTION_ENVIRONMENT$COR1 = COR1
}

sqrt_const_1 <- sqrt(1 - (COR1 * COR1))

# Correlation of the remaining resources
if(COR2 == -1){
  COR2 <- runif(1, -0.2, 0.8)
  FIRM$PRODUCTION_ENVIRONMENT$COR2 = COR2
}

sqrt_const_2 <- sqrt(1 - (COR2 * COR2))

for (i in 1:(FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE*FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)) #unitsize+1
{
  RES_CONS_PAT[,i] <- (COR1 * BASE)+ sqrt_const_1 * RES_CONS_PATpre[,i];
}

for (i in ((FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE*FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)+1) : FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES) #nonunitsize+1 (34+1)
{
  RES_CONS_PAT[,i] <- (COR1 * BASE)+ sqrt_const_2 * RES_CONS_PATpre[,i];
}

## ====================== STEP 1.b DENSITY ========================= 
res_cons_pat_b_pre = runif(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO*FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)

## 1/0 DENSITY
res_cons_part_b <- matrix(ifelse(res_cons_pat_b_pre > DENS, 0,1),
                          FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)


RES_CONS_PAT = res_cons_part_b * RES_CONS_PAT
FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PAT = RES_CONS_PAT

## ====================== STEP 1.c Ceiling and Scaling ============= 

# take absolute value of X and Z and scale by 10 and round them
# Anand et al. 2019
##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS
RES_CONS_PAT[,1] <- (BASE)
RES_CONS_PAT <- ceiling(abs(RES_CONS_PAT) * 10)
##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAND
RES_CONS_PAT_TOTAL <- RES_CONS_PAT * FIRM$PRODUCTION_ENVIRONMENT$DEMAND       
##CALCULATING TCU
TCU <- colSums(RES_CONS_PAT_TOTAL)
##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAMD / TRU (Currently like this in Anand et al. 2019)
RES_CONS_PATp <- sweep((RES_CONS_PAT_TOTAL),2,TCU,"/") #Absolute matrix to relative matrix

## ===================== EXCPETION HANDLER ====================

# EXPECTION HANDLER  & CHECKS AFTER ANAND ET AL. 2019 # It is important the the first RES_CONS_PAT column has no zeros
# in accordance with Anand etl. 2019 and Balakrishnan et al. 2011; Substantiation of this hidden formalization remains unclear. 

PRO_ZEROS<-any(rowSums(RES_CONS_PAT[,])==0)   #every product need at least one resource
RES_ZEROS<-any(colSums(RES_CONS_PAT[,])==0)   #every resource needs to be used at least once
BASE_ZEROS <-any(RES_CONS_PAT[,1]==0)         #first resource needs to be in every product ->why?

if(PRO_ZEROS==FALSE & RES_ZEROS==FALSE & BASE_ZEROS==FALSE) #discard the matrix if one of these conditions is not met
{
  break
}

}


## ====================== STEP 3 CHECK ========================= 

# AverageZeroConsumption
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$NonZeroConsumption = sum(colSums(RES_CONS_PAT != 0))/     #Ratio of Zeros in Res_cons_pat
  (FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO * FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)

# Average consumption of products consuming a resource
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$countNonZero<-mean(colSums(RES_CONS_PAT[,]>0))

# Correlation Test
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$COR1<-mean(cor(RES_CONS_PAT[,1:(FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE*    #correltion of first 16 resources with first resource
                                                                      FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)])[1,])
  
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$COR2<-mean(cor(RES_CONS_PAT[,c(1,((FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE*
                                                                         FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)
                                               +1):FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)])[1,])



#Average distance 
FIRM$PRODUCTION_ENVIRONMENT$TRU = colSums(RES_CONS_PAT*FIRM$PRODUCTION_ENVIRONMENT$DEMAND) #total demand of every resource
FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PAT = RES_CONS_PAT
FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PAT_TOTAL = RES_CONS_PAT_TOTAL
FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp = RES_CONS_PATp
## OPEN ´´
# for schliefe? 

print(COR1)
print(FIRM$PRODUCTION_ENVIRONMENT$CHECK$COR1)
print(COR2)
print(FIRM$PRODUCTION_ENVIRONMENT$CHECK$COR2)
#
#
#
return(FIRM)


}

.gen_RES_CONS_PAT_Anand <- function(FIRM) {
  
  ## ====================== STEP 0 Determining the activities =========================
  UNITLEVEL_ACT_SHARE_MIN = 0.2    #0.2 is the size of DISP1 =10
  UNITLEVEL_ACT_SHARE_MAX = 0.2
  FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE = runif(1, UNITLEVEL_ACT_SHARE_MIN, UNITLEVEL_ACT_SHARE_MAX) #random activity share between lower and upper bounds
  
  FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE_MIN = UNITLEVEL_ACT_SHARE_MIN
  FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE_MAX = UNITLEVEL_ACT_SHARE_MAX
  ## ====================== STEP 1 Determining the amount of cost categories =================
  
  unitsize = floor(FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE*FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)
  nonunitsize = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES-unitsize
  
  FIRM$PRODUCTION_ENVIRONMENT$UNITSIZE = unitsize
  FIRM$PRODUCTION_ENVIRONMENT$NONUNITSIZE = nonunitsize
  DENS = FIRM$PRODUCTION_ENVIRONMENT$DENS

  ## ====================== STEP 0.b Determining the density (DENS)  =========================
  #Randomization and setting clear design points. 

  if(DENS == -1)
  {
    DENS_MIN = 0.4;
    DENS_MAX = 0.7;
    DENS = runif(1, DENS_MIN, DENS_MAX);
  }

FIRM$PRODUCTION_ENVIRONMENT$DENS_draw = DENS

  ## ====================== STEP 1 BASELINE NORM ========================= 
  
  repeat    {
    
    BASE = rnorm(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO) #creates for every CO (product) a random number
    
    RES_CONS_PATpre = matrix(rnorm(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO*FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES,mean=0,sd=1), 
                             FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO, FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)                            #random pre matrix, as Baseline
    
    RES_CONS_PAT = matrix(0, nrow = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO, ncol = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES, byrow = TRUE) #empy matrix, that is going to be filled 
    
    
    
    
    ## ====================== STEP 1.a CORRELATION ========================= 
    # Products and Resource are transposed in constrast to Anand 2019 but there is no issue in the model
    # Rows Products Colums Resources
    
    
    COR1 = FIRM$PRODUCTION_ENVIRONMENT$COR1
    COR2 = FIRM$PRODUCTION_ENVIRONMENT$COR2
    
    
    # Correlation of the top [DISP1] resources
    if(COR1 == -1){
      COR1 <- runif(1, -0.2, 0.8)
    }
    FIRM$PRODUCTION_ENVIRONMENT$COR1_draw = COR1
    sqrt_const_1 <- sqrt(1 - (COR1 * COR1))
    
    # Correlation of the remaining resources
    if(COR2 == -1){
      COR2 <- runif(1, -0.2, 0.8)
    }
    FIRM$PRODUCTION_ENVIRONMENT$COR2_draw = COR2
    sqrt_const_2 <- sqrt(1 - (COR2 * COR2))
    
    DISP1= FIRM$PRODUCTION_ENVIRONMENT$DISP1
    
    
    for (i in 1:(DISP1)) #unitsize+1
    {
      RES_CONS_PAT[,i] <- (COR1 * BASE)+ sqrt_const_1 * RES_CONS_PATpre[,(i)];
    }
    
    for (i in ((DISP1+1)) : FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES) #nonunitsize+1 (34+1)
    {
      RES_CONS_PAT[,i] <- (COR2 * BASE)+ sqrt_const_2 * RES_CONS_PATpre[,(i)];
    }
    
    ## ====================== STEP 1.b DENSITY =========================
    
    res_cons_pat_b_pre = runif(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO*FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)
    ## 1/0 DENSITY
    res_cons_part_b <- matrix(ifelse(res_cons_pat_b_pre > DENS, 0,1),
                                FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)
    
    RES_CONS_PAT = res_cons_part_b * RES_CONS_PAT
    
    FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PAT = RES_CONS_PAT
    
    ## ====================== STEP 1.c Ceiling and Scaling ============= 
    
    # take absolute value of X and Z and scale by 10 and round them
    # Anand et al. 2019
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS
    RES_CONS_PAT[,1] <- (BASE)
    RES_CONS_PAT <- ceiling(abs(RES_CONS_PAT) * 10)
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAND
    RES_CONS_PAT_TOTAL <- sweep(RES_CONS_PAT,MARGIN = 1,FIRM$PRODUCTION_ENVIRONMENT$DEMAND,'*')     #does this needs to be a matrix multiplication?
    ##CALCULATING TCU
    TCU <- colSums(RES_CONS_PAT_TOTAL)
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAMD / TRU (Currently like this in Anand et al. 2019)
    RES_CONS_PATp <- sweep((RES_CONS_PAT_TOTAL),2,TCU,"/") #Absolute matrix to relative matrix
    
    TRU = colSums(RES_CONS_PAT)
    RES_CONS_PATps <- sweep((RES_CONS_PAT),2,TRU,"/")     #relative matrix for single production quantity without weitghted by demand
    
    ## ===================== EXCPETION HANDLER ====================
    
    # EXPECTION HANDLER  & CHECKS AFTER ANAND ET AL. 2019 # It is important the the first RES_CONS_PAT column has no zeros
    # in accordance with Anand etl. 2019 and Balakrishnan et al. 2011; Substantiation of this hidden formalization remains unclear. 
    
    PRO_ZEROS<-any(rowSums(RES_CONS_PAT[,])==0)   #every product need at least one resource (exclude column one??)
    RES_ZEROS<-any(colSums(RES_CONS_PAT[,])==0)   #every resource needs to be used at least once
    BASE_ZEROS <-any(RES_CONS_PAT[,1]==0)         #first resource needs to be in every product ->why?
    
    if(PRO_ZEROS==FALSE & RES_ZEROS==FALSE & BASE_ZEROS==FALSE) #discard the matrix if one of these conditions is not met
    {
      break
    }
    
  }
  
  
  ## ====================== STEP 3 CHECK ========================= 
  
  # AverageZeroConsumption
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$NonZeroConsumption = sum(colSums(RES_CONS_PAT != 0))/     #Ratio of Zeros in Res_cons_pat
    (FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO * FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)
  
  # Average consumption of products consuming a resource
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$countNonZero<-mean(colSums(RES_CONS_PAT[,]>0))
  
  # Correlation Test
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$COR1<-mean(cor(RES_CONS_PATp[,1],RES_CONS_PATp[,2:(DISP1)]))
  
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$COR2<-mean(cor(RES_CONS_PATp[,1],RES_CONS_PATp[,DISP1:FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES]))
  
  
  
  #Average distance 
  FIRM$PRODUCTION_ENVIRONMENT$TRU = colSums(RES_CONS_PAT_TOTAL) #total demand of every resource
  FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PAT = RES_CONS_PAT
  FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PAT_TOTAL = RES_CONS_PAT_TOTAL
  FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp = RES_CONS_PATp
  FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATps = RES_CONS_PATps
  ## OPEN ´´
  # for schliefe? 
  # 
  #
  #
  #
  return(FIRM)
  
  
}


#####################################################
# BUILDING THE ACT_CONS_PAT  /  RES_CONS_PAT 
#####################################################

.gen_RES_CONS_PAT <- function(FIRM) {

## ====================== STEP 1 BASELINE NORM ========================= 

  repeat    {
    
BASE = rnorm(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO)
  
RES_CONS_PATpre = matrix(rnorm(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO*FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES,mean=0,sd=1), 
                         FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO, FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES) 
RES_CONS_PAT = matrix(0, nrow = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO, ncol = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES, byrow = TRUE)  

## ====================== STEP 1.a CORRELATION ========================= 
# Products and Resource are transposed in constrast to Anand 2019 but there is no issue in the model
# Rows Products Colums Resources

# Correlation of the top [DISP1] resources
COR1 <- runif(1, -0.2, 0.8);
sqrt_const_1 <- sqrt(1 - (COR1 * COR1))

# Correlation of the remaining resources
COR2 <- runif(1, -0.2, 0.8);
sqrt_const_2 <- sqrt(1 - (COR2 * COR2))

for (i in 1:(FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE*FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)+1)
{
  RES_CONS_PAT[,i] <- (COR1 * BASE)+ sqrt_const_1 * RES_CONS_PATpre[,(i - 1)];
}

for (i in ((FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE*FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)) : FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES+1)
{
  RES_CONS_PAT[,i] <- (COR1 * BASE)+ sqrt_const_2 * RES_CONS_PATpre[,(i - 1)];
}

## ====================== STEP 1.b DENSITY ========================= 
res_cons_pat_b_pre = runif(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO*FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)

## 1/0 DENSITY
res_cons_part_b <- matrix(ifelse(res_cons_pat_b_pre > FIRM$PRODUCTION_ENVIRONMENT$DENS, 0,1),
                          FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)
  
RES_CONS_PAT = res_cons_part_b * RES_CONS_PAT
FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PAT = RES_CONS_PAT

## ====================== STEP 1.c Ceiling and Scaling ============= 

# take absolute value of X and Z and scale by 10 and round them
# Anand et al. 2019 
RES_CONS_PAT[,1] <- (BASE);
RES_CONS_PAT <- ceiling(abs(RES_CONS_PAT) * 10); 


## ===================== EXCPETION HANDLER ====================

# EXPECTION HANDLER  & CHECKS AFTER ANAND ET AL. 2019
PRO_ZEROS<-any(rowSums(RES_CONS_PAT[,])==0)
RES_ZEROS<-any(colSums(RES_CONS_PAT[,])==0)
BASE_ZEROS <-any(RES_CONS_PAT[,1]==0)


if(PRO_ZEROS==FALSE & RES_ZEROS==FALSE & BASE_ZEROS==FALSE)
{
  break
}

}


## ====================== STEP 3 CHECK ========================= 

# AverageZeroConsumption
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$NonZeroConsumption = sum(colSums(RES_CONS_PAT != 0))/
  (FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO * FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)

# Average consumption of products consuming a resource
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$countNonZero<-mean(colSums(RES_CONS_PAT[,]>0))

# Correlation Test
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$COR1<-mean(cor(RES_CONS_PAT[,1:(FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE*
                                                                      FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)])[1,])
  FIRM$PRODUCTION_ENVIRONMENT$CHECK$COR2<-mean(cor(RES_CONS_PAT[,c(1,((FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE*
                                                                         FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)
                                               +1):FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)])[1,])


#Average distance 
RES_CONS_PATp = t(t(RES_CONS_PAT*FIRM$PRODUCTION_ENVIRONMENT$DEMAND)/rowSums(t(RES_CONS_PAT*FIRM$PRODUCTION_ENVIRONMENT$DEMAND))) # CHECKED 19/09/12
FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PATp = RES_CONS_PATp
FIRM$PRODUCTION_ENVIRONMENT$TRU = colSums(RES_CONS_PAT*FIRM$PRODUCTION_ENVIRONMENT$DEMAND)
FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PAT = RES_CONS_PAT  
## OPEN ´´
# for schliefe? 
# 
#
#
#
return(FIRM)

}



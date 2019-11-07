---
title: "Cost System Design Model Documentation"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, eval = FALSE, tidy=TRUE, tidy.opts=list(width.cutoff=10))
```

## 1. Introduction

This readme file provides an overview of the underlying Cost System Design Model written in R. The model is mostly based on the numerical framework on Cost System Design proposed by  Anand, V., R. Balakrishnan, and E. Labro (2019). The code and documentation for Anand's et al. model can be found under http://vicanand.weebly.com/abl_jmar_code.html. Goal with this model was to replicate Anand's model and produce the same results using the same overall logic.
However, further heuristics, as well as  adaptions to the Programming Language R distinguish this models Sytnax from its predecessor's. It also contains further improvements and developments. 

The overall objective of this replication model is  first to create a fictional firm (including it's production environment), measrue the production costs of each product (Benachmark Product Costs), secondly build a costing system to calculate again the product costs using different heuristics to e.g. allocate resource costs to pools or choose a cost-driver (Estimated Product Costs) and eventually compare these two measures to compute the error.

In the following pages this documentation will go along the models structure (Figure 1) and explain the basic logic behind single functions and structures of the code. Overall objective is to describe the models logic as well as the implementation in R. 
<br>

## 2. Working with the model in RStudio

RStudio is an open source and free to use software which provides an integrated development environment (IDE) for the Programming Language R.

Rstudio can be downloaded here : https://rstudio.com/products/rstudio/download/#download

The Programming Language R itself must be downloaded and installed separatly: https://cran.rstudio.com/

Furthermore the model works with the following library packages which provide functions for e.g. plotting or analysing data. These libraries are installes by running the follwing code in the 0_preparing_project.R file. This needs to be done before running the model itself. 

<br>
```{r library packages, include= TRUE, eval = FALSE}
install.packages(c(
  "dplyr",
  "tidyr",
  "rmarkdown",
  "ggplot2"

))

Packages <- c("dplyr", "ggplot2", "rmarkdown", "tidyr")
lapply(Packages, library, character.only = TRUE)

```

<br>
\newpage

## 3. Model Overview (0_preparing_project.R)

The model contains of 9 R-Script files which can be seen in the following model overview. The 0_preparing_project.R file is the starting point for running the model. There, all other files are sourced and so their functions called.

![Figure 1: Cost System Design Model Overview](C:\Users\cms9023\Documents\CostSystemDesignSim\Modeloverview.png)
<br>
\newpage

## 4. Model Initialization (1_INIT.R)

The 1_INIT.R file loads all input parameters which are set by the modeler. These will later define the circumstances and limitations for the modelled Production Environment and Costing System. 
First step is to create an empty firm with its subsets Production Environment and Costing System. The **FIRM** variable is basically just a list that contains all necessary information that are need throughout the varius functions.  
The **DATA** variable is an empty dataframe which gets filled with the measures that go into the output file.
<br>
```{r Input Parameters, tidy=TRUE, tidy.opts=list(width.cutoff=10)}
  ## ======================================INPUT MASK======================================================
  FIRM = list()                           
  FIRM$PRODUCTION_ENVIRONMENT = list()
  FIRM$COSTING_SYSTEM = list()
  DATA = data.frame()
  
  
  NUMB_PRO =         50                     #INPUT independent Variable - Number of products 
  NUMB_RES  =        50                     #INPUT independent variable - Number of factors

  SIM_NUMB =         200                    #Control Variable - Number of Simulations for every single environment (standard: 30)     

  TC =               1000000                #Total costs

  ProductOutput=     1                      #Zero = no tracking
  set_pe_constant=   1                      #Control Variable -  Decide if Simulation is reproducible {1} or random {0}
  set_cs_constant=   0                      #Control Variable 
  vary_demand =      0                      #Control Variable
  
  dec_ERROR=         1                      #Control Variable - 
  seed=              13                     #Control Variable -
  
  #dec_DC=           0                      # = no direct costs 
  dec_CP=            1                      # =
  dec_CD=            1                      # =
  
  
  CP = c(1,2,4,6,8,10,12,14,16,18,20)       #No. of Cost Pools
  COR = c(0.6)                              #Correlation between resources
  RC_VAR =  c(-1)                           #Resource cost variation --> base for DISP2
  Q_VAR = c(1)                              #Demand variation
  Error = c(0)                              #Measurement error
  NUMB_Error = c(1)                         #Number of errornoues links
  DENS = c(-1)                              #Number of links between products and resources (sharing)
  CC = 0.4                                  #Correlation Cutoff for correlative assignement in CP HEURISTICS
  MISCPOOLSIZE = 0.25                       #share of total costs that are supposed to go into the miscpool 
  DISP1 = 10                                #No. of the biggest resources that have a DISP2 share of the total costs
  
## ======================================END OF INPUT MASK=========================================
```


The simulation is then run for every combination of the input parameters and the number of simulation that is set with **SIM_NUMB.** To do this the model loops over every input variable and creates a number of firms (**SIM_NUMBs**) for every combination of those. As an example you can see in the above code, that this model simulates over different number of cost pools.
<br>
```{r for loops for code running, eval = FALSE}
            set.seed(seed) #Reproducability
            o=1 # First design point
            
## ====================================== DESIGN OF EXPERIMENTS ===================================
## EVIRONMENTAL FACTORS [] 
  for (ix_CP in seq_along(CP)) {
     for (ix_COR in seq_along(COR)) {
       for (ix_RC_VAR in seq_along(RC_VAR)) {
         for (ix_Q_VAR in seq_along(Q_VAR)) {
           for (ix_Error in seq_along(Error)) {
               for (ix_NUMB_Error in seq_along(NUMB_Error)) {
                 for (ix_DENS in seq_along(DENS)) {
                   for(ix_CC in seq_along(CC)){
                     for(ix_MISCPOOLSIZE in seq_along(MISCPOOLSIZE)){
                       for(ix_DISP1 in seq_along(DISP1)){
```


For every created firm the variable is mapped into the **FIRM** list. By calling this list in every R-Script file of the model, the code gets access to all there contained variables. This ensures that always the same values are used and provides an easy possibility to map created variables back into the **FIRM.**
<br>

```{r Mapping created variables to the FIRM, eval = FALSE}
 ## ====================== PREDETERMINING AND PREALLOCATION  ========================          
    
    FIRM$PRODUCTION_ENVIRONMENT$DENS = DENS[ix_DENS]   
    FIRM$PRODUCTION_ENVIRONMENT$COR  = COR[ix_COR]
    FIRM$PRODUCTION_ENVIRONMENT$Q_VAR= Q_VAR[ix_Q_VAR]
    FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO = NUMB_PRO
    FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES = NUMB_RES
    FIRM$COSTING_SYSTEM$CP = CP[ix_CP]
    FIRM$COSTING_SYSTEM$RC_VAR = RC_VAR[ix_RC_VAR]
    FIRM$COSTING_SYSTEM$Error = Error[ix_Error]
    FIRM$COSTING_SYSTEM$NUMB_Error = NUMB_Error[ix_NUMB_Error]
    FIRM$COSTING_SYSTEM$TC = TC
    FIRM$COSTING_SYSTEM$CC = CC
    FIRM$COSTING_SYSTEM$MISCPOOLSIZE = MISCPOOLSIZE
    FIRM$COSTING_SYSTEM$DISP1 = DISP1
```

The simulation is now run for the defined number of simulations in **SIM_NUMB.** As shown in the model overview the production environment and the costing system (consisting of the chosen Cost Pool Allocation heuristic and the driver seltion heuristic) are generated. . To change the used heuristics only the called functions need to be changed. The below example currently calculates with a Size based correlation cutoff heuristic for building the cost pools and a big pool heuristic for selecting the driver. Since the costing system heuristics are working with some of the generated output parameter from the production environment (e.g. **RCC**, **RES_CONS_PAT**), the production environment is generated first.
<br>

```{r Running the simulation, eval = FALSE}

   nn=1 # necessary for repeating the SIM_NUMB loop
  #### ============================== SIMULATION =====================================
  for (nn in 1:SIM_NUMB) {
    
    #print(FIRM$COSTING_SYSTEM$CP)  
    #print(FIRM$COSTING_SYSTEM$Error)  
    
    
    FIRM = gen_ProductionEnvironment(FIRM) #Generate Production Environment with RES_CONS_PAT

  
    FIRM = MAP_RES_CP_SIZE_CORREL_CUTOFF_MISC_ANAND2(FIRM) #Building the cost pools
  
  
    FIRM = MAP_CP_P_BIGPOOL(FIRM,Error) #Selecting the drivers of a cost pool

```
<br>

\newpage
## 5. Generating the Production Environment (gen_ProductionEnviroment.R)

<br>
The R-Script File gen_ProductionEnvironment.R builds the Production Environment by calling its sub-functions to generate the demand function, the resource-consumption-matrix, the resource cost vector (**RCC**) and the benchmark product costs. 

Here, the Density is defined as well. 
The Density can be fixly determined by the modeler in the input parameter or (if it is set there to -1) it is chosen from a uniform distribution U[**DENS_MIN**, **DENS_MAX**]. The Density (Sparsity) defines approximately how many zeros are in the **RES_CONS_PAT** (e.g **DENS** = 0.6 means that roughly 40% of the elements in **RES_CONS_PAT** are zero). Therefore the Density has a major influence on how eqaul and well distributed the resource consumption is among the products. 

```{r}
#Randomization and setting clear design points. 
if(DENS == -1)
{
DENS_MIN = 0.4;
DENS_MAX = 0.7;
DENS = runif(1, DENS_MIN, DENS_MAX);
FIRM$PRODUCTION_ENVIRONMENT$DENS = DENS
}


## ================= STEP 2 Building Demand, RES_CONS_PAT RCC and Benchmark Product Costs  ==============



FIRM = .gen_Demand_Anand(FIRM);

FIRM = .gen_RES_CONS_PAT_Anand(FIRM);

FIRM = .gen_RCC_Anand(FIRM);

FIRM = .genCOST_CONS_PAT(FIRM,COST_APPROACH = "ANAND")
```

### 5.1. Generating the Demand Vector (.gen_Q.R)

The first sub-function and output parameter which is set by the production environment is the demand function. Here, for the first time it becomes possible for the modeler to have an influence on the output, without changing the input parameters, by changing how the demand vector is build.

The demand vector defines the production quantity of each product. It therefore is a vector with the length of number of products (**NUMB_PRO**). 

Since there are now multiple ways of generating the demand, the R-Script file .gen_Q.R contains different functions which each create a different demand vector. 
Below is the code that generates a demand vector similar to Anands. It draws random values from a uniform distribution between 10 and 40 and then brings it down to a total number of 1000 units and distributes the volumes for each product on this overall quantity.

Each element in the vector is then the production quantity for the respective product.

Since there is a relatively small margin (between 10 and 40) for each product quantity it results that the demand vector is evenly distributed. Meaning that the here simulated firm sells from each of its product approximately the same amount. 

```{r Generating the Demand function}
NUMB_PRO = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO
  
  units = 10^3

  preDemand = as.integer(runif(NUMB_PRO,10,40))
  
  DEMAND = ceiling((preDemand/sum(preDemand))*units) #ceiled realized demand for each product
  
  FIRM$PRODUCTION_ENVIRONMENT$DEMAND = DEMAND
```

### 5.2. Generating the resource consumption matrix (.gen_RES_CONS_PAT.R)

The second component of the production environment is the randomly created resource consumption matrix, which is basically a matrix[i,j] which elements are the amount of resources[j] consumed to produce one element of a product[i]. An example is shown below.

```{r, eval = TRUE, echo = FALSE}
res_cons_pat = matrix(c(3,5,6,2,8,7,6,3,1,0,9,12,0,5,3),ncol = 5, nrow = 3)
print(res_cons_pat)
```
Each row stands for the resource consumption for one product while each column shows the resource consumption for this peticular resource ampong the products.
For example, the production of one unit product 1 needs six units of resource 3. 
Therefore, in this case a resource consumption matrix of the size **NUMB_PRO** * **NUMB_RES** is needed. 
The resource consumption needs to satisfy some correlation parameters, that create a specific similarity among products and resources. Also, the density must be satisfied, as well as the constraint that all resources are used in the products a least once and every product needs at least one resource. 
Additionally, there is the assumption that one resource (in this case the first) goes into every product. 

The heuristic for the construction of the resource consumption matrix developed by Balakrishnan, Hansen, and Labro (2011) is desctribed in the following sections. 
<br>

#### 5.2.1. Baseline RES_CONS_PAT

```{r RES_CONS_PAT 1, eval = FALSE}
 ## ====================== STEP 1 BASELINE NORM ========================= 
  
  repeat    {
    
    BASE = rnorm(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO) #creates for every CO (product) a random number
    
    RES_CONS_PATpre = matrix(rnorm(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO*FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES,mean=0,sd=1), 
                             FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO, FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)                            #random pre matrix, as Baseline
    
    RES_CONS_PAT = matrix(0, nrow = FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO, ncol = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES, byrow = TRUE) #empy matrix, that is going to be filled 

```

**BASE** is the first resource in the resource consumption matrix (therefore first column), that is used in every product and therefore is built seperatly. 
Then the **RES_CONS_PATpre** is build, a matrix with the same size of the **RES_CONS_PAT** filled with random values. 
Also an empty **RES_CONS_PAT** is generated which is going to be filled throughout the process.
<br>

#### 5.2.2. Correlation constraints

Next step is filling the yet empty matrix while satisfying the correlation constraints. The matrix is split into two parts based on the input variable **DISP1** which defines the number of big resources that account for a **DISP2** (value between 0 and 1) share of total costs (TC). In this case **DISP1** is set to 10, meaning that the 10 biggest resources account for 40% (when DISP2 =0.4) of the total costs. The matrix is therefore split columnwise into the first 1:DISP1 big resources and the remaining DISP1+1:NUMB_RES small resources. 

There are two correlations, one for the big resources and one for the small resources, meaning that the big resources correlate among themselves and with the **BASE** with approximately a correlation of COR1 as the small resources correlate among themselves and the **BASE** with a correlation of **COR2** respectively. 
The correlations are drawn form a uniform distribution with boundaries set by the modeler.
Based on the correlations a squared constant is computed. These squared constants approaches to equal one, the smaller the respective correlation is. 
With these two parameters the first version of the **RES_CONS_PAT** can be build. For that, the random values in each column from **RES_CONS_PATpre** are multiplied with the squared constant and then added up with a product of the respective correlation and the **BASE.** Subsequently, the higher the correlation, the greater is the influence of the **BASE** on the peticular element and the smaller the random value from **RES_CONS_PAT_pre.** Again, this process is done two times: for the 1-DISP1 resurces and again for the remaining resources (columns). Resulting is a resource consumption matrix in the desired size satisfying the correlation constraints.

The implementation of this calculation in R is shown in the next window. 

```{r RES_CONS_PAT 2, eval = FALSE}
## ====================== STEP 1.a CORRELATION ========================= 
    # Products and Resource are transposed in constrast to Anand 2019 but there is no issue in the model
    # Rows Products Colums Resources
    
    # Correlation of the top [DISP1] resources
    COR1 <- runif(1, -0.2, 0.8);
    
    sqrt_const_1 <- sqrt(1 - (COR1 * COR1))
    
    # Correlation of the remaining resources
    COR2 <- runif(1, -0.2, 0.8);
    
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
```

#### 5.2.3. Includcing the densitiy 

The already computed density is now included in the resource consumption matrix. Since the density is a value between 0 and 1, a matrix of the size of the resource consumption matrix is generated consisting of randomized values with the same boundaries (called **res_cons_pat_b_pre**). Every element in this matrix is then compared with the density. If the element is larger then the density, this element is set to 0, otherwise to 1. For instance, if the density is 0.6 and one element of **res_const_pat_b_pre** is 0.7 and another is 0.4. The latter element is set to 1 while the first one is set to 0. This results in a matrix consisting of zeros and ones. By multiplying this matrix with the first version of the **RES_CONS_PAT**, zeros are punched into it and creates a **RES_CONS_PAT** with the desired density. 

```{r Calculating the density into the resource consumption matrix}
 ## ====================== STEP 1.b DENSITY =========================
    
    res_cons_pat_b_pre = runif(FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO*FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)
    ## 1/0 DENSITY
    res_cons_part_b <- matrix(ifelse(res_cons_pat_b_pre > FIRM$PRODUCTION_ENVIRONMENT$DENS, 0,1),
                                FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES)
    
    RES_CONS_PAT = res_cons_part_b * RES_CONS_PAT
    
    FIRM$PRODUCTION_ENVIRONMENT$RES_CONS_PAT = RES_CONS_PAT
```

#### 5.2.4. Ceiling and Scaling

The **BASE** is now added to the **RES_CONS_PAT** making it the first column and therefore first resource in this matrix. The last step is to multiply the absoule values of each element with 10 and ceiling it. The elements now describe the resource consumption for the production of exactly one unit of every product. By multiplying it with the generated demand vector the total resource consumption matrix is computed displying the resources need to produce the desired demand quantity of each product (**RES_CONS_PAT_TOTAL**). 

The column sums from **RES_CONS_PAT_TOTAL** are therefore the the total amount of units of every resource needed to produce the demand quantity (**TCU**). With this information one can compute a relative resource consumption matrix containing the share of each resource that is needed to produce a product (**RES_CONS_PATp**).

```{r Ceiling and Scaling of the RES_CONS_PAT}
## ====================== STEP 1.c Ceiling and Scaling ============= 
    
    # take absolute value of X and Z and scale by 10 and round them
    # Anand et al. 2019
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS
    RES_CONS_PAT[,1] <- (BASE)
    RES_CONS_PAT <- ceiling(abs(RES_CONS_PAT) * 10)
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAND
    RES_CONS_PAT_TOTAL <- RES_CONS_PAT*FIRM$PRODUCTION_ENVIRONMENT$DEMAND
    ##CALCULATING TCU
    TCU <- colSums(RES_CONS_PAT_TOTAL)
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAMD / TRU (Currently like this in Anand et al. 2019)
    RES_CONS_PATp <- sweep((RES_CONS_PAT_TOTAL),2,TCU,"/") #Absolute matrix to relative matrix
```

#### 5.2.5. Exception Handler

To prevent, that some of the constraints are not met, the program now checks first if there is any resource never used in one of the products, second if there is one product withou any resource consumption and third, if the first resource (BASE resurce) contains any zeros. If one of the constraints is not satisfied the just generated matrix is discarded and build again. 

```{r Exception Handler, eval = FALSE}
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
  
```


#### 5.2.6. Including the check functions???

\newpage

### 5.3. Generating the resource costs (.gen_RC.R)

With the generated demand vector and the resoruce consumption matrix, information about the desired production quantity of each product and the consumption of the underlying reosurces are available. For calculating the actual costs of this production environment only the costs of the resources itself are missing. 

These are caluclated and stored in the **RCC** vector. As well as for the resource consumption matrix and the demand vector different ways exist to generate this environmental factor, depending on which assumptions are made. Here, the heuristic from Anand is used, to create a good replication of his model. 

Analogous to the resource consumption matrix the resource costs vector consists of two parts: The costs for the big resources (1:**DISP1**) and the small resources (**DISP1**+1:**NUMB_RES**). As already described in *5.2.2.*, the big resources account for a **DISP2** share of the total costs. First step is therefore to draw **DISP2** for a uniform distribution U[**DISP2_MIN**, *DIPS2_MAX*] (under the condition that **DISP2** is not set by the model in the input file). 

After that, boundaries for the minimum allowable costs (r_MIN) and maximum allowable costs (r_MAX) for the big resources are set. 
Noting that the sum of the small resources must be equal **(1-DISP2) * TC**. In addition, the costwise smallest of the big resources must be larger or equal to the costwise biggest of the small resources. Therefore the minimum allowable costs for the big resources is the average costs per small resource of **(1-DISP2)*TC** (see Step 1).

```{r Generating RCC Big Resources}
  if(RC_VAR == -1)
  {
    DISP2_MIN = 0.4
    DISP2_MAX = 0.7
    DISP2 = runif(1, DISP2_MIN, DISP2_MAX);  #DISP2 = RC_VAR
   # FIRM$COSTING_SYSTEM$RC_VAR = RC_VAR
  }
  
DISP1 = FIRM$PRODUCTION_ENVIRONMENT$DISP1
TC=FIRM$COSTING_SYSTEM$TC
NUMB_RES = FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES

# Step 1
r_MIN<-((1-DISP2)*TC)/(NUMB_RES-DISP1) 

#Step 2
r1_MAX<-(DISP2*TC)-((DISP1-1)*r_MIN)

# Step 3
r_MIN<-r_MIN+(r1_MAX-r_MIN)*0.025   #0.025?

## Step 4
#Initalize Values
RCC<-vector(mode="numeric")
r_MAX<-vector(mode="numeric")
temp1_ADD<-vector(mode="numeric", length = DISP1-1)
temp1_ADD[1]<-0


for (i in 1:(DISP1-1)) {
  
  r_MAX[i]<-(DISP2*TC-sum(temp1_ADD))-(DISP1-i)*r_MIN
  
  RCC[i]<-runif(1,min=r_MIN,max=r_MAX[i])
  temp1_ADD[i]<-RCC[i]
  
  
}

## The final element is computed to ensure that the total rescource cost is exactly DISP2*TC
RCC<-c(RCC,DISP2*TC-sum(temp1_ADD))

## Move the biggest resource to the front
largest_RC<-sort(RCC,decreasing = TRUE,index.return=TRUE)$ix[1]
RCC<-c(RCC[largest_RC],RCC[-largest_RC])
```



```{r INIT, eval = FALSE, include = FALSE}
 ## Calculating the estimated product costs
    FIRM$COSTING_SYSTEM$PCH =  apply((FIRM$COSTING_SYSTEM$ACP) * t(FIRM$COSTING_SYSTEM$ACT_CONS_PAT),2,sum) # CHECKED 2019/09/12
  
      ## ERROR MEASURES AFTER LABRO & VANHOUCKE 2007 
    EUCD = round(sqrt(sum((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)^2)),digits=2)
    MAPE = round(mean(abs(FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)/FIRM$COSTING_SYSTEM$PCB),digits=4)
    MSE = round(mean(((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)^2)),digits=2);
  
    
    
  #### ======== COLLECTING THE DATA FOR OUTPUT ==== ####
    preData = data.frame(o,nn,FIRM$COSTING_SYSTEM$CP,FIRM$COSTING_SYSTEM$RC_VAR, FIRM$COSTING_SYSTEM$NUMB_Error, FIRM$COSTING_SYSTEM$Error,
                         FIRM$PRODUCTION_ENVIRONMENT$DENS, FIRM$PRODUCTION_ENVIRONMENT$COR, FIRM$PRODUCTION_ENVIRONMENT$Q_VAR, FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,
                         FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES,EUCD,MAPE,MSE)
  
    #preData_p = .datalogging()
    colnames(preData) = c('o','nn','CP','RCC_VAR', 'NUMB_ME', 'NUMB_ME_AD','DENS', 'COR', 'Q_VAR', 
                       'NUMB_PRO', 'NUMB_RES' ,'EUCD','MAPE','MSE')  
    
    #stacking the data with each run
    DATA = rbind(DATA,preData)
    #DATA = rbind(preData,preData)
    
    #Print outputs;
    print(o)
    print(FIRM$COSTING_SYSTEM$CP)
    print((MAPE))
    
    o=o+1 #Counting for the total number of runs
  }
                  }
                }
              }  
            }  
          }
        }
      }
    }
  }
}
```


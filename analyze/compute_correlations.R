### compute correlation table

input <- read.csv("output/ProductCost_2020-02-18-2127.csv")
input$PE <- input$PE * 100

###### COMPUTING BIAS AND IMPRECISION #####
pre = group_by(input, input$PCb, input$DENS, input$Q_VAR, input$RCC_VAR, input$CP, input$NUMB_Error)

input_grouped =  (summarize(pre,n=n(), md=median(PE), mn=mean(PE),sd=sd(PE)*2)) ####
rm(pre)


LookUP = data.frame(input_grouped$`input$PCb`, input_grouped$`input$CP`, input_grouped$`input$NUMB_Error`, input_grouped$mn, input_grouped$sd) # Building data frame
colnames(LookUP) <- c("PCB", "cp", "ERROR" ,"BIAS","IMPRECISION") # rename columns. 
Base = data.frame(cbind(input$PRODUCT, input$PCb, input$PCh, input$NUMB_Error, 
                        input$RCC_VAR, input$Q_VAR, input$DENS, input$CP,
                        input$PE))
colnames(Base) <- c("P_NUMB","PC_B", "PC_H", "ERROR",
                    "RCC_VAR", "Q_VAR","DENS","CP", 
                    "PE") # rename columns
#Left JOIN : keep ll rows in X even if there is no match  // all.x true = join y into x ; 
input = merge(Base,LookUP , by.x = c("PC_B","CP","ERROR"), by.y =c("PCB","cp","ERROR"), all.x = TRUE)

rm(LookUP)
rm(Base)
rm(input_grouped)

##### DELETE UNNECESSARY PARAMETERS #####
input$P_NUMB <- NULL
input$CP<- NULL
input$ERROR<- NULL
input$Q_VAR<- NULL
input$DENS<- NULL
input$RCC_VAR<- NULL

input$OVERCOSTED = input$PE > 5
input$OVERCOSTED = as.numeric(input$OVERCOSTED)
input$UNDERCOSTED = input$PE < -5
input$UNDERCOSTED = as.numeric(input$UNDERCOSTED)
input$WITHINRANGE = input$PE > -5 & input$PE <= 5
input$WITHINRANGE = as.numeric(input$WITHINRANGE)
input$APE <- abs(input$PE)
input$EUCD <- input$PC_H - input$PC_B

##### CORRELATION ######

t=cor(input, method = c("pearson"))
t= round(t, digits=2) 
t


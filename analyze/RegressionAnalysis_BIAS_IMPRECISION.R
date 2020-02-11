#install.packages('Metrics')
library(lsr)
library(dplyr)
library(QuantPsyc)

library(car)
library(heplots)
library(Metrics)


input <- read.csv("C:/Users/kaigm/OneDrive/00 Paperprojects/01 PRODUCT COST _ WORKING PAPER/16 SUBMISSION/01 EXPERIMENTS/03 Regression/ProductCost_2020-02-10-1300.csv")
input <- read.csv("output/ProductCost_2020-02-11-1709.csv")
summary(input)
#### DATA WRANGLING AND SETTING ####  #####
input$PE <- input$PE * 100

####GROUPING THE TOTAL DATASET (PIVOT) AND PUT THE ESTIMATE IN IT  ####  ##########

pre = group_by(input, input$PCb,
               input$DENS, input$Q_VAR, input$RCC_VAR,
              input$CP,
              input$Q,
              input$Error, input$NUMB_Error)
              #input$VARSIZE_b_p_u, input$VARSIZE_b_p_T, input$VARSIZE_b_T, input$VARSIZE_b, ) # Grouping like Pivot tables. 
input_grouped = (summarize(pre,n=n(), md=median(PE), mn=mean(PE),sd=(sd(PE)*1.96), var=var(PE),mse=mean(((PCb-PCh)/PCb*100)^2)))#What is abot --- 
# var=var(DELTA),mse=mean(((PC_B-PC_H)/PC_B)^2
rm(pre)
gc()


#### REGRESSION WITH input_grouped #####
input_grouped
input_grouped$ABSBIAS = abs(input_grouped$mn)
input_grouped$IMPRECISION = input_grouped$sd


fit <- lm(input$APE ~ (input$DENS + input$Q_VAR  + input$CP + 
                       input$Q + input$PCb + input$Error + input$NUMB_Error))


fit <- lm(input_grouped$IMPRECISION ~ (input_grouped$`input$DENS` + input_grouped$`input$Q_VAR`  +input_grouped$`input$CP` + 
                                     input_grouped$`input$Q` + input_grouped$`input$PCb`+
                                     input_grouped$`input$Error` + input_grouped$`input$NUMB_Error`))



fit <- lm(input_grouped$ABSBIAS ~ (input_grouped$`input$DENS` + input_grouped$`input$Q_VAR`  +input_grouped$`input$CP` + 
                                     input_grouped$`input$Q` + input_grouped$`input$PCb`+
                                     input_grouped$`input$Error` + input_grouped$`input$NUMB_Error`))



#+ input_grouped$CPH_FACTOR

summary(fit)
lm.beta <- lm.beta(fit)
lm.beta <- round(lm.beta, digits=2)
print(lm.beta)
vif(fit)


aov_all <- aov(input_grouped$ABSBIAS ~(input_grouped$`input$DENS` + input_grouped$`input$Q_VAR`  +input_grouped$`input$CP` + 
                                             input_grouped$`input$Q` + input_grouped$`input$PCb`+
                                             input_grouped$`input$Error` + input_grouped$`input$NUMB_Error` ))



aov_all <- aov(input$APE ~(input$DENS + input$Q_VAR  + input$CP + 
                                             input$Q + input$PCb + input$Error + input$NUMB_Error))




drop1(aov_all,~.,test="F") # type III SS and F Test
summary(aov_all)

eta = etasq(aov_all)
eta <- round(eta, digits=3)
print(eta)






#### CORRELATION ####
t=cor(input, method = c("pearson"))
t= round(t, digits=2) 




#install.packages('Metrics')

library(lsr)
library(dplyr)
library(QuantPsyc)
library(car)
library(heplots)
library(Metrics)
library(ggplot2)


#input <- read.csv("C:/Users/kaigm/OneDrive/00 Paperprojects/01 PRODUCT COST _ WORKING PAPER/16 SUBMISSION/01 EXPERIMENTS/03 Regression/ProductCost_2020-02-10-1300.csv")
input <- read.csv("output/ProductCost_2020-02-19-1407.csv")


#### DATA WRANGLING AND SETTING ####
input$PE <- input$PE * 100

####GROUPING THE TOTAL DATASET (PIVOT) AND PUT THE ESTIMATE IN IT  ####  ##########
pre = group_by(input, input$PCb,
              input$DENS,
              input$CP,
              input$Q,
              input$Q_VAR,
              input$DISP1,
              input$Error, input$NUMB_Error)
              #input$VARSIZE_b_p_u, input$VARSIZE_b_p_T, input$VARSIZE_b_T, input$VARSIZE_b, ) # Grouping like Pivot tables. 
input_grouped = (summarize(pre,n=n(), md=median(PE), mn=mean(PE),sd=(sd(PE)*1.96), var=var(PE),mse=mean(((PCb-PCh)/PCb*100)^2)))#What is abot --- 
# var=var(DELTA),mse=mean(((PC_B-PC_H)/PC_B)^2
summary(input_grouped)


rm(pre)
gc()


# Check the number of different settings; 
input_grouped$n <- as.factor(input_grouped$n)
summary(input_grouped$n)




#### REGRESSION WITH input_grouped #####
input_grouped$ABSBIAS = abs(input_grouped$mn)
input_grouped$IMPRECISION = input_grouped$sd
input_grouped$PCbUnit = input_grouped$`input$PCb`/input_grouped$`input$Q`

fit <- lm(input$APE ~ (input$Q + input$PCb + input$CP + input$Error + input$NUMB_Error + input$DENS))


fit <- lm(input_grouped$IMPRECISION ~ (input_grouped$`input$DENS`  + input_grouped$`input$DISP1` +
                                         input_grouped$`input$CP` + 
                                     input_grouped$`input$Q`+
                                     input_grouped$`input$Error` + input_grouped$`input$NUMB_Error`))



fit <- lm(input_grouped$ABSBIAS ~ (input_grouped$`input$DENS` +input_grouped$`input$CP` + 
                                   input_grouped$`input$Q`+
                                   input_grouped$`input$Error` + input_grouped$`input$NUMB_Error`))

#+ input_grouped$CPH_FACTOR

summary(fit)
lm.beta <- lm.beta(fit)
lm.beta <- round(lm.beta, digits=2)
print(lm.beta)
vif(fit)


aov_all <- aov(input_grouped$ABSBIAS ~(input_grouped$`input$DENS` + input_grouped$`input$Q_VAR` + input_grouped$`input$DISP1` +
                                         input_grouped$`input$CP` + 
                                         input_grouped$`input$PCb`+
                                         input_grouped$`input$Error` + input_grouped$`input$NUMB_Error` ))



aov_all <- aov(input$APE ~ (input$Q + input$PCb + input$CP + input$Error + input$NUMB_Error + input$DENS))


drop1(aov_all,~.,test="F") # type III SS and F Test
summary(aov_all)

eta = etasq(aov_all)
eta <- round(eta, digits=3)
print(eta)









input_grouped$PCB_F <- cut(input_grouped$`input$PCb`, quantile(input_grouped$`input$PCb`,(0:4)/4));
input_grouped$Q_F <- cut(input_grouped$`input$Q`, quantile(input_grouped$`input$Q`,(0:4)/4));


interaction.plot(input_grouped$Q_F, input_grouped$`input$CP`, input_grouped$IMPRECISION)
interaction.plot(input_grouped$`input$CP`, input_grouped$`input$NUMB_Error`, input_grouped$IMPRECISION)

#### CORRELATION ####



t=cor(input_grouped, method = c("spearman"))
t= round(t, digits=2) 
t







##### SONSTIGES #####
summary(input_grouped)
input_grouped = subset(input_grouped, input_grouped$`input$CP`==35)

zplot2 <- ggplot(input_grouped, aes(x=input_grouped$PCbUnit, y=input_grouped$IMPRECISION, fill=input_grouped$`input$CP`)) +
  geom_point(size=2, shape=23)
zplot2


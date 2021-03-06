##### COMPUTE PROBABILITIES
library(dplyr)


input <- read.csv("output/ProductCost_2020-02-18-1244.csv")
input = subset(input, input$CP==25)
input = subset(input, input$DENS==0.5)
input$PRODUCT <- as.factor(DATAa$PRODUCT)
input$PE <- input$PE * 100






###### Calculating +-5% after Anderson for botch datasets ####
input$OVERCOSTED = input$PE > 5
input$OVERCOSTED = as.numeric(input$OVERCOSTED)
input$UNDERCOSTED = input$PE < -5
input$UNDERCOSTED = as.numeric(input$UNDERCOSTED)
input$WITHINRANGE = input$PE > -5 & input$PE <= 5
input$WITHINRANGE = as.numeric(input$WITHINRANGE)

###### COUNTING OVER AND UNDERCOSTING #########
#ABC
#45,000   Used for the Table in this section

input = group_by(input, input$PRODUCT)
summarize(input,n=n())

tapply(input$OVERCOSTED,  list(input$PRODUCT), sum)
tapply(input$UNDERCOSTED,  list(input$PRODUCT), sum)

tapply(input$OVERCOSTED,  list(input$PRODUCT), sum)*100/1800
tapply(input$UNDERCOSTED, list(input$PRODUCT), sum)*100/1800
tapply(input$WITHINRANGE, list(input$PRODUCT), sum)*100/1800

###### Computing Bias +- Imprecision

error_bias = (tapply(input$PE,  list(input$PRODUCT), mean))
error_imprecision = (tapply(input$PE,  list(input$PRODUCT), sd))*2


###### Computing PCh
PCh_bias = (tapply(input$PCh,  list(input$PRODUCT), mean))
PCh_imprecision = (tapply(input$PCh,  list(input$PRODUCT), sd))*2


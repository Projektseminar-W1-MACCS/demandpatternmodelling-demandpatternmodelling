##### COMPUTE PROBABILITIES
library(dplyr)


input <- read.csv("output/ProductCost_2020-02-18-2127.csv")
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

output.overcost = tapply(input$OVERCOSTED,  list(input$PRODUCT), sum)*100/1800
output.undercost = tapply(input$UNDERCOSTED, list(input$PRODUCT), sum)*100/1800
output.withinrange = tapply(input$WITHINRANGE, list(input$PRODUCT), sum)*100/1800

###### Computing Bias +- Imprecision

output.error_bias = (tapply(input$PE,  list(input$PRODUCT), mean))
output.error_imprecision = (tapply(input$PE,  list(input$PRODUCT), sd))*2


###### Computing PCh
output.PCh_bias = (tapply(input$PCh,  list(input$PRODUCT), mean))
output.PCh_imprecision = (tapply(input$PCh,  list(input$PRODUCT), sd))*2


output <- cbind(output.overcost, output.undercost, output.withinrange, 
                output.error_bias, output.error_imprecision, 
                output.PCh_bias,output.PCh_imprecision)

library(plyr)
library(dplyr)

require(utils)
library(zoo)
library(mgcv)
library(mlogit)

finaldata <- read.csv('finaldata.csv')

finaldata[,c('LOYALTY.0',
             'AVGRATE.0',
             'DISPLAY.0',
             'PRICEREDUCTION.0',
             'FEATURE.0')] <- NULL

finaldata <- finaldata[finaldata$choice != 0,]

finaldata[finaldata$choice == 3,]$choice <- 0
finaldata[finaldata$choice == 4,]$choice <- 3
finaldata[finaldata$choice == 5,]$choice <- 4
finaldata[finaldata$choice == 6,]$choice <- 5

mnlmodel <- gam(list(choice~Combined.Pre.Tax.Income.of.HH + 
                       MALE_SMOKE + FEM_SMOKE + WEEK + 
                       LOYALTY.10158 + AVGRATE.10158 +  
                       DISPLAY.10158 + FEATURE.10158, 
                     ~Combined.Pre.Tax.Income.of.HH + MALE_SMOKE + 
                       FEM_SMOKE + WEEK +  LOYALTY.33200 + AVGRATE.33200 + 
                       DISPLAY.33200 + FEATURE.33200,
                     ~Combined.Pre.Tax.Income.of.HH + MALE_SMOKE + 
                       FEM_SMOKE + WEEK + LOYALTY.37000 + 
                       AVGRATE.37000 + DISPLAY.37000 + 
                       FEATURE.37000, 
                     ~Combined.Pre.Tax.Income.of.HH + 
                       MALE_SMOKE + FEM_SMOKE + WEEK +
                       LOYALTY.53100 + AVGRATE.53100 + 
                       DISPLAY.53100 + FEATURE.53100, 
                     ~Combined.Pre.Tax.Income.of.HH + MALE_SMOKE + 
                       FEM_SMOKE +  WEEK + LOYALTY.77326 + 
                       AVGRATE.77326 + DISPLAY.77326 + 
                       FEATURE.77326), data=finaldata, family=multinom(K=5))

gamweekloyaltypricemodel <- gam(list(choice~Combined.Pre.Tax.Income.of.HH + 
                       MALE_SMOKE + FEM_SMOKE + s(WEEK) + 
                       s(LOYALTY.10158) + s(AVGRATE.10158) +  
                       DISPLAY.10158 + FEATURE.10158, 
                     ~Combined.Pre.Tax.Income.of.HH + MALE_SMOKE + 
                       FEM_SMOKE + s(WEEK) +  s(LOYALTY.33200) + s(AVGRATE.33200) + 
                       DISPLAY.33200 + FEATURE.33200,
                     ~Combined.Pre.Tax.Income.of.HH + MALE_SMOKE + 
                       FEM_SMOKE + s(WEEK) + s(LOYALTY.37000) + 
                       s(AVGRATE.37000) + DISPLAY.37000 + 
                       FEATURE.37000, 
                     ~Combined.Pre.Tax.Income.of.HH + 
                       MALE_SMOKE + FEM_SMOKE + s(WEEK) +
                       s(LOYALTY.53100) + s(AVGRATE.53100) + 
                       DISPLAY.53100 + FEATURE.53100, 
                     ~Combined.Pre.Tax.Income.of.HH + MALE_SMOKE + 
                       FEM_SMOKE +  s(WEEK) + s(LOYALTY.77326) + 
                       s(AVGRATE.77326) + DISPLAY.77326 + 
                       FEATURE.77326), data=finaldata, family=multinom(K=5))

newData <- finaldata[,c('Combined.Pre.Tax.Income.of.HH', 
                       'MALE_SMOKE',
                       'FEM_SMOKE',
                       'WEEK',
                       'LOYALTY.10158',
                       'AVGRATE.10158',  
                       'DISPLAY.10158',
                       'FEATURE.10158',
                       'LOYALTY.33200',
                       'AVGRATE.33200',
                       'DISPLAY.33200',
                       'FEATURE.33200',
                       'LOYALTY.37000', 
                       'AVGRATE.37000',   
                       'DISPLAY.37000', 
                       'FEATURE.37000',
                       'LOYALTY.53100',
                       'AVGRATE.53100',
                       'DISPLAY.53100',
                       'FEATURE.53100',
                       'LOYALTY.77326',
                       'AVGRATE.77326',
                       'DISPLAY.77326', 
                       'FEATURE.77326'
                       )]
mnlPredicted <- predict(mnlmodel, newData, type="response")
gamPredicted <- predict(gamweekloyaltypricemodel, newData, type="response")

colnames(mnlPredicted) <- c('0', '1', '2', '3', '4', '5')
colnames(gamPredicted) <- c('0', '1', '2', '3', '4', '5')

mnlPredicted <- as.integer(apply(mnlPredicted,1,function(x) names(which(x==max(x)))[1]))
gamPredicted <- as.integer(apply(gamPredicted,1,function(x) names(which(x==max(x)))[1]))

mnlmisclassification <- mean(as.character(mnlPredicted) != as.character(finaldata$choice))
gammisclassification <- mean(as.character(gamPredicted) != as.character(finaldata$choice))

mlogitdata <- finaldata[,c(3, 10:42, 44)]

mlogitdata <- mlogit.data(mlogitdata, varying = c(5:34), shape = "wide", choice = "choiceLabel")
mlogitmodel <- mlogit(choiceLabel ~ 1 | Combined.Pre.Tax.Income.of.HH + MALE_SMOKE + FEM_SMOKE  + WEEK | AVGRATE + DISPLAY  + FEATURE + LOYALTY, mlogitdata, reflevel = '35000')

mlogitPredicted <- predict(mlogitmodel, newdata=mlogitdata)
colnames(mlogitPredicted) <- c('35000', '10158', '33200', '37000', '53100', '77326')
mlogitPredicted <- as.integer(apply(mlogitPredicted,1,function(x) names(which(x==max(x)))[1]))

mlogitmisclassification <- mean(as.character(mlogitPredicted) != as.character(finaldata$choiceLabel))

gamweekloyaltypriceImodel <- gam(list(choice~Combined.Pre.Tax.Income.of.HH + 
                                       MALE_SMOKE + FEM_SMOKE + s(WEEK) + 
                                       s(LOYALTY.10158) + s(AVGRATE.10158) + te(AVGRATE.10158, WEEK, k=8) + te(LOYALTY.10158, AVGRATE.10158, k=8) + 
                                       DISPLAY.10158 + s(AVGRATE.10158, by=FEATURE.10158), 
                                     ~Combined.Pre.Tax.Income.of.HH + MALE_SMOKE + 
                                       FEM_SMOKE + s(WEEK) +  s(LOYALTY.33200) + s(AVGRATE.33200) + 
                                       te(AVGRATE.33200, WEEK, k=8) +  te(AVGRATE.33200, LOYALTY.33200, k=8) +
                                       DISPLAY.33200 + s(AVGRATE.33200, by=FEATURE.33200),
                                     ~Combined.Pre.Tax.Income.of.HH + MALE_SMOKE + 
                                       FEM_SMOKE + s(WEEK) + s(LOYALTY.37000) + 
                                       s(AVGRATE.37000) + DISPLAY.37000 + te(AVGRATE.37000, WEEK, k=8) +
                                       te(AVGRATE.37000, LOYALTY.37000, k=8) + s(AVGRATE.37000, by=FEATURE.37000), 
                                     ~Combined.Pre.Tax.Income.of.HH + 
                                       MALE_SMOKE + FEM_SMOKE + s(WEEK) +
                                       s(LOYALTY.53100) + s(AVGRATE.53100) + te(AVGRATE.53100, WEEK, k=8) +
                                       DISPLAY.53100 + te(AVGRATE.53100, LOYALTY.53100, k=8) + s(AVGRATE.53100, by=FEATURE.53100), 
                                     ~Combined.Pre.Tax.Income.of.HH + MALE_SMOKE + 
                                       FEM_SMOKE +  s(WEEK) + s(LOYALTY.77326) + 
                                       s(AVGRATE.77326) + DISPLAY.77326 + te(LOYALTY.77326, AVGRATE.77326, k=8) +
                                       te(WEEK, AVGRATE.77326, k=8) + s(AVGRATE.77326, by=FEATURE.77326)), data=finaldata, family=multinom(K=5))
summary(mnlmodel)
plot(gamweekloyaltypricemodel)

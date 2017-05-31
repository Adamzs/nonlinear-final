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
summary(mnlmodel)
summary(gamWeekLoyaltyPriceDplyModel)
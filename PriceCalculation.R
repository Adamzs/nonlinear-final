library(plyr)
library(dplyr)

require(utils)
library(zoo)


trans.groc = read.table("data/toothpa_groc_1427_1478", sep="", header = TRUE, na.strings="", stringsAsFactors = F, colClasses = c(rep("character",2),rep("numeric", 6), "character", "numeric", "numeric"))
trans.groc<-combineupc(trans.groc)
trans.groc<-trimspaces(trans.groc)

trans.drug = read.table("data/toothpa_drug_1427_1478", sep="", header = TRUE, na.strings="", stringsAsFactors = F, colClasses = c(rep("character",2),rep("numeric", 6), "character", "numeric", "numeric"))
trans.drug<-combineupc(trans.drug)
trans.drug<-trimspaces(trans.drug)

trans.all<-rbind(trans.groc, trans.drug)
tpriceDf <- trans.all %>% group_by(WEEK, VEND) %>% 
  dplyr::summarise(AVG_RATE = mean(DOLLARS/UNITS))

priceDf <- expand.grid(WEEK=c(min(tpriceDf$WEEK) : max(tpriceDf$WEEK)), VEND = unique(tpriceDf$VEND))

priceDf <- merge(x=priceDf, y=tpriceDf, by=c("WEEK", "VEND"), all.x=TRUE)

priceDf <- priceDf[order(priceDf$VEND, priceDf$WEEK),]
# 
# impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
# imputedPrice <- ddply(priceDf, ~ VEND, transform, AVG_RATE = impute.mean(AVG_RATE))

for(i in unique(priceDf$VEND)){
  if(is.na(priceDf[priceDf$WEEK == 1427 &
             priceDf$VEND == i,]$AVG_RATE)){
    tdf <- priceDf[priceDf$VEND == i &
                     !is.na(priceDf$AVG_RATE),]
    priceDf[priceDf$WEEK == 1427 &
              priceDf$VEND == i,]$AVG_RATE <- tdf[order(tdf$VEND),]$AVG_RATE[1]
  }
}

priceDf <- priceDf %>%
  group_by(VEND) %>% 
  mutate(AVG_RATE = na.locf(AVG_RATE, na.rm = T))



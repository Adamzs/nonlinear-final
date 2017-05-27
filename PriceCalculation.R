library(dplyr)
library(plyr)
require(utils)

trans.groc = read.table("data/toothpa_groc_1427_1478", sep="", header = TRUE, na.strings="", stringsAsFactors = F, colClasses = c(rep("character",2),rep("numeric", 6), "character", "numeric", "numeric"))
trans.groc<-combineupc(trans.groc)
trans.groc<-trimspaces(trans.groc)

trans.drug = read.table("data/toothpa_drug_1427_1478", sep="", header = TRUE, na.strings="", stringsAsFactors = F, colClasses = c(rep("character",2),rep("numeric", 6), "character", "numeric", "numeric"))
trans.drug<-combineupc(trans.drug)
trans.drug<-trimspaces(trans.drug)

trans.all<-rbind(trans.groc, trans.drug)

tpriceDf <- group_by(trans.all, WEEK, VEND)
tpriceDf <- summarize(tpriceDf, AVG_RATE = mean(DOLLARS/UNITS))
tpriceDf <- tpriceDf[tpriceDf$VEND %in% prod.attr[prod.attr$PRODUCT.TYPE == 'TOOTHPASTE',]$VEND,]

priceDf <- expand.grid(WEEK=c(min(tpriceDf$WEEK) : max(tpriceDf$WEEK)), VEND = unique(tpriceDf$VEND))

priceDf <- merge(x=priceDf, y=tpriceDf, by=c("WEEK", "VEND"), all.x=TRUE)

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
imputedPrice <- ddply(priceDf, ~ VEND, transform, AVG_RATE = impute.mean(AVG_RATE))

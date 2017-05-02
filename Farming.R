library(xlsx)
library(dplyr)
setwd("D:/.users/Sanya/odesk/_Fouad/Kernel")
SFO <- read.xlsx("Farming.xlsx", sheetIndex = 1)
SF <- subset(SFO, SFO$Financial.year != 2017)
SF$Financial.year <- as.factor(SF$Financial.year)
SF$hedge[is.na(SF$hedge)] <- 0
SF$EBITDA..mio <- SF$EBITDA..mio + SF$hedge

#annual <- summarise(group_by(SFO_known, Financial.year), sum(Bulk.oil.sales..mmt))
#split(SFO_known$Bulk.oil.sales..mmt, SFO_known$Financial.year) %>%
#mutate(SFO_known, share = SFO_known$Bulk.oil.sales..mmt-annual[Financial.year==SFO_known$Financial.year, 2])
#SFO_known$Bulk.oil.sales..mmt-[Financial.year==SFO_known$Financial.year, 3]
#SF <- mutate(SF, share = 0)

#Export sales, mmt
#SF$share <- tapply(SF$Bulk.oil.sales..mmt, SF$Financial.year, sum)
#SF$share <- SF$Bulk.oil.sales..mmt/SF$share
#boxplot(SF$share ~ SF$quarter)
#summarise(group_by(SF, quarter), mean(share))/colMeans(summarise(group_by(SF, quarter), mean(share)))[2]/4
#summarise(group_by(SF, quarter), sd(share))/colMeans(summarise(group_by(SF, quarter), mean(share)))[2]/4

#Export EBITDA, mio
SF$share <- tapply(SF$EBITDA..mio, SF$Financial.year, sum)
SF$share <- SF$EBITDA..mio/SF$share
boxplot(SF$share ~ SF$quarter, title("Share of quarterly EBITDA in annual EBITDA", xlab = "Quarter", ylab = "Share", sub = "for financial years 2013-2016"))
summarise(group_by(SF, quarter), mean(share))/colMeans(summarise(group_by(SF, quarter), mean(share)))[2]/4
summarise(group_by(SF, quarter), sd(share))/colMeans(summarise(group_by(SF, quarter), mean(share)))[2]/4

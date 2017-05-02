library(xlsx)
library(dplyr)
library(ggplot2)
setwd("D:/.users/Sanya/odesk/_Fouad/Kernel")
SFO <- read.xlsx("grains prices.xlsx", sheetIndex = 1)
SF <- SFO[5:16,]
SF$year <- as.factor(SF$year)
SF$quarter <- as.factor(SF$quarter)

plot(SF$Kernel.grains ~ SF$Wheat.Chicago)
plot(SF$Kernel.grains ~ SF$Corn.Chicago)

#equotion <- lm(SF$Kernel.grains ~ SF$Wheat.Chicago + SF$Corn.Chicago)
#plot(equotion$residuals ~ SF$period, type= "S")
#plot(equotion$residuals ~ SF$quarter)
#summary(equotion)
spread <- SF$Wheat.Chicago - SF$Corn.Chicago
SP <- SF$Kernel.grains - SF$Wheat.Chicago
SF$FY <- c(rep(2014, 4),rep(2015, 4), rep(2016, 4))
SF$FY <- as.factor(SF$FY)
plot(SP ~ SF$FY)


SF$intrayear <- tapply(SP, SF$FY, mean)
SF$intrayear <- SP - SF$intrayear
plot(SF$intrayear ~ SF$quarter)



eq1 <- lm(SP ~ spread)
summary(eq1)
summary(lm(SF$Kernel.grains ~ SF$Wheat.Chicago + spread))

cor(SF$Kernel.grains, SF$Wheat.Chicago)
cor(SF$Kernel.grains, SF$Corn.Chicago)
summary(lm(SF$Kernel.grains~SF$Corn.Chicago))
summary(lm(SF$Kernel.grains~SF$Wheat.Chicago))
plot(SP~spread, col=SF$quarter)
plot(SP~SF$quarter)
plot(SP~SF$year)
plot(spread~SF$quarter)


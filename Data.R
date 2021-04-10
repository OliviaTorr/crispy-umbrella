

library(haven)
library(tidyverse)
library(ggplot2)
library(ivreg)

data <- read_dta("mortality.dta")


## Simple OLS regressions with and without control variables 

lmwithout <- lm(data$before67dead ~ data$dist65_ageATend4emp, data = data)
summary(lmwithout)

lmwith <- lm(data$before67dead ~ data$dist65_ageATend4emp + data$nutsATage50 + data$halfyearOFbirth + data$czeit1yATage50 + data$czeit2yATage50 + data$czeit5yATage50 + data$czeit10yATage50 + data$czeit25yATage50, data = data)
summary(lmwith)

##Plotting densities

plot1data <- data[which(data$Zd_during == 0), ]
plot1 <- density(plot1data$dist65_ageATend4emp)
plot(plot1)

plot2data <- data[which(data$Zd_during == 1), ]
plot2 <- density(plot2data$dist65_ageATend4emp)
plot(plot2)

###First stage regression

lm <- lm(data$dist65_ageATend4emp ~ I(data$Zd_during*data$halfyearOFbirth) + data$nutsATage50 + data$halfyearOFbirth +
           data$czeit1yATage50 + data$czeit2yATage50 + data$czeit5yATage50 + data$czeit10yATage50 + data$czeit25yATage50, data = data)
summary(lm)

##Second stage regression

IVwithcontrols <- ivreg(data$before67dead ~ data$dist65_ageATend4emp + data$nutsATage50 + data$halfyearOFbirth + data$czeit1yATage50 + data$czeit2yATage50 + data$czeit5yATage50 + data$czeit10yATage50 + data$czeit25yATage50| I(data$Zd_during*data$halfyearOFbirth) + data$nutsATage50 + data$halfyearOFbirth + data$czeit1yATage50 + data$czeit2yATage50 + data$czeit5yATage50 + data$czeit10yATage50 + data$czeit25yATage50, data = data)
summary(IVwithcontrols)

IVwithoutcontrols <- ivreg(data$before67dead ~ data$dist65_ageATend4emp | I(data$Zd_during*data$halfyearOFbirth), data = data)
summary(IVwithoutcontrols)





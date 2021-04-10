

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


---
title: "PS4"
author: "Sari Issa"
date: "8 4 2021"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

5.
a.
```{r}
mortality <- read_dta("mortality.dta")
#View(mortality)

str(mortality)
summary(mortality)
```
The outcome variable is a dummy variable which takes on the value 1 if someone dies before the age of 67, and 0 otherwise. As summary() shows, around 7.3% of people die before age 67. The endogenous variable, dist65ageATend4emp describes the years spent in early retirement. The variable varies has quite long tails, but there is large mass around the mode. The instrument, Zd_during denotes whether a worker was eligible to a program that granted access to early retirement. The summary() shows that a bit less than half of the dataset had access to early retirement.

b.
```{r}
m1<-lm(before67dead~dist65_ageATend4emp,data=mortality)

m2<-lm(before67dead~dist65_ageATend4emp+halfyearOFbirth+czeit1yATage50+czeit1yATage50+czeit2yATage50+czeit5yATage50+czeit10yATage50+czeit25yATage50+nutsATage50,data=mortality)
```


c.
```{r}
summary(m1)

summary(m2)

anova(m1,m2)
```


d.
People who choose to enter into retirement early are probably systematically different than their peers in a way which effects the outcome variable, death before age 67. I find it reasonable that many retire early precisely due to health complications. So, Beta_1 will probably be biased as early retirement is probably correlated with health and health is correlated with death before 67. Beta_1 is probably overestimating the true causal effect. This happens if early retirement correlates positively with bad health, and bad health correlates positively with death before 67, which seems reasonable.

e.
```{r}
(dist65_ageATend4emp~Zd_during,data=mortality)

```

f.
```{r}
m3<-lm(dist65_ageATend4emp~I(Zd_during*halfyearOFbirth)+halfyearOFbirth+czeit1yATage50+czeit1yATage50+czeit2yATage50+czeit5yATage50+czeit10yATage50+czeit25yATage50+nutsATage50,data=mortality)

summary(m3)
```


g.
```{r}
require(ivreg)
m4<-ivreg(before67dead~dist65_ageATend4emp+halfyearOFbirth+czeit1yATage50+czeit1yATage50+czeit2yATage50+czeit5yATage50+czeit10yATage50+czeit25yATage50+nutsATage50|halfyearOFbirth+czeit1yATage50+czeit1yATage50+czeit2yATage50+czeit5yATage50+czeit10yATage50+czeit25yATage50+nutsATage50+Zd_during,data=mortality)

summary(m4)
```






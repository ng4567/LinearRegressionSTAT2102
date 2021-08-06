setwd("/Users/nikhilgopal/Google Drive/Notability/Applied Linear Regression Analysis/final")

#Question 1


summary(model)
#Question 2
data2 <- read.csv("/Users/nikhilgopal/Google Drive/Notability/Applied Linear Regression Analysis/final/2.csv")

library(tidyverse)
library(RNHANES)
library(ggplot2)
library(pROC)

data2$race = as.factor(data2$race)
data2$treatment = as.factor(data2$treatment)
data2$gender = as.factor(data2$gender)
data2$ICU = as.factor(data2$ICU)


logit <- glm(ICU~age+comorbid+gender+race+treatment, data = data2, family = "binomial")

summary(logit)


#Question 3

data3 <- read.csv("/Users/nikhilgopal/Google Drive/Notability/Applied Linear Regression Analysis/final/dataset3.csv")
library(survival)
library(survminer)

#2007-2008 days = 2,555 - 3287

data3$yearinterest = ifelse(data3$default_date >= 2557 & data3$default_date <= 3287, 1, 0)
data3$iscensored = ifelse(is.na(data3$default_date), 1, 0)

survival_obj = Surv(time = data3$default_date, event = data3$iscensored)
kaplan_meir_fit <- survfit(survival_obj~data3$yearinterest+data3$credit_score)

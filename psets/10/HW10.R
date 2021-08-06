data <- read.csv("C:/Users/d/Google Drive/Notability/Applied Linear Regression Analysis/psets/10/10bdata.csv")

library(survival)
library(survminer)


#models for transplants existing

#change health and status to factors bc they are categorical data

data$health = as.factor(data$health)
data$transplant_factor = ifelse(!is.na(data$transplant), 1,0)
data$transplant_factor = as.factor(data$transplant_factor)
#1 = failure, 0 = censoring
time.ind.pairs = Surv(time=data$time, event = data$status)

#kaplan meir no transplant
kaplan_meir_fit <- survfit(time.ind.pairs ~ health, data = data)

#estimates of survival function for each level of health
summary(kaplan_meir_fit)
ggsurvplot(kaplan_meir_fit, data = data)

#Proportional hazards regression, with transplant data included (both transplants and no transplants)
phr.fit.transplants = coxph(time.ind.pairs ~ health + transplant_factor+transplant, data = data)


#models for transplants not existing
data_with_transplant_removed = subset(data, subset = is.na(data$transplant))
time.ind.pairs.nt <- Surv(time = data_with_transplant_removed$time, event = data_with_transplant_removed$status)

#kaplan meir fit no transplant
kmf.nt <- survfit(time.ind.pairs.nt ~ health, data = data_with_transplant_removed)

phr.fit.nt = coxph(time.ind.pairs.nt ~ health, data = data_with_transplant_removed)


#just the models

#transplants exist
summary(phr.fit)

#transplants dont exist
summary(phr.fit.nt)
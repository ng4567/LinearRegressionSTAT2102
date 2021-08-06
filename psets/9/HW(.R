setwd("/Users/nikhilgopal/Google Drive/Notability/Applied Linear Regression Analysis")
library(ISLR)

#Question 1
library(ISLR)
data <- read.csv("psets/9/CaseControl_Listeriosis_Repos.csv")

sex <- glm(outcome~sex, data = data)
AC <- glm(outcome~AC, data = data)
SES <- glm(outcome~SES, data = data)
Residence <- glm(outcome~residence, data = data)
GAS <- glm(outcome~GAS, data = data)
IDT <- glm(outcome~IDT, data = data)
meat <- glm(outcome~meat, data = data)
packmeat <- glm(outcome~packmeat, data = data)

multiple_regression <- glm(outcome~sex+AC+SES+residence+GAS+IDT+meat+packmeat, data = data)

interactions <- glm(outcome~sex*GAS+sex+AC+SES+residence+GAS+IDT+meat+packmeat, data = data)

#Question 2

data2 <- read.csv("/Users/nikhilgopal/Google Drive/Notability/Applied Linear Regression Analysis/psets/9/2.csv")


risk_of_mortality <- glm(deaths~population+smoking.status+age, data = data2)

setwd("C:/Users/d/Google Drive/Notability/Applied Linear Regression Analysis/psets/6")

data <- read.csv("week6.csv", header = FALSE)

names(data)[1] <- "Brain.Weight"
names(data)[2] <- "Body.Weight"
data[1,1] <- 3.385


#A
linear_model <- lm(data$Brain.Weight ~ data$Body.Weight)

#B
#variance is residual standard error^2
summary(linear_model)

#C
linear_model

#D
summary(linear_model)

#E
#plug into the model for this one

#F
#get the value from E and then subtract from 12.30 which is the body weight for brain weight of 2

#G
sterr <- 323.5

#pt function is t distribution
pvalue <- 2*pt(abs(sterr), df = 60, lower.tail = FALSE)

#H
#SSE = Residual Standard Error
SST = sum((data$Brain.Weight - mean(data$Brain.Weight))^2)

summary_model <- summary(linear_model)

SSE = summary_model$sigma^2 * 60
SSR = SST - SSE
R2 <- summary_model$r.squared

MSE = SSE/(62-2)
MSR = SSR/1

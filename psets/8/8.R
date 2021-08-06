data1 <- read.csv("/Users/nikhilgopal/Desktop/1.csv")

model <- lm(bp~rt, data = data1)

ttest <- t.test(data1$bp[data1$rt=="Placebo"], data1$bp[data1$rt=="Medication"])


#2

data2 <-read.csv("/Users/nikhilgopal/Desktop/2.csv")

modell <- lm(time~treatment_amount+order_of_runs, data = data2)

summary(modell)

#3

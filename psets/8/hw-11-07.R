setwd("/Users/nikhilgopal/Google Drive/Notability/Applied Linear Regression Analysis/psets")

data <- read.csv("insurance.csv")

#Question 1

lin_mod <- lm(charges~age+sex+bmi+children+smoker+region, data = data)

lin_mod_log <-lm(log(charges)~age+sex+bmi+children+smoker+region, data = data)

hist(x=log(data$charges))

#log is the best transformation

#Question 2

#Question 3
lm_age <- lm(charges~age, data = data)
summary(lm_age)

#f=131.2 on 1336 DF

lm_sex <- lm(charges~sex, data=data)
summary(lm_sex)
anova(lm_sex)

#p = 0.03615

lm_bmi <- lm(charges~bmi, data=data)
summary(lm_bmi)

#F = 54.71 on 1336 DF

lm_children <- lm(charges~children, data = data)
summary(lm_children)

#F = 6.206 on 1336 DF

lm_smoker <- lm(charges~smoker, data = data)
anova(lm_smoker)

#p is basically zero 2.2e^-16

lm_region <- lm(charges~region, data=data)
anova(lm_region)

#P = 0.03089


#Question 4
summary(lin_mod_log)

#Question 4.5
plot(x=data$charges,y=residuals(lin_mod_log), xlab = "Health insurance Charges (Dollars)", ylab = "Residuals of model using log on Y variable")

plot(residuals(lin_mod_log))

#Question 5



poly1 <- lm(log(charges)~poly(age,3, raw = TRUE)+sex+data$smoker:bmi+children+data$smoker+region, data = data)




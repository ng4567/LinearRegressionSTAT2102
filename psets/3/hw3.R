setwd("/Users/nikhilgopal/Google Drive/Notability/Applied Linear Regression Analysis")

#3.1

num_observations <- 15
mu_parameter <- 10

#X
var1 <- rnorm(num_observations, mean = mu_parameter, sd = 4)

epsilon <- rnorm(num_observations, mean = 0, sd =1)

#Y
var2 <- 9*var1 + epsilon

#Plot
plot(var1, var2)

#covariance
covariance <- cov(var1, var2)

#correlation
correlation <- cor(var1, var2)

'''
Our covariance was  about 169.399 and our correlation was about 0.999. This means that generally, 
our two variables are extremely related to each other. This we would expect since Y is a function of X.
'''

#3.2
abline(a = 90, b =0)

#sample average squared distances
sample_avg <- var(var2)*(num_observations-1)/num_observations

'''
The expected value of the squared differences was 325, while the sample average squared distances was 1433.
This means that we there was expected to be much less variability in the data than what was actually obeserved.
'''

#3.3
abline(a = 0, b = 9, col = "blue")

#sample avg squared distace of Y - E(X)
sample_avg_y_minus_EX <- sum(epsilon^2)/num_observations


'''
The sample average squared distance was 1.28 and the expected was 1. This makes a lot of sense because Y is defined
as a function of X plus some error which is normally distributed, which is why we see the sample distance being 
different than the expected distance.

'''
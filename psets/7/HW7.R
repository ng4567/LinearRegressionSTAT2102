setwd("C:/Users/d/Google Drive/Notability/Applied Linear Regression Analysis/psets/7")

data <- read.csv("df_for_comp_7.csv")


#Question 1
slide1data <- subset(data, subset = data$slide.no == "slide1")

model = lm(slide1data$y~slide1data$x)
summary(model)

#Question 2/3

slide2data <- subset(data, subset = data$slide.no == "slide2.3")

data$w = as.factor(data$w)

model = lm( + slide2data$w)
summary(model)

#test if they are the same height
same.heightmodel = lm(slide2data$y~slide2data$x)
anova(same.heightmodel, model)

#test if projectiles started on the ground
ground_height_model = lm(slide2data$y ~ slide2data$x -1)
anova(ground_height_model, model)


#acceleration due to gravity
slide2data$x.squared = slide2data$x^2

acceleration_model = lm(slide2data$y~slide2data$x.squared + slide1data$x + slide2data$w)
summary(acceleration_model)

#acceleration due to gravity is the beta coefficient 0.12

#Question 4

slide4data <- subset(data, subset = data$slide.no == "slide4")

plot(slide4data$x, slide4data$y, main = "Position vs time of car", xlab = "Time", ylab = "Position")

for(i in 1:100){
  if(i==1){
    slide4data$velocity[i] = 0
  }else{
    delta_x = slide4data$x[i]-slide4data$x[i-1]
    delta_y = slide4data$y[i]-slide4data$y[i-1]
    slide4data$velocity[i] = delta_y/delta_x
  }
}

#Question 5


slide5data <- subset(data, subset = data$slide.no == "slide5")




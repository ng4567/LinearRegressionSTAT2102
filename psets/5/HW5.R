setwd("C:/Users/d/Google Drive/Notability/Applied Linear Regression Analysis")

data <- read.csv("deadseaminerals.csv")




data$difference <- data$post_y - data$pre_x

linmod_trt_group <- lm(data$difference ~ as.factor(data$trt_Grp))
anova(linmod_trt_group)

p_value_f_dist <- pf(26.792, 2,57,lower.tail = FALSE)

#gel

linmod_gel <- lm(data$difference ~ as.factor(data$gel))
anova(linmod_gel)


p_value_f_dist_gel <- pf(26.792, 2,57,lower.tail = FALSE)

summary(linmod_gel)
p_value_f_dist_gel

#gelDS

linmod_gel_DS <- lm(data$difference ~ as.factor(data$gelDS))
anova(linmod_gel_DS)

p_value_f_dist_gelDS <- pf(26.792, 2,57,lower.tail = FALSE)

summary(linmod_gel_DS)
p_value_f_dist_gel_DS
library(tidyverse)
library(ggfortify)
library(splines)
library(xtable)
rm(list = ls())

data_wage = read_csv("wage_data.csv")

head(data_wage)

attach(data_wage)
summary(data_wage)

plot(age,wage)

lm_prelim = lm(wage ~ age+education ,data = data_wage)

lm_wage = lm(wage ~ age*education ,data = data_wage)

data_wage$resid_int_lin = resid(lm_wage)
ggplot(data_wage,aes(x = age,y = resid_int_lin)) + geom_point() +
  geom_smooth() + facet_grid(cols = vars(education))


autoplot(lm_wage, which = 1, ncol = 1, nrow = 1) +
  theme(aspect.ratio = 1)

autoplot(lm_wage, which = 2, ncol = 1, nrow = 1) +
  theme(aspect.ratio = 1)
# box plot for education



ggplot(data_wage) + 
  geom_boxplot(aes(x = education,y = wage,col = education)) + 
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(size = 12,angle = 270,
                                   hjust = 0,vjust = .5),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  theme(aspect.ratio = 1)


# scatter age and wage

ggplot(data_wage) + geom_point(aes(x = age,y = wage)) +
  theme_bw() +
  theme(aspect.ratio = 1)

ggplot(data_wage,aes(x = age,y = wage)) + geom_point() +
  geom_smooth() +
  theme_bw() +
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(size = 12,angle = 270,hjust = 0,vjust = .5),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  theme(aspect.ratio = 1)

ggplot(data_wage) + geom_point(aes(x = age,y = wage,col = education)) + 
  geom_smooth(aes(x = age,y = wage))


ggplot(data_wage) + geom_point(aes(x = age,y = wage,col = education)) + 
  geom_smooth(aes(x = age,y = wage,group = education, col = education),size = 1.5)

ggplot(data_wage,aes(x = age,y = wage,col = education)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(se= FALSE,method = "lm") +
  theme_bw() +
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(size = 12,angle = 270,hjust = 0,vjust = .5),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  theme(aspect.ratio = 1)



# scatter age with linear model fit


ggplot(data_wage,aes(x = age,y = wage,col = education)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(se= FALSE) +
  theme_bw() +
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(size = 12,angle = 270,hjust = 0,vjust = .5),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  theme(aspect.ratio = 1)


################# Step-wise functions models


lm_step = lm(wage ~ education +
               I(age >= 25 & age < 35) + 
               I(age >= 35 & age < 55) + 
               I(age >= 55 & age < 65) + 
               I(age >= 65) ,data = data_wage)
data_wage$pred_step = predict(lm_step)


lm_step2 = lm(wage ~ education*(I(age >= 25 & age < 35) + 
               I(age >= 35 & age < 55) + 
               I(age >= 55 & age < 65) + 
               I(age >= 65)) ,data = data_wage)
data_wage$pred_step2 = predict(lm_step2)

ggplot(data_wage[order(data_wage$age),]) + geom_point(aes(x = age,y = wage)) + 
  geom_line(aes(x = age,y = pred_step,group = education, col = education),size = 1.5)+
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(size = 12,angle = 270,hjust = 0,vjust = .5),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  theme(aspect.ratio = 1)

ggplot(data_wage[order(data_wage$age),]) + geom_point(aes(x = age,y = wage)) + 
  geom_line(aes(x = age,y = pred_step2,group = education, col = education),size = 1.5)+
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(size = 12,angle = 270,hjust = 0,vjust = .5),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  theme(aspect.ratio = 1)

summary(lm_step)

anova(lm_prelim,lm_step,lm_step2)



################# polynomial models

lm_poly = lm(wage ~ education +  poly(age,4),data = data_wage)
lm_poly_int = lm(wage ~ education*poly(age,4),data = data_wage)

data_wage$pred_poly = predict(lm_poly)
data_wage$pred_poly_int = predict(lm_poly_int)

ggplot(data_wage[order(data_wage$age),]) + geom_point(aes(x = age,y = wage)) + 
  geom_line(aes(x = age,y = pred_poly,group = education, col = education),size = 1.5)+
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(size = 12,angle = 270,hjust = 0,vjust = .5),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  theme(aspect.ratio = 1)

ggplot(data_wage[order(data_wage$age),]) + geom_point(aes(x = age,y = wage)) + 
  geom_line(aes(x = age,y = pred_poly_int,group = education, col = education),size = 1.5)+
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(size = 12,angle = 270,hjust = 0,vjust = .5),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  theme(aspect.ratio = 1)



################# spline models



lm_spline_linear = lm(wage ~ education + 
                        bs(age,knots = quantile(age,c(0.5)), degree = 1),
                      data = data_wage)

lm_spline_linear_int = lm(wage ~ education * 
                        bs(age,knots = quantile(age,c(0.5)), degree = 1),
                      data = data_wage)

data_wage$pred_linear_spline = predict(lm_spline_linear)
data_wage$pred_linear_spline_int = predict(lm_spline_linear_int)

ggplot(data_wage[order(data_wage$age),]) + geom_point(aes(x = age,y = wage)) + 
  geom_line(aes(x = age,y = pred_linear_spline,group = education, col = education),size = 1.5)+
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(size = 12,angle = 270,hjust = 0,vjust = .5),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  theme(aspect.ratio = 1)

ggplot(data_wage[order(data_wage$age),]) + geom_point(aes(x = age,y = wage)) + 
  geom_line(aes(x = age,y = pred_linear_spline_int,group = education, col = education),size = 1.5)+
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(size = 12,angle = 270,hjust = 0,vjust = .5),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  theme(aspect.ratio = 1)


lm_spline_quad = lm(wage ~ education + 
                        bs(age,knots = quantile(age,c(0.5)), degree = 2),
                      data = data_wage)

lm_spline_quad_int = lm(wage ~ education * 
                            bs(age,knots = quantile(age,c(0.5)), degree = 2),
                          data = data_wage)

data_wage$pred_quad_spline = predict(lm_spline_quad)
data_wage$pred_quad_spline_int = predict(lm_spline_quad_int)

ggplot(data_wage[order(data_wage$age),]) + geom_point(aes(x = age,y = wage)) + 
  geom_line(aes(x = age,y = pred_quad_spline,group = education, col = education),size = 1.5)+
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(size = 12,angle = 270,hjust = 0,vjust = .5),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  theme(aspect.ratio = 1)

ggplot(data_wage[order(data_wage$age),]) + geom_point(aes(x = age,y = wage)) + 
  geom_line(aes(x = age,y = pred_quad_spline_int,group = education, col = education),size = 1.5)+
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(size = 12,angle = 270,hjust = 0,vjust = .5),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  theme(aspect.ratio = 1)




lm_spline_cubic = lm(wage ~ education + 
                      bs(age,knots = quantile(age,c(0.5)), degree = 3),
                    data = data_wage)

lm_spline_cubic_int = lm(wage ~ education * 
                          bs(age,knots = quantile(age,c(0.5)), degree = 3),
                        data = data_wage)

data_wage$pred_cubic_spline = predict(lm_spline_cubic)
data_wage$pred_cubic_spline_int = predict(lm_spline_cubic_int)



ggplot(data_wage[order(data_wage$age),]) + geom_point(aes(x = age,y = wage)) + 
  geom_line(aes(x = age,y = pred_cubic_spline,group = education, col = education),size = 1.5)+
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(size = 12,angle = 270,hjust = 0,vjust = .5),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  theme(aspect.ratio = 1)

ggplot(data_wage[order(data_wage$age),]) + geom_point(aes(x = age,y = wage)) + 
  geom_line(aes(x = age,y = pred_cubic_spline_int,group = education, col = education),size = 1.5)+
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(size = 12,angle = 270,hjust = 0,vjust = .5),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  theme(aspect.ratio = 1)




AIC(lm_prelim)
AIC(lm_wage)

AIC(lm_step)
AIC(lm_step2)

AIC(lm_poly)
AIC(lm_poly_int)

AIC(lm_spline_linear)
AIC(lm_spline_linear_int)

AIC(lm_spline_quad)
AIC(lm_spline_quad_int)

AIC(lm_spline_cubic)
AIC(lm_spline_cubic_int)


ggplot(data_wage,aes(x = age,y = wage,group = education)) + 
  geom_point(alpha = 0.3) +
  stat_smooth(aes(col = education),method = "lm",
              formula = y ~ poly(x,4), size = 1) +
  theme_bw() +
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(size = 12,angle = 270,hjust = 0,vjust = .5),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  theme(aspect.ratio = 0.5)



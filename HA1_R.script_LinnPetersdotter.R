##########################################################
################### ASSIGNMENT I #########################
##########################################################

# getting started: setting working directory, importing data, loading packages

# set working directory
setwd("~/Documents/Lund Uni/StatisticsPSYP13") 

# import data home_sample_1
home_sample_1 = read_csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv")

# load preinstalled packages which might be needed 
library(lsr)
library(psych) 
library(tidyverse)
library(gsheet)
library(lm.beta)
library(gridExtra)
library(dplyr)
library(car)
library(olsrr)
library(Hmisc)
library(moments)

# source function for opening a new window whenever something is going to be plotted
source('GraphPlot.R')

##########################################################
################## Data preparation ######################
##########################################################

# First, I like to take a look at my data, getting familiar with the variables and have a look at possible missing values and if there is any abnormality visible at the first glance
summary(home_sample_1)
describe(home_sample_1)

# Secondly, as I fortunately could not find any missing values, I check for possible coding errors. Therefore I list all the possible min and max values and compared them with my sample description concerning lowest and highest values.

# The State Trait Anxiety Inventory - T: measures trait anxiety on a scale of 20 to 80 
# creating a new sample where I can delete the coding error 
home_sample_1.2 <- home_sample_1[!home_sample_1$STAI_trait <= 20, ] # one row was deleted

# Pain Cat is supposed to be on the range from 0-52 --> looks fine
# mindfulness can range from 0-6
# cortisol serum and saliva --> also fine
# household income showed one negative value which I cannot be sure of if it is a coding error or the person maybe actually had a negative income and was in debt. As I do not know about that I decide to remove the row from my sample. 
home_sample_1.2 <- home_sample_1.2[!home_sample_1.2$household_income <= 0, ]


##########################################################
################## creating model 1 ######################
##########################################################

# Creating the first model based on theory. (review of theory model1 pain = intercept + x1*age + x2*sex)
model1 <- lm(pain ~ age + sex, data = home_sample_1.2)	
model1

# visualizing age as a predictor for pain
plot1 = home_sample_1.2 %>% ggplot() + aes(x = age, y = pain) + geom_point() + geom_smooth(method = "lm")
plot(plot1)
# visualiying sex as a predictor for pain
plot1 = home_sample_1.2 %>% ggplot() + aes(x = sex, y = pain) + geom_point() + geom_smooth(method = "lm")
plot(plot1)


##########################################################
################ ASSUMPTION TESTING ######################
###############     for model 1     ######################

#checking assumptions for model 1 and run therefore required model diagnostics for a regression

# 1. influential outliers --> scatterplot / Cook's distance 
# 2. normality of residuals 
# 3. linearity 
# 4. homoscedasticity 
# 5. multicollinearity 

# 1. I look for influential outlier in my variables. 
x11() # opens my graphic window seperatly 
home_sample_1.2 %>% ggplot() + aes(x = age, y = pain) + geom_point()
x11()
home_sample_1.2 %>% ggplot() + aes(x = sex, y = pain) + geom_point()
# there is this one value among the female participants scoring extraordinary high on the pain scale (but still within the possible range)

# For identifying extreme cases with a probably high leverage, I plot the residual-leverage plot and by using Cook's distance. 
x11()
home_sample_1.2 %>% ggplot() + aes(x = age, y = pain) + geom_point() + geom_smooth(method = "lm")
x11()
model1 %>% plot(which = 5)
model1 %>% plot(which = 4)
# possibility that 88, 74 or 28 actually could have an influence on my model statistics
# rules of thumb for Cook's distance: no value > 1 (Cooks distance), due to the criterion 4/N (4/158 = 0.025) I would have to exclude 6-7 rows
# I would like to keep as many participants as possible in my dataset, so I first check the other assumptions without excluding any further outlier. If any assumption is hurt I will try it again with excluding the most suspicious value (No. 88 so far) and see what it does to my model diagnostics. 

# 2. normality of residuals 
model1 %>% plot(which = 2)
# case 88 is not fitting on or near to the line. I plot a histogram of the model's residuals:
residuals_model1 = enframe(residuals(model1))
residuals_model1 %>% ggplot() + aes(x = value) + geom_histogram()

# skewness and kurtosis
# rule of thumb: if skewness is close to zero, data is not normally distributed.rule of thumb for kurtosis: if it is close to 3, we assume normal distribution. 
skewness(residuals(model1))
# skewness is close to 0, which indicates normally distributed residuals
kurtosis(residuals(model1))
# kurtosis is close to 3 

# normality of residuals tests, Shapiro Wilk test
shapiro.test(residuals(model1))
# Shapiro Wilk's p-value indicates to keep the null-hypothesis (in that case: our residuals are normally distributed)

# 3. linearity between predictors and criterion must be linear 
x11()
model1 %>% residualPlots()
# even though the plots do not show a perfectly straight line, we might assume linearity not to be violated due to the non significant test statistics 

# 4. homoscedasticity 
x11()
model1 %>% plot(which = 3)
# Breusch Pagan Test and non constant variance score test (NCV) for testing homoscedasticity  
ols_test_breusch_pagan(model1, rhs = TRUE, multiple = TRUE)
ncvTest(model1)
# both tests indicate no heteroscedasticity 

# 5. Testing the assumption of multi-collinearity with vif 
model1 %>% vif()
# as both values are <3 no violation of multi-collinearity is assumend 

# as no violation of assumption for linear regression was found, I decide to not exclude any possible outlier since they also seem to not have a high leverage or influence on the regression 

######################################################################

# creating the second model 1.2
model1.2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = home_sample_1.2)	
model1.2

##########################################################
################ ASSUMPTION TESTING ######################
###############     for model 1.2     ####################

# 1. I look for influential outlier in the predicting variables of this model. 
x11() # opens my graphic window seperatly 
home_sample_1.2 %>% ggplot() + aes(x = STAI_trait, y = pain) + geom_point()
x11()
home_sample_1.2 %>% ggplot() + aes(x = pain_cat, y = pain) + geom_point()
x11()
home_sample_1.2 %>% ggplot() + aes(x = cortisol_serum, y = pain) + geom_point()
x11()
home_sample_1.2 %>% ggplot() + aes(x = cortisol_saliva, y = pain) + geom_point()
x11()
home_sample_1.2 %>% ggplot() + aes(x = mindfulness, y = pain) + geom_point()

# For identifying extreme cases with a probably high leverage, also Cook's distance is calculated. 
x11()
model1.2 %>% plot(which = 5)
model1.2 %>% plot(which = 4)
# possibility that72, 86 or 121 actually could have an influence on my model statistics


# 2. normality of residuals 
model1.2 %>% plot(which = 2)
# looking pretty normal on the or close to the line. I plot a histogram of the model's residuals:
residuals_model1.2 = enframe(residuals(model1.2))
residuals_model1.2 %>% ggplot() + aes(x = value) + geom_histogram()

# skew and kurtosis
# rule of thumb: if skewness is close to zero, data is not normally distributed.rule of thumb for kurtosis: if it is close to 3, we assume normal distribution. 
skewness(residuals(model1.2))
# skewness is close to 0, which indicates normally distributed residuals
kurtosis(residuals(model1.2))
# kurtosis is close to 3 


# normality of residuals tests, Shapiro Wilk test
shapiro.test(residuals(model1.2))
# Shapiro Wilk's p-value indicates to keep the null-hypothesis (in that case: our residuals are normally distributed)

# 3. linearity between predictors and criterion must be linear 
x11()
model1.2 %>% residualPlots()
# even though the plots show some curvature especially when looking at STAI_trait, the p-values of the test statistics indicate normality. However, the p-value for the variable STAI_trait is the closest to a significance level of .05

# 4. homoscedasticity 
x11()
model1.2 %>% plot(which = 3)
# Breusch Pagan Test and non constant variance score test (NCV) for testing homoscedasticity  
ols_test_breusch_pagan(model1.2, rhs = TRUE, multiple = TRUE)
ncvTest(model1.2)
# both tests indicate no heteroscedasticity 

# 5. Testing the assumption of multi-collinearity with vif 
model1.2 %>% vif()
# both cortisol_serum and cortisol_saliva show a (theoretically not surprising) high value (>3) for the VIF, indicating a high correlation between them

# the two cortisol values show a significantly high correlation
data_matrix <- data.matrix(home_sample_1.2)
cor(data_matrix)
cor.test(home_sample_1.2$cortisol_saliva, home_sample_1.2$cortisol_serum)

# I want to test which predictor works best for the model
# a) no cortisol included 
# b) both cortisol values included 
# c) just serum included
# d) just saliva included 

model_a <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness, data = home_sample_1.2)	
summary(model_a)

model_b <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = home_sample_1.2)	  
summary(model_b)

model_c <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = home_sample_1.2)	  
summary(model_c)

model_d
model_d <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_saliva + mindfulness, data = home_sample_1.2)	  
summary(model_d)

# adding just cortisol saliva results in a better overall model fit than adding either serum or both cortisol values.  
# even though, serum is the theoretical better backed-up variable, so I stick to serum
# creating a version of model 2 without cortisol_saliva -> model2.1
model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = home_sample_1.2)	  
x11()
par(mfrow=c(2, 2))
plot(model2)

summary(model2)

# after creating the final model (model2) I will run all the assumption tests again 

##########################################################
################ ASSUMPTION TESTING ######################
###############     for model 2     #####################

# 1. I look for influential outlier in the predicting variables of this model. As I plotted all variables of interest before, I will not do this again.  

# For identifying extreme cases with a probably high leverage,  Cook's distance is calculated. 
x11()
model2%>% plot(which = 5)
model2%>% plot(which = 4)
# possibility that 72, 86 or 121 actually could have an influence on my model statistics
# though my goal would be to stick to as many of the participants as possible as coding errors are already sortt out
# I will first have another look at model statistics before I decide on excluding any possible outlier

# 2. normality of residuals 
x11()
model2 %>% plot(which = 2)
# plot looks normal as residuals are on the or close to the line. I plot a histogram of the model's residuals:
residuals_model2 = enframe(residuals(model2.1))
residuals_model2 %>% ggplot() + aes(x = value) + geom_histogram()

# skew and kurtosis
skewness(residuals(model2))
# skewness is close to 0, which indicates normally distributed residuals
kurtosis(residuals(model2))
# kurtosis is close to 3 

# normality of residuals tests, Shapiro Wilk test
shapiro.test(residuals(model2))
# Shapiro Wilk's p-value indicates to keep the null-hypothesis (in that case: our residuals are normally distributed)

# 3. linearity between predictors and criterion must be linear 
x11()
model2 %>% residualPlots()
# Tukey test does not get significant for the model
# however: statistic shows significance for STAI_trait and pain_cat
# option to include higher order terms, but I decide against it as I do not want to increase the flexibility pf my model too much 

# 4. homoscedasticity 
x11()
model2 %>% plot(which = 3)
# Breusch Pagan Test and non constant variance score test (NCV) for testing homoscedasticity  
ols_test_breusch_pagan(model2, rhs = TRUE, multiple = TRUE)
ncvTest(model2)
# both tests indicate homoscedasticity 

# 5. Testing the assumption of multi-collinearity with vif 
model2 %>% vif()
# VIF values look unsuspicious concerning multi-collinearity 

##########################################################
################## Model comparison ######################
##########################################################

# confidence intervals for both models
confint(model1)
lm.beta(model1)

confint(model2.1)
lm.beta(model2.1)

model2
# equation for model_final 𝑌 = 𝑏0 + 𝑏1 ∗ X1 + 𝑏2 ∗ X2 +...+ bn * Xn,
# model 2  y(pain) = 2.85  + (-0.04)*x_age + (0,49) *sex + (-0.002)*STAI_trait + 0.08*pain_cat + 0,43*cortisol_serum + (-0.29)*(mindfulness) 

# comparation of the two models with R^2, ANOVA (as we hav enested models) and AIC

summary(model1)$adj.r.squared
summary(model2)$adj.r.squared
# for model 1 R^2=.12 for model 2.1 R^2=.40

anova(model1, model2)
# significant difference between the models is found due to ANOVA

AIC(model1)
AIC(model2)
AIC(model1)-AIC(model2)
# AIC for model 2.1 is approx. 56 AIC points lower than for model 1

# overview of the coifficients of the two regression models

sm_1 <- summary(model1)
sm_2 <- summary(model2)

sm_1_p_values = as.character(round(sm_1$coefficients[,4], 3))	
sm_1_p_values[sm_1_p_values != "0" & sm_1_p_values != "1"] = substr(sm_1_p_values[sm_1_p_values != "0" & sm_1_p_values != "1"], 2, nchar(sm_1_p_values[sm_1_p_values != "0" & sm_1_p_values != "1"]))	
sm_1_p_values[sm1_p_values == "0"] = "<.001"

sm_table1 = cbind(as.data.frame(round(cbind(coef(model1), confint(model1), c(0, lm.beta::lm.beta(model1)$standardized.coefficients[c(2,3)])), 2)), sm_1_p_values)	
names(sm_table1) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm_table1["(Intercept)","Std.Beta"] = "0"

sm_table1

sm_2_p_values = as.character(round(sm_2$coefficients[,4], 3))	
sm_2_p_values[sm_2_p_values != "0" & sm_2_p_values != "1"] = substr(sm_2_p_values[sm_2_p_values != "0" & sm_2_p_values != "1"], 2, nchar(sm_2_p_values[sm_2_p_values != "0" & sm_2_p_values != "1"]))	
sm_2_p_values[sm_2_p_values == "0"] = "<.001"

sm_2_table = cbind(as.data.frame(round(cbind(coef(model2), confint(model2), c(0, lm.beta::lm.beta(model2)$standardized.coefficients[c(2,3)])), 2)), sm_2_p_values)	
names(sm_2_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm_2_table["(Intercept)","Std.Beta"] = "0"

sm_2_table

############################################################


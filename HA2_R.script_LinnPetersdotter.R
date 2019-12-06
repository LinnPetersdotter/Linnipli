##########################################################
################### ASSIGNMENT II ########################
##########################################################

# getting started: setting working directory, importing data, loading packages a

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
library(leaps)
library(MASS)
library(moments)

##########################################################
################### Coding errors ########################
##########################################################

#eliminate the coding errors removed in assignemnt 1
home_sample_1.2 <- home_sample_1[!home_sample_1$STAI_trait <= 20, ]

home_sample_1.2 <- home_sample_1.2[!home_sample_1.2$household_income <= 0, ]


# defining the backward starting model including all the variables (besides cortisol_saliva which our fictitious research friend also excluded)
model_bw <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + IQ + household_income, data = home_sample_1.2)	
summary(model_bw)

##########################################################
################ ASSUMPTION TESTING ######################
###############     for the full model    ################

#checking assumptions for the full model  (model_bw) and run therefore required model diagnostics for a regression

# 1. influential outliers --> scatterplot / Cook's distance 
# 2. normality of residuals 
# 3. linearity 
# 4. homoscedasticity 
# 5. multicollinearity 

# 1. I look for influential outlier in my variables. 
x11() # opens my graphic window seperatly 
home_sample_1.2 %>% ggplot() + aes(x = STAI_trait, y = pain) + geom_point()
x11()
home_sample_1.2 %>% ggplot() + aes(x = pain_cat, y = pain) + geom_point()
x11()
home_sample_1.2 %>% ggplot() + aes(x = cortisol_serum, y = pain) + geom_point()
x11()
home_sample_1.2 %>% ggplot() + aes(x = cortisol_serum, y = pain) + geom_point()
x11()
home_sample_1.2 %>% ggplot() + aes(x = mindfulness, y = pain) + geom_point()
x11()
home_sample_1.2 %>% ggplot() + aes(x = weight, y = pain) + geom_point()
x11()
home_sample_1.2 %>% ggplot() + aes(x = IQ, y = pain) + geom_point()
x11()
home_sample_1.2 %>% ggplot() + aes(x = household_income, y = pain) + geom_point()

# there is this one value among the participants scoring extraordinary high on the pain scale (but still within the possible range)

# For identifying extreme cases with a probably high leverage, I plot the residual-leverage plot and by using Cook's distance. 
x11()
model_bw %>% plot(which = 5)
model_bw %>% plot(which = 4)
# possibility that 53, 72 or 86  actually could have an influence on my model statistics
# rules of thumb for Cook's distance: no value > 1 (Cooks distance), due to the criterion 4/N (4/158 = 0.025) 
# I would like to keep as many participants as possible in my dataset, so I first check the other assumptions without excluding any further outlier. If any assumption is hurt I will try it again with excluding the most suspicious value (No. 88 so far) and see what it does to my model diagnostics. 

# 2. normality of residuals 
model_bw %>% plot(which = 2)
# there are some cases not really on or near to the line. I plot a histogram of the model's residuals:
residuals_model_bw = enframe(residuals(model_bw))
residuals_model_bw %>% ggplot() + aes(x = value) + geom_histogram()

# skewness and kurtosis
# rule of thumb: if skewness is close to zero, data is not normally distributed.rule of thumb for kurtosis: if it is close to 3, we assume normal distribution. 
skewness(residuals(model_bw))
# skewness is close to 0, which indicates normally distributed residuals
kurtosis(residuals(model_bw))
# kurtosis is close to 3 

# normality of residuals tests, Shapiro Wilk test
shapiro.test(residuals(model_bw))
# Shapiro Wilk's p-value indicates to keep the null-hypothesis (in that case: our residuals are normally distributed)
# normality of residuals is assumed 

# 3. linearity between predictors and criterion must be linear 
x11()
model_bw %>% residualPlots()
# pain_cat looks more like a curved and non-linear relationship and also the model statistics' p-value indicate non-linearity for the variable pain_cat

# including the higher order term of the predictor in the model. The more higher order terms are included the more flexible we allow the regression line to become. 
# Disadvantage: the more flexibility we allow, the higher the probability of "overfitting", so our model will not work well in the "real world".
# including the second order term for pain_cat into the model 

model_bw1 = lm(pain ~ age + sex + STAI_trait + pain_cat + I(pain_cat^2) + cortisol_serum + mindfulness + weight + IQ + household_income, data = home_sample_1.2)	
summary(model_bw1)

# checking again for the linearity between predictors and critetion 
x11()
model_bw1 %>% residualPlots()
# now the test statistics for checking the assumption of lineartiy does not become significant for any predictor, but the more higher order terms are included the more flexible we allow the regression line to become. 
# This is why I decide to not include the higher order term and will assume that the assumption of linearity for the residuals is just slightly deviant from the ideal. 
# Thus I just will accept the small violation of linearity for pain_cat and work without the higher order term. 


# 4. homoscedasticity 
x11()
model_bw %>% plot(which = 3)
# Breusch Pagan Test and non constant variance score test (NCV) for testing homoscedasticity  
ols_test_breusch_pagan(model_bw, rhs = TRUE, multiple = TRUE)
ncvTest(model_bw)
# both tests indicate homocedasticity 

# 5. Testing the assumption of multi-collinearity with vif 
model_bw %>% vif()
###############################################################

# stepwise backward elimination, starting with the full model
step(object=model_bw, direction = "backward")

# create the suggested best fit model (model_bw2) but including all the lower order terms as well 
model_bw2 <- lm(pain ~ age + sex + pain_cat + cortisol_serum + mindfulness + weight, data = home_sample_1.2)	


##########################################################
################ ASSUMPTION TESTING ######################
###############   for model_bw2     #####################

# For identifying extreme cases with a probably high leverage, I plot the residual-leverage plot and by using Cook's distance. 
x11()
model_bw2 %>% plot(which = 5)
model_bw2 %>% plot(which = 4)
# possibility that 155, 72 or 86 actually could have an influence on my model statistics

# 2. normality of residuals 
x11()
model_bw2 %>% plot(which = 2)
# there are some cases not really on or near to the line. I plot a histogram of the model's residuals:
residuals_model_bw2 = enframe(residuals(model_bw2))
residuals_model_bw2 %>% ggplot() + aes(x = value) + geom_histogram()

# skewness and kurtosis
# rule of thumb: if skewness is close to zero, data is not normally distributed.rule of thumb for kurtosis: if it is close to 3, we assume normal distribution. 
skewness(residuals(model_bw2))
# skewness is close to 0, which indicates normally distributed residuals
kurtosis(residuals(model_bw2))
# kurtosis is close to 3 

# normality of residuals tests, Shapiro Wilk test
shapiro.test(residuals(model_bw2))
# Shapiro Wilk's p-value indicates to keep the null-hypothesis (in that case: our residuals are normally distributed)
# normality of residuals is assumed 

# 4. homoscedasticity 
x11()
model_bw2 %>% plot(which = 3)
# Breusch Pagan Test and non constant variance score test (NCV) for testing homoscedasticity  
ols_test_breusch_pagan(model_bw2, rhs = TRUE, multiple = TRUE)
ncvTest(model_bw2)
# both tests indicate homoscedasticity 

# 5. Testing the assumption of multi-collinearity with vif 
model_bw2 %>% vif()
# no value looks suspicious concerning a high correlation 


##########################################################
################ Model comparison  ######################
#########################################################

# comparing the two models bw (starting model) and bw2 (after backwards regression)
AIC(model_bw)
AIC(model_bw2)

anova( model_bw2, model_bw)

# our AIC for the best fit model (model_bw2) resulted in a significanlty better AIC value (>2) than our starting model (model_bw)
summary(model_bw2)

# backward model as a new R object saved
backward_model <- lm(pain ~ age + sex + pain_cat + cortisol_serum + mindfulness + weight, data = home_sample_1.2)	
summary(backward_model)

# theory model as a new R object saved
theory_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = home_sample_1.2)	  
summary(theory_model)

# comparing the theory and the backward model 
AIC(theory_model)
AIC(backward_model)
AIC(theory_model)-AIC(backward_model)
# AIC value for the backward model is lower by >2, indicating a significantly better model fit

# ANOVA is not appropriate as our two models are not nested

# also R^2 can be used to compare the models, our theory model in this case explains ca. 1% more of the overall variance (which is actually a really small difference)


##########################################################
############ Prediction on a new dataset  ###############
#########################################################

home_sample_2 = read_csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")
home_sample_2
summary(home_sample_2)
# I use the sample as it is for the prediction of pain. 

# Comparing theoretical and backward models on a new dataset (home_sample_2)
# comparing our two models in respect to predictive success

Theorymodel_predict <- predict(theory_model, home_sample_2)

Backwardmodel_predict <- predict(backward_model, home_sample_2)

# showing the predicted values of theoretical and backward equation 
Theorymodel_predict
Backwardmodel_predict

# comparing the sum of squares for both models, building two vectors showing me the squared differences between the real pain values and the predicted ones by the equations
RSS_theorymodel = sum((home_sample_2[, "pain"] - Theorymodel_predict)^2) 
RSS_theorymodel
RSS_backwardmodel = sum((home_sample_2[, "pain"] - Backwardmodel_predict)^2)
RSS_backwardmodel

# our theory model shows slightly lower square of sums than our backward model, implying that the theory model works better in predicting pain on the new sample

# overview of coefficients for both models 
sm_th  <- summary(theory_model)
sm_bw <- summary(backward_model)

sm_th_p_values = as.character(round(sm_th$coefficients[,4], 3))	
sm_th_p_values[sm_th_p_values != "0" & sm_th_p_values != "1"] = substr(sm_th_p_values[sm_th_p_values != "0" & sm_th_p_values != "1"], 2, nchar(sm_th_p_values[sm_th_p_values != "0" & sm_th_p_values != "1"]))	
sm_th_p_values[sm_th_p_values == "0"] = "<.001"

sm_table_th = cbind(as.data.frame(round(cbind(coef(theory_model), confint(theory_model), c(0, lm.beta::lm.beta(theory_model)$standardized.coefficients[c(2,3)])), 2)), sm_th_p_values)	
names(sm_table_th) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm_table_th["(Intercept)","Std.Beta"] = "0"

sm_table_th

sm_bw_p_values = as.character(round(sm_bw$coefficients[,4], 3))	
sm_bw_p_values[sm_bw_p_values != "0" & sm_bw_p_values != "1"] = substr(sm_bw_p_values[sm_bw_p_values != "0" & sm_bw_p_values != "1"], 2, nchar(sm_bw_p_values[sm_bw_p_values != "0" & sm_bw_p_values != "1"]))	
sm_bw_p_values[sm_bw_p_values == "0"] = "<.001"

sm_table_bw = cbind(as.data.frame(round(cbind(coef(backward_model), confint(backward_model), c(0, lm.beta::lm.beta(backward_model)$standardized.coefficients[c(2,3)])), 2)), sm_bw_p_values)	
names(sm_table_bw) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm_table_bw["(Intercept)","Std.Beta"] = "0"

sm_table_bw

################################################################
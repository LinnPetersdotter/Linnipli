##########################################################
################### ASSIGNMENT III #######################
##########################################################


# import data file
home_sample_3 = as_tibble(read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_3.csv"))

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
library(lme4)
library(insight)
library(sjstats)
library(MuMIn)
library(cAIC4)
library(r2glmm)
library(lmerTest)
library(influence.ME)
library(gridExtra)

# including function for displaying standard coefficients in a table format at the end of the assignment
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

###################################################################

##########################################################
################### Data preparation #####################
##########################################################

# looking at the dataset to find possible coding errors or missing values 
summary(home_sample_3)
describe(home_sample_3)

# there is a "third gender" in the data set --> coding error: Female instead of female
# taking care of the coding error
home_sample_3 <- home_sample_3 %>% mutate(sex = droplevels(replace(sex, sex == "Female", "female")))
summary(home_sample_3)

# the max. value for mindfulness exceeds the range of the scale (1-6), in our case the max. value is 6.05 
# excluding the one row with a mionduflness value >6 
home_sample_3 <- home_sample_3[!home_sample_3$mindfulness >6, ]

# examining the scale level of the variables
str(home_sample_3)
# no mutation necessary concerning the variable scales 

##########################################################
################ Creating a mixed model ##################
##########################################################

# creating a mixed model with random intercepts for hospitals and fixed effect predictors, using predictors of final model form Ass. I 
mixed_pain_model <- lmer(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1|hospital), data= home_sample_3, REML = FALSE)

# rebuilding the linear model with the predictors from Ass I 
ass1_pain_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data= home_sample_3)


##########################################################
######### Assumption testing & outlier detection #########
############## for mixed_pain_model ######################

#checking for multivariate outliers and assumptions for the mixed_pain_model and run therefore required model diagnostics for a multilevel regression

# 1. influential outliers --> scatterplots for visualization and calculate Mahalanobis distance
# 2. normality of residuals 
# 3. linearity 
# 4. homoscedasticity 
# 5. multicollinearity 

# 1. ##### influential outlier #####
x11() # opens my graphic window seperatly for Mac
home_sample_3 %>% ggplot() + aes(x = age, y = pain) + geom_point()
x11()
home_sample_3 %>% ggplot() + aes(x = sex, y = pain) + geom_point()
x11()
home_sample_3 %>% ggplot() + aes(x = STAI_trait, y = pain) + geom_point()
x11()
home_sample_3 %>% ggplot() + aes(x = pain_cat, y = pain) + geom_point()
x11()
home_sample_3 %>% ggplot() + aes(x = cortisol_serum, y = pain) + geom_point()
x11()
home_sample_3 %>% ggplot() + aes(x = mindfulness, y = pain) + geom_point()

# checking for outliers with the influence function
influence_observation = influence(mixed_pain_model, obs = T)$alt.fixed
influence_group = influence(mixed_pain_model, group = "hospital")$alt.fixed	

x11()
data_plot_influence = as_tibble(influence_group) %>% 	
  gather(colnames(influence_group), value = coefficient, key = predictor)	

data_plot_influence %>% 	
  ggplot() +	
  aes(x = 1, y = coefficient, group = predictor) +	
  geom_violin() +	
  facet_wrap( ~ predictor, scales = "free")	
# I will not exclude any value on the basis of these plots


#### 2. Normality	of residuals ####
x11()
qqmath(mixed_pain_model, id=0.05)	
# The points on the plot should roughly fit on a straight line.
x11()
qqmath(ranef(mixed_pain_model))	
# looks like normality of residuals is fulfilled

#### 3.Linearity ####	
# plotting the scatterplot of the standardized residuals and the predicted values
x11()
plot(mixed_pain_model, arg = "pearson")	

# I also want to look at the scatterplot of the residuals and the fixed predictors separately.	
home_sample_3 = home_sample_3 %>% 	
  mutate(resid = residuals(mixed_pain_model))	

x11()
grid.arrange(age_res, sex_res, stai_res, pain_res, serum_res, mind_res, ncol = 2, nrow = 3)

age_res <- home_sample_3 %>% 	
  ggplot() +	
  aes(x = age, y = resid) +	
  geom_point()	

sex_res <- home_sample_3 %>% 
  ggplot() +	
  aes(x = sex, y = resid) +	
  geom_point() +
	
stai_res <- home_sample_3 %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = resid) +	
  geom_point()	

pain_res <- home_sample_3 %>% 	
  ggplot() +	
  aes(x = pain_cat, y = resid) +	
  geom_point()	

serum_res <- home_sample_3 %>% 	
  ggplot() +	
  aes(x = cortisol_serum, y = resid) +	
  geom_point()	

mind_res <- home_sample_3 %>% 	
  ggplot() +	
  aes(x = mindfulness, y = resid) +	
  geom_point()	

#### 4. Homoscedasticity ####	

# again I plot the standardized residuals ~ predicted values. This time a funnel shape indicates heteroscedasticity. 
x11()
plot(mixed_pain_model, arg = "pearson")	
# there is no funnel shape observalbe, thus I assume homoscedasticity.

# Homoscedasticity check on the cluster level (hospital) running a significance test 
homosced_mod = lm(resid^2 ~ hospital, data = home_sample_3)	
summary(homosced_mod)	
# significance test indicates homoscedasticity because p > .05, only for hospital 5 and hospital 9 the p-value gets slightly significant, indicating a violation of homoscedasticity
# based to the F-test, homoscedasticity is assumed. 

#### 5. Multicollinearity	####

# Finally, I should check for multicollinearity of the fixed effect predictors. 
x11()
pairs.panels(home_sample_3[,c("age", "sex", "STAI_trait", "pain_cat", "mindfulness", "cortisol_serum")], col = "red", lm = T, stars = T)	
# The correlations seems a little bit problematic, because we have some highly significant correlations between the fixed effect predictors 
# however, we might have many significant correlations but hardly any value exceeds r = .55 
# how can we deal with multicollinearity? 
# 1. excluding the predictor -> does not seem a good option since the correlation is not extraordinary high
# 2. linearly combining predictors 
# 3. using an entirely different statistic

# For now I accept the displayed correlations as the highest correlation is .55 between cortisol_serum and STAI_trait
#########################################################################################


##########################################################
    ############## Model comparison ###################
################ ass1 model & mixed model ################

# compare the coefficients of the mixed random intercept model and the linear model
summary(mixed_pain_model)
summary(ass1_pain_model)

# looking at the different variance components
get_variance_fixed(mixed_pain_model)
get_variance_random(mixed_pain_model)
get_variance_residual(mixed_pain_model)

# unstandardized coefficients of mixed model with random intercepts
coef(mixed_pain_model)        
# unstandardized coefficients of ass1 model model with random intercepts
coef(ass1_pain_model)  

# standardized coefficients of mixed model with random intercepts   
stdCoef.merMod(mixed_pain_model)

# calculating confidence intervals for the mixed model 
confint(mixed_pain_model)

# calculating confidence intervals for the ass1 model
confint(ass1_pain_model)

# Variance explained by the fixed effect predictors using marginal R^2 
R2_marginal <- r2beta(mixed_pain_model, method = "nsj", data = home_sample_3)	# marginal R^2 (variance explained by fixed effect predictors)
# and calculating the variance explained by the fixed and random effect terms combined using conditional R^2
R2_conditional <- r.squaredGLMM(mixed_pain_model) # and conditional R^2 fixed effects plus random effects

R2_marginal
R2_conditional

# Likelihood ratio test for comparing the null model (using just the mean) with the mixed model 
Nullmodel <- lmer(pain ~ 1 + (1|hospital), data = home_sample_3, REML = FALSE)
anova(Nullmodel, mixed_pain_model)
# significant difference between those models

#################################################################


##########################################################
############# Mixed model on new data  ###################
#########################################################

home_sample_4 = as_tibble(read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_4.csv"))

# Using the regression equation obtained on data file 3 to predict pain in data file 4, using the predict function
Preditcmodel_mixed <- predict(mixed_pain_model, home_sample_4, allow.new.levels = TRUE)

# Compute the variance explained by the model on data file 4
# calculating the residual and total sum of squares
RSS = sum((home_sample_4$pain - predict(mixed_pain_model, home_sample_4, allow.new.levels = TRUE))^2)	
TSS = sum((home_sample_4$pain - predict(Nullmodel, home_sample_4, allow.new.levels = TRUE ))^2)
# now R^2 equals the following:
R2 = 1 - (RSS/TSS)
R2
# residual sum of squares for mixed pain model
RSS
# total sum of squares for the null model (just mean / random intercept included)
TSS
# total 
R2
# total variance explained 34% for the model on data file 4 

# comparing this R^2 to the marginal and conditional R^2 I computed on data 3

R2_marginal # of mixed pain model with random intercept on file 3
R2_conditional # of mixed pain model with random intercept on file 3
R2 # of mixed pain model with random intercept on file 4
# compared to the marginal and conditional R^2, the R^2 calculated on data file 4 is closer to the marginal R^2 form data file 3
# comparison to conditional R^2 on data file 3 (46.4%, >15% more explained by conditional R^2 on data 3)
# that is no surprise: our mixed model was fit on data file 3 and the random effect explain additionally 10% of the total variance
# on file 4 we did not fit our data, but our fixed effect predictors seem to work fine for the new data file since they explain approx. the same amount of variance as in data file 3

##############################################################


##############################################################
# Building a random slope model with the strongest predictor #
##############################################################

# selecting the most influential predictor 

sm = summary(mixed_pain_model)		
sm_p_values = as.character(round(sm$coefficients[,"Pr(>|t|)"], 3))		
sm_p_values[sm_p_values != "0" & sm_p_values != "1"] = substr(sm_p_values[sm_p_values != "0" & sm_p_values != "1"], 2, nchar(sm_p_values[sm_p_values != "0" & sm_p_values != "1"]))		
sm_p_values[sm_p_values == "0"] = "<.001"		

coef_CI = suppressWarnings(confint(mixed_pain_model))		
coef_CI		
stdCoef.merMod(mixed_pain_model)

sm_table = cbind(as.data.frame(round(cbind(as.data.frame(sm$coefficients[,"Estimate"]), coef_CI[c("(Intercept)", "age", "sexmale", "STAI_trait", "pain_cat", "cortisol_saliva", "mindfulness"),], c(0, stdCoef.merMod(mixed_pain_model)[2,7])), 2)), sm_p_values)		
names(sm_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
sm_table["(Intercept)","Std.Beta"] = "0"		
sm_table		

# the coefficient b and the confidence intervals indicate that cortisol_serum could be an appropriate candidate for the most influentioal predictor in our model
# the comparison of marginal R^2 among the fixed effect predictors, cortisol_serum is the most influential predictor for the criterion pain 
# R^2marginal for cortisol_serum = .08

# creating a random slope model with the most influential predictor from the mixed model (allowing both random intercept and slope)
mixed_pain_model_slope = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data=home_sample_3)

# summarising the new random slope model
summary(mixed_pain_model_slope)

# comparing the random intercept with the random slope model
cAIC(mixed_pain_model)
cAIC(mixed_pain_model_slope)
# no surprise that AIC is higher for the slope model as just included one variable was included

# using the predict function to let the slope model predict pain
home_sample_3 = home_sample_3 %>% 		
  mutate(pred_slope = predict(mixed_pain_model_slope))

# visualizing the regression line for every hospital 
x11()
home_sample_3 %>% ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) + geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
                                                                                                                                          aes(y = pred_slope, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)
                                                                                                                                           
# Model R^2 equals obviously the only predictor in the slope model (R^2 = .192)
r2beta(mixed_pain_model_slope, method = "nsj", data = home_sample_3)      

# Likelihood ratio test for comparing the random intercept modelwith the random slope model
anova(mixed_pain_model, mixed_pain_model_slope)

# standardized coefficients for random slope model
stdCoef.merMod(mixed_pain_model_slope)

# overview of coefficients 
sm = summary(mixed_pain_model_slope)		
sm_p_values = as.character(round(sm$coefficients[,"Pr(>|t|)"], 3))		
sm_p_values[sm_p_values != "0" & sm_p_values != "1"] = substr(sm_p_values[sm_p_values != "0" & sm_p_values != "1"], 2, nchar(sm_p_values[sm_p_values != "0" & sm_p_values != "1"]))		
sm_p_values[sm_p_values == "0"] = "<.001"		

coef_CI = suppressWarnings(confint(mixed_pain_model_slope))		
coef_CI		

sm_table_slope = cbind(as.data.frame(round(cbind(as.data.frame(sm$coefficients[,"Estimate"]), coef_CI[c("(Intercept)", "cortisol_serum"),], c(0, stdCoef.merMod(mixed_pain_model_slope)[2,1])), 2)), sm_p_values)		
names(sm_table_slope) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
sm_table_slope["(Intercept)","Std.Beta"] = "0"		
sm_table_slope


##################################################################
# Building a random intercept model with the strongest predictor #
##################################################################

# create a random intercept for cortisol_serum to compare the plot to the random slope model 
cortisol_model_int <-lmer(pain ~ cortisol_serum + (1|hospital), data= home_sample_3, REML = FALSE)

# using the predict function to let the random intercept model for cortisol_serum let predict pain 
home_sample_3 = home_sample_3 %>% 		
  mutate(pred_int = predict(cortisol_model_int))

# visualizing the regression line for every hospital 
x11()
home_sample_3 %>% ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) + geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
                                                                                                                                           aes(y = pred_int, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)
# looks for me pretty similar to the plot of the random slope model --> I conclude the slopes for the different predictors do not differ significantly and thus have not a strong influence for the different hospitals

############################################# The end. 

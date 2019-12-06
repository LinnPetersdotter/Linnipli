##########################################################
############## ASSIGNMENT I - PCA ########################
##########################################################

# setting working directory, loading data and getting packages ready
setwd("~/Documents/Lund Uni/StatisticsPSYP13") 

PAQ_Linn <- read.csv("~/Documents/Lund Uni/StatisticsPSYP13/PAQ_Linn.txt", sep="")
View(PAQ_Linn)

library(lsr)
library(psych) # load packages
source('GraphPlot.R')
library(tidyr) # package loading to tidy up 
library(distances)


# create a second dastaset in a better structure 
Linn2 <- spread(PAQ_Linn, key="var",value="value") 

# look at my data and look for suspicious values and N.A.s
describe(Linn2)
summary(Linn2)

# N.A.s are always unpleasant, but as there are only two in the whole data set, I decide to delete the rows where the NAs are. 
na.omit(Linn2)

Linn2.1 <- na.omit(Linn2)

# how does my sample look like now?
summary(Linn2.1)
sd(Linn2.1$age)

# deleting categorial data which I do not consider in my PCA
Linn2.1$sex <- NULL

Linn2.1$id <- NULL 

Linn2.1$age <- NULL

###### testing for assumptions for PCA & look for probable outlier

# 1. looking for multivariate outlier
# 2. correlation between variables (linear relationship between all variables)

# 1. detect Multivariate Outliers 
# The linear regression "predicts" subject number based on all other variables as predictors
regr01 <- lm(formula = id ~ age + Q1_cry + Q2_help + Q3_breathe + Q4_freeze + Q5_alien + Q6_inferior + Q7_weep + Q8_Support + Q9_Nerd + sex, data=Linn2)
lev <- hat(model.matrix(regr01))
Linn2[lev > .045,] 
x11()
plot(lev)

# I check Mahalanobis Distance and the critical Chi Squared value for 11 variables (performed on the whole data set)
# Chisquare value for 11 variables, p = .001 --> 31.26 (looked up at: https://www.medcalc.org/manual/chi-square-table.php)
N <- nrow(Linn2)                          
mahad <- (N-1)*(lev-1/N)                  
tail(sort(mahad),11)                        
order(mahad,decreasing=T)[c(5,4,3,2,1)]  
# according to Mahalanobis we do not remove any outlier, as no value among the five highest is > 31.26 

# 2. I want to check if the assumption of linearity between my variables is met. Therefore I will examine the correlations, as every variable of interest should be correlated to the others for dimensional reduction. 

source("http://www.sthda.com/upload/rquery_cormat.r")
x11()
cor.matrix <- rquery.cormat(Linn2.1, type="full")

# smallest correlation is .2 between Q8 and Q2, I check with the Kaiser-Meyer-Olkin Test if my data is adequate for PCA
cor.matrix$r
KMO(cor.matrix$r)

# Overall MSA = 0.84, as a rule of thumb values between 0.8-1.0 indicate an adequate sample 

###########################################################
################# performing the PCA ######################
###########################################################

# correlation or covariance matrix to perform the PCA?
# the variables of interest (Q1-Q9) have the same scale, thus I could perform the PCA on my covariance matrix
# however, I would like to check the standard deviations first to make sure, they are close enough to each other. If the variance of my data differs a lot among the variables I should definetly perform my PCA on the correlation matrix.

Linn2.1_sd <- c(sd(Linn2.1$Q1_cry), sd(Linn2.1$Q2_help), sd(Linn2.1$Q3_breathe), sd(Linn2.1$Q4_freeze), sd(Linn2.1$Q5_alien), sd(Linn2.1$Q6_inferior), sd(Linn2.1$Q7_weep), sd(Linn2.1$Q8_Support), sd(Linn2.1$Q9_Nerd))

Linn2.1_sd
# standard deviation of my data looks pretty similar for the different variables

# performing PCA on covariance matrix 
linnpca_cov <- princomp(Linn2.1, cor = F) 

summary(linnpca_cov, loadings= TRUE)

# showing me the eigenvalues 
eig <- (linnpca_cov$sdev)^2
eig
# first three have eig values >3, the first one is especially high

# variance in percentage
variance <- eig*100/sum(eig)
variance
# approx. half of my datas variance seems to be expklained by the first component

# cumulative variance 
cumvar <- cumsum(variance)
cumvar

# creating a plot of the variance explained by my components 
p = princomp(Linn2.1, cor=F)
p.variance.explained = p$sdev^2 / sum(p$sdev^2)
loadings = p$loadings[]
p.variance.explained = p$sdev^2 / sum(p$sdev^2)
x11()
barplot(100*p.variance.explained, las=2, xlab='', ylab='% Variance Explained', col ="hotpink4")

# summary of the important values 
eig.full <- data.frame(eig = eig, variance = variance,
                                    cumvariance = cumvar)
head(eig.full)


###########################################################
############### deciding on components ####################
###########################################################

# 1. retain the components which are able to explain 70-90% of the total variation
# --> keep 3 because those explain between alsmost 77%

# 2. retain the components with an eigenvalue > average of eigenvalues
mean(eig)
# keep the first 3 --> >3 1.163

# 3. visualize a scree plot and decide on visual basis

x11()

plot(linnpca_cov$sdev^2, xlab = "Component number",
     ylab = "Component variance", type = "l", 
     main = "Scree diagram")

# logeigenvalue diagram 
x11()
plot(log(linnpca_cov$sdev^2), xlab = "Component number",
     ylab = "log(Component variance)", type="l",
     main = "Log(eigenvalue) diagram")

# scree diagram has first "elbow" at Component 2, but as there is just 50% explained of my total variation, I choose rather to keep 3

###########################################################

# creating a biplot for the first two components 
x11()
biplot(linnpca_cov, col = c("lightpink", "cadetblue4"))

# c reating a biplot for the second and third component
x11()
biplot(linnpca_cov, choices = 2:3, scale = 1, pc.biplot = FALSE, col = c("azure2", "dodgerblue4"))

# for the discussion:
# Comp 1 = similar distributions of loadings - general statistics anxiety
# Comp 2 = weep, help and alien - hope for help / support / helplessness
# Comp 3 = inferior, nerd - self & external perception towards statistics / fear of humilation

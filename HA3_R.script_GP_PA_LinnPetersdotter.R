##########################################################
#################### Power Analysis ######################
#########################################################


# I  put the only kind of successful trial in the beginning
# however, the trials which were done before, are also in this script, at the very end

# load all packages
library('paramtest')
library('pwr')
library('ggplot2')
library('knitr')
library('nlme')
library('lavaan')
library('dplyr')
library(lsr)
library(psych)

##########################################################
############### Monte Carlo simultion ###################
#########################################################


an_func <- function(simNum, N, d) {
  v1 = rnorm(N, mean = 0.5, sd = 1) # generate the data
  v2 = rnorm(N, mean = -0.5, sd = 1)
  v3 = rnorm(N, mean = 0, sd = 1)
  v4 = rnorm(N, mean = 0, sd =1)
  grp1= c(rep(1, N)) # define how many in groups
  grp2= c(rep(2, N))
  grp3= c(rep(3, N))
  grp4= c(rep(4, N))
  f.treat=c(grp1, grp2, grp3, grp4) # make a single vector of groups
  meas=c(v1, v2, v3, v4) # make a single vector of the data (measurements)
  mydata=cbind(f.treat,meas) # column bind to create a matrix
  mydata=as.data.frame(mydata) # convert to a data frame
  mydata$f.treat = as.factor(mydata$f.treat) # convert groups to a factor
 
  my.anova <- aov(formula = meas ~ f.treat, data = mydata ) # run anova on generated data
  sm <- summary(my.anova)
  sm1 <- sm[[1]]
  sm2 <- sm1[-2,]
  stat <- sm2$`F value`
  p <- sm2$`Pr(>F)`
  
  return(c(my.anova = stat, p = p, sig = (p < .05)))
}

# N should be different and d should be Cohen's f 
power_anova <- grid_search(an_func, params=list(N = seq(10, 50, 1), d = .35),
                           n.iter=100, output='data.frame')
power <- results(power_anova) %>%
  group_by(N.test, d.test) %>%
  summarise(power=mean(sig))
print(power)

# plotting the power analysis 
x11()
ggplot(power, aes(x=N.test, y=power, group=factor(d.test), colour=factor(d.test))) +
  geom_point() +
  geom_line() +
  ylim(c(0, 1)) +
  labs(x='Sample Size', y='Power', colour="Cohen's f") +
  theme_minimal()

# how do we extract the amounts of p-values per n size? 

table(power_anova$results$p<.05)
# but that does not give us the amounts of single p-values for each group 
# I can create a vector getting me the p-values for every n
# I am pretty sure there is a more elegant way to do it....
# as I coded together with Ege, we split up the work for creating the following vectors, I hope this does not account as plagiarism

pcount10 <- power_anova$results$p[c(1:100)]
table(pcount10 < 0.05)

pcount11 <- power_anova$results$p[c(101:200)]
table(pcount11 < 0.05)

pcount12 <- power_anova$results$p[c(201:300)]
table(pcount12 < 0.05)

pcount13 <- power_anova$results$p[c(301:400)]
table(pcount13 < 0.05)

pcount14 <- power_anova$results$p[c(401:500)]
table(pcount14 < 0.05)

pcount15 <- power_anova$results$p[c(501:600)]
table(pcount15 < 0.05)

pcount16 <- power_anova$results$p[c(601:700)]
table(pcount16 < 0.05)

pcount17 <- power_anova$results$p[c(701:800)]
table(pcount17 < 0.05)

pcount18 <- power_anova$results$p[c(801:900)]
table(pcount18 < 0.05)

pcount19 <- power_anova$results$p[c(901:1000)]
table(pcount19 < 0.05)

pcount20 <- power_anova$results$p[c(1001:1100)]
table(pcount20 < 0.05)

pcount21 <- power_anova$results$p[c(1101:1200)]
table(pcount21 < 0.05)

pcount22 <- power_anova$results$p[c(1201:1300)]
table(pcount22 < 0.05)

pcount23 <- power_anova$results$p[c(1301:1400)]
table(pcount23 < 0.05)

pcount24 <- power_anova$results$p[c(1401:1500)]
table(pcount24 < 0.05)

pcount25 <- power_anova$results$p[c(1501:1600)]
table(pcount25 < 0.05)

pcount26 <- power_anova$results$p[c(1601:1700)]
table(pcount26 < 0.05)

pcount27 <- power_anova$results$p[c(1701:1800)]
table(pcount27 < 0.05)

pcount28 <- power_anova$results$p[c(1801:1900)]
table(pcount28 < 0.05)

pcount29 <- power_anova$results$p[c(1901:2000)]
table(pcount29 < 0.05)

pcount30 <- power_anova$results$p[c(2001:2100)]
table(pcount30 < 0.05)

pcount31 <- power_anova$results$p[c(2101:2200)]
table(pcount31 < 0.05)

pcount32 <- power_anova$results$p[c(2201:2300)]
table(pcount32 < 0.05)

pcount33 <- power_anova$results$p[c(2301:2400)]
table(pcount33 < 0.05)

pcount34 <- power_anova$results$p[c(2401:2500)]
table(pcount34 < 0.05)

pcount35 <- power_anova$results$p[c(2501:2600)]
table(pcount35 < 0.05)

pcount36 <- power_anova$results$p[c(2601:2700)]
table(pcount36 < 0.05)

pcount37 <- power_anova$results$p[c(2701:2800)]
table(pcount37 < 0.05)

pcount38 <- power_anova$results$p[c(2801:2900)]
table(pcount38 < 0.05)

pcount39 <- power_anova$results$p[c(2901:3000)]
table(pcount39 < 0.05)

pcount40 <- power_anova$results$p[c(3001:3100)]
table(pcount40 < 0.05)

pcount41 <- power_anova$results$p[c(3101:3200)]
table(pcount41 < 0.05)

pcount42 <- power_anova$results$p[c(3201:3300)]
table(pcount42 < 0.05)

pcount43 <- power_anova$results$p[c(3301:3400)]
table(pcount43 < 0.05)

pcount44 <- power_anova$results$p[c(3401:3500)]
table(pcount44 < 0.05)

pcount45 <- power_anova$results$p[c(3501:3600)]
table(pcount45 < 0.05)

pcount46 <- power_anova$results$p[c(3601:3700)]
table(pcount46 < 0.05)

pcount47 <- power_anova$results$p[c(3701:3800)]
table(pcount47 < 0.05)

pcount48 <- power_anova$results$p[c(3801:3900)]
table(pcount48 < 0.05)

pcount49 <- power_anova$results$p[c(3901:4000)]
table(pcount49 < 0.05)

pcount50 <- power_anova$results$p[c(4001:4100)]
table(pcount50 < 0.05)

##########################################################
#### Calculating power using noncentral F-distribution ###
##########################################################

############## trying to calculate power using the noncentral F distribution
### calculating lambda and F
?pf

ncf_func <- function(simNum, N, d) {
  
  ncf = pf(q = qf(0.95, 3, 4*N - 3), df1 = 3, df2 = 4*N -3, ncp = (4*N*(d)^2)) # non central f - value
  
  p <- ncf # this should be power 
  
  return(c(d.test = d, p = ncf, sig = (p < .05))) # but if p stands for power .05 is not the way to go here
  
}

# power is 1- ß so maybe something is wrong in this calculation? 
# my friend Ege came up with a great change to this formula, so now it worked better:

ncf_func <- function(simNum, N, d) {
  
  ncf = (1 - pf(q = qf(0.95, 3, 4*N - 3), df1 = 3, df2 = 4*N -3, ncp = (4*N*(d)^2)))
  
  p <- ncf
  
  return(c(d.test = d, p = p)) # changing this contributed to a successful function (sometimes I do not understand R)
  
}

# give 'params' a list of parameters we want to vary;
# testing at N = (10:50)
power_ncf <- grid_search(ncf_func, params=list(N = seq(10, 50, 1)),
                         n.iter=1, output='data.frame', d = 0.35)


powerf <- results(power_ncf) %>%
  group_by(N.test, d.test) %>%
  summarise(power = p)
print(powerf)
ggplot(powerf, aes(x=N.test, y=power, group=factor(d.test), colour=factor(d.test))) +
  geom_point() +
  geom_line() +
  ylim(c(0, 1)) +
  labs(x='Sample Size', y='Power', colour="Cohen's f") +
  theme_minimal()

# testing at different N 
power_ncf <- grid_search(ncf_func, params=list(N = seq(10, 50, 1)),
                         n.iter=1, output='data.frame', d = 0.35)


powerf <- results(power_ncf) %>%
  group_by(N.test, d.test) %>%
  summarise(power=mean(sig)) # something is wrong here because power values are all zero
print(powerf)
x11()
ggplot(powerf, aes(x=N.test, y=power, group=factor(d.test), colour=factor(d.test))) +
  geom_point() +
  geom_line() +
  ylim(c(0, 1)) +
  labs(x='Sample Size', y='Power', colour="Cohen's f") +
  theme_minimal()

# power between 70% and 90% require a certain sample size which can be looked up at the power and respective powerf dataset created by the functions 

# it is not that we came up with this immeadiatley, so for the dedicated reader, here comes the section:
####.....what happened before....

##########################################################
################### Power Analysis #######################
########## a.k. a potpuri of desperate trials ############

# creating 4 samples with m = 5, -05, 0, 0 
# starting with n = 10 
# sd = 1 
# variance = equal 

library(gap)
library(pwr)
library(MonteCarlo)

# creating 4 samples on the normal distribution with means  0, 0, 5, -5 

sample1 <- rnorm(10, mean = 0, sd = 1)
sample1

sample2 <- rnorm(10, mean = 0, sd = 1)
sample2

sample3 <- rnorm(10, mean = 0.5, sd = 1)
sample3

sample4 <- rnorm(10, mean = -0.5, sd = 1)
sample4

# running every group with increasing n 100 times

n <- c(10:50)

sims1 <- rnorm(n = n, mean =0.5, sd =1) 
sample1_100 <- replicate(n=100, sims1)

sims2 <- rnorm(n = n, mean =0, sd =1) 
sample2_100 <- replicate(n=100, sims2)

sims3 <- rnorm(n = n, mean =0, sd =1) 
sample3_100 <- replicate(n=100, sims3)

sims4 <- rnorm(n = n, mean =-0.5, sd =1) 
sample4_100 <- replicate(n=100, sims4)

######################### and now?

##########################################################
################### trying Power analysis ################
################### ANOVA function #######################

n <- c(10:50)
pwr.anova.test(k = 4, n = n, f = 0.35, sig.level = 0.05, power = NULL)

# power analysis for anova for the 4 different groups with increasing n (10:50)
n <- c(10:50)
mu     <- c(0.5, -0.5, 0, 0)
sigma2 <-1
power.anova.test(groups = length(mu), n = n, between.var = var(mu), within.var = sigma2)
# still missing: drawing for every n 100 reps 


# power with ANOVA
pwr.anova.test(k = 4, n = 10, f = 0.35, sig.level = 0.05, power = NULL)

n <- seq(10,50,1)

pwr.anova.test(k = 4, n = 10, f = 0.35, sig.level = 0.05, power = NULL)

pwr.anova.test(k = 4, n = n, f = 0.35, sig.level = NULL, power = 0.5)

n <- seq(10,50,1)
p.out <- pwr.p.test(h = 0.35,
                    n = n,
                    sig.level = 0.05)
data.frame(n, power = sprintf("%.2f%%", p.out$power * 100))

?pwr.p.test



##########################################################
############### trying Power analysis ###################
################# with a regression model ################
# for sample 1 + 4 (same mean)

possible.ns <- seq(from=10, to=50, by=1)
powers <- rep(NA, length(possible.ns))
alpha <- 0.05  
sims <- 100

for (j in 1:length(possible.ns)){
  N <- possible.ns[4]  
  significant.experiments <- rep(NA, sims) 
  for (i in 1:sims){
    Y0 <-  rnorm(n=N, mean=0.5, sd = 1 )  
    tau <- 0.35
    Y1 <- Y0 + tau
    Z.sim <- rbinom(n=N, size=1, prob=.5)  
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim) 
    fit.sim <- lm(Y.sim ~ Z.sim) 
    p.value <- summary(fit.sim)$coefficients[1,4]
    significant.experiments[i] <- (p.value <= 0.05) 
  }
  powers[j] <- mean(significant.experiments)
}
x11()
plot(possible.ns, powers, ylim=c(0,1))
# many funny points....
# 41 significant p-values
# https://egap.org/content/power-analysis-simulations-r


# trial 123.4 (for sample 2)#####################

possible.ns <- seq(from=10, to=50, by=1) # The sample sizes we'll be considering 
powers <- rep(NA, length(possible.ns)) # Empty object to collect simulation estimates 
alpha <- 0.05 # Standard significance level 
sims <- 100 # Number of simulations to conduct for each N 

#### Outer loop to vary the number of subjects #### 
for (j in 1:length(possible.ns)){ N <- possible.ns[j] # Pick the jth value for N 

Y0 <- rnorm(n=N, mean=0, sd=1) # control potential outcome 
tau <- 0.35 # Hypothesize treatment effect 
Y1 <- Y0 + tau # treatment potential outcome                                   
significant.experiments <- rep(NA, sims) # Empty object to count significant experiments 

#### Inner loop to conduct experiments "sims" times over for each N #### 
for (i in 1:sims){
  Z.sim <- rbinom(n=N, size=1, prob=.5) # Do a random assignment 
  Y.sim <- Y1*Z.sim + Y0*(1-Z.sim) # Reveal outcomes according to assignment 
  fit.sim <- lm(Y.sim ~ Z.sim) # Do analysis (Simple regression) 
  p.value <- summary(fit.sim)$coefficients[1,4] # Extract p-values 
  significant.experiments[i] <- (p.value <= 0.05) # Determine significance according to p <= 0.05
}
powers[j] <- mean(significant.experiments) # store average success rate (power) for each N 
} 
powers
# plotting whatever I just did
x11()
plot(possible.ns, powers, ylim=c(0,1))
# 41x significant p-values for this group
# should this number not vary among the groups?


# trial 123.5 (for sample 3)#####################
possible.ns <- seq(from=10, to=50, by=1) # The sample sizes we'll be considering 
powers <- rep(NA, length(possible.ns)) # Empty object to collect simulation estimates 
alpha <- 0.05 # Standard significance level 
sims <- 100 # Number of simulations to conduct for each N 

#### Outer loop to vary the number of subjects #### 
for (j in 1:length(possible.ns)){ N <- possible.ns[j] # Pick the jth value for N 

Y0 <- rnorm(n=N, mean=-.5, sd=1) # control potential outcome 
tau <- 0.35 # Hypothesize treatment effect 
Y1 <- Y0 + tau # treatment potential outcome                                   
significant.experiments <- rep(NA, sims) # Empty object to count significant experiments 

#### Inner loop to conduct experiments "sims" times over for each N #### 
for (i in 1:sims){
  Z.sim <- rbinom(n=N, size=1, prob=.5) # Do a random assignment 
  Y.sim <- Y1*Z.sim + Y0*(1-Z.sim) # Reveal outcomes according to assignment 
  fit.sim <- lm(Y.sim ~ Z.sim) # Do analysis (Simple regression) 
  p.value <- summary(fit.sim)$coefficients[1,4] # Extract p-values 
  significant.experiments[i] <- (p.value <= 0.05) # Determine significance according to p <= 0.05
}
powers[j] <- mean(significant.experiments) # store average success rate (power) for each N 
} 
powers
# 42 significant p-values in this group when running the simulation 
# but how many participants do I need indeed do get a significant p-value?



##########################################################
################### trying Power analysis ################
################# another try.......      ################

n.sim <- 100
mu <- c(0.5, -0.5, 0, 0)
sigma2 <- 1
n <- seq(from=10, to=50, by=1)  #formula does not recognize this term

g     <- length(mu)
group <- factor(rep(LETTERS[1:g], each = n))

results <- numeric(n.sim)

for(i in 1:n.sim){
  ## Simulate new response, build data set
  y <- rnorm(n * g, mean = rep(mu, each = n), sd = sqrt(sigma2))
  data <- data.frame(y = y, group = group)
  
  ## Fit one-way ANOVA model
  fit  <- aov(y ~ group, data = data)
  
  ## Extract result of global F-test
  results[i] <- summary(fit)[[1]][1, "Pr(>F)"] < 0.05 ## 1 = reject
}
mean(results)
# does not recognize the term for increasing n


mu     <- c(sample1_100, sample2_100, sample3_100, sample4_100)
sigma2 <-1
power.anova.test(groups = length(mu), n = n, between.var = var(mu), within.var = sigma2)
# well, it cannot be 1 for every nr of participants and the 100x random sampling are missing as well


# was ist das?
runs <- 100000
sims <- rnorm(runs,mean=1,sd=10)
mc.integral <- sum(sims >= 3 & sims <= 6)/runs


#defining our 4 groups 
group_size <- c(10:50,10:50,10:50,10:50)
means <- c(-.5, .5, 0, 0)
sds <- c(1,1,1,1)

size <- 10
plot_df <- data.frame()
power <- 0
while(power < 0.90) {
  power <- pwr.anova.test(k=100, n=size, f= 0.35, sig.level=0.05)$power
  plot_df <- rbind(plot_df, data.frame("n" = size, "power" = power))
  size <- size + 2
  print(power)
}


##########################################################
######################### t-Power ########################
#### (not what I am supposed to do, but sounds sweet) ###


# function for t-power
tPower <- function(size){
  out <- replicate(100, genTTest(size))
  mean(out < 0.05)
}

tPower

n <- 10:50

p.est <- sapply(n,tPower)

reps_smaple1 <- replicate(n=10, sample(sample1))
mean(sample1)

size <- c(10:50)
size
#? 
results1 <- sapply(size, function())


random.normal.100.rep <- replicate(n=4, rnorm(100, 5, 1))
mean(random.normal.100.rep)
sd(random.normal.100.rep)

?sapply1

power.t.test(n = 20, delta = 1, sd = 1, sig.level = 0.05)


#### trying t-test function 
# sample 1 (mu = 0, sd = 1)

rnorm(10:50, 0, 1)
n <- seq(from=10, to=50, by=1)

reps=100
result=array(dim=reps)
for(i in 1:reps){ 
  x=rnorm(n,0,1)
  result[i]=t.test(x, mu=0)$p.value 
}
x11()
hist(result)

x11()
plot(density(result, from=0, to=1)) 

# how many p-values were counted?
table(result<.05) # 4 true

# sample 2 (mu = 0, sd = 1)
rnorm(10:50, 0, 1)
n <- seq(from=10, to=50, by=1)

reps=100
result=array(dim=reps)
for(i in 1:reps){ 
  x=rnorm(n,0,1)
  result[i]=t.test(x, mu=0)$p.value 
}
x11()
hist(result)

x11()
plot(density(result, from=0, to=1)) 

# how many p-values were counted?
table(result<.05) # 1 true

# sample 3 (mu = -0.5, sd = 1)
rnorm(10:50, 0, 1)
n <- seq(from=10, to=50, by=1)

reps=100
result=array(dim=reps)
for(i in 1:reps){ 
  x=rnorm(n,0,1)
  result[i]=t.test(x, mu=-0.5)$p.value 
}
x11()
hist(result)

x11()
plot(density(result, from=0, to=1)) 

# how many p-values were counted?
table(result<.05) # 86 true



# sample 4 (mu = 0.5, sd = 1)
rnorm(10:50, 0, 1)
n <- seq(from=10, to=50, by=1)

reps=100
result=array(dim=reps)
for(i in 1:reps){ 
  x=rnorm(n,0,1)
  result[i]=t.test(x, mu=0.5)$p.value 
}
x11()
hist(result)

x11()
plot(density(result, from=0, to=1)) 

# how many p-values were counted?
table(result<.05) # 90 true

###### what is missing here is including Cohen's f 

# let's try two-group t-test
n1 <- seq(from=10, to=50, by=1)
n2 <- n1
delta <- 0.35 # expected effect size
alpha <- .05

B <- 100 # nr of Monte Carlo simulations 
ps <- c()
for (i in 1:B) {(
  x <- rnorm(n1, mean=0, sd=1)(
  y <- rnorm(n2, mean=delta, sd=1)(
  t1 <- t.test(x, y)
  ps <- c(ps, t1$p.value)(
}

prop.table(table(ps < alpha))(
print(paste0("Power = ", sum(ps < alpha)/B*100, "%"))


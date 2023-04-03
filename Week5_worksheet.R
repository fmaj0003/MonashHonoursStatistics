####### WEEK 5 WORKSHEET - GLM 2 #######

library(haven)
library(data.table)
library(JWileymisc)
library(ggplot2)
library(ggpubr)
library(visreg)
library(survey)

dcount <- fread("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")

### 1. Poisson ### 

# Fit a poisson regression predicting the number of awards, 
# `num_awards` from `prog` in the `dcount` data. Try to 
# interpret the results using IRRs and make a graph. 

# Note: "There is no association between prog and num_awards so
# do not be surprised if the graphs etc are not that interesting.")

## Plot the distribution of the outcome num_awards:
plot(testDistribution(dcount$num_awards, distr = "poisson"))

## fit poisson regression num_awards on prog:
#m <- glm(num_awards ~ math, data = dcount, family = poisson())

m <- glm(num_awards ~ prog, data = dcount, family = poisson())

## Show the results:
summary(m)

## calculate IRRs and confidence intervals on the IRR scale:
exp(coef(m))
exp(confint(m))

## graph and visualize the (very exciting) results.
# First in linear space (predicted log number of awards):
visreg(m, xvar = "prog", 
       partial = FALSE, rug = FALSE, gg = TRUE) +
  ylab("predicted log nummber of awards") +
  theme_pubr()
# then for predicted number of awards
visreg(m, xvar = "prog", scale = "response",
       partial = FALSE, rug = FALSE, gg = TRUE) +
  ylab("predicted number of awards") +
  theme_pubr()

## Pretend the result was significant. How would you write
# up the interpretation of the results? Say that 'prog'
# is just something like 'their progress in the class' (I
# actually have no idea)

# First write it up for log awards:

# A poisson regression predicting the number of awards each student
# received based on their progress in the class showed that students
# who had *0* progress were expected to have  log awards, p = *0.098*. 
# Each one unit higher of progress was associated with  *-0.00251*  lower log
# awards, p = *0.985* 

# Now write it up for the more interpret-able IRRs:

# A poisson regression predicting the number of awards each student 
# received based on their progress in the class showed that students 
# who had a 0 progress were expected to have *0.633* awards, 
# [95% CI *1.07*]. Each one unit higher progress was associated 
# with having *0.9974936* times the number of awards, [95% CI *1.285*], 
# p = *0.985* 

### 2. Binary logistic ### 

# Run the below code to make a new variable in dcount called 'unicorn'
# which fills it in with binary values to say whether or not someone
# is a unicorn. Note: this will be different for everyone
set.seed(666) #this is so that the fake-data generated is the same as michelle's
dcount$unicorn <- sample(0:1, size = nrow(dcount), replace = TRUE)

# Now use math scores to predict if someone is a unicorn.
mlog <- glm(unicorn ~ math, data = dcount, family = binomial())
summary(mlog)
exp(coef(mlog))
exp(confint(mlog))

# Pretend your result is significant. How would you write it
# up (in odds)?

# Odds:
# For each *one unit* on math, participants have ** times
# the ____ of ____ (p = 0.983)

# Visualise the probability of being a unicorn by math score
# (Note: this might not look very curvy in your data and
# that's ok):
visreg(mlog, xvar = "math", scale = "response", #double check if this line should say scale = "response"
       partial = FALSE, rug = FALSE, gg = TRUE) +
  ylab("probability of being unicorn") +
  theme_pubr()

# Calculate the average marginal effect for a one higher 
# math score predicting the probability of being a
# unicorn:

## pick h value for difference, store that in variable h
h <- .01

## make the original dataset
originaldata <- dcount[, .(math)]

## make the increased self esteem dataset (selfesteem + h)
increaseddata <- dcount[, .(math = math + h)]

## calculate original predicted probabilities
originaldata$Prob <- predict(mlog, newdata = originaldata,
                             type = "response")

## calculate increased predicted probabilities
increaseddata$Prob <- predict(mlog, newdata = increaseddata,
                              type = "response")

## calculate the difference in probabilities per unit
diffprob <- (increaseddata$Prob - originaldata$Prob) / h

## calculate the average marginal effect
mean(diffprob)

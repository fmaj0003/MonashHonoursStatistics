####### WEEK 7 WORKSHEET - LMM Intro  #######

# Which library do we need for the read_sav function?
library(haven) 

# Which library do we need to open the daily diary as a data table?
library(data.table) 

# Which three libraries did we need in the readings for mixed effect models?
library(lme4)
library(lmerTest)
library(multilevelTools)

# You will also need:
library(JWileymisc)
library(visreg)
library(ggplot2)
library(ggpubr)
library(tufte)

# Set your working directory to the folder that has the DD dataset
setwd("/Users/farhan/Git Repos/MonashHonoursStatistics")

# Load in the daily diary dataset and convert it to a data table
dd <- as.data.table(read_sav("[2021] PSY4210 DD.sav")) # daily diary

# Build a random intercept model predicting 'dEnergy'. 
m  <- lmer(dEnergy ~ 1 + (1 | ID), data = dd)
summary(m)
# Q: On average, these people had an energy score of 4.02
# A: ???

# Bonus: find a second way in the code to just display the answer above:
fixef(m)

# Try to clean the within and between level of the variable:
#  `dEnergy` in the daily dataset with missing IDs removed, 
# `dd2`. 
dd[, dEnergy := as.numeric(dEnergy)]
# First, make a between (BEnergy) and within (WEnergy) variable of dEnergy:
dd[!is.na(ID), c("BEnergy", "WEnergy") := meanDeviations(dEnergy), by = ID]
head(dd[!is.na(ID), .(BEnergy, WEnergy, dEnergy, ID)])

# Second, assess distribution of within energy. 
# If needed, excluded rows / IDs (make extreme value threshold 0.5%). 
plot(testDistribution(dd[!is.na(ID)]$WEnergy,
                      extremevalues = "theoretical", ev.perc = .005),
     varlab = "Within Energy (WEnergy)")

dd2 <- dd[!is.na(ID)] #removing the NA ID values once and for all
testDistribution(dd2$WEnergy,
                 extremevalues = "theoretical", ev.perc = .005)$Data[isEV == "Yes"]

# Show and remove the outliers based on row number ("OriginalOrder"):
dd2[c(265, 8, 87, 194, 201, 153), .(dEnergy, BEnergy, WEnergy, SurveyDay, ID)]
dd.noev <- dd2[-c(265, 8, 87, 194, 201, 153)]

plot(testDistribution(dd.noev$WEnergy,
                      extremevalues = "theoretical", ev.perc = .005), 
     varlab = "Within Energy (WEnergy)")

# Now recreate between and within levels of energy on the dataset 
# without extreme within values.
dd.noev[, c("BEnergy", "WEnergy") := meanDeviations(dEnergy), by = ID]

# Make a dataset with only one row of data per person to clean the between level
dd.b <- dd.noev[!duplicated(ID)]

# Examine the distribution of the between level and remove any outliers/IDs
plot(testDistribution(dd.b$BEnergy,
                      extremevalues = "theoretical", ev.perc = .005),
     varlab = "Between Energy (BEnergy)")

testDistribution(dd.b$BEnergy,
                 extremevalues = "theoretical", ev.perc = .005)$Data[isEV == "Yes"]
dd.b[c(83), .(dEnergy, BEnergy, WEnergy, SurveyDay, ID)]
dd.b.noev <- dd.b[-c(83)]
plot(testDistribution(dd.b.noev$BEnergy,
                      extremevalues = "theoretical", ev.perc = .005),
     varlab = "Between Energy (BEnergy)")
######## WEEK 10 (LMM Moderation) WORKSHEET ######## 

library(data.table)
library(JWileymisc)
library(extraoperators)
library(lme4)
library(lmerTest)
library(multilevelTools)
library(visreg)
library(ggplot2)
library(ggpubr)
library(haven)
library(emmeans)

## load data collection exercise data (merged)
dm <- as.data.table(read_sav("[2021] PSY4210 merged.sav"))

# Review: Model fixed effects of agreeableness and daily mood 
# predicting daily self esteem, with a random intercept
summary(lmer(dSE ~ agreeableness + dMood + (1 | ID), data = dm))

# Controlling for ________, 
# as mood increases, self esteem on the same day increases

# Model fixed effects of between and within versions of mood 
# predicting self esteem
dm[, c("Bmood", "Wmood") := meanDeviations(dMood), by = ID]
summary(lmer(dSE ~ Bmood + Wmood + (1 |ID), data = dm))

# Greater deviations from a person's average ____ are associated 
# with ____ in _____ on the same day

# Model fixed and random effects of mood predicting self esteem, 
# controlling for agreeableness
???

# Int_Part is a variable asking whether or not someone interacted with a current 
# or possible romantic partner

dm[, Int_Part := factor(
  Int_Part, levels = c(0,1), 
  labels = c("No int partner", "int partner"))]

dm[, relsta := factor(
  relsta, levels = c(1,2,3), 
  labels = c("single", "committed excl rel", "committed nonexcl rel"))]

# Model the interaction of Int_Part and relationship status predicting daily self esteem
m1 <- lmer(dSE ~ Int_Part + relsta + Int_Part:relsta  + (1 | ID), data = dm)
summary(m1)

summary(lmer(dSE ~ Int_Part * relsta + (1 | ID), data = dm ))


# Visualise this categorical x categorical interaction
visreg(m1, xvar = "Int_Part",
       by = "relsta", overlay = TRUE,
       partial = FALSE, rug = FALSE
         )

em <- emmeans(m1, "Int_Part", by "relsta", lmer.df = "satterthwaite"
                )
emmip(???) +
  theme_pubr() +
  ylab("???")

#' ---
#' title: "Exercise 3 - Mixed Models"
#' author: "Alexander Konvalina"
#' ---

# 6.2 packages and data
setwd("~/Documents/ETH/HS26/Experimental design/Ex3")
getwd()

#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("emmeans")
#install.packages("pbkrtest")
library(lme4)
library(lmerTest)
library(emmeans)

# file from moodle, saved in my Ex3 folder
Data <- read.table("DelleyDef.txt", sep = "\t", skip = 5, header = TRUE)
str(Data)

Data$Replicate <- as.factor(Data$Replicate)
Data$MainPlot <- as.factor(Data$MainPlot)
Data$SubPlot <- as.factor(Data$SubPlot)
Data$Genotype <- as.factor(Data$Genotype)

# 6.3 split-plot with GLM
lm.bad <- aov(DMY ~ Genotype + NLevel + Genotype:NLevel + Replicate, data = Data)
summary(lm.bad)

lm.good <- aov(DMY ~ Genotype + NLevel + Genotype:NLevel + Replicate +
                 Error(MainPlot), data = Data)
summary(lm.good)

# 6.4 mixed model, fix genotypes
mix1 <- lmer(DMY ~ Genotype + NLevel + Genotype:NLevel + Replicate +
               (1|MainPlot), data = Data)
summary(mix1)

#Q1
res <- residuals(mix1)
qqnorm(res)
qqline(res)
plot(mix1)
# in the qq plot most of the points are on the line, only at the ends some points
# go away from it. theres 2 outliers, one very low and one very high. so the
# residuals are more or less normal distributed.
# in the residuals vs fitted plot the points are spread around 0 without a pattern.
# for low fitted values the spread is a bit bigger than for high ones, but its not
# a real funnel shape. so i dont see a strong deviation from normality or
# homoscedasticity, only the 2 outliers.

# 6.4.1
anova(mix1)

# 6.4.2
names(summary(mix1))
VarCorr(mix1)

Fix <- fixef(mix1)
Fix
levels(Data$Genotype)

#Q2
# A310, NLevelH and Replicate1 are the first levels of their factors (alphabetical),
# so R sets their effect to 0 and all the other effects are estimated relative to
# them. thats why theyre not in Fix. if they were listed their effect would be 0.
# A310 under low N = intercept + NLevelL + replicate effect
# (genotype and interaction are 0 because A310 is the first level)
134.95 + (-25.16) + (-6.68)
# EC169 under high N = intercept + EC169 effect + replicate effect
# (NLevel and interaction are 0 because H is the first level)
134.95 + 19.49 + (-6.68)
# so A310 under low N has 103.11 dt/ha and EC169 under high N has 147.76 dt/ha

Ran <- ranef(mix1)
Ran
mean(unlist(Ran$MainPlot))

#Q3
# the mean is practically 0 (R gives a super small number), so rounded its 0.
# this was expected because random effects are assumed to be normal distributed
# with mean 0. so the main plot effects are just deviations around 0 and they
# cancel each other out.

# 6.4.3
#?emmeans
Means <- as.data.frame(emmeans(mix1, specs = ~Genotype))
Means
MeansPT <- as.data.frame(emmeans(mix1, specs = ~Genotype|NLevel))
MeansPT

# 6.5 mixed model, random genotypes
mix2 <- lmer(DMY ~ NLevel + Replicate + (1|Genotype) + (1|Genotype:NLevel)
             + (1|MainPlot), data = Data)
anova(mix2)
VarCorr(mix2)

Ranvar <- unlist(VarCorr(mix2))
Errorvar <- sigma(mix2)^2
names(Errorvar) <- "Residual"
Var <- c(Ranvar, Errorvar)
Var

#Q4
# genotype: 54.5, genotype x N interaction: 0, main plot: 208.1, residual: 167.5

CI <- confint(mix2)
CI
# squared to get the CI for the variances (only the first 4 rows matter)
CI^2

#Q5
# sigma from VarCorr(mix2), the CI from confint, sigma^2 and its CI are squared
# genotype: sigma = 7.38 (CI 4.94-10.40), sigma^2 = 54.5 (CI 24.4-108.2) -> yes
# genotype x N: sigma = 0 (CI 0-4.63), sigma^2 = 0 (CI 0-21.5) -> no
# main plot: sigma = 14.43 (CI 4.81-16.92), sigma^2 = 208.1 (CI 23.1-286.2) -> yes
# residual: sigma = 12.94 (CI 11.69-14.43), sigma^2 = 167.5 (CI 136.7-208.3) -> yes
# so genotype and main plot (and the residual) are significant, because their CI
# doesnt include 0. genotype x N is not significant, its CI starts at 0.
# in anova(mix1) where genotype was fix, genotype was also significant and
# genotype:NLevel was also not significant. so yes, same result.

# 6.5.1
Ran2 <- ranef(mix2)
Ran2
str(Ran2)
ranef(mix2)$Genotype
GRan <- ranef(mix2)$Genotype$"(Intercept)"

Means <- as.data.frame(emmeans(mix1, specs = ~Genotype))
GFix <- Means$emmean

plot(GFix, GRan, xlab="BLUE genotypic means", ylab="BLUP genotypic means")
fit <- lm(GRan ~ GFix)
abline(fit)
summary(fit)

#Q6
max(GFix) - min(GFix)
max(GRan) - min(GRan)
# the fit is almost perfect, R squared is 1 and all the points are on the line.
# the slope is 0.661. difference between max and min: BLUEs 33.45, BLUPs 22.12
# R squared = 1 means that BLUPs and BLUEs are perfectly on one line, so the
# genotypes have the same ranking in both. the BLUPs are just the BLUEs shrunken
# towards the mean.
# the slope 0.661 says how much they are shrunken (its the heritability). you also
# see it in the ranges, 22.12 / 33.45 = 0.661, so the range of the BLUPs is only
# 66% of the range of the BLUEs.

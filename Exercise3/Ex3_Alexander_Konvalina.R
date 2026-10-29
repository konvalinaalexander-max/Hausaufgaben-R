#' ---
#' title: "Exercise 3 - Mixed Models"
#' author: "Alexander Konvalina"
#' ---

# 6.2 Package installation and data loading
setwd("~/Documents/ETH/HS26/Experimental design/Ex3")
getwd()

# installed once in the console:
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("emmeans")
#install.packages("pbkrtest")
library(lme4)
library(lmerTest)
library(emmeans)

# i read the file from my folder (downloaded from moodle)
Data <- read.table("DelleyDef.txt", sep = "\t", skip = 5, header = TRUE)
str(Data)

Data$Replicate <- as.factor(Data$Replicate)
Data$MainPlot <- as.factor(Data$MainPlot)
Data$SubPlot <- as.factor(Data$SubPlot)
Data$Genotype <- as.factor(Data$Genotype)

# 6.3 Analysis of split-plot design with GLM
lm.bad <- aov(DMY ~ Genotype + NLevel + Genotype:NLevel + Replicate, data = Data)
summary(lm.bad)

lm.good <- aov(DMY ~ Genotype + NLevel + Genotype:NLevel + Replicate +
                 Error(MainPlot), data = Data)
summary(lm.good)

# 6.4 Analysis of split-plot design with mixed model, fix genotypes
mix1 <- lmer(DMY ~ Genotype + NLevel + Genotype:NLevel + Replicate +
               (1|MainPlot), data = Data)
summary(mix1)

# Q1 (2p)
res <- residuals(mix1)
qqnorm(res)
qqline(res)
plot(mix1)
# in the Q-Q plot most points are on the line, only at the two ends some points go
# away from it. especially 2 outliers, one very low and one very high residual
# (both B97 x UH007 under low N). so the residuals are roughly normal distributed.
# in the residuals vs fitted plot the points are spread around 0 without a clear
# pattern. the spread is a bit bigger for low fitted values (low N) than for high
# ones, but its not a strong funnel.
# so i dont see a strong deviation from normality and homoscedasticity, apart from
# the 2 outliers.

# 6.4.1 Tests for statistical significance
anova(mix1)

# 6.4.2 Variances, fix effects and random effects
names(summary(mix1))
VarCorr(mix1)

Fix <- fixef(mix1)
Fix
levels(Data$Genotype)

# Q2 (2p)
# A310 x UH007, NLevelH and Replicate1 are the first levels of their factors
# (alphabetical order). R sets the effect of the first level to 0 and estimates all
# other levels relative to it, so they dont show up in Fix. if they were listed,
# their effect would be 0.
# (so the intercept 134.95 is the yield of A310 under high N in replicate 1)
# A310 x UH007 under low N: intercept + NLevelL + replicate
# (genotype and interaction effect are 0, because A310 is the first level)
134.95 + (-25.16) + (-6.68)
# EC169 x UH007 under high N: intercept + genotype effect + replicate
# (NLevel and interaction effect are 0, because H is the first level)
134.95 + 19.49 + (-6.68)
# so A310 under low N = 103.11 dt/ha and EC169 under high N = 147.76 dt/ha.
# these are the same values as in MeansPT further down.

Ran <- ranef(mix1)
Ran
mean(unlist(Ran$MainPlot))

# Q3 (1p)
# R gives a really tiny number (e-13), so rounded the mean of the main-plot effects
# is 0. this was expected because random effects are assumed to follow a normal
# distribution with mean 0 (E(u) = 0). so the main-plot effects are only deviations
# around 0 and they cancel each other out (e.g. 3.16 and -3.16).

# 6.4.3 Prediction of treatment means
#?emmeans
Means <- as.data.frame(emmeans(mix1, specs = ~Genotype))
Means
MeansPT <- as.data.frame(emmeans(mix1, specs = ~Genotype|NLevel))
MeansPT

# 6.5 Analysis of split-plot design with mixed model, random genotypes
mix2 <- lmer(DMY ~ NLevel + Replicate + (1|Genotype) + (1|Genotype:NLevel)
             + (1|MainPlot), data = Data)
anova(mix2)
VarCorr(mix2)

Ranvar <- unlist(VarCorr(mix2))
Errorvar <- sigma(mix2)^2
names(Errorvar) <- "Residual"
Var <- c(Ranvar, Errorvar)
Var

# Q4 (1p)
# genotype: 54.5
# genotype-by-fertilizer interaction: 0
# main-plot: 208.1
# residual error: 167.5
# (variances in (dt/ha)^2)

CI <- confint(mix2)
CI
# CIs squared, to get the CIs of the variances (the first 4 rows are the sigmas)
CI[1:4, ]^2

# Q5 (2p)
#                     sigma                     sigma^2
#                     estimate  conf. int.      estimate  conf. int.       signif.?
# Genotype             7.38     4.94 - 10.40     54.5     24.4 - 108.2     yes
# Genotype-by-NLevel   0        0    - 4.63       0        0   - 21.5      no
# Main-Plot           14.43     4.81 - 16.92    208.1     23.1 - 286.2     yes
# Residual            12.94    11.69 - 14.43    167.5    136.7 - 208.3     yes
#
# a variance is significant if its CI doesnt include 0. so genotype and main-plot
# (and the residual) are significant, genotype-by-NLevel is not, its CI starts at 0
# (and the estimate is 0).
# compared to anova(mix1) where genotype and genotype:NLevel were fix: genotype was
# significant there too (p = 1.2e-05) and genotype:NLevel was not significant there
# too (p = 0.86). so yes, the result is the same for both.

# 6.5.1 Fix vs. random genotypic effects
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

# Q6 (2p)
min(GFix)
max(GFix)
max(GFix) - min(GFix)
min(GRan)
max(GRan)
max(GRan) - min(GRan)
(max(GRan) - min(GRan)) / (max(GFix) - min(GFix))
# the fit is basically perfect, R-squared is 1 and all points are on the line.
# the slope is 0.661.
# BLUEs go from 104.67 to 138.12 -> difference 33.45
# BLUPs go from -12.20 to 9.92 -> difference 22.12
# R2 = 1 means BLUPs and BLUEs are perfectly linearly related, so the ranking of the
# genotypes is the same for both. the BLUPs are just the BLUEs shrunken towards
# the mean.
# the slope 0.661 is the shrinking factor (it is the heritability,
# h2 = 54.47 / 82.38 = 0.661). the range of the BLUPs is 0.661 times the range of
# the BLUEs (22.12 / 33.45 = 0.661), so the BLUPs are closer together than the BLUEs.

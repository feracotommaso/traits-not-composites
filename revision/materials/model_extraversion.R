# Hierarchical model for extraversion (Figure 1 of the commentary)
# lavaan syntax + check on simulated data that the models are identified and run.
# The simulated data are a technical check only, not evidence about personality.

library(lavaan)
set.seed(1)

N = 1000

#### measurement part ####
# each facet is measured by 4 self-report items (suffix "s") and 4 informant-report items (suffix "i");
# one method factor per rater (MS, MI), uncorrelated with all other factors,
# so that rater-specific variance (e.g., acquiescence in self-reports) does not enter the facets or E
facets = c(W = "w", G = "g", A = "a", Ac = "c", X = "x", P = "p")
items = function(f, r) paste0(f, 1:4, r)
meas = function(lt = "", lm = "") paste0(
  paste0(names(facets), " =~ ", sapply(facets, function(f) paste0(lt, c(items(f, "s"), items(f, "i")), collapse = " + ")), collapse = "\n"), "\n",
  "MS =~ ", paste0(lm, sapply(facets, items, r = "s"), collapse = " + "), "\n",
  "MI =~ ", paste0(lm, sapply(facets, items, r = "i"), collapse = " + "), "\n",
  "MS ~~ 0*MI + 0*SA\n",
  "MI ~~ 0*SA\n")

#### population model (illustrative values) ####
pop = paste0(meas(lt = ".6*", lm = ".3*"), '
  E  =~ .6*W + .7*G + .6*A + .5*Ac + .6*X + .7*P
  SA =~ 1*A
  A ~~ 0*A
  SA ~~ .64*SA
  SA ~~ 0*E
  MS ~~ 0*E
  MI ~~ 0*E
  E  ~ .3*RS
  Y  ~ .3*E + .25*SA
')
df = simulateData(pop, sample.nobs = N)

#### model 1: hierarchical model of the commentary ####
# E = domain-common component; SA = assertiveness-specific component
# RS = measured candidate cause (e.g., reward positivity); Y = emergent leadership
# Paths from the other specific components to Y are fixed to zero: with one
# criterion, E and all six specific components cannot be estimated together
m1 = paste0(meas(), '
  E  =~ W + G + A + Ac + X + P
  SA =~ 1*A
  A ~~ 0*A
  SA ~~ 0*E
  MS ~~ 0*E
  MI ~~ 0*E
  E  ~ RS
  Y  ~ E + SA
')
fit1 = sem(m1, data = df)
summary(fit1, standardized = TRUE)
fitMeasures(fit1, c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr"))

#### model 2: common only (facet-criterion associations proportional to loadings) ####
m2 = sub("Y  ~ E + SA", "Y  ~ E", m1, fixed = TRUE)
fit2 = sem(m2, data = df)

#### model 2b: specific only (no path from E to the criterion) ####
m2b = sub("Y  ~ E + SA", "Y  ~ 0*E + SA", m1, fixed = TRUE)
fit2b = sem(m2b, data = df)

#### model 3: correlated facets, no domain factor ####
# equivalent to entering the facets as separate predictors
m3 = paste0(sub("MS ~~ 0*MI + 0*SA\nMI ~~ 0*SA\n", "MS ~~ 0*MI\n", meas(), fixed = TRUE), '
  W + G + A + Ac + X + P ~ RS
  W ~~ G + A + Ac + X + P
  G ~~ A + Ac + X + P
  A ~~ Ac + X + P
  Ac ~~ X + P
  X ~~ P
  Y ~ W + G + A + Ac + X + P
')
fit3 = sem(m3, data = df)

lavTestLRT(fit1, fit2)
lavTestLRT(fit1, fit2b)
lavTestLRT(fit3, fit1)

#### proportionality of the candidate cause: free direct paths RS -> facets ####
# one at a time: with RS -> E estimated, not all six direct paths can be freed together
m4 = sub("E  ~ RS", "E  ~ RS\n  A ~ RS", m1, fixed = TRUE)
fit4 = sem(m4, data = df)
lavTestLRT(fit1, fit4)

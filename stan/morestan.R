ngroups = 10
nsubj = 500
balanced = TRUE

# data generation function
# x groups with x observations
# response is fixed on group-level, i.e. all subjects within a group have the same response
# there is also one lower-level predictor ('control variable') that affects the actual primary predictor
# data generation process: (lowpred -> prim. predictor) -> response
# returns a list with the first item being a data.frame and the second item a list that can be used with rstan

# ngroups: number of groups/countries/languages
# nsubjects: number of subjects/respondents
# balanced: create a balanced data set (approx. same number of subjects per group) or not

# fix lowlevel model for each predictor: intercept and slope; and group-level variance for intercept and slopes
# this is a list of lists (for future extensions with multiple low-level predictors)
lowlevel <- list(pred1 = list(intercept = 2, slope = -1, intvar = 1.2, slovar = 0.4, error = 1.7))
# global parameters
global <- list(intercept = 2, slope = 1.5, error = 1.3)

gendata <- function(ngroups = 10, nsubj = 500, balanced = TRUE,
                    global = NULL, lowlevel = NULL) {
  # set default parameters if none were specified
  if(is.null(global)) global <- list(intercept = 2, slope = 1.5, error = 1.3)
  if(is.null(lowlevel)) lowlevel <- list(pred1 = list(intercept = 2, slope = -1, intvar = 1.2, slovar = 0.4, error = 1.7))

  if(balanced) {
    xdata <- data.frame(group = sample(1:ngroups, nsubj, replace = TRUE))
  } else {
    xdata <- data.frame(group = sample(1:ngroups, nsubj, replace = TRUE, prob = log((1:ngroups) + 1)))
  }

  # create low-level predictor (e.g. SES)
  xdata$lowpred <- rnorm(n = nsubj, mean = 0, sd = 1)

  # make primary predictor(s) (based on lowpred); still a subject-level variable
  for(i in 1:length(lowlevel)) {
    pars <- lowlevel[[i]]
    # create per-group parameters
    predint <- pars$intercept + rnorm(ngroups, mean = 0, sd = pars$intvar)
    predslopes <- pars$slope + rnorm(ngroups, mean = 0, sd = pars$slovar)
    # add to data frame and rename
    xdata$temp <- predint[xdata$group] + predslopes[xdata$group] + rnorm(nsubj, mean = 0, sd = pars$error)
    colnames(xdata)[ncol(xdata)] <- names(lowlevel)[i]
  }

#
#   # add group-specific intercepts and slopes to data frame for 'lowpred -> primarypred'
#   xdata$lowpredint <- rnorm(n = ngroups, mean = 0, sd = primintercept)[xdata$group]
#   xdata$lowpredslope <- rnorm(n = ngroups, mean = 0, sd = primslope)[xdata$group]
#   # model primpred
#   xdata$primpred <- 0 + xdata$lowpredint +
#     (0 + xdata$lowpredslope) * xdata$lowpred +
#     rnorm(n = nsubj, mean = 0, sd = 1)


  # make response variable
  # model response as simple linear model of group-level means of primpred
  resp <- global$intercept +
    global$slope * tapply(xdata$pred1, INDEX = xdata$group, FUN = mean) +
    rnorm(n = ngroups, mean = 0, sd = global$error)
  xdata$resp <- resp[xdata$group]

  # prep data set as list (for rstan)
  # locmat: 0/1 matrix for indexing (nsubj x ngroups)
  xd <- list(Ngroups = length(unique(xdata$group)),           # number of groups
             Ncases = nrow(xdata),                            # number of total observations
             grouplevelresponse = resp,                       # response (one value per group)
             subjectlevelpred = xdata$pred1,                  # primary predictor (one val per subject)
             subjectlevellowpred = xdata$lowpred,             # low-level predictor (one val per subject)
             Ncasespergroup = as.numeric(table(xdata$group)), # number of cases per group
             subjectlevelgroup = xdata$group,                 # group membership (one val per subject)
             locmat = sapply(sort(unique(xdata$group)), function(X)as.numeric(xdata$group == X)))
  # grouplevelpred = as.numeric(tapply(xdata$pred1, xdata$group, mean)),
  # grouplevelsd = as.numeric(tapply(xdata$pred1, xdata$group, sd)),

  return(list(df = xdata, stanlist = xd))
}

library(rstan)
library(arm)
options(mc.cores = parallel::detectCores()-1)

# generate data
lowlevel <- list(pred1 = list(intercept = -0.5, slope = -1, intvar = 1.2, slovar = 0.4, error = 1.7))
global <- list(intercept = -3, slope = -1.5, error = 2.3)
xdata <- gendata(ngroups = 20, nsubj = 1000, balanced = TRUE, global = global, lowlevel = lowlevel)


fit1 <- stan(file = "morestan-mod1.stan", data = xdata$stanlist, iter = 4000, chains = 3, cores = 3)

# fit the OLS model
ldata <- xdata$df
plot(tapply(ldata$resp, ldata$group, mean) ,tapply(ldata$pred1, ldata$group, mean))
display(lm(tapply(ldata$resp, ldata$group, mean) ~ tapply(ldata$pred1, ldata$group, mean)))
print(fit1, pars = c("intercept", "slope", "sigma"))
sd(colMeans(extract(fit1)$g_sigma))
mean(colMeans(extract(fit1)$g_sigma))
sd(colMeans(extract(fit1)$g_mean))
mean(colMeans(extract(fit1)$g_mean))


par(mfrow = c(1, 2))
x <- colMeans(extract(fit1)$g_sigma)
y <- tapply(xdata$subjectlevelpred, xdata$subjectlevelgroup, sd)
plot(x, y, main = "group-level SD", xlab = "Stan", ylab = "observed data")
abline(0, 1)

x <- colMeans(extract(fit1)$g_meanval)
y <- tapply(xdata$subjectlevelpred, xdata$subjectlevelgroup, mean)
plot(x, y, main = "group means", cex = sqrt((xdata$Ncasespergroup * 0.5)/pi), xlab = "Stan", ylab = "observed data")
abline(0, 1)

# stan_dens(fit1, "slope")

# varying intercept, no varying slopes
fit2 <- stan(file = "morestan-mod2.stan", data = xdata$stanlist, iter = 1000, chains = 3)
print(fit2, pars = c("intercept", "slope", "sigma", "sigma_group"))

# varying intercept and varying slopes for 'group'
fit3 <- stan(file = "morestan-mod3.stan", data = xdata$stanlist, iter = 1000, chains = 3)
print(fit3, pars = c("intercept", "slope", "sigma", "sigma_group"))



res2 <- lmer(primpred ~ lowpred + (lowpred||group), data = xdata$df, REML = FALSE)



xdata <- gendata(ngroups = 20, nsubj = 600, balanced = FALSE, globalintercept = -5, globalslope = 2.4, primintercept = 1, primslope = 0, makelist = FALSE)
library(brms)
library(lme4)
res <- brm(primpred ~ lowpred + (lowpred||group), data = xdata, family = gaussian, chains = 3)
res2 <- lmer(primpred ~ lowpred + (lowpred||group), data = xdata, REML = FALSE)
summary(res)
summary(res2)



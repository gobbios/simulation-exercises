library(lme4)
library(MASS)
source("helper_functions/multicor.R")



nind <- 150
nperid <- 150

# backbone
xdata <- expand.grid(id = 1:nind, obsn = 1:nperid)
# predictor 1
xdata$pred1 <- as.numeric(scale(rnorm(n = nrow(xdata)))) # just make sure it's really centered at 0
# 'empty' predictor
xdata$response <- NA

# set parameters
# fixed effects
global_intercept <- -3
global_slope_pred1 <- 0.6
# global error/residuals
global_error <- 1.5

# random effects
subject_pred1_intercept_sd <- 1.3
subject_pred1_slope_sd <- 0.7
subject_pred1_cor <- -0.4

# create random effects
ran_ef1 <- ref(n = nind,
               xcor = subject_pred1_cor,
               sd1 = subject_pred1_intercept_sd,
               sd2 = subject_pred1_slope_sd,
               empirical = TRUE)

cor(ran_ef1[, 1], ran_ef1[, 2]) # check

# subject level data
idata <- data.frame(id = 1:nind,
                    pred1_intercept = ran_ef1[, 1],
                    pred1_slope = ran_ef1[, 2])

# create data
for (i in 1:nind) {
  # predictor values for ID i
  temp1 <- xdata$pred1[xdata$id == i]

  resp <- global_intercept +
    idata$pred1_intercept[i] +
    temp1 * (global_slope_pred1 + idata$pred1_slope[i]) +
    rnorm(length(temp1), sd = global_error)
  # put into xdata
  xdata$response[xdata$id == i] <- resp
  rm(resp)
}
xdata$id <- as.factor(xdata$id)
summary(xdata)


res <- lmer(response ~ pred1 + (pred1|id), data = xdata, control = lmerControl(optimizer = "bobyqa"))
data.frame(VarCorr(res))
x <- data.frame(VarCorr(res))
x$setup <- c(subject_pred1_intercept_sd, subject_pred1_slope_sd, subject_pred1_cor, global_error)
x
# should be close to zero...
x$sdcor - x$setup





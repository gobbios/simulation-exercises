library(lme4)
library(MASS)
source("helper_functions/multicor.R")



nind <- 100
nperid <- 500

# backbone
xdata <- expand.grid(id = 1:nind, obsn = 1:nperid)
# predictor 1 and 2
xdata$pred1 <- as.numeric(scale(rnorm(n = nrow(xdata)))) # just make sure it's really centered at 0
xdata$pred2 <- as.numeric(scale(xdata$pred1^2)) # just make sure it's really centered at 0
# 'empty' predictor
xdata$response <- NA

# set parameters
# fixed effects
global_intercept <- -3
global_slope_pred1 <- 0.6
global_slope_pred2 <- -0.7
# global error/residuals
global_error <- 1.5

# random effects
subject_pred1_intercept_sd <- 1.3
subject_pred1_slope_sd <- 0.7
subject_pred1_cor <- -0.4
subject_pred2_intercept_sd <- 1.1
subject_pred2_slope_sd <- 0.5
subject_pred2_cor <- 0.3

# create random effects
ran_ef1 <- ref(n = nind,
               xcor = subject_pred1_cor,
               sd1 = subject_pred1_intercept_sd,
               sd2 = subject_pred1_slope_sd,
               empirical = TRUE)
ran_ef2 <- ref(n = nind,
               xcor = subject_pred2_cor,
               sd1 = subject_pred2_intercept_sd,
               sd2 = subject_pred2_slope_sd,
               empirical = TRUE)

cor(ran_ef1[, 1], ran_ef1[, 2]) # check
cor(ran_ef2[, 1], ran_ef2[, 2]) # check

# subject level data
idata <- data.frame(id = 1:nind,
                    pred1_intercept = ran_ef1[, 1],
                    pred1_slope = ran_ef1[, 2],
                    pred2_intercept = ran_ef2[, 1],
                    pred2_slope = ran_ef2[, 2])

# create data
for (i in 1:nind) {
  # predictor values for ID i
  temp1 <- xdata$pred1[xdata$id == i]
  temp2 <- xdata$pred2[xdata$id == i]
  resp <- global_intercept +
    idata$pred1_intercept[i] +
    idata$pred2_intercept[i] +
    temp1 * (global_slope_pred1 + idata$pred1_slope[i]) +
    temp2 * (global_slope_pred2 + idata$pred2_slope[i]) +
    rnorm(length(temp1), sd = global_error)
  # put into xdata
  xdata$response[xdata$id == i] <- resp
  rm(resp)
}
xdata$id <- as.factor(xdata$id)
summary(xdata)


res <- lmer(response ~ pred1 + pred2 + (pred1|id) + (pred2|id), data = xdata, control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
res <- lmer(response ~ pred1 + pred2 + (pred1|id) + (pred2|id), data = xdata, control = lmerControl(optimizer = "bobyqa"))
VarCorr(res)
fixef(res)
res <- lmer(response ~ pred1 + I(pred1^2) + (pred1|id) + (I(pred1^2)|id), data = xdata, control = lmerControl(optimizer = "bobyqa"))
VarCorr(res)
fixef(res)
x <- data.frame(VarCorr(res))
x$setup <- c(subject_pred1_intercept_sd, subject_pred1_slope_sd, subject_pred1_cor,
             subject_pred2_intercept_sd, subject_pred2_slope_sd, subject_pred2_cor,
             global_error)
x
x$sdcor - x$setup

plot(xdata$pred1, xdata$response)

# look at deviations between set parameters and modelled parameters
# select a subset of subjects
testsubs <- sample(idata$id, 6)
par(mfrow = c(2, 3))

colours <- hcl.colors(n = length(testsubs))
i = 1
for (i in 1:length(testsubs)) {
  plot(xdata$pred1, xdata$response, type = "n")
  xline <- which(idata$id == testsubs[i])

  # data for predictions
  newdata <- data.frame(pred1 = seq(-4, 4, 0.1))
  newdata$pred2 <- as.numeric(scale(newdata$pred1^2))
  newdata$id <- testsubs[i]

  # global slope (same for each individual...)
  pdata <- predict(res, newdata = newdata, re.form = NA)
  points(newdata$pred1, pdata, type = "l", col = "grey")

  # predicted from model for individual i
  pdata <- predict(res, newdata = newdata)
  points(newdata$pred1, pdata, type = "l", lty = 2, col = colours[i], lwd = 2)


  # predicted by initial parameters
  newdata$response <- global_intercept +
    idata$pred1_intercept[xline] +
    idata$pred2_intercept[xline] +
    newdata$pred1 * (global_slope_pred1 + idata$pred1_slope[xline]) +
    newdata$pred2 * (global_slope_pred2 + idata$pred2_slope[xline])
  points(newdata$pred1, newdata$response, type = "l", col = colours[i], lwd = 2)


}








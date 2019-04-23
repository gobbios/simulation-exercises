# ngroups = 15
# nsubj = 500
# outlier = FALSE
# globalintercept = 2
# globalslope = 1.5
# balanced = TRUE

# data generation function
# x groups with x observations
# response is fixed on group-level, i.e. all subjects within a group have the same response
gendata <- function(ngroups = 10, nsubj = 500, balanced = TRUE, globalintercept = 2, globalslope = 1.5, outlier = FALSE) {
  if(balanced) {
    xdata <- data.frame(pred1 = rnorm(nsubj, mean = 0), group = sample(1:ngroups, nsubj, replace = TRUE))
  } else {
    xdata <- data.frame(pred1 = rnorm(nsubj, mean = 0), group = sample(1:ngroups, nsubj, replace = TRUE, prob = log((1:ngroups) + 1)))
  }
  # create one 'outlier' group
  outliergroup <- sample(1:ngroups, 1)
  if(outlier) xdata$pred1[xdata$group == outliergroup] <- rnorm(sum(xdata$group == outliergroup), mean = 0.8)
  # add group effects
  xdata$groupintercept <- rnorm(ngroups)[xdata$group]
  xdata$groupslope <- rnorm(ngroups, sd = 0.1)[xdata$group]
  # create subject-level response variable
  xdata$resp <- globalintercept + xdata$groupintercept +
    (globalslope + xdata$groupslope) * xdata$pred1 +
    rnorm(nsubj, sd = 0.1)
  # group-level response (same for all subjects within a group)
  xdata$resp2 <- tapply(xdata$resp, xdata$group, mean)[xdata$group]
  # prep data set as list (for rstan)
  # locmat: 0/1 matrix for indexing (nsubj x ngroups)
  xd <- list(Ngroups = length(unique(xdata$group)),
             Ncases = nrow(xdata),
             grouplevelresponse = as.numeric(tapply(xdata$resp, xdata$group, mean)),
             subjectlevelpred = xdata$pred1,
             Ncasespergroup = as.numeric(table(xdata$group)),
             # grouplevelpred = as.numeric(tapply(xdata$pred1, xdata$group, mean)),
             # grouplevelsd = as.numeric(tapply(xdata$pred1, xdata$group, sd)),
             subjectlevelgroup = xdata$group,
             locmat = sapply(sort(unique(xdata$group)), function(X)as.numeric(xdata$group == X)))
  return(xd)
}

xdata <- gendata(ngroups = 10, nsubj = 200, balanced = FALSE, globalintercept = -5, globalslope = 2.4)
lm(xdata$grouplevelresponse ~ tapply(xdata$subjectlevelpred, xdata$subjectlevelgroup, mean))



mod <- "
data {
  int<lower=0> Ngroups;
  int<lower=0> Ncases;
  vector[Ngroups] grouplevelresponse;
  int Ncasespergroup[Ngroups];
  vector[Ncases] subjectlevelpred;
  matrix[Ncases, Ngroups] locmat;
}
parameters {
  real intercept;
  real slope;
  real<lower=0> sigma;
  vector<lower=0>[Ngroups] g_sigma;
  vector[Ngroups] g_meanval;
}
model {
  for (n in 1:Ngroups) {
    // get subject-level data fro the predictor
    vector[Ncasespergroup[n]] tempdata;
    int pos;
    pos = 1;
    for (i in 1:rows(locmat)) {
      if (locmat[i, n]) {
        tempdata[pos] = subjectlevelpred[i];
        pos = pos + 1;
      }
    }
    // model group level predictor
    //
    g_sigma[n] ~ normal(1, 3);
    g_meanval[n] ~ normal(tempdata, g_sigma[n]);
    // the actual model on group level
    grouplevelresponse[n] ~ normal(intercept + slope * g_meanval[n], sigma);
  }
}
"
library(rstan)
options(mc.cores = parallel::detectCores()-1)
fit1 <- stan(model_code = mod, data = xdata, iter = 4000, chains = 3)
# fit the OLS model
coef(lm(xdata$grouplevelresponse ~ tapply(xdata$subjectlevelpred, xdata$subjectlevelgroup, mean)))
print(fit1, pars = c("intercept", "slope", "sigma"))


par(mfrow = c(1, 2))
x <- colMeans(extract(fit1)$g_sigma)
y <- tapply(xdata$subjectlevelpred, xdata$subjectlevelgroup, sd)
plot(x, y, main = "group-level SD")
abline(0, 1)

x <- colMeans(extract(fit1)$g_meanval)
y <- tapply(xdata$subjectlevelpred, xdata$subjectlevelgroup, mean)
plot(x, y, main = "group means", cex = sqrt((xdata$Ncasespergroup * 0.5)/pi))
abline(0, 1)

# stan_dens(fit1, "slope")

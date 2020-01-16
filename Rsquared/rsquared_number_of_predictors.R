
res <- data.frame(n = 1:200, rs1 = NA, rs2 = NA, rs3 = NA)

for (i in 1:nrow(res)) {
  # set sample size
  n <- sample(20:200, 1)
  # create data set
  xdata <- data.frame(resp = rnorm(n),
                      pred1 = rnorm(n),
                      pred2 = rnorm(n),
                      pred3 = rnorm(n))
  # fit two models
  m1 <- lm(resp ~ pred1, data = xdata)
  m2 <- lm(resp ~ pred1 + pred2, data = xdata)
  m3 <- lm(resp ~ pred1 + pred2 + pred3, data = xdata)
  res$rs1[i] <- summary(m1)$r.squared
  res$rs2[i] <- summary(m2)$r.squared
  res$rs3[i] <- summary(m3)$r.squared
}


boxplot(res$rs1, res$rs2, res$rs3)
boxplot(log(res$rs1), log(res$rs2), log(res$rs3))
mean(res$rs2 - res$rs1)
mean(res$rs3 - res$rs1)


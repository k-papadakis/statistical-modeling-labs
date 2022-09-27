# II (i)
library(car)
library(hnp)
library(pROC)

leuk <- read.table("./data/leukaemia.txt", header = TRUE)
mod <- glm(
  response ~ .,
  family = binomial, data = leuk
)

summary(mod)  # Includes Wald and AIC
ddev <- mod$null.deviance - mod$deviance
ddf <- mod$df.null - mod$df.residual
pvalue_ddev <- 1 - pchisq(ddev, ddf, lower.tail = FALSE)
pvalue_dev <- pchisq(mod$deviance, mod$df.residual, lower.tail = FALSE)
pvalue_ddev  # Difference in deviance with the constant model
pvalue_dev  # Deviance

# II (ii)
crPlots(mod)  # Partial residual plots

hnp(mod)  # Deviance residuals (half-normal plot)

plot(hatvalues(mod), ylab = "Hat values")  # h_{ii} index plot
p <- length(mod$coefficients)
n <- length(mod$fitted.values)
abline(h = 2*p/n)

plot(cooks.distance(mod), ylab = "Cook's Distance")  # Cook's Distance
abline(h = c(0.5, 1), lty = 2)

# Likelihood Residuals
par(mfrow = c(1, 2))
plot(rstudent(mod), ylab = "Likelihood Residuals")
abline(h = 0)
plot(hatvalues(mod), rstudent(mod),
     x_lab = "Hat values", ylab = "Likelihood Residuals")
abline(h = 0)
par(mfrow = c(1,1))

# II (iii)
confint(mod, level = 0.95)

# II (iii)
roc(leuk$response, fitted.values(mod), smooth=TRUE, plot=TRUE)
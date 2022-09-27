####### I (i)
library(ggplot2)
insurances <- read.table("./data/asfalies.txt", header = TRUE)
# factor_columns <- c("cartype", "agecat", "district")
# insurances[factor_columns] <- lapply(insurances[factor_columns], as.factor)
insurances$cartype <- as.factor(insurances$cartype)
mod <- glm(
  y ~ agecat + cartype + district, offset = log(n),
  family = poisson, data = insurances
)
summary(mod)  # Includes Wald and AIC
logLik(mod)  # Wasn't asked
ddev <- mod$null.deviance - mod$deviance
ddf <- mod$df.null - mod$df.residual
pvalue_ddev <- pchisq(ddev, ddf, lower.tail = FALSE)
pvalue_dev <- pchisq(mod$deviance, mod$df.residual, lower.tail = FALSE)
pvalue_ddev  # Difference in deviance with the constant model
pvalue_dev  # Deviance

####### I (ii)
confint(mod, level = 0.95)  # Confidence interval for the coefficients

####### I (iii)
r_pearson <- residuals(mod, type = "pearson")
r_deviance <- residuals(mod, type = "deviance")

PlotResiduals <- function (r, name) {
  plot(mod$fitted.values, r,
     xlab = "Fitted Values", ylab = name)
  abline(h = 0)
  qqnorm(r, main = paste("Normal Q-Q Plot for the", name))
  qqline(r)
}

PlotResiduals(r_pearson, "Pearson Residuals")
PlotResiduals(r_deviance, "Deviance Residuals")

plot(hatvalues(mod), ylab = "Hat values")
p = length(mod$coefficients)
n = length(mod$fitted.values)
abline(h = 2*p/n)

plot(cooks.distance(mod), ylab = "Cook's Distance")
abline(h = c(0.5, 1), lty = 2)

par(mfrow = c(1, 2))
plot(rstudent(mod), ylab = "Likelihood Residuals")
abline(h = 0)
plot(hatvalues(mod), rstudent(mod),
     x_lab = "Hat values", ylab = "Likelihood Residuals")
abline(h = 0)

####### I (iv)
# TODO: Explain results
mod2 <- glm(
  y ~ agecat + cartype + district + cartype:district, offset = log(n),
  family = poisson, data = insurances
)
# cartype:district is the only combination which creates a variable with p-value < 0.05
summary(mod2)

ggplot(insurances, aes(district, y/n, color = cartype)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

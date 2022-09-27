library("car")
library("corrplot")
library("olsrr")
library("glmnet")

vehicles_full <- read.table("./data/vehicles.txt", header = TRUE)

# We can convert the "vs" and "am" to factors, but since they only have two levels,
# with value 0 and 1, we can keep them as is.
# factor_columns <- c("vs", "am")
# df_full[factor_columns] <- lapply(df_full[factor_columns], as.factor)

vehicles <- vehicles_full[-1]
mod <- lm(mpg ~ ., vehicles)

# -------- A1 --------

# We see that there's large simple collinearity between variables
corr_matrix <- cor(vehicles)
corrplot(corr_matrix)

# The variance inflation factor for almost all variables is above 5
# Which means that there's large collinearity among certain subsets of our variables.
vif_values <- vif(mod)
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)

# From the Residuals vs Fitted, we see that the line is not far from horizontal,
# and that the spread around it is roughly homogenous. Thus, homoscedacity seems to hold.
# The QQ plot doesn't look great. We might need to transform the predicted variable.
# From the Residuals vs leverage plot, we see that the two most influential samples
# are 29 and 9. (We'll also calculate the leverages explicitely in the next step)
par(mfrow=c(2,2))
plot(mod)
par(mfrow=c(1,1))

# We see that according to the 2*p/n criterion, sample 9 is the only sample
# which we can be considered influential.
leverages <- hatvalues(mod)
influential_threshold <- 2 * (ncol(vehicles) - 1) / nrow(vehicles)
barplot(leverages, main = "Leverages", col = "steelblue")
abline(h = influential_threshold, lwd = 3, lty = 2)
print(leverages[9])

# According to Cook's distance, sample 9 is relatively influential,
# although its corresponding value is not greater than 1.
cooks <- cooks.distance(mod)
cooks_thresshold <- c(0.5, 1)
barplot(cooks, main = "Cook's Distances", col = "steelblue", ylim = c(0, max(cooks, 1.2)))
abline(h = cooks_thresshold, lwd = 3, lty = 2)


# DFFITS also shows us that the most influential points are 9 and 29
dffits_values <- dffits(mod)
fthresh <- 2 * sqrt((ncol(vehicles) - 1) / nrow(vehicles))
ylim <- max(fthresh, abs(dffits_values)) + 0.2
barplot(dffits_values, main = "DFFITS", col = "steelblue", ylim = c(-ylim, ylim))
abline(h = c(-fthresh, fthresh), lwd = 3, lty = 2)

# DFBETAS norms also show us that samples 9 and 29 are influential.
dfbetas_values <- dfbetas(mod)
dfbetas_norms <- apply(dfbetas_values, 1, function(v){sqrt(sum(v^2))})
bthresh <- 2 * sqrt((ncol(vehicles) - 1) / nrow(vehicles))
ylim <- max(bthresh, abs(dfbetas_norms))
barplot(dfbetas_norms, main = "DFBETAS L2 Norm", col = "steelblue",
         ylim = c(0, max(bthresh, dfbetas_norms)+0.2))
abline(h = c(0, bthresh), lwd = 3, lty = 2)
# We can also view them coordinate by coordinate
dfbetasPlots(mod, layout=c(3, 4))


# -------- A2 --------
# attach(df)
# print("-----FORWARD-----")
# mod_forward <- step(
#    lm(mpg ~ 1),
#    scope = mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb,
#    direction = "forward",
#    test = c("F"),
#    trace = 100,
# )
# print("-----BACKWARD-----")
# mod_backward <- step(
#    lm(mod),
#    scope = NULL,
#    direction = "backward",
#    test = "F",
#    trace = 100,
# )
# print("-----BOTH-----")
# mod_both<- step(
#    lm(mpg ~ 1),
#    scope = mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb,
#    direction = "both",
#    test = "F",
#    trace = 100,
# )
# detach()

# Since our variables are only 10, and our samples only 32,
# we can simply try out all possible 2^10 - 1 = 1023 models
# and pick the best one according to some measure (e.g. AIC).
steps_all <- ols_step_all_possible(mod)
step_min_aic <- steps_all[which.min(steps_all$aic),]
print.data.frame(step_min_aic)

# Fitting the model using the best parameters
best_vars <- strsplit(step_min_aic$predictors, " ")[[1]]
best_formula1 <- as.formula(paste("mpg", "~", paste0(best_vars, collapse = "+")))
best_mod1 <- lm(best_formula1, vehicles)
# Show the F-test and t-test results (all significant)
summary(best_mod1)


# -------- A3 --------
# Comparing old with new
anova(mod, best_mod1, test = "F")

# Original
par(mfrow=c(2,2))
plot(best_mod1)
par(mfrow=c(1,1))
# Log transformed y.
# QQ plot looks better. Residual plots look good. The only influential point seems to be 17.
# The values for sample 17 look alright, although I am not a car expert.
# R^2 went from 0.8497 to 0.8752
best_formula2 <- as.formula(paste("log(mpg)", "~", paste0(best_vars, collapse = "+")))
best_mod2 <- lm(best_formula2, vehicles)
par(mfrow=c(2,2))
plot(best_mod2)
par(mfrow=c(1,1))
summary(best_mod2)
# Using l1 and l2 regularization combined. gmlnet tries multiple values for lambda
# We notice the the smaller the lambda gets, the better the R^2 value (%Dev)
# with the maximum reached at 87.52, which is the same as the original model.
# This results indicates that our model is simple enough to the point where
# it doesn't need any regularization.
y <- log(vehicles$mpg)
x <- vehicles[best_vars]
elastic_mod <- glmnet(x, y, alpha = 0.2, family = "gaussian")
sprintf("Maximum R squared reached: %.4f", max(elastic_mod$dev.ratio))
plot(elastic_mod$lambda, elastic_mod$dev.ratio, type = "l", lty = 1,
     main = "Elastic Net", xlab = "lambda", ylab = "R squared")

# We can see that there's much less multicollinearity now
barplot(vif(best_mod2), main = "VIF Values", horiz = TRUE, col = "steelblue", xlim = c(0, 6))
abline(v = 5, lwd = 3, lty = 2)

# Indeed, 17 is the only influential point.
dffits_values <- dffits(best_mod2)
fthresh <- 2 * sqrt((length(best_mod2$coefficients) - 1) / nrow(vehicles))
ylim <- max(fthresh, abs(dffits_values)) + 0.2
barplot(dffits_values, main = "DFFITS", col = "steelblue", ylim = c(-ylim, ylim))
abline(h = c(-fthresh, fthresh), lwd = 3, lty = 2)

# Added variable plots
# We see that there is a linear relationship between the predicted variable
# and each predictor variable, separately. This means that all variables belong to the model.
avPlots(best_mod2)

# Partial residual plots
# For each variable we have that the residual and the component curves are
# relatively close to each other, so all the variables belong to the model.
crPlots(best_mod2)

# Confidence intervals for the model's coefficients.
cbind(
  best_mod2$coefficients,
  confint(best_mod2, level = 0.95)
)
# Synthetic data
synthetic_x <- list(wt = 1.6, qsec = 18.5, am = 1)
# Confidence interval of the expected value of the prediction
log_conf <- predict(best_mod2, synthetic_x, interval = "confidence", level = 0.95)
conf <- exp(log_conf)
conf
# Prediction interval of the value of the prediction
log_pred <- predict(best_mod2, synthetic_x, interval = "prediction", level = 0.95)
pred <- exp(log_pred)
pred
# Interpretation of the coefficients:
# **wt** in [-0.29, -0.15] means that the heavier the vehicle,
# the fewer miles it can travel given one gallon (-wt miles less)
# **qsec** in [0.026, 0.089] means that faster vehicles, can travel longer
# distances given one gallon (qsec miles more).
# This might be due the slow vehicles in the dataset being trucks.
# **am** in [-0.04, 0.21] means that manual vehicles tend to be able to travel
# more miles given one gallon.
# The confidence intervals are quite large, which might be due to the fact
# that the skill of manual car drivers varies a lot
# Now, considering the interval for **E[y]** and **y**, we see that our synthetic point
# is similar to sample 19 which has 30.4 mpg. Our prediction for the synthetic point is 30.15
# which is pretty much what we'd expect, although the interval lengths are very large.

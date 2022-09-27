library("olsrr")
library("ggplot2")

canary <- read.table("./data/canary.txt", header = TRUE)
canary$group <- as.factor(canary$group)

# This model uses 1 for B and 0 for A.
mod <- lm(Temp ~ pulses * group, data = canary)
steps_all <- ols_step_all_possible(mod)
print(steps_all[order(steps_all$cp),])

best_vars <- steps_all[which.min(steps_all$cp),]$predictors
best_vars <- strsplit(best_vars, " ")[[1]]
best_formula <- as.formula(paste("Temp", "~", paste0(best_vars, collapse = "+")))
best_mod <- lm(best_formula, data = canary)
summary(best_mod)

# (When we view our data by group, `group` is essentially part of the intercept, since it's constant)
# Case I, Two separate lines: When pulses:group is part of the model.
# Case II, Two parallel lines: When pulses:group is not part of the model, but group is part of the model.
# Case III, One common line: When neither pulses:group, nor group is part of the model.
# In our case, we have pulses + pulses:group. This means that we have two separate lines,
# but those lines share the same intercept, because group is not part of the model.
# NOTE ALSO THAT PARALLEL LINES ALSO WORK in our case, since pulses + group has an almost equally good R^2.

# Scatter for the common intercept lines
common_intercept <- best_mod$coefficients["(Intercept)"]
slopeA <- best_mod$coefficients["pulses:groupA"]
slopeB <- best_mod$coefficients["pulses:groupB"]
gg1 <- ggplot(data = canary, aes(x = pulses, y = Temp)) +
  geom_point(aes(col = group))
# Plot the lines
gg1 <- gg1 + geom_abline(aes(intercept = common_intercept,
                           slope = slopeA, color = "A"))
gg1 <- gg1 + geom_abline(aes(intercept = common_intercept,
                           slope = slopeB, color = "B"))
# Add labels
strA <- sprintf("A: y = %.4f + %.4f x", common_intercept, slopeA)
strB <- sprintf("B: y = %.4f + %.4f x", common_intercept, slopeB)
gg1 <- gg1 + scale_color_discrete(labels=c(strA, strB))
gg1 <- gg1 + labs(title = "Common intercept model for canary.txt (best)")
gg1

# Scatter for the two parallel lines
parallel_mod <- lm(Temp ~ group + pulses, data = canary)
interceptA <- parallel_mod$coefficients["(Intercept)"]
interceptB <- sum(parallel_mod$coefficients[c("(Intercept)", "groupB")])
common_slope <- parallel_mod$coefficients["pulses"]
gg2 <- ggplot(data = canary, aes(x = pulses, y = Temp)) +
  geom_point(aes(col = group))
# Plot the lines
gg2 <- gg2 + geom_abline(aes(intercept = interceptA,
                             slope = common_slope, color = "A"))
gg2 <- gg2 + geom_abline(aes(intercept = interceptB,
                             slope = common_slope, color = "B"))
# Add labels
strA <- sprintf("A: y = %.4f + %.4f x", interceptA, common_slope)
strB <- sprintf("B: y = %.4f + %.4f x", interceptB, common_slope)
gg2 <- gg2 + scale_color_discrete(labels=c(strA, strB))
gg2 <- gg2 + labs(title = "Parallel lines model for canary.txt")
gg2

# If we accept the common intercept model, then this means that groupA "produces"
# 0.2848 - 0.2504 = 0.0344 more temperature degree than groupB.
# If we accept the parallel lines model, then this means that groupA always "produces"
# 5.1420 - 2.4768 = 2.6652 more temperature degrees than groupB, regardless of temperature.


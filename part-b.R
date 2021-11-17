#!/usr/bin/R

# Load the data
df <- read.table('./data/cholesterol.txt', sep = "", header = TRUE)
print(dim(df))

# Fit a simple linear model
fitted = lm(y~x, df)
summ <- summary(fitted)
print(summ)

# Hypothesis testing and intervals
b1_pval <- summ$coefficients['x', 'Pr(>|t|)']
sprintf('H0: b1 = 0 is not rejected if and only if the significance level is set to be lower than %.16f', b1_pval)
# Explanation of b is that, if someones age increased by one year,
# then his cholesterol levels are increated by b.

confint.b1 <- confint(fitted, parm = 'x', level = 0.95)
print('Confidence interval for b1')
print(confint.b1)

x0 <- 35
confint.Ey <- predict.lm(fitted, data.frame(x=x0),
                         interval='confidence', level=0.99)
predint.y <- predict.lm(fitted, data.frame(x=x0),
                        interval='prediction', level=0.99)
print(confint.Ey)
print(predint.y)


# PLOT THE PREDICTIONS y~x

svg('./output/part-b-scatter.svg')

plot(
  df$x, df$y,
  pch = 16,
  col = 'black',
  xlab = 'Age (years)',
  ylab = 'Cholesterol (mg/ml)',
  main = 'Cholesterol levels with respect to Age',
  ylim = c(min(df$y) - 0.5, max(df$y) + 0.5)
)

abline(fitted, col='blue')
x0s <- seq(from = min(df$x) - 2, to = max(df$x) + 2, length.out=30)
confints.Ey <- predict.lm(fitted, data.frame(x=x0s),
                          interval='confidence', level=0.99)
predints.y <- predict.lm(fitted, data.frame(x=x0s),
                         interval='prediction', level=0.99)

lines(x0s, confints.Ey[,'lwr'], col='darkgreen', lty=2)
lines(x0s, confints.Ey[,'upr'], col='darkgreen', lty=2)

lines(x0s, predints.y[,'lwr'], col='darkviolet', lty=4)
lines(x0s, predints.y[,'upr'], col='darkviolet', lty=4)

dev.off()


# PLOT QQ ETC.
svg('./output/part-b-fourplot.svg')
par(mfrow=c(2,2))
plot(fitted)
dev.off()
# Homoscedasticity does appear to hold for the residuals,
# since for all y the divergence from the 0-line looks roughly the same
# In addition to that, the QQ-plot fit the theoritical normal line almost perfectly,
# thus we can safely assume that y is indeed normally distributed.






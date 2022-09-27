#!/usr/bin/R

transform.interval <- function(interval) {
  ret <- 3 - exp(interval)
  ret[, c('lwr', 'upr')] <- ret[, c('upr', 'lwr')]
  ret
}


df <- data.frame(x=c(2, 4, 6, 12, 18, 24),
                 y=c(1.07, 1.88, 2.26, 2.78, 2.97, 2.99))

################################################################
# ORIGINAL MODEL


attach(df)

pdf('./output/part-c-qqplot-original.pdf')
# Plot the original QQ plot
qqnorm(y, main='Normal Q-Q Plot for the original response variable')
qqline(y)
dev.off()

fitted_original <- lm(y~x, df)

# Plot the original linear fit attempt
pdf('./output/part-c-scatter-original.pdf')
plot(x, y, main='Linear model fitted on the original data')
abline(fitted_original)
dev.off()

# Not a good fit



################################################################
# TRANSFORMED MODEL

# y = 3 - a exp(bx) <=> log(3-y) = log(a) + bx

detach(df)
df$z <- log(3 - df$y)
attach(df)

# Plot the transformed QQ plot
pdf('./output/part-c-qqplot-transformed.pdf')
qqnorm(df$z, main='Normal Q-Q Plot for the new response variable')
qqline(df$z)
dev.off()
# Might be normal

fitted <- lm(z~x, data=df)

# Plot the transformed data
pdf('./output/part-c-scatter-transformed.pdf')
plot(x, z, xlab='x', ylab='log(3 - y)',
     main='Linear model fitted on the transformed data')
abline(fitted)
dev.off()



# PREDICTION INTERVAL AT x=9
# To get a prediction interval for Y we utilize the confidence interval
# of Z := f(Y) := log(3 - Y)
# f is a strictly decreasing function and we have, Y = f^(-1)(Z) = 3 - exp(Z)
# Considering a prediction interval [c1, c2] of level alpha for Z,
# (c1 and c2 are statistical function of the sample) we get: 
# c1 < Z < c2  if and only if 3 - exp(c2) < Y < 3 - exp(c1)
# Thus [3 - exp(c2), 3 - exp(c1)] is a confidence interval of level alpha for Y.


x0 <- 9

predint.z <- predict.lm(fitted, data.frame(x=x0),
                        interval='prediction', level=0.95)
predint.y <- transform.interval(predint.z)
print('95% prediction interval for Y')
print(predint.y)


# CONFIDENCE INTERVAL AT x=9
# To get a "confidence" interval for E(Y) we will "approximate" E[exp(3 - Y)]
# by exp(E[3 - Y]), and end up using the same transformation as in
# the prediction interval for Y,
# i.e. [c1, c2] --> [3 - exp(c2), 3 - exp(c1)]

confint.Ez <- predict.lm(fitted, data.frame(x=x0),
                        interval='confidence', level=0.95)
confint.Ey <- transform.interval(confint.Ez)
print('95% "approximate" confidence interval for E[Y]')
print(confint.Ey)



# PLOT THE NEW PREDICTIONS y ~ x

level = 0.95
x0s <- seq(from = min(df$x) - 1, to = max(df$x) + 1, length.out = 50)

confints.Ez <- predict.lm(fitted, data.frame(x=x0s),
                          interval='confidence', level=level)
confints.Ey <- transform.interval(confints.Ez)


predints.z <- predict.lm(fitted, data.frame(x=x0s),
                         interval='prediction', level=level)
predints.y <- transform.interval(predints.z)
  

pdf('./output/part-c-scatter-transformed-detransformed.pdf')

plot(x, y,
     pch = 16,
     col = 'black',
     main = sprintf('Data with regr line plus %.0f%% conf and pred bands', level*100))

lines(x0s, confints.Ey[,'fit'], col='blue', lty=1)

lines(x0s, confints.Ey[,'lwr'], col='darkgreen', lty=2)
lines(x0s, confints.Ey[,'upr'], col='darkgreen', lty=2)

lines(x0s, predints.y[,'lwr'], col='darkviolet', lty=4)
lines(x0s, predints.y[,'upr'], col='darkviolet', lty=4)

dev.off()

# Good fit

detach(df)











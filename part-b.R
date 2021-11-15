

# Load the data
df <- read.table('./data/cholesterol.txt', sep = "", header = TRUE)
print(dim(df))

# Fit a simple linear model
fitted = lm(y~x, df)
summ <- summary(fitted)
print(summ)

svg('./output/plot.svg')


# Plot
plot(
  df$x, df$y,
  pch = 16,
  col = 'black',
  xlab = 'Age (years)',
  ylab = 'Cholesterol (mg/ml)',
  main = 'Cholesterol levels with respect to Age',
  ylim = c(min(df$y) - 0.5, max(df$y) + 0.5)
)

# Ερμηνεία  1
# β : αν αυξηθεί η Χ κατά μία μονάδα (δηλαδή για ένα επιπλέον χαλασμένο εξάρτημα)
# αναμένεται η διάρκεια επισκευής της μηχανής να αυξηθεί κατά 15.5 περίπου λεπτά.
# abline(fitted, col='blue')

# Hypothesis testing and intervals
b1_pval <- summ$coefficients['x', 'Pr(>|t|)']
sprintf('H0: b1 = 0 is not rejected if and only if the significance level is set to be lower than %.16f', b1_pval)

cint.b1 <- confint(fitted, parm = 'x', level = 0.95)
print('Confidence interval for b1')
print(cint.b1)

x0 <- 35
cint.Ey <- predict(fitted, data.frame(x=x0), interval='confidence', level=0.99)
cint.y <- predict(fitted, data.frame(x=x0), interval='prediction', level=0.99)
print(cint.Ey)
print(cint.y)


# Plot the bands
x0 <- seq(from = min(df$x) - 2, to = max(df$x) + 2, length.out=30)
cint.Ey <- predict(fitted, data.frame(x=x0), interval='confidence', level=0.99)
cint.y <- predict(fitted, data.frame(x=x0), interval='prediction', level=0.99)

lines(x0, cint.Ey[,2], col='darkgreen', lty=2)
lines(x0, cint.Ey[,3], col='darkgreen', lty=2)

lines(x0, cint.y[,2], col='darkviolet', lty=4)
lines(x0, cint.y[,3], col='darkviolet', lty=4)

dev.off()


svg('./output/fourplot.svg')
par(mfrow=c(2,2))
plot(fitted)
dev.off()
# Homoscedasticity does appear to hold for the residuals,
# since for all y the divergence from the 0-line looks roughly the same
# In addition to that, the QQ-plot fit the theoritical normal line almost perfectly,
# thus we can safely assume that y is indeed normally distributed.






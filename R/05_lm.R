# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Script to fit linear model in R
# First version 2022-07-18
# --------------------------------------------------#

# loading packages
library(ggplot2)

# reading data
cat <- read.csv("data/raw/crawley_regression.csv") #Be careful with the names of the variables

# Do leaf chemical compounds affect the growth of caterpillars? ----------------

# the response variable: growth
boxplot(cat$growth, col = "darkgreen")

# the predictor variable: tanning quantity

unique(cat$tannin)

# creating the lm (linear model)
mod_cat <- lm(growth ~ tannin, data = cat)

summary(mod_cat)


## ----lm-plot------------------------------------------------------------------
plot(growth ~ tannin, data = cat, bty = 'l', pch = 19)

#coef(mod_cat)
#a=coef(mod_cat[1])
#b=coef(mod_cat[2])

abline(mod_cat, col = "red", lwd = 2)


## ----lm-ggplot----------------------------------------------------------------
ggplot(data = cat, mapping = aes(x = tannin, y = growth)) +
  geom_point() +
  geom_smooth(method = lm) + #it's important to specify the method we have created
  theme_classic()


## AOV table
summary.aov(mod_cat)


## fitted values
predict(mod_cat)
cat$fitted <- predict(mod_cat) #creating a column in mod_cat with my predicted values

# Comparing fitted vs. observed values
ggplot(data = cat) +
  geom_point(aes(x = growth, y = fitted)) +
  geom_abline(aes(slope = 1,  intercept = 0)) +
  theme_classic()


## Model diagnostics -----------------------------------------------------------
par(mfrow = c(2, 2))
plot(mod_cat) #plotting the model. The residuals vs Fitted plot should not show any pattern and the line should be horizontal
par(mfrow = c(1, 1))


# Comparing statistical distributions ------------------------------------------
library(fitdistrplus)

data("groundbeef")
?groundbeef
str(groundbeef)


plotdist(groundbeef$serving, histo = TRUE, demp = TRUE)

descdist(groundbeef$serving, boot = 1000)

fw <- fitdist(groundbeef$serving, "weibull") #It's important to specify the distribution (here, weibull)
summary(fw)

fg <- fitdist(groundbeef$serving, "gamma")
fln <- fitdist(groundbeef$serving, "lnorm")

par(mfrow = c(2, 2))
plot_legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot_legend) #comparison
qqcomp(list(fw, fln, fg), legendtext = plot_legend)
cdfcomp(list(fw, fln, fg), legendtext = plot_legend)
ppcomp(list(fw, fln, fg), legendtext = plot_legend)
par(mfrow = c(1, 1))

gofstat(list(fw, fln, fg))

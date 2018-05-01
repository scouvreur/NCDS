#Change working directory
setwd(paste("//ad.ucl.ac.uk/homeo/zcemsco/Documents",
            "/Research_Methods_Quantitative_Data",
            "/Session_1", sep=""))

#Read in the data
ncds = readRDS("ncds.Rds")

#Load libraries
library(MASS)
library(ggplot2)
library(car)

head(ncds)
summary(ncds)
cor(ncds[,c("MWB", "NHC", "SUPPORT", "SOCPART")])

plot(MWB ~ NHC, ncds)

ggplot(ncds, aes(NHC, MWB)) + geom_point() +
 geom_smooth(method = "lm") + theme_classic() +
 scale_y_continuous(limits = c(0, 56))

#You can fit a simple linear regression of Mental Wellbeing
#on Neighbourhood Cohesion as follows:
mod_1 = lm(MWB ~ NHC, ncds)
summary(mod_1)

#The standardised residuals can be calculated using the
#stdres command from the MASS package:
sr_1 = stdres(mod_1)

#Predicted values are stored in the model object (which
#we named mod_1), and can be found like so:
pred_1 = fitted(mod_1)

#Histogram of standardised residuals
x = seq(-3.5, 3.5, 0.01) # Define the x-axis for plotting
n = length(sr_1) # Store the sample size in the object n
bin_width = 0.5 # Define the width of the histogram bars
hist(sr_1, main = "Histogram of standardised residuals", xlab
     = "Standardised residuals", breaks = seq(-3.5, 3.5,bin_width))
curve(n*bin_width*dnorm(x, mean = 0, sd = 1), add = TRUE, col
      = "orangered", lwd = 2)

#Spread-level plot of standardised residuals against predicted values
plot(pred_1, sr_1, main = "Spread-level plot: standardised
     residuals and predicted values", cex.main = 0.9,
     xlab = "Predicted values", ylab ="Standardised
     residuals")

#Tip: you can also get quick diagnostic plots like so:
par(mfrow = c(2,2))
plot(mod_1)

# We can fit a multiple linear regression like so:
mod_2 = lm(MWB ~ NHC + SUPPORT, ncds)
summary(mod_2)

# You can obtain confidence intervals for the regression coefficients like so:
confint(mod_2)

sr_2 = stdres(mod_2)
pred_2 = fitted(mod_2)
par(mfrow = c(1,1))
hist(sr_2, main = "Histogram of standardised residuals", xlab = "Standardi
     sed residuals")
curve(n*bin_width*dnorm(x, mean = 0, sd = 1), add = TRUE,
      lwd = 2) # draw a standard normal curve

plot(pred_2, sr_2, main = "Spread-level plot: standardised residuals and p
redicted values", cex.main = 0.9,
     xlab = "Predicted values", ylab ="Standardised residuals")

mod_3 = lm(MWB ~ NHC + SUPPORT + hq3, ncds)
summary(mod_3)
anova(mod_2, mod_3)

#Estimate the model and store standardised residuals and predicted values
mod_4 = lm(MWB ~ NHC + SUPPORT + SOCPART + limill + hq3, ncds)
sr_4 = stdres(mod_4)
pred_4 = fitted(mod_4)

#Model summary and confidence intervals
summary(mod_4)
confint(mod_4)

#Regression diagnostics
hist(sr_4, main = "Histogram of standardised residuals (Model 4)", xlab =
       "Standardised residuals", cex.main = 0.9, breaks = seq(-3.5, 3.5,bin_width
       ))
curve(n*bin_width*dnorm(x, mean = 0, sd = 1), add = TRUE, col = "orangered
      ", lwd = 2) # draw a standard normal curve

plot(pred_2, sr_2, main = "Spread-level plot: standardised residuals and p
redicted values (Model 4)", cex.main = 0.8,
     xlab = "Predicted values", ylab ="Standardised residuals")

#Compare Model 4 to Model 3
anova(mod_3, mod_4)

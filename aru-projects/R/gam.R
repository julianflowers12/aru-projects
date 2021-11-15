## gam
library(mgcv); library(broom)
mcycle <- MASS::mcycle
head(mcycle)
plot(mcycle)

lm_mod <- lm(accel ~ times, data = mcycle)

# Visualize the model
termplot(lm_mod, partial.resid = TRUE, se = TRUE)

# Fit the GAM model
gam_mod <- gam(accel ~ s(times), data = mcycle)

# Plot the results
plot(gam_mod, residuals = TRUE, pch = 1)

glance(gam_mod)
coef(gam_mod)

gam_mod_k3 <- gam(accel ~ s(times, k = 3), data = mcycle)

# Fit with 20 basis functions
gam_mod_k20 <- gam(accel ~ s(times, k = 20), data = mcycle)

# Visualize the GAMs
par(mfrow = c(1, 2))
plot(gam_mod_k3, residuals = TRUE, pch = 1)
plot(gam_mod_k20, residuals = TRUE, pch = 1)


gam_mod$sp

# Fix the smoothing parameter at 0.1
gam_mod_s1 <- gam(accel ~ s(times), data = mcycle, sp = 0.1)

# Fix the smoothing parameter at 0.0001
gam_mod_s2 <- gam(accel ~ s(times), data = mcycle, sp = 0.0001)

# Plot both models
par(mfrow = c(2, 1))
plot(gam_mod_s1, residuals = TRUE, pch = 1)
plot(gam_mod_s2, residuals = TRUE, pch = 1)

gam_mod_sk <- gam(accel ~ s(times, k = 50), data = mcycle, sp = 0.0001)

# Visualize the model
plot(gam_mod_sk, residuals = TRUE, pch = 1)


library(gamair)
data("mpg", package="gamair")

# Examine the data
head(mpg)
str(mpg)

# Fit a GAM to these data to predict city.mpg as the sum of smooth functions of weight, length, and price.
mod_city <- gam(city.mpg ~ s(weight) + s(length) + s(price),
                data = mpg, method = "REML")


mod_city2 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + fuel + drive + style,
                 data = mpg, method = "REML")

glance(mod_city2)

plot(mod_city2, all.terms = TRUE, pages = 1)

mod_city3 <- gam(city.mpg ~ s(weight, by = drive) + s(length, by = drive) + s(price, by = drive) + drive,
                 data = mpg, method = "REML")

# Plot the model
plot(mod_city3, pages = 1)

mod_city4 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + s(rpm) + s(width),
                 data = mpg, method = "REML")

# View the summary
summary(mod_city4)


# Fit the model
mod <- gam(accel ~ s(times), data = mcycle, method = "REML")

# Make the plot with residuals
plot(mod, residuals = TRUE)

# Change shape of residuals
plot(mod, residuals = TRUE, pch = 1, cex = 1)

# Fit the model
mod <- gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + comp.ratio,
           data = mpg, method = "REML")

# Plot the price effect
plot(mod, select = 3)

# Plot all effects
plot(mod, pages = 1, all.terms = TRUE)

# Plot the weight effect in pink
plot(mod, select = 1, shade = TRUE, shade.col = "hotpink")

# Make another plot adding the intercept value and uncertainty
plot(mod, select = 1, shade = TRUE, shade.col = "hotpink",
     shift = coef(mod)[1], seWithMean = TRUE)

set.seed(0)
dat <- gamSim(1,n=200)

# Fit the model
mod <- gam(y ~ s(x0, k = 5) + s(x1, k = 5) + s(x2, k = 5) + s(x3, k = 5),
           data = dat, method = "REML")

# Run the check function
gam.check(mod)

set.seed(0)
dat <- mgcv::gamSim(1,n=600, scale=0.6, verbose=FALSE)

# Fit the model
mod <- gam(y ~ s(x0, k = 3) + s(x1, k = 3) + s(x2, k = 3) + s(x3, k = 3),
           data = dat, method = "REML")

# Check the diagnostics
gam.check(mod)

# Refit to fix issues
mod2 <- gam(y ~ s(x0, k = 3) + s(x1, k = 3) + s(x2, k = 10) + s(x3, k = 3),
            data = dat, method = "REML")

# Check the new model
gam.check(mod2)

mod <- gam(hw.mpg ~ s(length) + s(width) + s(height) + s(weight),
           data = mpg, method = "REML")

# Check overall concurvity
concurvity(mod, full = TRUE)

#### Exercise 2.12 ####

# Check overall concurvity
concurvity(mod, full = FALSE)

library(mgcv)
data(meuse, package="sp")

# Inspect the data
head(meuse)
str(meuse)

# Fit the model
mod2d <- gam(cadmium ~ s(x, y), data = meuse, method = "REML")

# Inspect the model
summary(mod2d)
coef(mod2d)

# Fit the model
mod2da <- gam(cadmium ~ s(x, y) + s(dist) + s(elev),
              data = meuse, method = "REML")

# Inspect the model
summary(mod2da)

# Contour plot
plot(mod2da, pages = 1)

# 3D surface plot
plot(mod2da, scheme = 1, pages = 1)

# Colored heat map
plot(mod2da, scheme = 2, pages = 1)

mod2d <- gam(cadmium ~ s(x,y), data=meuse, method = "REML")

# Make the perspective plot with error surfaces
vis.gam(mod2d, view = c("x", "y"),
        plot.type = "persp", se = 2)

# Rotate the same plot
vis.gam(mod2d, view = c("x", "y"),
        plot.type = "persp", se = 2, theta = 135)

# Make plot with 5% extrapolation
vis.gam(mod2d, view = c("x", "y"),
        plot.type = "contour", too.far = 0.05)

# Overlay data
points(meuse)



# Make plot with 10% extrapolation
vis.gam(mod2d, view = c("x", "y"),
        plot.type = "contour", too.far = 0.1)

# Overlay data
points(meuse)



# Make plot with 25% extrapolation
vis.gam(mod2d, view = c("x", "y"),
        plot.type = "contour", too.far = 0.25)

# Overlay data
points(meuse)


# Fit a model with separate smooths for each land-use level
mod_sep <- gam(copper ~ s(dist, by = landuse) + landuse,
               data = meuse, method = "REML")

# Examine the summary
summary(mod_sep)

# Fit a model with factor-smooth interaction
mod_fs <- gam(copper ~ s(dist, landuse, bs = "fs"),
              data = meuse, method = "REML")

# Examine the summary
summary(mod_fs)


# Plot both the models with plot()
plot(mod_sep, pages = 1)
plot(mod_fs, pages = 1)


tensor_mod <- gam(cadmium ~ te(x, y, elev),
                  data = meuse, method = "REML")

# Summarize and plot
summary(tensor_mod)
plot(tensor_mod)

# Plot both the models with vis.gam()
vis.gam(mod_sep, view = c("dist", "landuse"), plot.type = "persp")
vis.gam(mod_fs, view = c("dist", "landuse"), plot.type = "persp")


# Fit the model
tensor_mod2 <- gam(cadmium ~ s(x, y) + s(elev) + ti(x, y, elev),
                   data = meuse, method = "REML")

# Summarize and plot
summary(tensor_mod2)
plot(tensor_mod2, pages = 1)

#### Exercise 4.2 ####

library(mgcv)
csale <- readRDS(url("https://github.com/noamross/gams-in-r-course/raw/master/csale.rds"))

# Examine the csale data frame
head(csale)
str(csale)

# Fit a logistic model
log_mod <- gam(purchase ~ s(mortgage_age), data = csale,
               family = binomial,
               method = "REML")

# Calculate the probability at the mean
plogis(coef(log_mod)[1])

#### Exercise 4.3 ####

# Fit a logistic model
log_mod2 <- gam(purchase ~ s(n_acts) + s(bal_crdt_ratio) +
                  s(avg_prem_balance) + s(retail_crdt_ratio) +
                  s(avg_fin_balance)  + s(mortgage_age) +
                  s(cred_limit),
                data = csale,
                family = binomial,
                method = "REML")

# View the summary
summary(log_mod2)

#### Exercise 4.5 ####

# Plot on the log-odds scale
plot(log_mod2, pages = 1)

# Plot on the probability scale
plot(log_mod2, pages = 1, trans = plogis)

# Plot with the intercept
plot(log_mod2, pages = 1, trans = plogis,
     shift = coef(log_mod2)[1])

# Plot with intercept uncertainty
plot(log_mod2, pages = 1, trans = plogis,
     shift = coef(log_mod2)[1], seWithMean = TRUE)

new_credit_data <- readRDS(url("https://github.com/noamross/gams-in-r-course/raw/master/new_credit_data.rds"))

# Calculate predictions and errors
predictions <- predict(log_mod2, newdata = new_credit_data,
                       type = "link", se.fit = TRUE)

# Inspect the predictions
predictions

# Calculate predictions and errors
predictions <- predict(log_mod2, newdata = new_credit_data,
                       type = "link", se.fit=TRUE)

# Calculate high and low predictions intervals
high_pred <- predictions$fit + 2*predictions$se.fit
low_pred <- predictions$fit - 2*predictions$se.fit

# Convert intervals to probability scale
high_prob <- plogis(high_pred)
low_prob <- plogis(low_pred)

high_prob
low_prob

# Predict from the model
prediction_1 <- predict(log_mod2,
                        newdata = new_credit_data[1, ],
                        type = "terms")

# Inspect
prediction_1


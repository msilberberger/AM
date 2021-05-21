
## Load and install the packages that we'll be using today
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, broom, xts, magrittr, tseries)

## Load data from csv file into dataframe (bike_share)
## Make sure you have the data in you working directory
bike_share <- read_csv("day.csv")
head(bike_share)

## We create a timeseries from our data using the xts package. 
## We create a time-series index, our start date is 2011-01-01, by “days”
## The length is the number of observations
dates <- seq(as.Date("2011-01-01"), by = "days", length.out = 731)
# We now use "select" to select the variables we need
bike_ts <- 
  bike_share %>% 
  select(instant, season, holiday, weekday, workingday, weathersit, temp, atemp, hum, windspeed, casual, registered, cnt) %>% 
  xts(dates) 

## The data now have a date index indicating to which date each observations belongs
bike_ts %>% 
  head()


## Estimate the total number of riders as a function of time
bike_ts %>%
  lm(cnt ~ instant) 

## Test the residuals for first order auto correlation using the auxiliary regression approach
# retrieve the residuals as e
e <- 
  bike_ts %$%
  lm(cnt ~ instant)$residuals

# auxiliary regression
e_l <- 
  lag.xts(e)
lm (e ~ e_l) %>%
  tidy()

## Our t-statistic is 30.50 so we can reject the null hypothesis of no autocorrelation in the error term.

## Correcting autocorrelation (Newey-West)
# estimate the model (the lm_object)
bike_lm <- lm(cnt ~ instant, bike_ts)
bike_lm %>% 
  tidy()

## Determine the number of lags for the Newey-West correction 
# (rule of thumb: sample size ^ 1/4, see e.g. Greene "Econometric Analysis")
lags <- length(bike_ts$instant)^(.25)
# NW corrected standard errors from the sandwhich package
nw_vcov <- sandwich::NeweyWest(bike_lm, lag = lags, prewhite = F, adjust = T)
# model with corrected errors
lmtest::coeftest(bike_lm, vcov. = nw_vcov)

## Dynamic Model
# We can call the argument lag(variable_name, number_of_lags)
# We want to see if yesterday’s weather affects today’s rentals.

bike_lm_dyn <- lm(cnt ~ instant + lag(temp, 1), bike_ts)
bike_lm_dyn %>% 
  tidy()

## Using an augmented Dickey-Fuller test, "adf.test(time_series)"

bike_ts$cnt %>% 
  tseries::adf.test()

# From the results we can conclude that cnf is non-stationary

# One solution to non-stationarity is first differencing
# We test the difference for stationarity

bike_ts$cnt %>% 
  diff() %>% 
  tseries::na.remove() %>% # first differencing introduces NA's into the data
  tseries::adf.test()

# Based on the p-value, we can reject the null-hypothesis of non-stationarity

# Let's estimate the model

lm(diff(cnt) ~ diff(temp), bike_ts) %>% 
  tidy()

# Compare to same equation in the levels

lm(cnt ~ temp, bike_ts) %>% 
  tidy()

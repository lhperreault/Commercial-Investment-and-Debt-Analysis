graphics.off()
rm(list = ls())
library(fable)
library(forecast)
library(fpp2)
library(ggplot2)
library(fabletools)
library(datasets)
library(tidyverse)
library(fma)
library(expsmooth)


df <- read_csv("df_OECD.csv")
View(df)


# Ensure the Year column is numeric
df$Year <- as.numeric(df$Year)

# View a sample of your data
head(df)

# Filter for specific countries (optional)
countries_of_interest <- c("Australia", "Germany", "United States", "Japan", "Coast Rica", "Mexico", "Spain", "Norway", "Italy", "Poland", "France", "Switzerland", "United Kingdon", "Colombia")
df_filtered <- df %>%
  filter(`Country Name` %in% countries_of_interest)

# Create the time series plot
ggplot(df_filtered, aes(x = Year, y = `Central Government Debt (Percent of GDP)`, color = `Country Name`)) +
  geom_line(size = 1) +
  labs(
    title = "Central Government Debt (Percent of GDP) Over Time",
    x = "Year",
    y = "Debt as % of GDP",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Create the time series plot
ggplot(df_filtered, aes(x = Year, y = `Population ages 15-64 (% of total population)`, color = `Country Name`)) +
  geom_line(size = 1) +
  labs(
    title = "Population ages 15-64 (% of total population)",
    x = "Year",
    y = "Population ages 15-64 (% of total population)",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# Create the time series plot
ggplot(df_filtered, aes(x = Year, y = `GDP (current US$)`, color = `Country Name`)) +
  geom_line(size = 1) +
  labs(
    title = "GDP (current US$)",
    x = "Year",
    y = "GDP (current US$)",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

## REGION
# Calculate the average debt by Region and Year
df_region_avg <- df_filtered %>%
  group_by(Year, Region) %>%
  summarise(avg_debt = mean(`Central Government Debt (Percent of GDP)`, na.rm = TRUE))

# Create the time series plot by Region
ggplot(df_region_avg, aes(x = Year, y = avg_debt, color = Region)) +
  geom_line(size = 1) +
  labs(
    title = "Average Central Government Debt (Percent of GDP) Over Time by Region",
    x = "Year",
    y = "Average Debt as % of GDP",
    color = "Region"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# All Countries 
# Filter the dataset for specific regions
df_filtered1 <- df %>%
  filter(Region %in% c("United States", "Central & Eastern Europe", "East Asia", "Oceania"))

ggplot(df_filtered1, aes(x = Year, y = `Central Government Debt (Percent of GDP)`, color = `Country Name`, group = `Country Name`)) +
  geom_line(size = 1) +
  labs(
    title = "Central Government Debt (Percent of GDP) Over Time by Country (Colored by Region)",
    x = "Year",
    y = "Debt as % of GDP",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

df_filtered1 <- df %>%
  filter(Region %in% c("United States", "Western Europe"))

ggplot(df_filtered1, aes(x = Year, y = `Central Government Debt (Percent of GDP)`, color = `Country Name`, group = `Country Name`)) +
  geom_line(size = 1) +
  labs(
    title = "Central Government Debt (Percent of GDP) Over Time by Country (Colored by Region)",
    x = "Year",
    y = "Debt as % of GDP",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


### BIN DEBT TO SEE THE GROUPS OF COUNTRIES
# Find the last year for each country
last_year_df <- df %>%
  group_by(`Country Name`) %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  ungroup()

# Create bins for debt intervals based on the last year's debt value
df_grouped <- last_year_df %>%
  mutate(Debt_Group = cut(`Central Government Debt (Percent of GDP)`, 
                          breaks = seq(0, 240, by = 20), 
                          include.lowest = TRUE, 
                          right = FALSE, 
                          labels = paste(seq(0, 180, by = 20), seq(20, 240, by = 20), sep = "-"))) %>%
  group_by(Debt_Group) %>%
  summarise(Countries = paste(`Country Name`, collapse = ", "), 
            Avg_Debt = mean(`Central Government Debt (Percent of GDP)`, na.rm = TRUE))

# View the result
print(df_grouped)

# Specify the countries you are interested in
selected_countries <- c("Czechia", "Luxembourg", "Netherlands")

# Filter the dataset for the selected countries and the year 2022
df_2022 <- df %>%
  filter(`Country Name` %in% selected_countries & Year == 2021)

# Create bins for debt intervals based on 2022 debt values
df_grouped_2022 <- df_2022 %>%
  mutate(Debt_Group = cut(`Central Government Debt (Percent of GDP)`, 
                          breaks = seq(0, 200, by = 20), 
                          include.lowest = TRUE, 
                          right = FALSE, 
                          labels = paste(seq(0, 180, by = 20), seq(20, 200, by = 20), sep = "-"))) %>%
  group_by(Debt_Group) %>%
  summarise(Countries = paste(`Country Name`, collapse = ", "), 
            Avg_Debt = mean(`Central Government Debt (Percent of GDP)`, na.rm = TRUE))

# View the result
print(df_grouped_2022)








#MAYBE DO THESE FORECASTS LATER

retail <- ts(df[,"Central Government Debt (Percent of GDP)"], frequency=12, start=c(1970,4))

#a. Can you spot any seasonality, cyclicity and trend?
autoplot(retail)
ggseasonplot(retail)
ggsubseriesplot(retail)
gglagplot(retail)
ggAcf(retail)

#b. Replace the column name with your own chosen column:
myts <- ts(retaildata[,"A3349873A"],frequency=12, start=c(1982,4))
colnames(myts)[1] <-"col1"
view(myts)

#c.Split the data into two parts using:
myts.train <- window(myts, end=c(2010,12)) 
myts.test <- window(myts, start=2011)

#d. Check that your data have been split appropriately by producing a plot.
autoplot(myts.train)
autoplot(myts.test)

#e. Calculate forecasts using snaive applied to myts.train.
fit <- snaive(myts.train)
autoplot(fit)

#f. Compare the accuracy of your forecasts against the actual values stored in myts.test.
forecasted_values <- forecast(fit)
accuracy_metrics <- accuracy(forecasted_values, myts.test)
print(accuracy_metrics)

#g. Check the residuals.
checkresiduals(fit)

#h. Do the residuals appear to be uncorrelated and normally distributed?
#the residuals appear to be normally distributed being slightly to the left, 
#they appear to be correlated since many lags are outside the confidence interval
#The residuals seem roughly normal, but there are deviations and potential outliers that suggest the distribution is not perfectly normal.


#Use the bricksq data (Australian quarterly clay brick production. 1956–1994) for this exercise. (15 points)
#a. Use an STL decomposition to calculate the trend-cycle and seasonal indices. (Experiment with having fixed or changing seasonality.)
bricksq
mcopper
# Convert mcopper to a time series object if it's not one already
brick_sq <- ts(bricksq, frequency=4, start=c(1956,1)) # Quarterly data starts in 1956

# Perform STL decomposition with fixed seasonality
stl_fixed <- stl(brick_sq, s.window="periodic")

# Perform STL decomposition with changing seasonality
# 's.window' can be set to a number to allow seasonality to change over time
# The number represents the length of the seasonal window; 
# smaller numbers allow more rapid changes in seasonality.
stl_changing <- stl(brick_sq, s.window=7) # Example with a seasonal window of 7 years

# Extract the trend-cycle and seasonal components
trend_cycle_fixed <- stl_fixed$time.series[, "trend"]
seasonal_fixed <- stl_fixed$time.series[, "seasonal"]
autoplot(seasonal_fixed)
trend_cycle_changing <- stl_changing$time.series[, "trend"]
seasonal_changing <- stl_changing$time.series[, "seasonal"]

# You could plot the components to visualize them
plot(stl_fixed)
plot(stl_changing)

#b. Use a naïve method to produce forecasts of the seasonally adjusted data.
# Perform STL decomposition
stl_fit <- stl(brick_sq, s.window=7)  # or use another value for changing seasonality

# Create seasonally adjusted data
seasonally_adjusted <- seasadj(stl_fit)

# Apply the naïve method to the seasonally adjusted data
naive_forecast <- naive(seasonally_adjusted)

# Plot the forecast
autoplot(naive_forecast)

#c. Use stlf to reseasonalize the results, giving forecasts for the original data.
# Use stlf to automatically forecast the original data with reseasonalization
stlf_forecast <- stlf(brick_sq, method='naive', s.window='periodic')

# Plot the reseasonalized forecast
autoplot(stlf_forecast)

#d. Do the residuals look uncorrelated?
checkresiduals(stlf_forecast)
#more than 5% of the lags are outside the confidence interval so it can be said that there is some correlation


#6. From the TS Data qcement apply the exponential smoothing method based on the observation
#and recognition of time series components. Then, generate forecasts for the next twelve periods. (15 points)

view(qcement)

# Estimate parameters
fc <- ses(qcement, h=12)
summary(fc[["model"]])

autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("Oil (millions of tonnes)") + xlab("Year")

#7. For the mcopper data:
  #a. If necessary, find a suitable Box-Cox transformation for the data;
view(mcopper)
lambda <- BoxCox.lambda(mcopper)
  
# Transform the data using the estimated lambda
mcopper_transformed <- BoxCox(mcopper, lambda)

# Plot the transformed data
autoplot(mcopper_transformed)

#b. Fit a suitable ARIMA model to the transformed data using auto.arima();
# Fit an ARIMA model to the transformed data
fit_arima <- auto.arima(mcopper_transformed)

# Summary of the fitted model
summary(fit_arima)

# Check the diagnostics of the ARIMA model
checkresiduals(fit_arima)

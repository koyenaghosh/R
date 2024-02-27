#Load all the necessary packages library(readxl)
library(ggplot2)
library(astsa)
library(tseries)
library(forecast)
# Read the data from the Excel file 
data <- project_aqi_ts

# Convert the time series into a data frame
aqi_df <- as.data.frame(data)

z_scores <- scale(aqi_df$AQI)

# Define a threshold for outlier detection (e.g., z-score threshold of 3)
z_score_threshold <- 3

# Identify and remove outliers based on the z-score threshold
clean_data <- aqi_df[abs(z_scores) <= z_score_threshold, ]
column_name <- "AQI"
# Original data
original_summary <- summary(data[[column_name]])

# Cleaned data
cleaned_summary <- summary(clean_data[[column_name]])
# Print the summary statistics
cat("Original Data Summary:\n")
print(original_summary)


cat("\nCleaned Data Summary:\n") 
print(cleaned_summary)

print(clean_data)

# Convert the data into a time series format
ts_data <- ts(clean_data$AQI,start=c(2014,1),end=c(2017,12),frequency=183)
print(ts_data)

# Plot the time series data
plot(ts_data,xlab="Date",ylab="Aqi", main = "Daily AQI of Kolkata", col = "brown", lwd = 1.5,xaxt="n",yaxt="n")

# Decompose the time series
ts_decomp <- decompose(ts_data,type="additive")

# Plot the decomposed time series 
plot(ts_decomp)

#determination of trend values 
trend=ts_decomp$trend
trend

# Plot the trend component
plot(trend, main = "Trend Component",col="purple",lwd="3.5",lty="twodash") #calculating seasonal indices
seasonal_component <- ts_decomp$seasonal
seasonal_component <- matrix(seasonal_component, nrow = 12, byrow = TRUE)

seasonal_indicesR <- rowMeans(seasonal_component) 
#seasonal_indicesC <- colMeans(seasonal_component) 
#seasonal_indicesC
seasonal_indicesR

# Plotting the seasonal component

#plot(seasonal_indicesC,main="Columnwise	Seasonal Indices",ty="b",lwd="2.3",col="violet",xlab="Years",ylab="seasonal indices")
plot(seasonal_indicesR,main="Rowwise	Seasonal Indices",ty="l",lwd="3",col="cyan",xlab="Years",ylab="seasonal indices")


# Seasonal Adjustment
adjusted_data <- seasonal_component

# Seasonal Forecasting
forecast_with_seasonality <- seasonal_indicesC + seasonal_component

# Plotting the irregular component
plot(ts_decomp$random, type = "l", main = "Random Component", xlab = "Time", ylab = "AQI",col="seagreen",lwd="2")

#creating histogram 
ggplot(aqi_df, aes(x = AQI)) +
geom_histogram(binwidth = 5, fill = "orchid", color = "black") +
  labs(x = "AQI", y = "Frequency", title = "Histogram for AQI data from 2014-2017") +
  theme_void()
#detrending
dts=detrend(ts_data, order = 1, lowess = FALSE, lowspan = 2/3)

# Perform ADF test on your time series data to check stationarity #Ho=not stationary vs H1=stationary
adf_result <- adf.test(ts_data)

# Print the results
cat("ADF test statistic:", adf_result$statistic, "\n") 
cat("p-value:", adf_result$p.value, "\n") 
cat("Number of lags used:", adf_result$lags, "\n") 
cat("Critical values:")
print(adf_result$critical)
#since pvalue=0.01<level of significance=0.05, we reject the null hypothesis

#Forecasting
forecasted_values <- forecast(dts, h = 15) 
print(forecasted_values)
# Plot the forecasted values
plot(forecasted_values, main = "Forecasted values", xlab = "Year", ylab = "AQI", lty = "solid", col = "brown")

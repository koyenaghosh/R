library(readxl)
library(forecast) 
library(ggplot2) 
library(urca) 
library(tseries)

# Read the data from the Excel file 
file_path <- "D:/STAT HONS/aqidata.xlsx"
data <- read_excel(file_path) print(data,ncol=5)

# Convert the time series into a data frame
aqi_df <- as.data.frame(data)

# Calculate z-scores for the 'Value' column
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
plot(trend, main = "Trend Component",col="purple",lwd="3.5",lty="twodash")
#calculating seasonal indices
seasonal_component <- ts_decomp$seasonal
seasonal_component <- matrix(seasonal_component, nrow = 12, byrow = TRUE)

seasonal_indicesR <- rowMeans(seasonal_component)
seasonal_indicesC <- colMeans(seasonal_component)
seasonal_indicesC
seasonal_indicesR
# Plotting the seasonal component
plot(seasonal_indicesC,main="Columnwise	Seasonal Indices",ty="b",lwd="2.3",col="violet",xlab="Years",ylab="seasonal indices") 
plot(seasonal_indicesR,main="Rowwise	Seasonal Indices",ty="l",lwd="3",col="cyan",xlab="Years",ylab="seasonal indices")


# Seasonal Adjustment
adjusted_data <- seasonal_component

# Seasonal Forecasting
forecast_with_seasonality <- seasonal_indicesC + seasonal_component

# Plotting the irregular component
plot(ts_decomp$random, type = "l", main = "Random Component", xlab = "Time", ylab = "AQI",col="seagreen",lwd="2")

# Convert time series data to a matrix
ts_matrix <- matrix(ts_data, nrow = 20, ncol = 12, byrow = TRUE)

# Convert matrix to data frame
ts_df <- as.data.frame(ts_matrix)

# Add column names
colnames(ts_df) <- c("AQI for Jan", "AQI for Feb", "AQI for Mar", "AQI for Apr", "AQI for May", "AQI for Jun", "AQI for Jul", "AQI for Aug", "AQI for Sep", "AQI for Oct", "AQI for Nov", "AQI for Dec")

# Add Year column
ts_df$Year <- 2014:2017

# Reorder columns
ts_df <- ts_df[, c("Year", colnames(ts_df)[-ncol(ts_df)])]

# Print the data frame 
print(ts_df)
# Create the bubble plot using ggplot2

# Create a data frame with our AQI data
aqi_df <- data.frame(Date = time(ts_data), AQI = as.vector(ts_data))
colnames(aqi_df)=c("Year","AQI")
# Add a new column for the bubble size 
aqi_df$Size <- runif(nrow(aqi_df), 1, 10)

ggplot(aqi_df, aes(x = Year, y = AQI, size = Size, color = AQI)) + geom_point() +
  scale_color_gradient(low = "maroon", high = "black") +
  labs(x = "Year", y = "AQI", size = "Bubble Size", color = "AQI") + ggtitle("Bubble Plot of The AQI Data") +
  theme_dark()

#creating histogram 
ggplot(aqi_df, aes(x = AQI)) +
geom_histogram(binwidth = 5, fill = "orchid", color = "black") +
  labs(x = "AQI", y = "Frequency", title = "Histogram for AQI data of Kolkata from 2014-2017") +
  theme_void()


# Perform ADF test on your time series data to check stationarity 
#Ho=not stationary vs H1=stationary
adf_result <- adf.test(ts_data)

# Print the results
cat("ADF test statistic:", adf_result$statistic, "\n")
cat("p-value:", adf_result$p.value, "\n") 
cat("Number of lags used:", adf_result$lags, "\n") 
cat("Critical values:")
print(adf_result$critical)
#since pvalue=0.01<level of significance=0.05, we reject the null hypothesis
#Determine the model order using ACF and PACF analysis

pacf(ts_data)
acf(ts_data)
#number of significant spikes beyond the confidence interval in the PACF plot =2, which is the order of AR process
#number of significant spikes beyond the confidence interval in the ACF plot =4 , which is the order of MA process

arma_model <- arima(ts_data, order = c(2, 1, 4))

# Print the model summary 
summary(arma_model)
#This is the output of the `summary()` function applied to the `arma_model`. The output provides the following information:

# - `coef`: the estimated coefficients of the ARMA model # - `sigma2`: the estimated variance of the error term
# - `var.coef`: the estimated variances and covariances of the coefficient estimates # - `mask`: the mask used for the model, if any
# - `loglik`: the maximized log-likelihood of the model
# - `aic`: the Akaike Information Criterion value of the model # - `arma`: the order and coefficients of the ARMA model
# - `residuals`: the residuals of the model # - `call`: the call used to fit the model
# - `series`: the name of the time series
# - `code`: the error code returned by the optimization routine
# - `n.cond`: the number of initial conditions used in the optimization routine # - `nobs`: the number of observations in the time series
# - `model`: a list containing the estimated parameters of the AR, MA, and constant terms.

#Assess model diagnostics
residuals <- residuals(arma_model)
print(residuals,n="730")
plot(residuals, main = "Residuals of ARMA Model")

#test for normality check of residuals
shapiro.test(residuals)
theoretical_quantiles <- qnorm(ppoints(length(residuals)))
# Create the Q-Q plot
qqplot(residuals, theoretical_quantiles, main = "Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")

# Add a reference line for a perfect normal distribution 
abline(0, 1, col = "red", lwd = 2)

#Forecasting
forecasted_values <- forecast(arma_model, h = 15) 
print(forecasted_values)
# Plot the forecasted values
plot(forecasted_values, main = "Forecasted values", xlab = "Year", ylab = "AQI", lty = "solid", col = "brown")
# Perform the Ljung-Box test (AUTO CORRELATION CHECK) on the model residuals
Box.test(arma_model$residuals, lag = 10)

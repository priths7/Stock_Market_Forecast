# Stock_Market_Forecast
Stock Market Forecast Using R

Libraries used in this program are:
ggplot2
forecast
plotly
ggfortify
tseries
gridExtra
docstring

Do make sure to install these packages before running the script

This uses ARIMA (Auto Regresssive Integrated Moving Average) model with: 
p ( order of Auto Regressive Term or the number of lags of Y to be used as predictors) 
q ( order of Moving Average , the number of lagged forecast errors that should go into the model)
d ( the number of differencing required to make the time series stationary)

as 0,1,1 respectively.

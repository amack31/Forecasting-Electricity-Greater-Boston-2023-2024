# Forecasting-Electricity-Greater-Boston-2023-2024
Electricity is a vital resource that we rely on daily for various purposes. Unfortunately, the cost
of electricity has significantly increased since January 2020 due to factors such as the COVID-19
pandemic, global conflicts affecting energy supply and demand, and inflationary pressures. This
is a concern for everyone, from individual households to large multinational corporations. That is
why the goal of the project was to forecast the average price of electricity per kilowatt/hour in
the Greater Boston Area for the next 12 months, starting in April 2023. The project utilized
historical data from January 1978 to January 2023, obtained from the U.S. Bureau of Labor
Statistics.
The analysis began with an exploratory data analysis, where different graphs like the Seasonal
Plot, the Lags Plot, the ACF, and an STL Decomposition helped highlight a significant increase
in electricity prices starting from January 2020 to January 2023. This unprecedented increase
posed challenges for the forecast due to the abrupt change in data patterns. Seasonality in the
data was observed, with higher prices during winter months and a decrease in May. However, the
impact of seasonality diminished after 2020, making it less influential in price fluctuations.
Different basic forecasting models, including mean, drift, naive, and seasonal naive, were
initially employed, but they provided poor results. More complex models were then explored,
including ARIMA, LSTM, Fourier, ETS, and VAR models. The ARIMA models, specifically the
seasonal ARIMA models, showed some improvement but needed more data for accurate
forecasting. LSTM, a type of recurrent neural network, provided better results but experienced a
decrease in accuracy when applied to the test data. The Fourier Seasonal Analysis model, based
on the mathematics of Joseph Fourier, showed impressive performance, with the best forecasting
accuracy among the models used. Lastly, the Exponential Smoothing (ETS) model did not
perform as well as expected.
To evaluate the models' performance during a different period, historical data from 1990 to 2015
was used, and forecasts were compared with actual electricity prices from 2015 to 2020. The
Fourier model, which performed well for the 2023-2024 forecast, did not achieve the same
accuracy for this extended period. However, it excelled in shorter-term predictions. This time the
model that performed the best was the ETS since the actual prices were in between the prediction
intervals of the model. In conclusion, forecasting the average price of electricity per kilowatt/hour in the Greater Boston
area for the next 12 months was challenging due to the significant price increase in recent years.
The Fourier Seasonal Analysis model demonstrated the highest accuracy among the models
tested, although performance varied depending on the time period being forecasted. In a more
“normal” situation the Seasonal ARIMA and ETS models would be the best models to use when
forecasting electricity prices but in this specific case, the VAR is the model with the most
accurate and more realistic predictions.
These findings emphasize the importance of considering the volatility and seasonality of
electricity prices when selecting forecasting models and the need for ongoing evaluation and
refinement to improve accuracy.

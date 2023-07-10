####FINAL####
library(fpp3)
library(ggplot2)
library(tsibble)
library(urca)
library(MLmetrics)
#Convert the electricity CSV file to a tsibble to work with the data
Elec<-Electricity|>
mutate(Month = yearmonth(observation_date))|>as_tsibble(index=Month)|>
select(-observation_date)
Electricity2020<-Elec|>filter(between(year(Month),2020,2023))
#Convert the oil CSV file to a tsibble
Oil<-Oil_price_data|>mutate(Month= yearmonth(observation_date))|>
  as_tsibble(index=Month)|>select(-observation_date)
Oil2020<-Oil|>filter(between(year(Month),2020,2023))
#Box-Cox electricity
lambda <- Electricity2020 |>
features(`Average Price: Electricity per Kilowatt-Hour`, features = guerrero) |>
pull(lambda_guerrero)
Electricity2020 |>autoplot(box_cox(`Average Price: Electricity per Kilowatt-Hour`, lambda)) +
labs(y = "",title = "Transformed electricity price Box-Cox ",round(lambda,2))
#Box-Cox Oil
lambdaoil<-Oil2020|>features(`Average Price Fuel Oil per Gallon`,features=guerrero)|>
  pull(lambda_guerrero)
Oil2020|> autoplot(box_cox(`Average Price Fuel Oil per Gallon`,lambda))+
  labs(y="",title ="Transformed Oil price Box-Cox ",round(lambda,2))
#Create a graph to see the data electricity
autoplot(Elec)+labs(y="Price in US Dollars",x="Year",title = "Average Price: Electricity per Kilowatt-Hour in Boston-Cambridge-Newton from 1980 to 2023")
#Create a graph to see the data Oil
autoplot(Oil)+labs(y="Price in US Dollars",x="Year",title = "Average Price: Oil per galon from 1980 to 2023")

##Summary of the electricity data
summary(Electricity_per_Kilowatt_Hour_in_Boston_Newton_and_Cambridge_Monthly_Not_Seasonally_Adjusted)
gg_season(Electricity2020,`Average Price: Electricity per Kilowatt-Hour`)+labs(title = "Seasonal Plot for Average Electricity Prices in Boston-Cambridge-Newton")
gg_subseries(Electricity2020,`Average Price: Electricity per Kilowatt-Hour`)+labs(title = "Seasonal Subseries Plot for Average Electricity Prices in Boston-Cambridge-Newton")
gg_lag(Electricity2020,`Average Price: Electricity per Kilowatt-Hour`)+labs(title = "Lags Plot for Average Electricity Prices in Boston-Cambridge-Newton")
Electricity2020|>ACF(`Average Price: Electricity per Kilowatt-Hour`)
Electricity2020|>ACF(`Average Price: Electricity per Kilowatt-Hour`)|>autoplot()+labs(title = "Correlogram for Average Electricity Prices in Boston-Cambridge-Newton")

##Summary of the Oil data
summary(Oil_price_data)
gg_season(Oil2020,`Average Price Fuel Oil per Gallon`)
gg_subseries(Oil2020,`Average Price Fuel Oil per Gallon`)
gg_lag(Oil2020,`Average Price Fuel Oil per Gallon`)
Oil2020|>ACF(`Average Price Fuel Oil per Gallon`)
Oil2020|>ACF(`Average Price Fuel Oil per Gallon`)|>autoplot()
##DECOMP electricity
#Classical
ELECdecomp<-Electricity2020|>model(classical_decomposition(`Average Price: Electricity per Kilowatt-Hour`,type = "multiplicative"))
components(ELECdecomp)|>autoplot()+labs(title="Classical Decomposition for Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2020 to 2023")
#STL trend or season and how many windows
ELECdecomp<-Electricity2020|>model(STL(`Average Price: Electricity per Kilowatt-Hour`~season(window=9),robust=TRUE))
components(ELECdecomp)|>autoplot()+labs(title="STL Decomposition for Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2020 to 2023")
##DECOMP Oil
#STL trend 
Oildecomp<-Oil2020|>model(STL(`Average Price Fuel Oil per Gallon`~season(window=9),robust=TRUE))
components(Oildecomp)|>autoplot()+labs(title="STL Decomposition for Average Oil Price per galon from 2020 to 2023")
###Training Data from 2020 to 2022 electricity
electraining<-Elec|>filter_index("2020 January"~"2022 January")|>select(`Average Price: Electricity per Kilowatt-Hour`)
###Training Data from 2020 to 2022 oil
Oiltraining<-Oil|>filter_index("2020 January"~"2022 January")|>select(`Average Price Fuel Oil per Gallon`)

###Simple Forecasting methods
#Fit models
elec_fit<-electraining|>model(Mean = MEAN(`Average Price: Electricity per Kilowatt-Hour`),
`Naïve` = NAIVE(`Average Price: Electricity per Kilowatt-Hour`),`Seasonal naïve` = SNAIVE(`Average Price: Electricity per Kilowatt-Hour`),Drift = NAIVE(`Average Price: Electricity per Kilowatt-Hour` ~ drift()))

elec_fc<-elec_fit|>forecast(h=12)
elec_fc|>autoplot(electraining, level = NULL) +
autolayer(filter_index(Elec, "2022 Feb" ~ .),colour = "black") +labs(y = "Price in US Dollars",title = "12 Month Forecasts for monthly Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2020 to 2022 ") +guides(colour = guide_legend(title = "Forecast"))                      

#Fitted Values and residuals
augment(elec_fit)
elecmean|>model(NAIVE(`Average Price: Electricity per Kilowatt-Hour`)) |>gg_tsresiduals()+labs(title = "Residual Diagnostic Plots for 12 Month Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2020 to 2022 (NAIVE model)")
elecmean|>model(SNAIVE(`Average Price: Electricity per Kilowatt-Hour`)) |>gg_tsresiduals()+labs(title = "Residual Diagnostic Plots for 12 Month Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2020 to 2022 (SNAIVE model)")
elecmean|>model(NAIVE(`Average Price: Electricity per Kilowatt-Hour`~drift())) |>gg_tsresiduals()+labs(title = "Residual Diagnostic Plots for 12 Month Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2020 to 2022 (DRIFT model)")

##Intervals
electraining|>model(NAIVE(`Average Price: Electricity per Kilowatt-Hour`))|>forecast(h=12)|>autoplot(Electricity2020)+labs(y="Price in US Dollars",title="12 Month Prediction Interval for Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2020 to 2022 (NAIVE)")
electraining|>model(SNAIVE(`Average Price: Electricity per Kilowatt-Hour`))|>forecast(h=12)|>autoplot(Electricity2020)+labs(y="Price in US Dollars",title="12 Month Prediction Interval for Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2020 to 2022 (SNAIVE)")
electraining|>model(NAIVE(`Average Price: Electricity per Kilowatt-Hour`~drift()))|>forecast(h=12)|>autoplot(Electricity2020)+labs(y="Price in US Dollars",title="12 Month Prediction Interval for Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2020 to 2022 (DRIFT)")

###TEST Data from 2020 to 2023 electricity
electest<-Elec|>filter_index("2022 February"~"2023 February")|>select(`Average Price: Electricity per Kilowatt-Hour`)
###TEST Data from 2020 to 2023 oil
Oiltest<-Oil|>filter_index("2022 February"~"2023 February")|>select(`Average Price Fuel Oil per Gallon`)

##Fit models
elec_fitest<-electest|>model(Mean = MEAN(`Average Price: Electricity per Kilowatt-Hour`),
`Naïve` = NAIVE(`Average Price: Electricity per Kilowatt-Hour`),`Seasonal naïve` = SNAIVE(`Average Price: Electricity per Kilowatt-Hour`),Drift = NAIVE(`Average Price: Electricity per Kilowatt-Hour` ~ drift()))
elec_fctest<-elec_fitest|>forecast(h=12)
elec_fctest|>autoplot(electest, level = NULL) +
autolayer(filter_index(Elec, "2023 January" ~ .),colour = "black") +labs(y = "Price in US Dollars",title = "12 Month Forecasts for monthly Average Price: Electricity per Kilowatt-Hour in Boston-Cambridge-Newton from 2023 to 2024") +guides(colour = guide_legend(title = "Forecast"))                      
#Fitted Values and residuals
augment(elec_fitest)
##ARIMA 
#training
NONseasonalArima<-electraining|>model(ARIMA(`Average Price: Electricity per Kilowatt-Hour`))
report(NONseasonalArima)
accuracy(NONseasonalArima)
NONseasonalArima|>forecast(h=12)|>autoplot(Electricity2020)+geom_line(aes(y = .fitted), col="#D55E00",data = augment(NONseasonalArima)) +labs(y="Price in US Dollars",title = "12 Month Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2020 to 2022 (non-seasonal ARIMA model)")
NONseasonalArima|>gg_tsresiduals()+labs(title = "Residual Diagnostic Plots for 12 Month Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2020 to 2022 (no-seasonal ARIMA model)")

#test
NONseasonalArima<-Electricity2020|>model(ARIMA(`Average Price: Electricity per Kilowatt-Hour`))
report(NONseasonalArima)
accuracy(NONseasonalArima)
NONseasonalArima|>forecast(h=12)|>autoplot(Electricity2020)+geom_line(aes(y = .fitted), col="#D55E00",data = augment(NONseasonalArima)) +labs(y="Price in US Dollars",title = "12 Month Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2023to 2024 (non-seasonal ARIMA model)")
##SEASONAL
Electricity1990 |>gg_tsdisplay(difference(`Average Price: Electricity per Kilowatt-Hour`, 12)|>difference(),plot_type='partial', lag=36) +
labs(title="Seasonally double differenced", y="")
#Training
SeasonalArima<-electraining|>model(arima1=ARIMA(`Average Price: Electricity per Kilowatt-Hour`~pdq(0,0,0)+PDQ(0,1,0)),
arima2=ARIMA(`Average Price: Electricity per Kilowatt-Hour`~pdq(2,1,0)+PDQ(0,1,0)),
auto=ARIMA(`Average Price: Electricity per Kilowatt-Hour`,stepwise = FALSE,approx=FALSE))
SeasonalArima|>pivot_longer(everything(),names_to = "Model name",values_to = "Orders")
glance(SeasonalArima) |> arrange(AICc) |> select(.model:BIC)
SeasonalArima|>select(auto)|>gg_tsresiduals(lag=36)+labs(title = "Residual Diagnostic Plots for 12 Month Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2020 to 2022 (ARIMA model)")
augment(SeasonalArima)|>filter(.model=="auto")|>features(.innov,ljung_box,lag=24,dox=4)
forecast(SeasonalArima,h=12)|>filter(.model=="auto")|>autoplot(Electricity2020)+geom_line(aes(y = .fitted), col="#D55E00",data = augment(SeasonalArima)) +labs(y="Price in US Dollars",title = "12 Month Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2020 to 2022 (ARIMA model)")
#test
SeasonalArima<-Electricity2020|>model(arima1=ARIMA(`Average Price: Electricity per Kilowatt-Hour`~pdq(0,0,0)+PDQ(0,1,0)),
arima2=ARIMA(`Average Price: Electricity per Kilowatt-Hour`~pdq(2,1,0)+PDQ(0,1,1)),
auto=ARIMA(`Average Price: Electricity per Kilowatt-Hour`,stepwise = FALSE,approx=FALSE))
SeasonalArima|>pivot_longer(everything(),names_to = "Model name",values_to = "Orders")
glance(SeasonalArima)|>arrange(AICc)|>select(.model:BIC)
SeasonalArima|>select(auto)|>gg_tsresiduals(lag=36)+labs(title = "Residual Diagnostic Plots for 12 Month Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2023 to 2024 (ARIMA model)")
augment(SeasonalArima)|>filter(.model=="auto")|>features(.innov,ljung_box,lag=24,dox=4)
forecast(SeasonalArima,h=12)|>filter(.model=="auto")|>autoplot(Electricity2020)+geom_line(aes(y = .fitted), col="#D55E00",data = augment(SeasonalArima)) +labs(y="Price in US Dollars",title = "12 Month Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2023 to 2024 (ARIMA model)")
##LSTM
#Training
LSTMelec<-electraining|>model(TSLM(`Average Price: Electricity per Kilowatt-Hour`~trend()+season()))
glance(LSTMelec) |>select(adj_r_squared, CV, AIC, AICc, BIC)
report(LSTMelec)
accuracy(LSTMelec)
fcLSTM<-forecast(LSTMelec,h=12)
fcLSTM|>autoplot(Electricity2020)+geom_line(aes(y = .fitted), col="#D55E00",data = augment(LSTMelec)) +labs(y="Price in US Dollars",title = "12 Month Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2020 to 2022 (LSTM model)")
#Test
LSTMelec<-Electricity2020|>model(TSLM(`Average Price: Electricity per Kilowatt-Hour`~trend()+season()))
glance(LSTMelec) |>select(adj_r_squared, CV, AIC, AICc, BIC)
report(LSTMelec)
accuracy(LSTMelec)
fcLSTM<-forecast(LSTMelec,h=12)
fcLSTM|>autoplot(Electricity2020)+geom_line(aes(y = .fitted), col="#D55E00",data = augment(LSTMelec)) +labs(y="Price in US Dollars",title = "12 Month Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2023 to 2024 (LSTM model)")
##Fourier
#Training
Fourierelec<-electraining|> model(TSLM(`Average Price: Electricity per Kilowatt-Hour` ~ trend() + fourier(K = 4)))
report(Fourierelec)
accuracy(Fourierelec)
fcFourierelec<-forecast(Fourierelec,h=12)
fcFourierelec|>autoplot(Electricity2020)+geom_line(aes(y = .fitted), col="#D55E00",data = augment(Fourierelec)) +labs(y="Price in US Dollars",title = "12 Month Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2020 to 2022 (Fourier model)")
#test
Fourierelec<-electest|> model(TSLM(`Average Price: Electricity per Kilowatt-Hour` ~ trend() + fourier(K = 4)))
report(Fourierelec)
accuracy(Fourierelec)
fcFourierelec<-forecast(Fourierelec,h=12)
fcFourierelec|>autoplot(Electricity2020)+geom_line(aes(y = .fitted), col="#D55E00",data = augment(Fourierelec)) +labs(y="Price in US Dollars",title = "12 Month Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2023 to 2024 (Fourier model)")
##ETS
#training
ETSelec<-electraining|>model(ETS(`Average Price: Electricity per Kilowatt-Hour`))
ETSfc<-ETSelec|>forecast(h=12)
report(ETSelec)
accuracy(ETSelec)
ETSfc|>autoplot(Electricity2020) +geom_line(aes(y = .fitted), col="#D55E00",data = augment(ETSelec)) +
  labs(y="Price in US Dollars", title="12 Month Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2020 to 2022 (ETS (M,A,N) model)") +
  guides(colour = "none")
#test
ETSelec<-Electricity2020|>model(ETS(`Average Price: Electricity per Kilowatt-Hour`))
ETSfc<-ETSelec|>forecast(h=12)
report(ETSelec)
accuracy(ETSelec)
ETSfc|>autoplot(Electricity2020)+geom_line(aes(y = .fitted), col="#D55E00",data = augment(ETSelec))+
  labs(y="Price in US Dollars", title="12 Month Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2023 to 2024 (ETS (M,N,N) model)")
components(ETSelec)|>autoplot()


###Seeing model accuracy with other time frames other dates###
Electricity1990<-Elec|>filter(between(year(Month),1990,2020))
##Data from 1990 to 2015 electricity
electraining2.0<-Electricity1990|>filter_index("1990 January"~"2015 January")|>select(`Average Price: Electricity per Kilowatt-Hour`)
##Fourier model(How to increase the K?)
#Training
Fourierelec2.0<-electraining2.0|> model(TSLM(`Average Price: Electricity per Kilowatt-Hour` ~ trend() + fourier(K = 2)))
report(Fourierelec2.0)
accuracy(Fourierelec2.0)
fcFourierelec2.0<-forecast(Fourierelec2.0,h=72)
fcFourierelec2.0|>autoplot(Electricity1990)+geom_line(aes(y = .fitted), col="#D55E00",data = augment(Fourierelec2.0)) +labs(y="Price in US Dollars",title = "5 Year Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 1990 to 2015 (Fourier model)")

## ARIMA 
#training
NONseasonalArima2.0<-electraining2.0|>model(ARIMA(`Average Price: Electricity per Kilowatt-Hour`))
report(NONseasonalArima2.0)
accuracy(NONseasonalArima2.0)
NONseasonalArima2.0|>forecast(h=72)|>autoplot(Electricity1990)+geom_line(aes(y = .fitted), col="#D55E00",data = augment(NONseasonalArima2.0)) +labs(y="Price in US Dollars",title = "5 Year Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 1990 to 2015 (ARIMA model)")
NONseasonalArima2.0|>gg_tsresiduals()+labs(title = "Residual Diagnostic Plots for 5 year Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2015 to 2020 (ARIMA model)")

##LSTM
#Training
LSTMelec2.0<-electraining2.0|>model(TSLM(`Average Price: Electricity per Kilowatt-Hour`~trend()+season()))
glance(LSTMelec2.0) |>select(adj_r_squared, CV, AIC, AICc, BIC)
report(LSTMelec2.0)
accuracy(LSTMelec2.0)
fcLSTM2.0<-forecast(LSTMelec2.0,h=72)
fcLSTM2.0|>autoplot(Electricity1990)+geom_line(aes(y = .fitted), col="#D55E00",data = augment(LSTMelec2.0)) +labs(y="Price in US Dollars",title = "12 Month Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 2015 to 2020 (LSTM model)")
##ETS
#training
ETSelec2.0<-electraining2.0|>model(ETS(`Average Price: Electricity per Kilowatt-Hour`))
ETSfc2.0<-ETSelec2.0|>forecast(h=72)
report(ETSelec2.0)
accuracy(ETSelec2.0)
ETSfc2.0|>autoplot(Electricity1990) +geom_line(aes(y = .fitted), col="#D55E00",data = augment(ETSelec2.0)) +
  labs(y="Price in US Dollars", title="5 Year Forecast Average Electricity Price per Kilowatt-Hour in Boston-Cambridge-Newton from 1990 to 2015 (ETS (M,Ad,M) model)") +
  guides(colour = "none")

#####Vector autoregression model####
#Convert the CSV file to a tsibble to work with the data
OilElec<-OilElec_Final|>
  mutate(Month = yearmonth(observation_date))|>as_tsibble(index=Month)|>
  select(-observation_date)
#Regression#
na.omit(OilElec)
ggplot(OilElec, aes(x=`Average Price Fuel Oil per Gallon`, y=`Average Price: Electricity per Kilowatt-Hour`))+geom_point()+
  ggtitle("Average Electricity Price VS Average Oil Prices")+ 
  geom_smooth(method = "lm", se = FALSE)
summary(lm(OilElec$`Average Price: Electricity per Kilowatt-Hour`~OilElec$`Average Price Fuel Oil per Gallon`))
#Model
vectoroilelec<-OilElec |>
  model(
    aicc = VAR(vars(`Average Price: Electricity per Kilowatt-Hour`, `Average Price Fuel Oil per Gallon`)),
    bic = VAR(vars(`Average Price: Electricity per Kilowatt-Hour`, `Average Price Fuel Oil per Gallon`), ic = "bic"))
vectoroilelec
glance(vectoroilelec)
vectoroilelec |>augment() |>ACF(.innov) |>autoplot()
vectoroilelec |>select(aicc) |> forecast(h=12) |>autoplot(OilElec|>filter(year(Month)>2020))+labs(y="Price in US Dollars", title="Vector autoregressions models")
accuracy(vectoroilelec)
#Seeing performance
OilElec1990<-OilElec|>filter(between(year(Month),1990,2020))



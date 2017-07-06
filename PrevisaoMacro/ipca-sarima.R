oldwd <- getwd()
setwd('C:/Users/Mario/Dropbox/R/PrevisaoMacro')

################### Carregar dados #######################################
ipca <- read.csv('ipca.csv',header = T, sep = ';', dec = ',')
ipca <- ts(ipca[,2], start = c(1980,1), freq = 12)

##################### Selecionar subamostra ###############################
library(changepoint)
library(ggfortify)

autoplot(cpt.meanvar(ipca), main='Variação mensal do IPCA (%)')+
  scale_x_date(date_breaks = '1 year',date_labels = "%b %y")
ipca <- window(ipca, c(1995,1), freq =12)
ipca <- window(ipca, c(2004,1), freq =12)
ipca <- window(ipca, c(2007,2), freq =12)
train <- window(ipca, end = end(ipca) - c(1,0))
test <- window(ipca, start = end(train) + c(0,1))

##################### SARIMA e analise residuos ###########################
library(forecast)

fit_sarima <- auto.arima(train, seasonal = T)
summary(fit_sarima)

ggtsdisplay(residuals(fit_sarima))
ggAcf(residuals(fit_sarima), main = "Autocorrelação resíduos")
# Ljung Box: H0 resíduos sao iid
Box.test(residuals(fit_sarima), lag=24, fitdf=length(coef(fit_sarima)),
         type="Ljung")

############### Avaliando a previsao #####################################
# 12 passoas a frente (dinamica)
fcast.fit_sarima <- forecast(fit_sarima, h=length(test))$mean
accuracy(fcast.fit_sarima, test)

# 1 passoa a frente (estatica)
fit <- Arima(test, model = fit_sarima)
accuracy(fit)

############### Previsão ###############################################
onestep <- fitted(fit) # valores previsao um passo a frente

plot(forecast(fit_sarima, h=12),xlab='', ylab='(% a.m.)', bty='l',
     main='IPCA Mensal')
lines(test, col='black', lwd=2)
lines(onestep, col='red', lwd=2)
legend('topleft', col=c('blue','red'), lty=c(1,1), lwd=c(2,2),
       legend=c('12 meses', '1 mês'))

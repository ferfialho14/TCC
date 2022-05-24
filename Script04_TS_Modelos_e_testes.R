#### --------------------------
#### Modelos 
#### --------------------------

fixa.hwa <- forecast::hw(treino_fixa, h = 3, seasonal = "additive")
var.hwa <- forecast::hw(treino_var, h = 3, seasonal = "additive")
invext.hwa <- forecast::hw(treino_invext, h = 3, seasonal = "additive")

fixa.hwm <- forecast::hw(treino_fixa, h = 3, seasonal = "multiplicative")
var.hwm <- forecast::hw(treino_var, h = 3, seasonal = "multiplicative")
invext.hwm <- forecast::hw(treino_invext, h = 3, seasonal = "multiplicative")

fixa.ses <- forecast::ses(treino_fixa, h = 3)
var.ses <- forecast::ses(treino_var, h = 3)
invext.ses <- forecast::ses(treino_invext, h = 3)

fixa.holt <- forecast::holt(treino_fixa, h = 3)
var.holt <- forecast::holt(treino_var, h = 3)
invext.holt <- forecast::holt(treino_invext, h = 3)

fixa.ets <- ets(treino_fixa, model = "ZZZ")
fixa.ets.forecasts <- forecast.ets(fixa.ets, h = 3)
var.ets <- ets(treino_var, model = "ZZZ")
var.ets.forecasts <- forecast.ets(var.ets, h = 3)
invext.ets <- ets(treino_invext, model = "ZZZ")
invext.ets.forecasts <- forecast.ets(invext.ets, h = 3)

arima_fixa=auto.arima(treino_fixa, trace=T)
fixa.arima.forecast=forecast::forecast(arima_fixa, h=3)
arima_var=auto.arima(treino_var, trace=T)
var.arima.forecast=forecast::forecast(arima_var, h=3)
arima_invext=auto.arima(treino_invext, trace=T)
invext.arima.forecast=forecast::forecast(arima_invext, h=3)


summary(fixa.hwa) #HoltWinters Sazonal Aditivo
summary(var.hwa) #HoltWinters Sazonal Aditivo
summary(invext.hwa) #HoltWinters Sazonal Aditivo

summary(fixa.hwm) #HoltWinters Sazonal Multiplicativo
summary(var.hwm) #HoltWinters Sazonal Multiplicativo
summary(invext.hwm) #HoltWinters Sazonal Multiplicativo

summary(fixa.ses) #Suzvização Exponencial Simples
summary(var.ses) #Suzvização Exponencial Simples
summary(invext.ses) #Suzvização Exponencial Simples

summary(fixa.holt) #HoltWinters com Tendência
summary(var.holt) #HoltWinters com Tendência
summary(invext.holt) #HoltWinters com Tendência

summary(fixa.ets.forecasts) #ETS(A,N,N)
summary(var.ets.forecasts) #ETS(A,N,N)
summary(invext.ets.forecasts) #ETS(A,A,N)

summary(fixa.arima.forecast) #ARIMA (0,1,0)
summary(var.arima.forecast) #ARIMA (0,1,0)
summary(invext.arima.forecast) #ARIMA (0,1,0) with drift 

#### --------------------------
# Acurácia
#### --------------------------

accuracy(fixa.hwa$mean,teste_fixa)
accuracy(var.hwa$mean,teste_var)
accuracy(invext.hwa$mean,teste_invext)

accuracy(fixa.hwm$mean,teste_fixa)
accuracy(var.hwm$mean,teste_var)
accuracy(invext.hwm$mean,teste_invext)

accuracy(fixa.ses$mean,teste_fixa)
accuracy(var.ses$mean,teste_var)
accuracy(invext.ses$mean,teste_invext)

accuracy(fixa.holt$mean,teste_fixa)
accuracy(var.holt$mean,teste_var)
accuracy(invext.holt$mean,teste_invext)

accuracy(fixa.ets.forecasts$mean,teste_fixa)
accuracy(var.ets.forecasts$mean,teste_var)
accuracy(invext.ets.forecasts$mean,teste_invext)

accuracy(fixa.arima.forecast$mean, teste_fixa)
accuracy(var.arima.forecast$mean, teste_var)
accuracy(invext.arima.forecast$mean, teste_invext)

#### --------------------------
# Teste estatísitco de Ljung-box
# H0: os resíduos são iid (modelo não exibe falhas de ajustes) Independentes e igualmente distribuídos
# H1: os resíduos não são iid (modelo exibe falhas de ajustes)
# p-valor > 0.05 resíduos não são autocorrelacionados
# Não podem ter autocorrelação e precisam ter distribuição normal
# Na função de autocorrelação (ACF), todos os lags estão dentro do intervalo de confiança
#### --------------------------

checkresiduals(arima_fixa)
checkresiduals(arima_var)
checkresiduals(arima_invext)

checkresiduals(fixa.ets)
checkresiduals(var.ets)
checkresiduals(invext.ets)

#### --------------------------
#Normalidade dos resíduos
#Teste Shapiro-Wilk
#### --------------------------
shapiro.test(arima_fixa$residuals) #0.7271
shapiro.test(arima_var$residuals) # 0.2221
shapiro.test(arima_invext$residuals) #0.01705

shapiro.test(fixa.ets$residuals) # 0.9457
shapiro.test(var.ets$residuals) # 0.2668
shapiro.test(invext.ets$residuals) # 0.08371

shapiro.test(base_fixa)


ks.test(arima_fixa$residuals, "pnorm", mean(arima_fixa$residuals),
        sd(arima_fixa$residuals))
#Resultado: p-valor 0.8969 > 0.05
#H0 - Resíduos normais

ks.test(arima_var$residuals, "pnorm", mean(arima_var$residuals),
        sd(arima_var$residuals))
#Resultado: p-valor 0.9124 > 0.05
#H0 - Resíduos normais

ks.test(arima_invext$residuals, "pnorm", mean(arima_invext$residuals),
        sd(arima_invext$residuals))
#Resultado: p-valor 0.3205 > 0.05
#H0 - Resíduos normais

ks.test(fixa.ets$residuals, "pnorm", mean(fixa.ets$residuals),
        sd(fixa.ets$residuals))
#Resultado: p-valor 0.9753 > 0.05
#H0 - Resíduos normais

ks.test(var.ets$residuals, "pnorm", mean(var.ets$residuals),
        sd(var.ets$residuals))
#Resultado: p-valor 0.9228 > 0.05
#H0 - Resíduos normais

ks.test(invext.ets$residuals, "pnorm", mean(invext.ets$residuals),
        sd(invext.ets$residuals))
#Resultado: p-valor 0.5934 > 0.05
#H0 - Resíduos normais


#### --------------------------
# verificar se existe efeitos ARCH
#### --------------------------

ArchTest(arima_fixa$residuals)
# p-valor 0.7029 > 0.05, 
#H0, não existência de efeitos ARCH

ArchTest(arima_var$residuals)
# p-valor 0.7029 > 0.05, 
#H0, não existência de efeitos ARCH

ArchTest(arima_invext$residuals)
# p-valor 0.7029 > 0.05, 
#H0, não existência de efeitos ARCH
#a variância está estabilizada/estacionária

ArchTest(fixa.ets$residuals)
# p-valor 0.7029 > 0.05, 
#H0, não existência de efeitos ARCH

ArchTest(var.ets$residuals)
# p-valor 0.7029 > 0.05, 
#H0, não existência de efeitos ARCH

ArchTest(invext.ets$residuals)
# p-valor 0.7029 > 0.05, 
#H0, não existência de efeitos ARCH

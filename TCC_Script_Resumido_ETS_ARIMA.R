#### --------------------------
#### Instalação e Carregamento de Todos os Pacotes
#### --------------------------

pacotes <- c("readr","readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth", "tsibble", "fable","tsibbledata", "fpp3","lubridate",
             "urca", "dygraphs", "quantmod","BETS","tseries","FinTS",
             "scales", "caret","xtable", "tsutils","GetBCBData", 
             "quantmod","dgof","seasonal", "lubridate")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}


#### --------------------------
#### Importando do Excel Base Sudeste
####                e
#### Transformando base em tsibble
#### --------------------------

#----------Sudeste----------- 96 x 5
base_sudeste <- read_excel("TS_base_sudeste_por_segmento.xlsx")
base_sudeste_tsibble <- base_sudeste %>%
  mutate(ANO_MES = yearmonth(as.character(ANO_MES))) %>%
  as_tsibble(index = ANO_MES, key = CO_UF)  
base_sudeste_tsibble

glimpse(base_sudeste_tsibble)

#### --------------------------
#### ARIMA e ETS
#### --------------------------

#----------Renda Fixa----------
renda_fixa_modelo_comparativo=base_sudeste_tsibble %>%
  mutate(ANO_MES = as.Date(ANO_MES)) %>% 
  filter(ANO_MES < "2021-10-01") %>% 
  mutate(ANO_MES = yearmonth(ANO_MES)) %>% 
  model(arima = fable::ARIMA(`Renda Fixa`),
        ets <- ETS(`Renda Fixa`))

renda_fixa_modelo_comparativo

renda_fixa_modelo_comparativo %>%
  forecast(h = 3) %>%
  mutate(ANO_MES = as.Date(ANO_MES)) %>% 
  autoplot(level = NULL) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

renda_fixa_modelo_comparativo %>%
  forecast(h = 3) %>%
  #mutate(ANO_MES = as.Date(ANO_MES)) %>% 
  autoplot(base_sudeste_tsibble,level = NULL) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

fc_renda_fixa_comparativo = renda_fixa_modelo_comparativo %>%
  forecast(h = 3)

fc_renda_fixa_comparativo

#Acurácia
accuracy(fc_renda_fixa_comparativo, base_sudeste_tsibble)

#Análise dos resíduos
augment(renda_fixa_modelo_comparativo)

#Teste de Ljung-box
augment(renda_fixa_modelo_comparativo) %>%
  features(.resid, ljung_box)
#Resultado - Rejeito H0 para ARIMA MG pois os resíduos não são iid

#----------Renda Variável----------
renda_var_modelo_comparativo=base_sudeste_tsibble %>%
  mutate(ANO_MES = as.Date(ANO_MES)) %>% 
  filter(ANO_MES < "2021-10-01") %>% 
  mutate(ANO_MES = yearmonth(ANO_MES)) %>% 
  model(arima = fable::ARIMA(`Renda Variável e Investimentos Estruturados`),
        ets <- ETS(`Renda Variável e Investimentos Estruturados`))

renda_var_modelo_comparativo

renda_var_modelo_comparativo %>%
  forecast(h = 3) %>%
  mutate(ANO_MES = as.Date(ANO_MES)) %>% 
  autoplot(level = NULL) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

renda_var_modelo_comparativo %>%
  forecast(h = 3) %>%
  #mutate(ANO_MES = as.Date(ANO_MES)) %>% 
  autoplot(base_sudeste_tsibble,level = NULL) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

fc_renda_var_comparativo = renda_var_modelo_comparativo %>%
  forecast(h = 3)

fc_renda_var_comparativo

#Acurácia
accuracy(fc_renda_var_comparativo, base_sudeste_tsibble)

#Análise dos resíduos
augment(renda_var_modelo_comparativo)

#Teste de Ljung-box
augment(renda_var_modelo_comparativo) %>%
  features(.resid, ljung_box)


#----------Investimentos no Exterior----------
invext_modelo_comparativo=base_sudeste_tsibble %>%
  mutate(ANO_MES = as.Date(ANO_MES)) %>% 
  filter(ANO_MES < "2021-10-01") %>% 
  mutate(ANO_MES = yearmonth(ANO_MES)) %>% 
  model(arima = fable::ARIMA(`Investimentos no Exterior`),
        ets <- ETS(`Investimentos no Exterior`))

invext_modelo_comparativo

invext_modelo_comparativo %>%
  forecast(h = 3) %>%
  mutate(ANO_MES = as.Date(ANO_MES)) %>% 
  autoplot(level = NULL) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

invext_modelo_comparativo %>%
  forecast(h = 3) %>%
  #mutate(ANO_MES = as.Date(ANO_MES)) %>% 
  autoplot(base_sudeste_tsibble,level = NULL) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

fc_invext_comparativo = invext_modelo_comparativo %>%
  forecast(h = 3)

fc_invext_comparativo

#Acurácia
accuracy(fc_invext_comparativo, base_sudeste_tsibble)

#Análise dos resíduos
augment(invext_modelo_comparativo)

#Teste de Ljung-box
augment(invext_modelo_comparativo) %>%
  features(.resid, ljung_box)
#Resultado - Rejeito H0 para ETS MG pois os resíduos não são iid





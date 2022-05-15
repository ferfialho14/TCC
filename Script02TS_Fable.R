# Instalação e Carregamento de Todos os Pacotes ---------------------------
pacotes <- c("readr","readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth", "tsibble", "fable","tsibbledata", "fpp3","lubridate",
             "urca", "dygraphs", "quantmod","BETS","tseries","FinTS",
             "gridExtra", "scales", "caret","xtable", "tsutils","GetBCBData", 
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



###### Importando dados do Excel

#Base Sudeste Completa - 96 x 5
 base_sudeste <- read_excel("TS_base_sudeste_por_segmento.xlsx")
 base_sudeste_tsibble <- base_sudeste %>%
   mutate(ANO_MES = yearmonth(as.character(ANO_MES))) %>%
 as_tsibble(index = ANO_MES, key = CO_UF)  
 base_sudeste_tsibble
 
 #Base Sudeste Treino - 21 meses - 84 x 5
 base_sudeste_treino <- read_excel("TS_base_sudeste_por_segmento_treino.xlsx")
 base_sudeste_tsibble_treino <- base_sudeste_treino %>%
   mutate(ANO_MES = yearmonth(as.character(ANO_MES))) %>%
   as_tsibble(index = ANO_MES, key = CO_UF)  
 base_sudeste_tsibble_treino
 
 #Base Sudeste Teste - 3 meses - 12 x 5
 base_sudeste_teste <- read_excel("TS_base_sudeste_por_segmento_teste.xlsx")
 base_sudeste_tsibble_teste <- base_sudeste_teste %>%
   mutate(ANO_MES = yearmonth(as.character(ANO_MES))) %>%
   as_tsibble(index = ANO_MES, key = CO_UF)  
 base_sudeste_tsibble_teste

###### Lendo a base de dados

View(base_sudeste_tsibble)
View(base_sudeste_tsibble_treino)
View(base_sudeste_tsibble_teste)

####### Gráfico por Estado
  autoplot(base_sudeste_tsibble, vars('Renda Fixa')) + 
    scale_color_viridis_d() +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
    labs(x="Dia",y="Valores em Carteira") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 10),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey90"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.text = element_text(size = 7),
          legend.position = "bottom")
  
  autoplot(base_sudeste_tsibble, vars('Renda Variável e Investimentos Estruturados')) + 
    scale_color_viridis_d() +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
    labs(x="Dia",y="Valores em Carteira") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 10),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey90"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.text = element_text(size = 7),
          legend.position = "bottom")
  
  autoplot(base_sudeste_tsibble, vars('Investimentos no Exterior')) + 
    scale_color_viridis_d() +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
    labs(x="Dia",y="Valores em Carteira") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 10),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey90"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.text = element_text(size = 7),
          legend.position = "bottom")
  
  
  # Vamos rodar uma série de modelos padrões de séries temporais por variável
  # MEAN, NAIVE, SNAIVE e RW
        
  #----------Renda Fixa----------
    sudeste_fit_renda_fixa = base_sudeste_tsibble_treino %>%
      model(
      Seasonal_naive = SNAIVE(`Renda Fixa`),
      Naive = NAIVE(`Renda Fixa`),
      Drift = RW(`Renda Fixa` ~ drift()),
      Mean = MEAN(`Renda Fixa`)
    )
    sudeste_fit_renda_fixa  
    
    #----------Renda Variavel----------    
    sudeste_fit_renda_variavel = base_sudeste_tsibble_treino %>%
      model(
        Seasonal_naive = SNAIVE(`Renda Variável e Investimentos Estruturados`),
        Naive = NAIVE(`Renda Variável e Investimentos Estruturados`),
        Drift = RW(`Renda Variável e Investimentos Estruturados` ~ drift()),
        Mean = MEAN(`Renda Variável e Investimentos Estruturados`)
      )
    sudeste_fit_renda_variavel
    
    #----------Investimentos no Exterior----------    
    sudeste_fit_investext = base_sudeste_tsibble_treino %>%
      model(
        Seasonal_naive = SNAIVE(`Investimentos no Exterior`),
        Naive = NAIVE(`Investimentos no Exterior`),
        Drift = RW(`Investimentos no Exterior` ~ drift()),
        Mean = MEAN(`Investimentos no Exterior`)
      )
    sudeste_fit_investext
    
    
    ### Previsões
    #----------Renda Fixa----------
    fc_sudeste_rf = sudeste_fit_renda_fixa %>%
      forecast(h=3)
    fc_sudeste_rf %>% View()
    
    #----------Renda Variavel----------  
    fc_sudeste_rv = sudeste_fit_renda_variavel %>%
      forecast(h=3)
    fc_sudeste_rv %>% View()
    
    #----------Investimentos no Exterior----------
    fc_sudeste_ie = sudeste_fit_investext %>%
      forecast(h=3)
    fc_sudeste_ie %>% View()
    
    
    #Plotando os resultados
    #NÃO DEU CERTO - VER POR QUÊ?
    fc_sudeste_rf %>%
      autoplot(base_sudeste_treino, level = NULL) +
      scale_color_viridis_d() +
      scale_fill_viridis_d() +
      scale_y_continuous(labels = scales::comma) +
      theme_bw()
    
    # Análise dos resíduos: Não devem ser correlacionados
    # caso sejam correlacionados, dignifica que ficaram informações nos resíduos
    # que deveriam estar no modelo
    # os resíduos devem possuir média zero, caso não seja então as previsões são viesadas
    
    augment(sudeste_fit_renda_fixa)
    
    augment(sudeste_fit_renda_variavel)
    
    augment(sudeste_fit_investext)
    
    #Plotando resíduos por modelo - Base Renda Fixa
    #----------Seasonal_naive----------
    augment(sudeste_fit_renda_fixa) %>%
      filter(.model == "Seasonal_naive") %>%
      autoplot(.resid) +
      scale_color_viridis_d() +
      scale_y_continuous(labels = scales::comma) +
      theme_bw()
    
    #----------Naive----------
    augment(sudeste_fit_renda_fixa) %>%
      filter(.model == "Naive") %>%
      autoplot(.resid) +
      scale_color_viridis_d() +
      scale_y_continuous(labels = scales::comma) +
      theme_bw()
    
    #----------Naive----------
    augment(sudeste_fit_renda_fixa) %>%
      filter(.model == "Drift") %>%
      autoplot(.resid) +
      scale_color_viridis_d() +
      scale_y_continuous(labels = scales::comma) +
      theme_bw()
    
    #----------Naive----------
    augment(sudeste_fit_renda_fixa) %>%
      filter(.model == "Mean") %>%
      autoplot(.resid) +
      scale_color_viridis_d() +
      scale_y_continuous(labels = scales::comma) +
      theme_bw()
    
    
    # Teste de Ljung-box
    # H0: os resíduos são iid
    # H1: os resíduos não são iid
    # (quero um pvalor grande)
    augment(sudeste_fit_renda_fixa) %>%
      features(.resid, ljung_box)
    
    augment(sudeste_fit_renda_variavel) %>%
      features(.resid, ljung_box)
    
    augment(sudeste_fit_investext) %>%
      features(.resid, ljung_box)
    #Resultado: os p-values foram baixos, rejeito a H0 
    
    #Acurácia
    accuracy(fc_sudeste_rf, base_sudeste_tsibble_teste)
    

    # ARIMA
    # AR: autoregressivo (observações defasadas como input)
    #  I: integrado (diferenciação para tornar a série estacionária)
    # MA: média móvel (erros defasados como input)
    
    #Renda Fixa
    fit_ARIMA_rf = base_sudeste_tsibble_treino %>%
      model(arima = fable::ARIMA(`Renda Fixa`))
    fit_ARIMA_rf
    # A mable: 4 x 2
    # Key:     CO_UF [4]
    # CO_UF                  arima
    #  1 ES            <ARIMA(0,1,0)>
    #  2 MG    <ARIMA(0,0,0) w/ mean>
    #  3 RJ            <ARIMA(0,1,3)>
    #  4 SP    <ARIMA(0,0,0) w/ mean>

    fabletools::report(fit_ARIMA_rf)   
    #Resultados:
    #CO_UF     .model  sigma2 log_lik   AIC  AICc   BIC ar_roots  ma_roots 
    #  1 ES    arima  2.29e15   -382.  766.  766.  767. <cpl [0]> <cpl [0]>
    #  2 MG    arima  3.02e16   -428.  859.  860.  862. <cpl [0]> <cpl [0]>
    #  3 RJ    arima  4.19e16   -411.  831.  834.  835. <cpl [0]> <cpl [3]>
    #  4 SP    arima  1.08e17   -441.  886.  887.  888. <cpl [0]> <cpl [0]>

    
    #Renda Variável
    fit_ARIMA_rv = base_sudeste_tsibble_treino %>%
      model(arima = fable::ARIMA(`Renda Variável e Investimentos Estruturados`))
    fit_ARIMA_rv
    # A mable: 4 x 2
    # Key:     CO_UF [4]
    # CO_UF                   arima
    #  1 ES             <ARIMA(0,1,0)>
    #  2 MG             <ARIMA(0,1,0)>
    #  3 RJ             <ARIMA(0,1,0)>
    #  4 SP    <ARIMA(0,1,0) w/ drift>
    
    fabletools::report(fit_ARIMA_rv) 
    #Resultados:
    #CO_UF     .model  sigma2 log_lik   AIC  AICc   BIC ar_roots  ma_roots 
    #  1 ES    arima  3.65e15   -387.  775.  776.  776. <cpl [0]> <cpl [0]>
    #  2 MG    arima  9.26e14   -373.  748.  748.  749. <cpl [0]> <cpl [0]>
    #  3 RJ    arima  1.32e15   -377.  755.  755.  756. <cpl [0]> <cpl [0]>
    #  4 SP    arima  1.40e16   -400.  803.  804.  805. <cpl [0]> <cpl [0]>
    
    #Investimentos no Exterior
    fit_ARIMA_ie = base_sudeste_tsibble_treino %>%
      model(arima = fable::ARIMA(`Investimentos no Exterior`))
    fit_ARIMA_ie
    # A mable: 4 x 2
    # Key:     CO_UF [4]
    # CO_UF                   arima
    #  1 ES    <ARIMA(0,1,0) w/ drift>
    #  2 MG    <ARIMA(0,1,0) w/ drift>
    #  3 RJ    <ARIMA(0,1,0) w/ drift>
    #  4 SP    <ARIMA(0,1,0) w/ drift>
    
    fabletools::report(fit_ARIMA_ie) 
    #Resultados:
    #CO_UF     .model  sigma2 log_lik   AIC  AICc   BIC ar_roots  ma_roots 
    #  1 ES    arima  6.04e14   -368.  740.  741.  742. <cpl [0]> <cpl [0]>
    #  2 MG    arima  9.30e13   -349.  703.  704.  705. <cpl [0]> <cpl [0]>
    #  3 RJ    arima  1.21e14   -352.  708.  709.  710. <cpl [0]> <cpl [0]>
    #  4 SP    arima  1.01e15   -373.  751.  751.  753. <cpl [0]> <cpl [0]>
    
    ### Previsões
    #----------Renda Fixa----------
    fc_fit_ARIMA_rf = fit_ARIMA_rf %>%
      forecast(h=3)
    fc_fit_ARIMA_rf %>% View()
    
    #----------Renda Variavel----------  
    fc_fit_ARIMA_rv = fit_ARIMA_rv %>%
      forecast(h=3)
    fc_fit_ARIMA_rv %>% View()
    
    #----------Investimentos no Exterior----------
    fc_fit_ARIMA_ie = fit_ARIMA_ie %>%
      forecast(h=3)
    fc_fit_ARIMA_ie %>% View()
    
    #Prevendo 3 meses e já plotando
    #----------Renda Fixa----------
    fit_ARIMA_rf %>%
      forecast(h=3) %>%
      autoplot(base_sudeste_tsibble, level = 95) +
      scale_y_continuous(labels = scales::number_format(big.mark = ".",
                                                        decimal.mark = ",")) +
      theme_bw()
    
    #----------Renda Variável----------
    fit_ARIMA_rv %>%
      forecast(h=3) %>%
      autoplot(base_sudeste_tsibble, level = 95) +
      scale_y_continuous(labels = scales::number_format(big.mark = ".",
                                                        decimal.mark = ",")) +
      theme_bw()
    
    #----------Investimentos no Exterior----------
    fit_ARIMA_ie %>%
      forecast(h=3) %>%
      autoplot(base_sudeste_tsibble, level = 95) +
      scale_y_continuous(labels = scales::number_format(big.mark = ".",
                                                        decimal.mark = ",")) +
      theme_bw()
    
    
    #Acurácia
    accuracy(fc_fit_ARIMA_rf, base_sudeste_tsibble_teste)
    accuracy(fc_fit_ARIMA_rv, base_sudeste_tsibble_teste)
    accuracy(fc_fit_ARIMA_ie, base_sudeste_tsibble_teste)
    
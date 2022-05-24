#### --------------------------
#### Importanto Base do Excel
#### --------------------------

#----------Sudeste----------- 
base_sudeste <- read_excel("TS_base_sudeste_por_segmento.xlsx") %>%
  mutate(ANO_MES = yearmonth(as.character(ANO_MES)))
base_sudeste

glimpse(base_sudeste)
View(base_sudeste)
base_sudeste


#### --------------------------
#### Transformando as bases em Séries Temporais Univariadas
#### --------------------------

#----------Renda Fixa----------
base_fixa <- ts(data = base_sudeste[, 2],
                         start = c(2020, 1),
                         end = c(2021, 12),
                         frequency = 12)

#----------Renda Variável----------
base_var <- ts(data = base_sudeste[, 3],
                      start = c(2020, 1),
                      end = c(2021, 12),
                      frequency = 12)

#----------Investimento no Exterior----------
base_invext <- ts(data = base_sudeste[, 4],
                      start = c(2020, 1),
                      end = c(2021, 12),
                      frequency = 12)


#### --------------------------
#### Medidas de Estatística Descritivas
#### --------------------------

summary(base_fixa)
summary(base_var)
summary(base_invext)
sd(base_fixa) #desvio padrão
sd(base_var) #desvio padrão
sd(base_invext) #desvio padrão

length(base_fixa) # comprimento da série
mean(base_fixa) #média


#### --------------------------
#### Definir janela de tempo da Série Temporal
#### --------------------------

#----------Renda Fixa----------
treino_fixa=window(base_fixa,start=c(2020,1), end=c(2021,9))
teste_fixa=window(base_fixa,start=c(2021,10),end=c(2021,12))

#----------Renda Variável----------
treino_var=window(base_var,start=c(2020,1), end=c(2021,9))
teste_var=window(base_var,start=c(2021,10),end=c(2021,12))

#----------Investimento no Exterior----------
treino_invext=window(base_invext,start=c(2020,1), end=c(2021,9))
teste_invext=window(base_invext,start=c(2021,10),end=c(2021,12))


#### --------------------------
#### Plotando janela completa
#### --------------------------

#----------Renda Fixa----------
autoplot(base_fixa)+
  autolayer(treino_fixa,series="Treino") +
  autolayer(teste_fixa,series = "Teste") +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

#----------Renda Variável----------
autoplot(base_var)+
  autolayer(treino_var,series="Treino") +
  autolayer(teste_var,series = "Teste") +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

#----------Investimento no Exterior----------
autoplot(base_invext)+
  autolayer(treino_invext,series="Treino") +
  autolayer(teste_invext,series = "Teste") +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()


#### --------------------------
#### Gráfico Série Temporal
#### --------------------------

#----------Renda Fixa----------
ggplotly(
  base_sudeste %>%
    mutate(ANO_MES = as.Date(ANO_MES)) %>% 
    ggplot() +
    geom_line(aes(x = ANO_MES, y = base_fixa)) +
    labs(title = "RPPS Sudeste - Segmento Renda Fixa",
         x = "Mês-Ano",
         y = "Valor em Carteira") +
    scale_color_viridis_d() +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey90"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "none")
)

#----------Renda Variável----------
ggplotly(
  base_sudeste %>%
    mutate(ANO_MES = as.Date(ANO_MES)) %>% 
    ggplot() +
    geom_line(aes(x = ANO_MES, y = base_var)) +
    labs(title = "RPPS Sudeste - Segmento Renda Variável",
         x = "Mês-Ano",
         y = "Valor em Carteira") +
    scale_color_viridis_d() +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey90"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "none")
)

#----------Investimento no Exterior----------
ggplotly(
  base_sudeste %>%
    mutate(ANO_MES = as.Date(ANO_MES)) %>% 
    ggplot() +
    geom_line(aes(x = ANO_MES, y = base_invext)) +
    labs(title = "RPPS Sudeste - Segmento Investimento no Exterior",
         x = "Mês-Ano",
         y = "Valor em Carteira") +
    scale_color_viridis_d() +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey90"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "none")
)

#### --------------------------
#### Gráfico Simples por segmento
#### --------------------------

dados1=ts(matrix(1,24,3))
dados1[,1]=base_fixa
dados1[,2]=base_var
dados1[,3]=base_invext

colnames(dados1)[1]='Renda Fixa'
colnames(dados1)[2]='Renda Variável'
colnames(dados1)[3]='Inv. no Exterior'

options(scipen = 999) # para resolver a notação científica

plot(dados1, main="Aplicações RPPS Sudeste por segmento",
     xlab="Tempo em meses (01/2020 a 12/2021)")

#### --------------------------
## Análise da Série
#### --------------------------
#----------Renda Fixa----------
ggtsdisplay(base_fixa)
ggtsdisplay(treino_fixa)

acf(base_fixa)
acf(treino_fixa)
pacf(base_fixa)
pacf(treino_fixa)

#----------Renda Variável----------
ggtsdisplay(base_var)
ggtsdisplay(treino_var)

acf(base_var)
acf(treino_var)
pacf(base_var)
pacf(treino_var)

#----------Investimento no Exterior----------
ggtsdisplay(base_invext)
ggtsdisplay(treino_invext)

acf(base_invext)
acf(treino_invext)
pacf(base_invext)
pacf(treino_invext)



#### --------------------------
# Teste de Dickey-Fuller
# Estacionariedade
#### --------------------------

#----------Renda Fixa----------
teste_est_fixa=ur.df(treino_fixa)
summary(teste_est_fixa)
# Conclusão: z.lag1 0.828 tem p-valor > 0.05 - aceita-se H0
# portanto a ST é não estacionária

#----------Renda Variável----------
teste_est_var=ur.df(treino_var)
summary(teste_est_var)
# Conclusão: z.lag1 0.263 tem p-valor > 0.05 - aceita-se H0
# portanto a ST é não estacionária

#----------Investimento no Exterior----------
teste_est_invext=ur.df(treino_invext)
summary(teste_est_invext)
# Conclusão: z.lag1 0.00766 tem p-valor < 0.05 - rejeita-se H0
# portanto a ST é estacionária


#### --------------------------
#### Diferenciação
#### remove a tendênca da série, para ser estacionária
#### --------------------------

#----------Renda Fixa----------
ndiffs(treino_fixa)
#Resultado = 1

difftreino_fixa=diff(treino_fixa)
ggtsdisplay(difftreino_fixa)

treino_est_fixa_dif=ur.df(difftreino_fixa)
summary(treino_est_fixa_dif)
# Conclusão: z.lag1 0.000355 tem p-valor > 0.05 - rejeita-se H0
# portanto a ST é estacionária

#----------Renda Variável----------
ndiffs(treino_var)
#Resultado = 1

difftreino_var=diff(treino_var)
ggtsdisplay(difftreino_var)

treino_est_var_dif=ur.df(difftreino_var)
summary(treino_est_var_dif)
# Conclusão: z.lag1 0.00525 tem p-valor > 0.05 - rejeita-se H0
# portanto a ST é estacionária

#----------Investimento no Exterior----------
#ST já era estacionária
ndiffs(treino_invext)
#Resultado = 1

difftreino_invext=diff(treino_invext)
ggtsdisplay(difftreino_invext)

treino_est_invext_dif=ur.df(difftreino_invext)
summary(treino_est_invext_dif)
# Conclusão: z.lag1 0.199 tem p-valor > 0.05 - aceita-se H0
# portanto a ST é não estacionária


#### --------------------------
#Normalidade dos dados
#Teste Shapiro-Wilk
#### --------------------------

shapiro.test(base_fixa) #0.4516
shapiro.test(base_var) #0.1338
shapiro.test(base_invext) #0.07104

#### --------------------------
#### Gráficos Resultados
#### --------------------------

#----------Renda Fixa----------
autoplot(base_fixa) +
  forecast::autolayer(fixa.hwa,
                      series = "Holt-Winters Adit",
                      PI = FALSE) +
  forecast::autolayer(fixa.hwm,
                      series = "Holt-Winters Mult",
                      PI = FALSE) +
  forecast::autolayer(fixa.ses,
                      series = "SES",
                      PI = FALSE) +
  forecast::autolayer(fixa.holt,
                      series = "Holt-Winters Tend",
                      PI = FALSE) +
  xlab("Time") +
  ylab("Valores em Carteira") +
  ggtitle("Forecast Renda Fixa") +
  guides(colour = guide_legend(title = "Forecast")) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()


autoplot(base_fixa) +
  forecast::autolayer(fixa.ets.forecasts,
                      series = "ETS",
                      PI = FALSE) +
  forecast::autolayer(fixa.arima.forecast,
                      series = "ARIMA",
                      PI = FALSE) +
  xlab("Time") +
  ylab("Valores em Carteira") +
  ggtitle("Forecast Renda Fixa") +
  guides(colour = guide_legend(title = "Forecast")) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

#----------Renda Variável----------
autoplot(base_var) +
  forecast::autolayer(var.hwa,
                      series = "Holt-Winters Adit",
                      PI = FALSE) +
  forecast::autolayer(var.hwm,
                      series = "Holt-Winters Mult",
                      PI = FALSE) +
  forecast::autolayer(var.ses,
                      series = "SES",
                      PI = FALSE) +
  forecast::autolayer(var.holt,
                      series = "Holt-Winters Tend",
                      PI = FALSE) +
  xlab("Time") +
  ylab("Valores em Carteira") +
  ggtitle("Forecast Renda Variável") +
  guides(colour = guide_legend(title = "Forecast")) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

autoplot(base_var) +
  forecast::autolayer(var.ets.forecasts,
                      series = "ETS",
                      PI = FALSE) +
  forecast::autolayer(var.arima.forecast,
                      series = "ARIMA",
                      PI = FALSE) +
  xlab("Time") +
  ylab("Valores em Carteira") +
  ggtitle("Forecast Renda Variável") +
  guides(colour = guide_legend(title = "Forecast")) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()


#----------Investimento no Exterior----------
autoplot(base_invext) +
  forecast::autolayer(invext.hwa,
                      series = "Holt-Winters Adit",
                      PI = FALSE) +
  forecast::autolayer(invext.hwm,
                      series = "Holt-Winters Mult",
                      PI = FALSE) +
  forecast::autolayer(invext.ses,
                      series = "SES",
                      PI = FALSE) +
  forecast::autolayer(invext.holt,
                      series = "Holt-Winters Tend",
                      PI = FALSE) +
  xlab("Time") +
  ylab("Valores em Carteira") +
  ggtitle("Forecast Investimento no Exterior") +
  guides(colour = guide_legend(title = "Forecast")) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

autoplot(base_invext) +
  forecast::autolayer(invext.ets.forecasts,
                      series = "ETS",
                      PI = FALSE) +
  forecast::autolayer(invext.arima.forecast,
                      series = "ARIMA",
                      PI = FALSE) +
  xlab("Time") +
  ylab("Valores em Carteira") +
  ggtitle("Forecast Investimento no Exterior") +
  guides(colour = guide_legend(title = "Forecast")) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()


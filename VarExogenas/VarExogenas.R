# Modelos con variables exogenas
# Las tasas de interés de los bonos del Tesoro a corto y largo plazo están estrechamente
# vinculadas a las condiciones macroeconómicas. Si bien las tasas de interés de ambos 
# tipos de bonos tienen las mismas tendencias a largo plazo, se comportan de manera 
# bastante diferente a corto plazo. La diferencia en las tasas de interés de dos bonos
# con distintos vencimientos se denomina diferencial de plazo o term spread

# La idea en este ejercicio es analizar los efectos del term spread sobre el crecimiento
# económico

# Se tienen datos trimestrales para los Estados Unidos entre 1957Q1 y 2013Q4, para las siguientes variables
# GDPC96: PIB real a precios de 1996
# JAPAN_IP: índice de producción industrial Japones 
# GS10: tasa de interés de los bonos del tesoro de US a 10 años
# TB3MS: tasa de interés de los bonos del tesoro de US a 3 meses
# UNRATE: tasa de desempleo
# EXUSUK: tasa de cambio dólares libra esterlina
# Y otras variables

library(tidyverse); library(dynlm); library(dLagM); library(AER); library(xts)
library(ecm); library(openxlsx); library(urca)

data <- read.xlsx("https://www.princeton.edu/~mwatson/Stock-Watson_3u/Students/EE_Datasets/us_macro_quarterly.xlsx",
                        sheet = 1) |>
  rename(date=X1) |> 
  mutate(date = as.yearqtr(date, format = "%Y:0%q"),
         TSpread = GS10 - TB3MS,
         TSpread.1 = lag(TSpread),
         GDPGrowth = 400*log(GDPC96/lag(GDPC96))) 

# Se trabajan con los datos de 1960 Q1 hasta 2012 Q4
subdata <- data |> 
  filter(date>="1960 Q1" & date<="2012 Q4")

# Determinamos si son o no estacionarias utilizando el test DF
plot.ts(subdata[, c("GDPGrowth", "TSpread")])

DF.GDPGrowth <- ur.df(subdata$GDPGrowth, type = "trend", selectlags = "AIC") 
summary(DF.GDPGrowth)

DF.TSpread <- ur.df(subdata$TSpread, type = "trend", selectlags = "AIC") 
summary(DF.TSpread)

# Las series son estacionarias, por lo que se trabajan en su forma original

# Modelo de rezago distribuido
# Dos rezagos en TSpread
rezagod <- dlm(formula = GDPGrowth ~ TSpread, 
               data = data.frame(subdata), q = 2)
summary(rezagod)

# Modelo de rezago distribuido: método de Koyck
koyck <- koyckDlm(subdata$TSpread , subdata$GDPGrowth)
summary(koyck, diagnostic=T)

koyck2 <- lm(GDPGrowth ~ lag(GDPGrowth) + TSpread,
            data=subdata)
summary(koyck2)

# Calculando el h de Durbin en un modelo autorregresivo
# H0: No autocorrelación, rechazo si |h|> 1.96 o si pvalor<5%
# If Durbin's h-statistic is greater than 1.96, it is likely that autocorrelation exists.
h <- durbinH(koyck2, "lag(GDPGrowth)")
abs(h)
2*pnorm(-abs(h)) # Calculando el pvalor

# Mediana de los rezagos = log(2)/log(lambda)
medianar <- -(log(2)/log(0.3447753))
medianar
# 50% del cambio total en el crecimiento del PIB se logra en un poco menos de un trimestre

# Media de los rezagos = lambda/(1-lambda)
mediar <- 0.3447753/(1-0.3447753)
mediar
# Se requiere en promedio medio trimestre para que el efecto de los cambios en el term spread se sientan
# en los cambios en el crecimiento económico

# Modelo ADL(1,1)
ADL11 <- ardlDlm(formula = GDPGrowth ~ TSpread, 
                 data = data.frame(subdata), p = 1, q = 1)
summary(ADL11)

predict.ADl11 <- forecast(ADL11, x = c(1.8633333, 1.9466667, 2.6766667, 2.6833333), 
         h = 4, interval = T, nSim = 1000)
predict.ADl11

error.predict <- predict.ADl11[["forecasts"]][["Forecast"]][1] - data$GDPGrowth[225] 
error.predict

GDPGrowth <- ts(subdata$GDPGrowth, start = c(1960, 1), end = c(2012, 4), frequency = 4)

predictADL11 <- ts(c(rep(NA, length(GDPGrowth) - 1), GDPGrowth[length(GDPGrowth)], predict.ADl11[["forecasts"]][["Forecast"]]), 
              start=1960, frequency = 4)
upperADL11 <- ts(c(rep(NA, length(GDPGrowth) - 1), GDPGrowth[length(GDPGrowth)], predict.ADl11[["forecasts"]][["95% UB"]]), 
              start=1960, frequency = 4)
lowerADL11 <- ts(c(rep(NA, length(GDPGrowth) - 1), GDPGrowth[length(GDPGrowth)], predict.ADl11[["forecasts"]][["95% LB"]]), 
            start=1960, frequency = 4) 
observed <- ts(c(GDPGrowth, rep(NA, 4)), start = 1960, frequency = 4) 

data.predictADL11 <- data.frame(date = seq(as.Date("1960-01-01"), by="quarter", length.out = 216),
                           actual = observed, predicho = predictADL11, ic_l = lowerADL11, ic_u = upperADL11) |> 
  mutate(date = as.yearqtr(date, format = "%Y-%m-%d"))

ggplot(data.predictADL11[201:2016,]) + 
  geom_line(aes(x = date, y = actual, color = "Observada"), linewidth = 0.8) + 
  geom_line(aes(x = date, y = predicho, color = "Predicha"), linewidth = 0.8) + 
  geom_ribbon(aes(x = date, y = predicho, ymin = ic_l, ymax = ic_u, fill="IC"), alpha = 0.1)+
  theme(legend.text = element_text(size = 10), text = element_text(size=8), legend.spacing.y = unit(-0.3, "cm"), legend.background=element_blank()) + 
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 1)) + 
  scale_color_manual(name = "", values = c("Observada" = "darkblue", "Predicha" = "red")) + 
  scale_fill_manual(values = c(IC = "steelblue"), labels = c(IC = "IC 95%")) + 
  labs(x = "Trimestre", y ="", fill = "") + ggtitle("Predicción del ADL(1,1)") 

ADL12 <- ardlDlm(formula = GDPGrowth ~ TSpread, 
                    data = data.frame(subdata), p = 1, q = 2)
summary(ADL12)

predict.ADl12 <- forecast(ADL12, x = c(1.8633333, 1.9466667, 2.6766667, 2.6833333), 
                         h = 4, interval = T)
predict.ADl12

error.predict <- predic.ADl12[["forecasts"]][["Forecast"]][1] - data$GDPGrowth[225] 
error.predict

# Determinando el orden del ADL
finiteDLMauto(formula = GDPGrowth ~ TSpread, data = data.frame(subdata),
              q.min = 1, q.max = 5, model.type = "dlm",
              error.type = "AIC", trace = F)

# Utilizando el paquete dynlm
# Es útil trabajar con objetos de series temporales que realizan un seguimiento de la frecuencia de los datos y 
# son extensibles. Utilizamos la función ts
# Se trabajan con los datos de 1960 Q1 hasta 2012 Q4
GDP <- ts(subdata$GDPC96, start = c(1960, 1), end = c(2012, 4), frequency = 4)
GDPGrowth <- ts(subdata$GDPGrowth, start = c(1960, 1), end = c(2012, 4), frequency = 4)
TB3MS <- ts(subdata$TB3MS, start = c(1960, 1), end = c(2012, 4), frequency = 4)
TB10YS <- ts(subdata$GS10, start = c(1960, 1), end = c(2012, 4), frequency = 4)
TSpread <- TB10YS-TB3MS

ADLdata <- ts.union(GDPGrowth, TSpread)
# estimate the ADL(1,1) model of GDP growth
rd2 <- dynlm(GDPGrowth ~ TSpread + L(TSpread) + L(GDPGrowth), 
             start = c(1960, 1), end = c(2012, 4))
summary(rd2)

# ARMAX
subdata2 <- subdata |> 
  select(date, GDPGrowth, TSpread, TSpread.1) |> 
  drop_na()

ARMAX <- arima(subdata2$GDPGrowth, order =c(1, 0, 1), xreg = subdata2[,3:4])
summary(ARMAX)
coeftest(ARMAX)

newxreg <- data.frame(TSpread = c(1.8633333, 1.9466667, 2.6766667, 2.6833333),
                      TSpread.1 = c(1.6200000, 1.8633333, 1.9466667, 2.6766667))

predict.ARMAX <- predict(ARMAX, 
                         n.ahead = 4,
                         newxreg = newxreg)

predict.ARMAX
upper95 <- predict.ARMAX[["pred"]] + 1.96*predict.ARMAX[["se"]]
lower95 <- predict.ARMAX[["pred"]] - 1.96*predict.ARMAX[["se"]]

predictARMAX <- ts(c(rep(NA, length(GDPGrowth) - 1), GDPGrowth[length(GDPGrowth)], predict.ARMAX[["pred"]]), 
              start=1960, frequency = 4)
upperARMAX <- ts(c(rep(NA, length(GDPGrowth) - 1), GDPGrowth[length(GDPGrowth)], upper95), 
            start=1960, frequency = 4)
lowerARMAX <- ts(c(rep(NA, length(GDPGrowth) - 1), GDPGrowth[length(GDPGrowth)], lower95), 
            start=1960, frequency = 4) 
observed <- ts(c(GDPGrowth, rep(NA, 4)), start = 1960, frequency = 4) 

data.predictARMAX <- data.frame(date = seq(as.Date("1960-01-01"), by="quarter", length.out = 216),
                           actual = observed, predicho = predictARMAX,
                           ic_l = lower, ic_u = upper) |> 
  mutate(date = as.yearqtr(date, format = "%Y-%m-%d"))

ggplot(data.predictARMAX[201:216,]) + 
  geom_line(aes(x = date, y = actual, color = "Observada"), linewidth = 0.8) + 
  geom_line(aes(x = date, y = predicho, color = "Predicha"), linewidth = 0.8) + 
  geom_ribbon(aes(x = date, y = predicho, ymin = ic_l, ymax = ic_u, fill="IC"), alpha = 0.1)+
  theme(legend.text = element_text(size = 10), text = element_text(size=8), legend.spacing.y = unit(-0.3, "cm"), legend.background=element_blank()) + 
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 1)) + 
  scale_color_manual(name = "", values = c("Observada" = "darkblue", "Predicha" = "red")) + 
  scale_fill_manual(values = c(IC = "steelblue"), labels = c(IC = "IC 95%")) + 
  labs(x = "Trimestre", y ="", fill = "") + ggtitle("Predicción del ARMAX(1,0,1)")

data.predictADLARMAX <- data.frame(date = seq(as.Date("1960-01-01"), by="quarter", length.out = 216),
                                actual = observed, predicho1 = predictADL11, predicho2 = predictARMAX,
                                ic_l = lower, ic_u = upper) |> 
  mutate(date = as.yearqtr(date, format = "%Y-%m-%d"))


ggplot(data.predictADLARMAX[201:216,]) + 
  geom_line(aes(x = date, y = actual, color = "Observada"), linewidth = 0.8) + 
  geom_line(aes(x = date, y = predicho1, color = "ADL11"), linewidth = 0.8) +
  geom_line(aes(x = date, y = predicho2, color = "ARMAX(1,0,1)"), linewidth = 0.8) +
  scale_color_manual(name = "", values = c("Observada" = "darkblue", "ADL11" = "red",
                                           "ARMAX(1,0,1)" = "green")) +
  theme(legend.text = element_text(size = 10), text = element_text(size=8), legend.spacing.y = unit(-0.3, "cm"), legend.background=element_blank()) + 
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 1)) + 
  labs(x = "Trimestre", y ="", fill = "") + ggtitle("Predicción ADL(1,1) vs ARMAX(1,0,1)") 

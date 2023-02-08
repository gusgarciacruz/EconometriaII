# Introducción a series de tiempo

# Utilizamos los datos de Gujarati y Portes, capítulo 21. Variables macroeconómicas de 
# los Estados Unidos, trimestrales entre 1947 y 2007 (precios constantes de 2000)

# En este ejercicio analizamos la serie PIB

# Recordemos que el análisis se centra en determinar si la serie es o no estacionaria
  
# Análisis gráfico

library(gujarati); library(tidyverse); library(zoo); library(forecast); library(urca); library(tseries)

data(Table21_1)

data <- Table21_1 |> 
  mutate(time    = as.yearqtr(paste0(Quarter," ",Year), format = "%q %Y"),
         year    = as.numeric(as.character(Year)), quarter = as.numeric(as.character(Quarter)),
         rpd     = as.numeric(as.character(RPD)), pib     = as.numeric(as.character(PIB)),
         dcp     = as.numeric(as.character(DCP)), lc      = as.numeric(as.character(LC))) |> 
  select(time, year, quarter, rpd, pib, dcp, lc) |> 
  mutate(lpib = log(pib))

ggplot(data) +
  geom_line(aes(time, lpib, color = "Log(PIB)"), linewidth = 1)  + 
  scale_color_manual(name = "", values = c("Log(PIB)" = "blue"))   +
  labs(x="Trimestre", y="") + 
  theme(legend.text = element_text(size = 5), axis.title.x = element_text(size = 2), 
        text = element_text(size=6)) + 
  scale_x_yearqtr(format = "%YQ%q", breaks = seq(from = min(data$time), to = max(data$time), by = 5))

# La serie de tiempo del PIB crece a lo largo de todo el período, es decir que muestra una 
# tendencia ascendente y por lo tanto no es estacionaria

# Correlograma de la serie
ggAcf(data$lpib,main="AC de LPIB")
ggPacf(data$lpib, main="PAC de LPIB")

# Se observa que la AC disminuye lentamente, mientras que la PAC cae abruptamente, lo que indica
# una estructura AR(1)

# Tests de raices unitarias
# Test de Dickey-Fuller aumentado

# Como primer paso, se estima una regresión con una constante y tendencia. Se selecciona
# AIC como criterio de información para seleccionar el número de rezagos óptimos
DF1 <- ur.df(data$lpib, type="trend", selectlags = "AIC")
summary(DF1)
plot(DF1)

# Luego, la hipótessis phi3 = (B1, B2, delta) = (B1, 0, 0) es probado con un test F usual.
# Esto es, la tendencia y el valor rezagado de LPIB son restringidas a cero (H0: B2=delta=0).
# El valor del test estadístico tiene un valor de 4.2409. Los valores críticos de 10%, 5% y 1%
# son 8.43,  6.49 y  5.47, respectivamente. Por tanto, H0 no puede ser rechazada, lo cual
# implica que el LPIB tiene una raiz unitaria

# Estos resultados son confirmados por un t ratio de -2.6718 para el rezago de la variable
# endógena. Este valor es inferior a los valores críticos (-3.99, -3.43, -3.13) con lo cual no es posible
# rechazar H0: delta=0, es decir, existe una raiz unitaria.

# Luego, se prueba si LPIB es una caminata aleatoria con o sin intercepto (H0: B1=B2=delta=0). El test
# estadístico relevante es phi2, el cual tiene un valor de 20.2828, que comparado a los
# valores críticos (6.22,  4.75,  4.07) es mayor con lo que se rechaza H0, así que la
# serie LPIB es una caminata aleatoria con intercepto

# Dado los resultados de phi3 se estima sin tendencia.
DF2 <- ur.df(data$lpib, type="drift", selectlags = "AIC")
summary(DF2)
plot(DF2)

# El test estadístico ph1 (H0: B1=delta=0) es 26.4655 mayor a los valores críticos
# (6.52,  4.63,  3.81), por lo que rechazo H0, es decir que el intercepto es
# significante. El tau2 muestra que -1.3328 es menor que los valores críticos 
# (-3.46, -2.88, -2.57), lo que indica que LPIB tiene raíz unitaria, no existe una tendencia,
# pero si un intercepto en el proceso generador de datos

# La estimación sin tendencia ni intercepto, no tiene sentido. Vemos que delta es
# positivo lo cual hace que la serie sea explosiva y no tenga sentido estimar
DF3 <- ur.df(data$lpib, type="none", selectlags = "AIC")
summary(DF3)
plot(DF3)

# Finalmente, se prueba la serie diferenciada para ver si logra la estacionariedad
data <- data |> 
  mutate(dlpib = c(NA,diff(data$lpib)))

DF4 <- ur.df(data[!is.na(data$dlpib),]$dlpib, type="trend", selectlags = "AIC")
summary(DF4)
plot(DF4)

# Una interpretación más completa puede encontrarse en 
# - https://stats.stackexchange.com/questions/24072/interpreting-rs-ur-df-dickey-fuller-unit-root-test-results
# - Analysis of Integrated and Cointegrated Time Series with R, Bernhard Pfaff, Cap 5

# Test de Phillips-Perron
PP1 <- ur.pp(data$lpib, type="Z-tau", model="trend", lags="short")
summary(PP1)
plot(PP1)

# Test de Elliot-Rothenberg-Stock
ERS <- ur.ers(data$lpib, type = "P-test", model = "trend", lag = 1)
summary(ERS)

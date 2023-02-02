# Tema 12. Autocorrelación
# En este ejemplo vamos analizar la relación entre salarios y productividad
# en el sector de negocios de Estados Unidos entre 1960 y 2005

# Se tienen datos sobre ?ndices de remuneración real por hora (Y) y producción por hora (X), la
# base de los índices es 1992=100

setwd("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Cursos EAFIT/Econometria I/Stata")

library(gujarati); library(tidyverse)
library(Hmisc); library(forecast); library(lmtest)
library(prais); library(orcutt); library(sandwich)

data("Table12_4")
data <- Table12_4 |> 
  mutate(year = as.numeric(as.character(Year)),
         y = as.numeric(as.character(Y)),
         x = as.numeric(as.character(X))) |> 
  select(year, x, y)

# Primero se grafica Y contra X
ggplot(data, aes(x=x, y=y)) +
  geom_point(color = "blue", size = 4) +
  labs(x="Productividad", y="Salarios") +
  geom_smooth(method = "lm", se = FALSE, formula= y~x, color="red")

# Modelo de regresión log-log
modelo <- lm(log(y) ~ log(x), data=data)
summary(modelo)

# Detección de la autocorrelación
# Graficando los residuales
data <- data |> mutate(u = modelo$residuals, u_e = rstandard(modelo))

ggplot(data, aes(x=year, y=u)) +
  geom_line(color = "blue") + labs(x="Años", y="Residuales")

ggplot(data, aes(x=year, y=u_e)) +
  geom_line(color = "blue") +  labs(x="Años", y="Residuales estandarizados")

# Observamos que tanto u como u_est presentan un patrón, 
# lo que indica que tal vez las u no sean aleatorias

# ut contra ut-1
data <- data |> mutate(u_lag1 = lag(u, 1, order_by=year))

ggplot(data, aes(x=u_lag1, y=u)) +
  geom_point(color = "blue", size = 3) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_smooth(method = "lm", se = FALSE, formula= y~x, color="red")

# Test de Durbin-Watson
dwtest(modelo1)

# La prueba de multiplicadores de Lagrange (ML) o de Breusch-Godfrey (BF)
# Se empezará incluyendo 4 rezagos de u

data <- data |> mutate(u_lag2 = lag(u, 2, order_by=year),
                       u_lag3 = lag(u, 3, order_by=year),
                       u_lag4 = lag(u, 4, order_by=year)) |> 
                mutate(u_lag1 = case_when(row_number() == 1 ~ 0,
                                          TRUE ~ u_lag1),
                       u_lag2 = case_when(row_number() >= 1 & row_number() <=2 ~ 0,
                                          TRUE ~ u_lag2),
                       u_lag3 = case_when(row_number() >= 1 & row_number() <=3 ~ 0,
                                          TRUE ~ u_lag3),
                       u_lag4 = case_when(row_number() >= 1 & row_number() <=4 ~ 0,
                                          TRUE ~ u_lag4)) # En el test BF es necesario ponerle 0 a los missing generados en los rezagos
summary(lm(u ~ log(x) + u_lag1 + u_lag2 + u_lag3 + u_lag4, data = data))
summary(lm(u ~ log(x) + u_lag1 + u_lag2 + u_lag3, data = data))
summary(lm(u ~ log(x) + u_lag1 + u_lag2, data = data))
summary(lm(u ~ log(x) + u_lag1, data = data))

# Nos quedamos con un sólo rezago en u
# Calculando el estadístco n*R2
chi2cal_1 <- 46*0.7396
chi2cal_1
pvalor_1 <- 1-pchisq(chi2cal_1, 1)
pvalor_1

# Por función
bg1 <- bgtest(modelo1, order = 1)
bg1
coeftest(bg1)

# Test basados en el correlograma
acf(data$u, 20)
acf(data$u, 20, xlim=c(1,20), main = "", ylab = "", xlab = "", xaxt="none")
axis(1, 1:20)
mtext(side=1, line=3, "Lag",cex=1.1)
mtext(side=2, line=3, "Autocorrelation (AC)",cex=1.1)

ggAcf(data$u,main="ACF de los residuales")

pacf <- pacf(data$u, 20)
pacf(data$u, 20, main = "", ylab = "", xlab = "", xaxt="none")
axis(1, 1:20)
mtext(side=1, line=3, "Lag",cex=1.1)
mtext(side=2, line=3, "Partial autocorrelation (PAC)",cex=1.1)

ggPacf(data$u,main="PAC de los residuales", ylab="PAC")

# Se detecta un problema de autocorrelación en los residuales y es del tipo AR(1)

# Test de Ljung-Box
Box.test(data$u, lag = 1, type = "Ljung-Box")

# Corrección
# El método de Prais-Winsten: corrige AR(1)
pw <- prais_winsten(log(y)~log(x), data = data, index = data$year)
summary(pw)

# El método Cochrane-Orcutt: corrige AR(1)
coch <- cochrane.orcutt(modelo)
summary(coch)

# El método Newey-West: corrige con orden de rezago más alto
coeftest(modelo,vcov=NeweyWest(modelo,lag=2,verbose=T))

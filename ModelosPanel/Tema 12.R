# MODELOS DE DATOS PANEL
# Los datos analizan los costos de seis líneas de aviación 
# comercial de 1970 a 1984, para un total de 90 observaciones 
# de datos de panel

# i: identificación de la aerolínea; t: identificación del año
# q: producción, como ingresos por milla por pasajero, un índice
# c: costo total, en 1000 dólares; 
# pf: precio del combustible
# lf: factor de carga, la utilización promedio de la capacidad de la flotilla

# La idea es estimar una función de costos: c = b1 + b2q + b3pf + b4lf + U
library(haven); library(plm); library(tidyverse); library(ggplot2); library(stargazer)

setwd("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Cursos EAFIT/Econometria I/Stata")

# Data
data <- read_dta("Table 16.1 Gujarati.dta")
View(data)
summary(data)
head(data) # take a quick peak at the data

# Determinando si el panel se encuentra balanceado
length(unique(data$i))
length(unique(data$t))

cbind(table(data$t))
cbind(table(data$i))

pdim(data)$balanced
is.pbalanced(data)

# El panel se encuentra balenceado

# Algunos gráficos
ggplot(data, aes(x = t, y = c)) + geom_line() + facet_wrap(~i)
ggplot(data, aes(x = t, y = c, group = i, color = i)) + geom_line()

# Heterogeneidad a través de aerolíneas (unidades)
df1 <- data %>% group_by(i) %>%
               summarise(c_mean = mean(c))

ggplot(df1, aes(x=i, y=c_mean)) +
  geom_line() +
  geom_point(color= "red") +
  geom_point(data=data, aes(x=i, y = c), color= "blue", shape=21)

# Heterogeneidad a través de años
df2 <- data %>% group_by(t) %>%
  summarise(c_mean = mean(c))

ggplot(df2, aes(x=t, y=c_mean)) +
  geom_line() +
  geom_point(color= "red") +
  geom_point(data=data, aes(x=t, y = c), color= "blue", shape=21)

# Modelo Pooling
pool <- plm(c ~ q + pf + lf, 
            data = data, model = "pooling")
summary(pool)

poolreg <- lm(c ~ q + pf + lf, 
              data = data)
summary(poolreg)

ggplot(data, aes(x = q, y = c, color = factor(i))) + 
  geom_point(size=1, shape=21) + 
  theme(legend.position = "none") +
  labs(x="Producción", y="Costos totales") +
  #scale_color_manual(values = c("black","blue", "red","yellow","orange","green")) +
  geom_text(aes(label=i), size=3, nudge_y = 200000) +
  geom_smooth(method = "lm", se = T, color='red', linetype="dashed")

# Modelo de efectos fijos
# Modelo de efectos fijos usando MCVD
MCVDreg <- lm(c ~ q + pf + lf + factor(i), 
              data = data)
summary(MCVDreg)

# MCVD provee una buena forma de entender los efectos fijos
# El efecto de las variables Xs mediado por las diferencias a través de las unidades
# Adicionando dummy para cada aerolínea se está estimando el efecto puro
# de las Xs (controlando por la heterogeneidad no observable)

# Modelo de efectos fijos within
fe <- plm(c ~ q + pf + lf, 
          data = data, model = "within")
summary(fe)

stargazer(pool, MCVDreg, fe,
          header = FALSE, 
          type = "text",
          #keep = c("q","pf","lf"),
          covariate.labels = c("Producción", "Precio", "Carga", 
                               "Aerolínea2", "Aerolínea3", "Aerolínea4", 
                               "Aerolínea5", "Aerolínea6", "Constante"),
          digits = 3, 
          out.header = T,
          model.names = F,
          column.labels = c("Pool", "MCVD", "FE"),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Y: Costo total",
          omit.stat = c("f", "ser"))

ggplot(data, aes(x = q, y = c, color = factor(i), shape = factor(i))) + 
  geom_point(size=1, shape=21) + 
  theme(legend.position = "none") +
  labs(x="Producción", y="Costos totales") +
  #scale_color_manual(values = c("black","blue", "red","yellow","orange","green")) +
  geom_text(aes(label=i), size=3, nudge_y = 200000) +
  geom_smooth(method = "lm", se = T, color='red', linetype="dashed")

# Tests of poolability
# Pooling Vs Efectos fijos: F-stat
# Ho: all intercepts are equals
R2r <- summary(poolreg)$r.squared
R2nr <- summary(MCVDreg)$r.squared

F <- ((R2nr-R2r)/5)/((1-R2nr)/81)
F

qf(.05, 5, 81, lower.tail=FALSE) # Valor crítico al 5% 
pf(F, 5, 81, lower.tail = FALSE) # P-valor

# Por funciones
pooltest(pool, fe)
pFtest(fe, pool)

# Fc>Ft o Pvalue<0.05, con lo cual rechazo Ho: alpha_i=alpha, es decir que es mejor efectos fijos a Pooling

# Modelo de efectos aleatorios
re <- plm(c ~ q + pf + lf, 
          data = data, model = "random")
summary(re)

# Test de Breusch-Pagan
# El test LM nos ayuda a decidir entre random effects y pooling.
# La Ho: varianzas entre las unidades de corte transversal son cero (modelo pooling).
# Esto es, no hay diferencias significativas entre unidades 
# (es decir, no hay efectos de panel)

plmtest(pool, type="bp")

# Se falla a rechazar Ho y se concluye que efectos aleatorios
# no es apropiado. Esto es, no hay evidencia de diferencias
# significativas entre unidades

# Test de Hausman
# Para decidir entre efectos fijos o efectos aleatorios se puede
# calcular el test de Hausman donde la Ho es que el modelo preferido
# es de efectos aleatorios. Este test basicamente prueba si los
# errores Uit están correlacionados con los regresores, Ho es que ellos
# no lo están

phtest(fe, re)

# Se rechaza Ho, lo que implica que rechazamos el modelo de
# efectos aleatorios en favor de el de efectos fijos


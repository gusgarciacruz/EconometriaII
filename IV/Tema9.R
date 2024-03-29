# EFECTOS DE LA EDUCACI�N DE LA MUJERES SOBRE LA FERTILIDAD!

# Variables instrumentales
# Las mujeres m�s educadas tienen menos hijos?
# Numerosos estudios indican que la educaci�n de las mujeres tiene un efecto negativo sobre
# la fertilidad. Varias son las posibles explicaciones: la escolarizaci�n aumenta el 
# coste de oportunidad de tener un hijo, aumenta la eficiencia del control fertilidad o 
# simplemente reduce la preferencia por los hijos. En este ejercicio vamos a estudiar este
# posible efecto negativo

# El siguiente ejercicio se basa en el paper: McCrary, J and Royer, H. (2011). "The Effect
# of Female Education on Fertility and Infant Health: Evidence from School Entry Policies 
# Using Exact Date of Birth". American Economic Review, 101: 158-195.

# Cargamos paquetes que vamos a necesitar
install.packages('devtools')
devtools::install_github("beniaminogreen/cragg")
install.packages('AER')
install.packages('stargazer')
library(haven); library(dplyr); library(cragg); library(AER); library(tidyverse)
library(stargazer); library(summarytools)

# Leyendo los datos
setwd("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Cursos EAFIT/Econometria II/R/Tema 9")

data <- read_dta("GSS2012_2018.DTA") %>%
  select(year, age, sex, race, educ, childs, paeduc, maeduc, wrkstat, marital) %>%      
  filter(sex==2, year>=2014 & year<=2018, age>=35 & age<=55) %>%
  mutate(age2 = age*age, afroa = case_when(race == 1 ~ 1,
                                           race == 2 ~ 0),
                         working = case_when(wrkstat >= 1 & wrkstat<= 2 ~ 1,
                                             wrkstat >= 3 & wrkstat<= 8 ~ 0),
                         casado = case_when(marital == 1 ~ 1,
                                            marital != 1 ~ 0)) %>% drop_na()
        
View(data[,c("race","afroa")])
View(data[,c("wrkstat","working")])
View(data[,c("marital","casado")])

# OLS
ols <- lm(childs ~ educ+age+I(age2)+casado+afroa+working, data=data)
summary(ols)

# IV: se instrumenta la educaci�n de la mujeres con la educaci�n de los padres
iv <- ivreg(childs ~ educ+age+I(age2)+casado+afroa+working |
                           age+I(age2)+casado+afroa+working+paeduc+maeduc, data=data)

summary(iv)

# Diagn�sticos de los instrumentos
# Relevancia de los instrumentos
# La idea es probar la significancia conjunta de los instrumentos en la primera etapa
# Se calcula entonces el F estad�stico de la primera etapa
# Existen dos formas
fstage <- lm(educ ~ age+I(age2)+casado+afroa+working+paeduc+maeduc, data=data)
summary(fstage)

linearHypothesis(fstage, 
                 c("paeduc = 0", "maeduc = 0"))

summary(iv, diagnostics=TRUE)
# Weak instruments: La H0 es que los instrumentos son d�biles, as� que un rechazo
# significa que los instrumento no son d�biles, lo cual es bueno

# El F es bastante grando con los cual rehazamos H0 que los instrumentos no tienen efecto
# sobre la educaci�n, con lo cual son relevantes

# Wu-Hausman: Es un test de endogeneidad, donde H0: Cov(educ, error) = 0. Rechazando H0
# indicate la existencia de endogeniedad la necesidad por variables instrumentales
# En otras palabras, prueba la consistencia de las estimaciones OLS bajo el supuesto que el
# IV es consistente. Cuando se rechaza H0, indica que OLS es no cosistente, sugiriendo
# que la endogeneidad es presente. Si no se rechaza H0, significa que OLS y IV son similares
# y la endogeneidad no es un problema

# Sargan: sirve para probar la validez de los instrumentos (los instrumentos no est�n
# correlacionados con los errores). Este test s�lo puede calcularse si los instrumentos
# exceden el n�mero de variables end�genas. Este test tambi�n es llamado test de
# restricciones de sobre-identificaci�n. H0: Cov(z,error)=0. Lo bueno ser�a no rechazar

# Caundo existe m�s de una variable potencialmente end�gene, es mejor utilizar el test Cragg-Donald
# En nuestro ejemplo al tener un s�lo regresor end�genor el F de la primera etapa y el Cragg-Donald
# ser�n iguales
cragg_donald(X=~age+age2+casado+afroa+working, # Control Variables
             D=~educ, # Treatments
             Z=~paeduc+maeduc,# Instruments
             data = data)


# Stock y Yogo reportan los valores cr�ticos de comparaci�n del Cragg-Donald
# En este ejemplo, sale un error ya que s�lo reporta lo valores cr�ticos cuando se tiene
# m�s de una variable potencialmente end�gena
stock_yogo_test(X=~age+age2+casado+afroa+working, # Control Variables
                D=~educ, # Trea
                Z=~paeduc+maeduc,# Instruments
                size_bias="bias", #Default
                B=.05, #Default
                data = data)

# Tabla de resultados
stargazer(ols, iv,
          header = FALSE, 
          type = "text",
          covariate.labels = c("Educaci�n", "Edad", "Edad2", "Casado (=1)", 
                               "afroamericano (=1)", "Trabajando (=1)", "Constante"),
          digits = 4, 
          out.header = T,
          model.names = F,
          column.labels = c("OLS", "IV"),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Y: n�mero de hijos",
          omit.stat = c("f", "ser"))

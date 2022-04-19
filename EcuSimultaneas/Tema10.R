# Modelos de ecuaciones simultáneas

# Condición de rango
# Determinante de la matriz de coeficientes de las variables excluidas
# Se debe instalar el paquete rSymPy, que depende de los paquetes rjson y rJython 
# En el siguiente link se puede descargar el paquete rJython https://cran.r-project.org/src/contrib/Archive/rJython/ (bajar la versión 4)
# En el siguiente link se puede descargar el paquete rSymPy https://cran.r-project.org/src/contrib/Archive/rSymPy/ (bajar la última versión)
# Si tienen problema con RJava pueden mirar este blog donde mencionan la solución: https://www.r-statistics.com/2012/08/how-to-load-the-rjava-package-after-the-error-java_home-cannot-be-determined-from-the-registry/ 
setwd("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Cursos EAFIT/Econometria II/R/Tema 10")
install.packages("rjson")
library(rjson)
install.packages("rJava")
library(rJava)
install.packages("rJython_0.0-4.tar.gz", repos = NULL, type = "source")
library(rJython)
install.packages("rSymPy_0.2-1.2.tar.gz", repos = NULL, type = "source")
library(rSymPy)

Det <- function(x) Sym("(", x, ").det()")

gamma22 <- Var("gamma22") 
gamma32 <- Var("gamma32") 
gamma43 <- Var("gamma43")
A <- Matrix(List(0,-gamma22,0), List(0,-gamma32,0), List(1,0,-gamma43))
A
Det(A)

# Analicemos el modelo del comportamiento del gasto gubernamental de US
# El sistema de ecuaciones que se va a estimar es:

# EXP = B1 + B2AID + B3INC + B4POP + U1
# AID = d1 + d2EXP + d3PS + U2

# EXP = gasto público de los gobiernos estatales y locales (endógena)
# AID = nivel de ayuda mediante subsidio federal (endógena)
# INC = ingreso de los estados (exógena)
# POP = población estatal (exógena)
# PS = población estudiantil de primaria y secundaria (exógena)
# U1 y U2 = términos de error

# Determinando si las ecuaciones están identificadas
# Condición de orden: para que una ecuación esté identificada es necesario 
# que al menos una variable exógena se excluya de la ecuación analizada
# La ecuación 1 satisface la condición de orden ya que PS se encuentra excluida (exactamente identificada)
# La ecuación 2 satisface la condición de orden ya que INC y POP se encuentran excluidas (sobreidentificada)

# Condición de rango: una ecuación está identificada si, y sólo si, la segunda
# ecuación contiene al menos una variable exógena (con un coeficiente diferente de 
# cero) excluida de la primera

# Para corroborar esta condición se estiman por MCO las ecuaciones de la forma 
# reducida:
# EXP = PI1 + PI2 INC + PI3 POP + PI4 PS + e1
# AID = PI5 + PI6 INC + PI7 POP + PI8 PS + e2

# Y luego se prueba que los coeficientes de las variables excluidas sean 
# estadísticamente significativas 

rm(list=ls())
library(stargazer)
library(car)
library(readxl)

data <- read_excel("EX73.xlsx")

# Identificando la ecuación de EXP: se estima en forma reducida la ecuación de AID y se corrobora que el coef de PS sea
# estadísticamente significativo

AID.reducida <- lm(AID~INC+POP+PS, data=data)
summary(AID.reducida)
linearHypothesis(AID.reducida, "PS = 0")

# El coeficiente de PS es estadísticamente significativo, lo cual indica que la 
# ecuación de EXP está identificada

# Identificando la ecuación de AID: se estima en forma reducida la ecuación de EXP y se corrobora que el coef de INC o POP sean
# estadísticamente significativos

EXP.reducida <- lm(EXP~INC+POP+PS, data=data)
summary(EXP.reducida)
linearHypothesis(EXP.reducida, c("INC = 0", "POP = 0"))

# Los coeficientes de INC y POP son estadísticamente significativos, lo cual indica
# que la ecuación de AID está identificada

# Prueba de simultaneidad
# El test de Hausman comprende los siguientes pasos:
# Paso 1. Efect?e la regresión de la forma reducida de AID y obtenga 
# los residuales de esta regresión (w)
# Paso 2. Efectúe la regresión de EXP sobre AID, INC, POP y w, y realice una 
# prueba t sobre el coeficiente de w. Si éste es significativo, no se rechaza 
# la hipótesis de simultaneidad

data$w <- residuals(AID.reducida)
data$v <- residuals(EXP.reducida)

hausman1 <- lm(EXP~AID+INC+POP+w, data=data)
summary(hausman1)

# El coeficiente de w es estadísticamente significativo al 10%, con lo cual hay
# problemas de simultaneidad

hausman2 <- lm(AID~EXP+PS+v, data=data)
summary(hausman2)

# El coeficiente de v es estadísticamente significativo al 1%, con lo cual hay
# problemas de simultaneidad

# Estimación de sistemas de ecuaciones
install.packages('systemfit')
library(systemfit)

# Primero se crea el sistema
eqEXP <- EXP ~ AID + INC + POP
eqAID <- AID ~ EXP + PS
eq.sys <- list(EXP = eqEXP,
                AID = eqAID)

# MCO: ignorando la estructura de sistema de ecuaciones
# Equivalente a estimar cada ecuación por separado
MCO <- systemfit(eq.sys, method = "OLS", data = data)
summary(MCO)

# MC2E: equivalente a la estimación de variables instrumentales de la ecuación única
# Para la estimación por MC2E es necesario tener variables instrumentales
# para la identificación. Los instrumentos son las variables exógenas

# Mismo instrumento para las dos ecuaciones
inst <- ~ INC + POP + PS
MC2E.1 <- systemfit(eq.sys, method = "2SLS", inst = inst, data = data)
summary(MC2E.1)

# Note que es los mismo que hacer IV
library('AER')
IV <- ivreg(EXP ~ AID+INC+POP |
                  INC+POP+PS, data=data)
summary(IV)

# Diferentes instrumentos por ecuación
inst1 <- ~ INC + POP + PS
inst2 <- ~ INC + POP
instlist <- list(inst1, inst2)
MC2E.2 <- systemfit(eq.sys, method = "2SLS", inst = instlist, data = data)
summary(MC2E.2)

# SUR: los residuales son correlacionados a través de las ecuaciones
# no da cuenta del problema de variable explicativa endógena
SUR <- systemfit(eq.sys, method = "SUR", data = data)
summary(SUR)

EXP.mco <- lm(EXP ~ AID + INC + POP, data = data)
AID.mco <- lm(AID ~ EXP + PS, data = data)

stargazer(EXP.mco, AID.mco,
          header = FALSE, 
          type = "text",
          digits = 4, 
          out.header = T,
          model.names = F,
          omit.stat = c("f", "ser"))

# Test de Breusch-Pagan. Ho: la matrix de covarianza de los errores es diagonal, 
# las ecuaciones son independientes
# Breusch y Pagan (1980) derivaron una forma simple para realizar esta prueba y se basa
# en construir un estadístico de multiplicadores de lagrange (LM) sobre los coeficientes
# de correlaci?n muestral de los residuales MCO

# Primero calculamos la matrix var-cov de los residuales estimados. Con la siguiente
# función nos muestra dicha matriz var-cov y la matriz de correlación de los residuales
summary(MCO, residCov = TRUE, equations = FALSE)

# Ahora calculamos el r del estadístico de prueba del test de BP y lo comparamos 
# con el coeficiente de correlación, deben dar igual
# r = sigma_ij / (sigma_ii * sigma_jj)^(1/2)

(-25845.3)/((140757.9*14996.3)^(1/2))

# Ahora procedemos calcular el estadístico de prueba

BP <- 50*((-0.5625)^2)
BP
pchisq(BP, df=1, lower.tail = F) # Pvalor de BP
qchisq(0.05, df=1, lower.tail = F) # Chi de la tabla al 5% y con 1 gdl

# Pvalor = 0, rechazamos Ho, es decir que la estimación SUR es adecuada

# MC3E: toma en cuenta la correlación contemporanea entre los residuales y
# la existencia de una variable explicativa endógena
MC3E <- systemfit(eq.sys, method = "3SLS", inst = inst, data = data)
summary(MC3E)

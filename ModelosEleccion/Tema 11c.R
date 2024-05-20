# EL EFECTO DE LA EDUCACION SOBRE EL CRIMEN!
  
# MODELOS DE PROBABILIDAD
# El archivo inmates.dta contiene informacion sobre el paper Lochner, L. y Moretti, E. (2004).
# "The Effect of Education on Crime: Evidence from Prison Inmates, Arrests, and Self-Reports",
# American Economic Review, 94(1):155-189 (http://www.nber.org/papers/w8605), donde se miden
# los efectos de la educacion sobre el crimen. 

# Las principales variables para el análisis son:
# prision: variable binaria igual a 1 si la persona esta en prision, 0 no
# educ: anos de escolaridad
# age: edad
# AfAm: variable binaria igual a 1 para afroamericano, 0 no

# Se estima un modelo donde se relaciona el crimen con la educacion, la edad y su cuadrado, 
# y si el individuo es afroamericano

# Cargando algunos paquetes necesario
# Para instalar margins: https://github.com/leeper/margins
library(tidyverse); library(haven); library(knitr); library(margins); library(pscl)
library(vcdExtra); library(generalhoslem); library(ResourceSelection); library(epiDisplay)
library(readxl); library(summarytools); library(marginaleffects)

# Cargando los datos
setwd("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Cursos EAFIT/Econometria II/Quices/Q3/")
data <- read_excel("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Cursos EAFIT/Econometria II/R/Tema 11/data.xlsx")

# Algunas estadísticas descriptivas
kable(summary(data[,c("prison", "educ", "age", "age2", "AfAm")]))

kable(data %>% group_by(prison) %>%
         dplyr::summarise(N = n()) %>%
         mutate(Porcent = N / sum(N))) #tibble, kable

kable(data %>% count(prison, AfAm) %>%
        group_by(prison) %>%
        mutate(percent = n / sum(n)) %>%
        dplyr::select(-n) %>%
        spread(AfAm, percent)) 

freq(data$prison)

kable(data %>% group_by(prison) %>%
        dplyr::summarise(educ_media = mean(educ), 
                   age_media = mean(age)))

# Modelo de probabilidad lineal (MLP)
mpl <- lm(prison ~ educ+age+I(age^2)+AfAm, data=data) 
summary(mpl)

data$y_est <- predict(mpl)
summary(data$y_est) # Se observa que los y_estimado dan negativos, lo cual no tiene sentido al estar hablando de probabilidades

# Efectos marginales
# Efecto parcial promedio (Average marginal effects)
summary(margins(mpl))
avg_slopes(mpl)

# Efecto parcial en el promedio
summary(margins(mpl, at = list(educ=mean(data$educ), 
                               age=mean(data$age),
                               AfAm=mean(data$AfAm))))

slopes(mpl, newdata = "mean")

# En el MPL el efecto parcial promedio y el efecto parcial en el promedio son iguales

# Modelo probit
probit <- glm(prison ~ educ+age+I(age^2)+factor(AfAm), data=data,
              family=binomial(link="probit")) 
summary(probit)
data$y_est_p <- predict(probit, type = "response")
summary(data$y_est_p)

# Efectos marginales
# Efecto parcial promedio (Average marginal effects)
summary(margins(probit))
avg_slopes(probit)

# Efecto parcial en el promedio
summary(margins(probit, at = list(educ=mean(data$educ), 
                                  age=mean(data$age),
                                  AfAm=c("0","1"))))

slopes(probit, newdata = datagrid(educ=mean(data$educ),
                                  age=mean(data$age),
                                  AfAm=c("0","1")))


slopes(probit, newdata = "mean")

# Cálculo de la probabilidad de estar preso para individuos afro y no afro
# En las medias de las variables
prob_mediasvar <- predict(probit, 
                       newdata = data.frame("educ" = mean(data$educ),
                                            "age" = mean(data$age),
                                            "I(age^2)"= mean(data$age2),
                                            "AfAm" = c(0, 1)),
                       type = "response")
prob_mediasvar
diff(prob_mediasvar) # Muestra el efecto marginal

# Promedio de las probabilidades
prob_promedio_AfAm1 <- predict(probit, 
                       newdata = data.frame("educ" = data$educ,
                                            "age" = data$age,
                                            "I(age^2)"= data$age2,
                                            "AfAm" = 1),
                       type = "response")
prob_promedio_AfAm0 <- predict(probit, 
                               newdata = data.frame("educ" = data$educ,
                                                    "age" = data$age,
                                                    "I(age^2)"= data$age2,
                                                    "AfAm" = 0),
                               type = "response")

summary(prob_promedio_AfAm1)["Mean"]
summary(prob_promedio_AfAm0)["Mean"]

# Curvas de probabilidad
newdata <- data.frame("educ" = seq(from = 0, to = 18, length.out = 19),
                     "age" = mean(data$age),
                     "age2"= mean(data$age2),
                     "AfAm" = 1)

newdata[,c("peduc1","se1")] <- predict(probit, 
                 newdata,
                 type = "response", se.fit = TRUE)[-3]

newdata[,c("AfAm")] <- 0

newdata[,c("peduc0","se0")] <- predict(probit, 
                                      newdata,
                                      type = "response", se.fit = TRUE)[-3]

newdata$peduc1_lb <- newdata$peduc1-qt(0.025, 19-1, lower.tail = FALSE)*newdata$se1
newdata$peduc1_ub <- newdata$peduc1+qt(0.025, 19-1, lower.tail = FALSE)*newdata$se1
newdata$peduc0_lb <- newdata$peduc0-qt(0.025, 19-1, lower.tail = FALSE)*newdata$se0
newdata$peduc0_ub <- newdata$peduc0+qt(0.025, 19-1, lower.tail = FALSE)*newdata$se0

ggplot(newdata) +
     geom_line(aes(x = educ, y =peduc1), colour="blue") +
     geom_line(aes(x = educ, y =peduc0), color="red") +
     geom_ribbon(aes(ymin=peduc1_lb,ymax=peduc1_ub, x=educ, fill="IC 95%"),alpha=0.3) +
     geom_ribbon(aes(ymin=peduc0_lb,ymax=peduc0_ub, x=educ, fill="IC 95%"),alpha=0.3) +
     labs(x="A?os de educaci?n", y="Prob(Preso=1)") +
     scale_fill_manual(name = "",  values=c("IC 95%" = "grey12")) +
     theme(legend.position = c(0.85, 0.3), legend.background = element_rect(fill = "transparent")) +
     geom_text(x=3.8, y=0.08, label="Afro", size=4, color="blue") +
     geom_text(x=4, y=0.026, label="No Afro", size=4, color="red")
ggsave("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Cursos EAFIT/Econometria II/R/Tema 11/fig.png", width = 4, height = 4)

# Medidas de bondad de ajuste
# Pseudo R2 = 1 - (LogL_nr / LogL_r)

# No restringido
probit_r <- glm(prison ~ 1, data=data,
              family=binomial(link="probit")) 

pR2 <- 1- logLik(probit)/logLik(probit_r)
pR2

# Otras formas
pseudoR2 <- 1 - (probit$deviance) / (probit$null.deviance)
pseudoR2

pR2(probit)

# Proporción de predicciones correctas
hitmiss(probit)

kable(data %>% group_by(prison) %>%
        dplyr::summarise (n = n()) %>%
  mutate(percent = n / sum(n)))

hitmiss(probit, k=0.00675)

# Hosmer-Lemeshow goodness-of-fit test
# Este test nos dice qu? tan bien se ajustan sus datos al modelo
# Ho: el modelo ajuste bien vs Ha: el modelo ajusta pobremente 
HLtest(probit)
summary(HLtest(probit))
plot(HLtest(probit))

hoslem.test(data$prison, fitted(probit))
logitgof(data$prison, fitted(probit))

# La curva ROC
# La elecci?n se realiza mediante la comparaci?n del ?rea bajo la curva (AUC) de 
# ambas pruebas. Esta ?rea posee un valor comprendido entre 0,5 y 1, donde 1 
# representa un valor de predicci?n perfecto y 0,5 mala predicci?n. Es decir, si AUC
# es 0.8 significa que existe un 80% de probabilidad de predecir el evento
auc <- lroc(probit, auc.coords=c(.1,.1))
auc["auc"]

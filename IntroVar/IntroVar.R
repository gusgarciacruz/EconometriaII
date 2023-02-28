# Introducción a modelos VAR

library(fpp3); library(tidyverse); library(vars); library(urca); library(forecast)

data(us_change)

# Convirtiendo las variables a series de tiempo
consum <- ts(us_change$Consumption, start = c(1970, 1), freq = 4)
income <- ts(us_change$Income, start = c(1970, 1), freq = 4)

# Determinando si son estacionarias
plot.ts(cbind(consum,income))

ggAcf(consum)
ggPacf(consum)

ggAcf(income)
ggPacf(income)

DF_c <- ur.df(consum, type="trend", selectlags = "AIC")
summary(DF_c)

DF_i <- ur.df(income, type="trend", selectlags = "AIC")
summary(DF_i)

# Seleccionando el orden del rezago del VAR
data.var <- cbind(consum, income)
colnames(data.var) <- c("consum", "income")

select.var <- VARselect(data.var, lag.max = 12, type = "const")
select.var$selection

var1.est <- VAR(data.var, p = 1, type = "const", season = NULL, 
                exog = NULL)
summary(var1.est)

var5.est <- VAR(data.var, p = 5, type = "const", season = NULL, 
              exog = NULL)
summary(var5.est)

# Estabilidad del VAR
barplot(roots(var5.est), ylim = c(0,1),
        main = "Estabilidad: Raíces de Polinomio Característico",
        names.arg = as.character(1:10))
abline(h = 1, col = "red", lty = 2, lwd = 2)

# Chequeando los residuales del VAR
plot(var5.est, names = "consum")
plot(var5.est, names = "income")

# H0: No correlación serial
var5.serial <- serial.test(var5.est, lags.pt = 12, type = "PT.asymptotic")
var5.serial
plot(var5.serial, names="consum")
plot(var5.serial, names="income")

## H0: homoscedasticidad
var5.arch <- arch.test(var5.est, lags.multi = 5, 
                        multivariate.only = TRUE)
var5.arch

# H0: normalidad
var5.norm <- normality.test(var5.est, multivariate.only = TRUE)
var5.norm

# Estabilidad estructural del VAR
reccusum <- stability(var5.est, type = "OLS-CUSUM")
plot(reccusum)

fluctuation <- stability(var5.est, type = "fluctuation")
plot(fluctuation)

# Test de causalidad de Granger
var.cause.consum <- causality(var5.est, cause = "consum")
var.cause.consum
var.cause.income <- causality(var5.est, cause = "income")
var.cause.income

# Predicción 
predictions <- predict(var5.est, n.ahead = 8, ci = 0.95)
plot(predictions, names = "consum")
plot(predictions, names = "income")

fanchart(predictions, names = "income")

predict_consum <- ts(c(rep(NA, length(consum) - 1), consum[length(consum)], 
                       predictions$fcst$consum[1:6,1]), start=c(1970, 1), 
                     end=c(2020, 4), frequency = 4)
lower_consum <- ts(c(rep(NA, length(consum) - 1), consum[length(consum)], 
                     predictions$fcst$consum[1:6,2]), start=c(1970, 1), 
                   end=c(2020, 4), frequency = 4)
upper_consum <- ts(c(rep(NA, length(consum) - 1), consum[length(consum)], 
                     predictions$fcst$consum[1:6,3]), start=c(1970, 1), 
                   end=c(2020, 4), frequency = 4) 
observed_consum <- ts(c(consum, rep(NA, 6)), start=c(1970, 1), 
                      end=c(2020, 4), frequency = 4)

data_consum <- data.frame(date = as.yearqtr(seq(as.Date("1970-01-01"), by="quarter", length.out = 204), "%Y, %Q"),
                          actual = observed_consum, 
                          predicho = predict_consum, 
                          ic_l = lower_consum, 
                          ic_u = upper_consum)

ggplot(data_consum[data_consum$date>"2015 Q1",]) + 
  geom_line(aes(x = date, y = actual, color = "Observada"), linewidth = 0.6) + 
  geom_line(aes(x = date, y = predicho, color = "Predicha"), linewidth = 0.6) + 
  geom_ribbon(aes(x = date, y = predicho, ymin = ic_l, ymax = ic_u, fill="IC"), alpha = 0.1) +
  theme(legend.text = element_text(size = 6), text = element_text(size=7), legend.spacing.y = unit(-0.3, "cm"), legend.background=element_blank()) + 
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 1)) + scale_color_manual(name = "", values = c("Observada" = "darkblue", "Predicha" = "red")) + 
  scale_fill_manual(values = c(IC = "steelblue"), labels = c(IC = "IC 95%")) + 
  labs(title = "Predicción del consumo", x = "Años", y ="", fill = "") + 
  scale_x_continuous(n.breaks = 8)

# Funciones impulso respuesta
irf.consum <- irf(var5.est, impulse = "income", response = "consum", 
               n.ahead = 10, boot = TRUE)
plot(irf.consum, ylab = "Consumo", main = "Shock del ingreso")

irf.income <- irf(var5.est, impulse = "consum", response = "income", 
                  n.ahead = 10, boot = TRUE)
plot(irf.income, ylab = "Ingreso", main = "Shock del consumo")

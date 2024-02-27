library(haven); library(tidyverse); library(cobalt); library(summarytools); library(lmtest); library(sandwich); library(fixest)
library(plm); library(lfe); library(stargazer); library(modelsummary); library(broom); library(ggview)

data <- read_dta("https://cameron.econ.ucdavis.edu/mmabook/nswpsid.dta") %>% 
  rename_all(., .funs = tolower) |> 
  mutate(id = row_number()) |> 
  relocate(id)

names(data)

# Tabla de balance
# Esta tabla nos permite determinar qué tan parecidos son los grupos de tratamiento y control en las variables observables
# Nos reporta la media, la diferencia en medias, el estadístico  de Kolmogorov-Smirnov (ks) y el estadítico del overlapping (ovl).
# ks esta entre 0 y 1, donde valores cercanos a 0 da indicativo de balance en observables entre tratados y control.
# ovl también está entre 0 y 1, donde 0 muestra balanceo
# Más detalle en: https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt.html

bal.tab(data[,c("age", "educ", "nodegree", "black", "hisp", "marr", 
                "re74", "re75", "re78", "u74", "u75")], treat = data$treat,
        disp = c("m"), stats = c("m", "ks", "ovl"), binary = "std")

love.plot(data[,c("age", "educ", "nodegree", "black", "hisp", "marr", 
                "re74", "re75", "re78", "u74", "u75")], treat = data$treat,
         stats = c("m", "ks", "ovl"), binary = "std")

t.test(age ~ treat, data = data)
t.test(educ ~ treat, data = data)
t.test(nodegree ~ treat, data = data)

# Las estadísticas muestran que el grupo de tratados difiere considerablemente del grupo de control,
# siendo desproporcionadamente más afro (84%) con menos de high school (71%) y desempleados en el año
# del pretratamiento 1974 (715) y 1975 (60%). Estimaciones del efecto de la formación debería controlar por
# estas diferencias

# Estimación DD
data1 <- data |> 
  pivot_longer(cols=c("re74","re75","re78"),
               names_to = "year",
               names_prefix = "re",
               values_to = "re") |>
  relocate(id, year, treat, re) |> 
  mutate(time=as.numeric(year),
         post = case_when(year==78~1,
                         year>=74 & year<=75~0))

# 5 formas de estimar el DD
dd1 <- lm(re ~ treat+post+treat:post, data=data1)
summary(dd1)

dd2 <- lm(re ~ treat*post, data=data1)
summary(dd2)

data_panel <- pdata.frame(data1, index = c("id", "year"))
dd3 <- plm(re ~ treat:post, 
               data = data_panel, model = "within", effect = "twoways")
summary(dd3)

dd4 <-felm(re ~ treat:post | id + year, data = data1)
summary(dd4)

dd5 <- feols(re ~ treat:post | id + year, vcov = "iid", data = data1)
summary(dd5)

modelos <- list("OLS1" = dd1,
                "OLS2" = dd2,
                "TWFE1" = dd3,
                "TWFE2" = dd4,
                "TWFE3" = dd5)

modelsummary(modelos, 
             output = 'gt',
             coef_map = c('(Intercept)' = 'Intercepto',
                          'treat' = 'Tratamiento',
                          'post' = 'Post',
                          'treat:post' = 'Trat × Post'),
             stars = c('*'=.1, '**'=.05, '***'=.01), 
             statistic = "std.error", 
             title = 'Modelos DD', 
             gof_omit = 'IC|Log|RMSE')

# Event study
event1 <- feols(re ~ i(year, treat, 75) | id + year, vcov = "iid", data = data1)
summary(event1)
coefplot(event1, col = 2, ci.lty = 2,  pch=15,
         main="", ylab="Efecto", ylim.add = c(0, 0.3))
iplot(event1, col = 2, ci.lty = 2,  pch=15,
      main="", ylab="Efecto", ylim.add = c(0, 0.1))

data1 <- data1 |> 
  mutate(treatXyear74 = case_when(treat==1 & year==74 ~ 1,
                                  TRUE ~ 0),
         treatXyear78 = case_when(treat==1 & year==78 ~ 1,
                                  TRUE ~ 0))
event2 <-felm(re ~ treatXyear74+treatXyear78 | id + year, data = data1)
summary(event2)

felm_coefs <- tidy(event2) |>
  mutate(year = substr(term, nchar(term) - 1, nchar(term)),
    ci_up = estimate + qnorm(0.975) * std.error,
    ci_low = estimate + qnorm(0.025) * std.error)

ggplot(felm_coefs, aes(year)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_up), alpha = 0.7, width = 0.1, linetype = "dashed", color="#FF4433") +
  geom_point(aes(x=year, y = estimate), color="#008000") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color="grey70") +
  geom_vline(aes(xintercept = 1.8), linetype = "dashed", color="grey70") +
  geom_text(x=1.77, y=-1000, label="Intervención", size=2.3, color="grey60", angle = 90) +
  scale_y_continuous(name = "Efecto de la formación",
    limits = c(-2000, 4000),
    breaks = c(-2000, -1000, 0, 1000, 2000, 3000, 4000), # Manually set axis breaks
    expand = c(0, 0)) +
  scale_x_discrete(name = "Año",
    labels = c("1974", "1978")) +
  theme_classic()

ggview(units = "cm", width = 15, height = 9, dpi = 300, bg="white")

# Tests de tendencias paralelas
# Testing for pre-trends 
# En el event study se observa que el efecto antes de la intervención no es estadísticamente significante
# indicando el cumplimiento del supuesto de tendencias paralelas

# Errores estandard robustos
coeftest(dd1, vcov = vcovHC(dd1, type="HC0"))
coeftest(dd2, vcov = vcovHC(dd2, type="HC0"))
coeftest(dd3, vcov = vcovHC(dd3, type="HC0"))
summary(dd4, robust = T)
summary(feols(re ~ treat:post | id + year, vcov = "hetero", data = data1))


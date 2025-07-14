# Leyendo las librerias
import wooldridge
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf
from statsmodels.iolib.summary2 import summary_col
from stargazer.stargazer import Stargazer
from IPython.core.display import HTML

# Cargando el dataset
data = wooldridge.data('wage1')
wooldridge.data('wage1', description=True)
print(data.head())

# Estimando el modelo de regresión lineal
# Modelo 1
ols1 = smf.ols('lwage  ~ 1 + educ', data=data).fit(cov_type = 'HC0') # estimando el modelo OLS con errores estándar robustos
print(ols1.summary())                       # mostrando el resumen de resultados

# Modelo 2
ols2 = smf.ols('lwage  ~ 1 + educ + female', data=data).fit(cov_type = 'HC0') 
print(ols2.summary())

dfoutput = summary_col([ols1,ols2],stars=True)
print(dfoutput)

# Con stargazer (https://github.com/StatsReporting/stargazer/blob/master/examples.ipynb)
stargazer = Stargazer([ols1, ols2])
stargazer.custom_columns(['ModSelo 1', 'Modelo 2'], [1, 1])
#stargazer
print(stargazer.render_latex())
#HTML(stargazer.render_html())

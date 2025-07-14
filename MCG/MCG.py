import pandas as pd
import numpy as np
import statsmodels.api as sm
import statsmodels.stats.api as sms
import statsmodels.formula.api as smf
from statsmodels.stats.diagnostic import het_breuschpagan
from statsmodels.iolib.summary2 import summary_col
from stargazer.stargazer import Stargazer
from IPython.core.display import HTML

data = pd.read_stata('https://stats.idre.ucla.edu/stat/stata/webbooks/reg/elemapi2.dta')

# Estimando por OLS
ols = smf.ols('api00 ~ meals + ell + emer', data=data[data['meals']>0]).fit()
print(ols.summary())

# Probando la existencia de heteroscedasticidad a partir del test de Breuch-Pagan (H0: homoscedasticidad)
bp_test = sms.het_breuschpagan(ols.resid, ols.model.exog)
print(bp_test)

print(f"Lagrange multiplier statistic: {bp_test[0]:.4f}")
print(f"p-value: {bp_test[1]:.4f}")
print(f"f-value: {bp_test[2]:.4f}")
print(f"f p-value: {bp_test[3]:.4f}")

# Ausumiendo que la variable 'meals' es la causante de la heteroscedasticidad
# y que la estructura de la heteroscedasticidad es Var(u) = σ²meals, el modelo corregido por MCP será
data=data[data['meals']>0]

X = data[['meals', 'ell', 'emer']]
X = sm.add_constant(X)
y = data['api00']
w = 1 / data['meals']
wls_model = sm.WLS(y, X, weights=w)
results = wls_model.fit()
print(results.summary())

# El estimador HC (Heteroskedasticity consistent)
hc0 = smf.ols('api00 ~ meals + ell + emer', data=data[data['meals']>0]).fit(cov_type = 'HC0')
print(hc0.summary())

hc1 = smf.ols('api00 ~ meals + ell + emer', data=data[data['meals']>0]).fit(cov_type = 'HC1')
print(hc1.summary())
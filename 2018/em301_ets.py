import numpy as np
import pandas as pd
import seaborn as sns
import statsmodels.api as sm

flights = sns.load_dataset('flights')
flights.head()

flights['passengers'].plot()


ETS = sm.tsa.ExponentialSmoothing
model_aaa_fitted = ETS(flights['passengers'],
                      trend='add',
                      seasonal='add',
                      seasonal_periods=12).fit()

model_aaa_fitted.forecast(steps=12)
model_aaa_fitted.aic
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from datetime import datetime


faith=pd.read_csv('faithful_2011.txt', 
    skiprows=6,
    names=['date_time','interval'])

faith=faith[0:-1]


dt=[]
for a in faith['date_time']: dt.append(datetime.strptime(a,'%m/%d/%y %H:%M:%S'))


# timedelta...
for a in faith['interval']: print datetime.strptime(a,' %H:%M:%S')


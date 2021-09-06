import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt
import warnings as warn
from pandas import Series, DataFrame
from matplotlib import rcParams

rcParams.update({'figure.autolayout': True}) #Set the matplot lib to auto layout

df = pd.read_csv("ttbs_reduced.csv")
print(df)


def make_sns_plot(data, hue_val):
  if hue_val == "":
    print("There was no hue value entered.")
    exit
  else:
    sns.pairplot(data=data, hue=hue_val)


with warn.catch_warnings():
  warn.simplefilter('ignore')
  make_sns_plot(df, 'TTBS_mins') #Suppress the zero variance errors here

df.describe()
df.ndim
df.dtypes


  

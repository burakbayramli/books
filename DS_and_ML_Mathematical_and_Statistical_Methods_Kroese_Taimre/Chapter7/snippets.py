""" snippets.py """ 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
from sklearn import datasets
digits = datasets.load_digits()
x_digits = digits.data            # explanatory variables
y_digits = digits.target          # responses
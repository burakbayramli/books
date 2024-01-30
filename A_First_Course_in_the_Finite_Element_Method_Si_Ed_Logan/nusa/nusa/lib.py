# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  Blog: numython.github.io
#  License: MIT License
# ***********************************
"""
Defining some common materials and other "things"
"""
from ._lib import *

## ================== Materials ======================

STEEL_1018 = Material("1018 Steel", E=205e9, nu=0.3)
ALUMINIUM_6061 = Material("6061 Aluminium Alloy", E=69e9, nu=0.33)
STEEL_1045 = Material("1045 Steel", E = 205e9, nu = 0.29)


if __name__=='__main__':
    s1 = RectangularSection(0.1,0.2)
    s2 = CircularSection(0.1)
    print(s1.A, s1.I)
    print(s2.A, s2.I)
    
    

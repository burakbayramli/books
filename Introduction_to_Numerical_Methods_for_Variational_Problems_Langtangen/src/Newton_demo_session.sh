#!/bin/bash 

python Newton_demo.py "x*x-1" "2*x" 29 -30 30

python Newton_demo.py "0.2 + exp(-0.5*x**2)*cos(pi*x)" "-x*exp(-x**2)*cos(pi*x) - pi*exp(-x**2)*sin(pi*x)" 0.85 -3 3

python Newton_demo.py "0.2 + exp(-0.5*x**2)*cos(pi*x)" "-x*exp(-x**2)*cos(pi*x) - pi*exp(-x**2)*sin(pi*x)" 0.8 -3 3

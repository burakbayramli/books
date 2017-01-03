.. image:: ..\Images\title_RegressionAnalysis.png
    :height: 100 px

.. Regression Analysis
.. ===================

In this chapter we will focus on regression analysis using the Python library statsmodels. We already briefly discussed regression analysis in the section *Relation between two continuous variables*, subsection *Regression*. Regression analysis has some underlying assumptions, and we invite you to read this section again if you don't remember these needed assumptions and also to familiarize yourself with the needed vocabulary.

A very general definition of a regression model is the following:

.. math::
   \label{eq:regmodel}
   Y = f(x,\varepsilon)
   

In the case of a linear regression model, the function f is simply the affine function, and the model can be rewritten as:

.. math::
    \label{eq:simplereg}
    Y = X \beta + \varepsilon
    
:math:`Y` is a vector of dimension :math:`(n \times 1)` and is called the endogenous variable, :math:`X` is a matrix of dimension :math:`(n \times k)` where each colum is  an explanatory variable and :math:`\varepsilon` is the error term. :math:`\beta` is the vector of dimension :math:`(k \times 1)` and contains the parameters we want to estimate.

Example: Program Effectiveness 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Using data from Spector and Mazzeo (1980), we estimate a linear regression model with statsmodels.

.. literalinclude:: ..\Code\regSpector.py


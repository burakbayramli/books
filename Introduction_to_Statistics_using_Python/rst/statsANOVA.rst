.. image:: ../Images/title_ANOVA.png
    :height: 100 px

.. Relation Between Several Variables
.. ==================================

When we have two groups, we can ask the question: "Are they different?"
The answer is provided by hypothesis tests: by a *t-test* if the data
are normally distributed, or by a *Mann-Whitney test* otherwise. If we
want to go one step further and predict the value of one variable from
another, we have to use the technique of *linear regression*.

So what happens when we have more than two groups?

To answer the question "Are they different?" for more than two groups,
we have to use the *Analysis of Variance (ANOVA)-test* for data where
the residuals are normally distributed. If this condition is not
fulfilled, the *Friedmann Test* has to be used. And if we want to and
predict the value of one variable *many* other variables, linear
regression has to be replaced by of *multilinear regression* , sometimes
also referred to as *multiple linear regression*.

Two-way ANOVA
-----------------

Compared to one-way ANOVAs (see :ref:`one-way ANOVAs`), the analysis with
two-way ANOVAs has a new element. We can look not only if each of the factors is
significant; we can also check if the *interaction* of the factors has a
significant influence on the distribution of the data. For sticking to the
example above, if only women with treatment B get healthy, we have a significant
interaction effect between "gender" and "treatment".

Example: two-way ANOVA 
~~~~~~~~~~~~~~~~~~~~~~~~

|ipynb| `90_anovaTwoway.ipynb <http://nbviewer.ipython.org/url/raw.github.com/thomas-haslwanter/statsintro/master/ipynb/90_anovaTwoway.ipynb>`_

|python| `anovaTwoway.py <https://github.com/thomas-haslwanter/statsintro/blob/master/Code3/anovaTwoway.py>`_

::

                        df  sum_sq mean_sq        F    PR(>F)
  C(fetus)               2  324.00  162.00  2113.10  1.05e-27
  C(observer)            3    1.19    0.39     5.21  6.497-03
  C(fetus):C(observer)   6    0.56    0.09     1.22  3.29e-01
  Residual              24    1.84    0.07      NaN       NaN
    

Multilinear Regression 
------------------------

If you have truly independent variables, *multilinear regression* is a
straightforward extension of the simple linear regression. However, if
your variables may be related to each other, you have to proceed much
more carefully. For example, you may want to investigate how the
prevalence of some disease correlates with age and with income: if you
do so, you have to keep in mind that age and income are most likely
correlated! For details, gives a good introduction to that topic. Also,
check out the chapter on Modeling.

|ipynb| `91_mult_regress.ipynb <http://nbviewer.ipython.org/url/raw.github.com/thomas-haslwanter/statsintro/master/ipynb/91_mult_regress.ipynb>`_

|python| `mult_regress.py <https://github.com/thomas-haslwanter/statsintro/blob/master/Code3/mult_regress.py>`_

.. |ipynb| image:: ../Images/IPython.jpg
    :scale: 50 % 
.. |python| image:: ../Images/python.jpg
    :scale: 50 % 

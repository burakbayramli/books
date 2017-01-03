.. image:: ..\Images\title_relations.png
    :height: 100 px

.. Relation Between Two Continuous Variables
.. =========================================

If we have two related variables, the *correlation* measures the
association between the two variables. In contrast, a *linear
regression* is used for the prediction of the value of one variable from
another. If we want to compare more than two groups of variables, we
have to use a technique known as *Analysis of Variance (ANOVA)*.

Correlation
-----------

Correlation Coefficient
~~~~~~~~~~~~~~~~~~~~~~~

If the two variables are normally distributed, the standard measure of
determining the *correlation coefficient*, often ascribed to *Pearson* ,
is

.. math::

   \label{eq:pearson}
     r = \frac{\sum\limits_{i=1}^n (X_i - \bar{X})(Y_i - \bar{Y})}{\sqrt{\sum\limits_{i=1}^n (X_i - \bar{X})^2} \sqrt{\sum\limits_{i=1}^n (Y_i - \bar{Y})^2}}

With

.. math::
  s_{xy} = \frac{\sum\limits_{i=1}^n (X_i - \bar{X})(Y_i - \bar{Y})}{n-1}

and :math:`s_x, s_y` the sample standard deviations of the *x* and *y* values, respectively, this can also be written as

.. math::

  r = \frac{s_{xy}}{s_x \cdot s_y}.

Pearson's correlation coefficient, sometimes also referred to as *population correlation coefficient* or *sample correlation*, can take any value from -1 to +1. Examples are given in the Figure below. Note that the formula for the correlation coefficient is symmetrical between *x* and *y*.


Coefficient of determination
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The *coefficient of determination*  or :math:`R^2` is the square of the correlation. It is easier to interpret than the correlation coefficient r: Values of :math:`R^2` close to 1 are good, values close to 0 are poor. To explain the interpretation of :math:`R^2`, let us look at the math more formally:

.. image:: ../Images/Coefficient_of_Determination.png
    :scale: 75 %

*The better the linear regression (on the right) fits the data in comparison to the simple average (on the left graph), the closer the value of* :math:`R^2` *is to one. The areas of the blue squares represent the squared residuals with respect to the linear regression. The areas of the red squares represent the squared residuals with respect to the average value (from Wikipedia)*

A data set has values :math:`y_i`, each of which has an associated modelled value :math:`f_i` (also sometimes referred to as :math:`\hat{y}_i`). Here, the values :math:`y_i` are called the *observed values* and the modelled values :math:`f_i` are sometimes called the *predicted values*.

In the following :math:`\bar{y}` is the mean of the observed data:

.. math::
    \bar{y}=\frac{1}{n}\sum_{i=1}^n y_i 

where n is the number of observations.

The "variability" of the data set is measured through different sums of squares:

    :math:`SS_\text{tot}=\sum_i (y_i-\bar{y})^2`, the total sum of squares (proportional to the sample variance);

    :math:`SS_\text{reg}=\sum_i (f_i -\bar{y})^2`, the regression sum of squares, also called the explained sum of squares.

    :math:`SS_\text{res}=\sum_i (y_i - f_i)^2\,`, the sum of squares of residuals, also called the residual sum of squares.

The notations :math:`SS_{R}` and :math:`SS_{E}` should be avoided, since in some texts their meaning is reversed to "Residual sum of squares" and "Explained sum of squares", respectively.

The most general definition of the coefficient of determination is

.. math::
    R^2 \equiv 1 - {SS_{\rm res}\over SS_{\rm tot}}.\,

**Relation to unexplained variance**

In a general form, :math:`R^2` can be seen to be related to the unexplained variance, since the second term compares the unexplained variance (variance of the model's errors) with the total variance (of the data). See fraction of variance unexplained.

**Adjusted** :math:`R^2`

For multiple regression, the *adjusted* :math:`R^2` value (written as :math:`\bar{R}^2`) is often used instead of :math:`R^2`:

.. math::
      \bar{R}^2 = 1 - (1 - R^2)\frac{n - 1}{n - p - 1}

where *n* is the sample size and *p* is the number of independent variables.

**Examples**

How large :math:`R^2` or :math:`\bar{R}^2` must be to be considered good depends on the discipline. They are usually expected to be larger in the physical sciences than it is in biology or the social sciences. In finance or marketing, it also depends on what is being modeled.

Caution: the sample correlation and :math:`R^2` are misleading if there is a nonlinear relationship between the independent and dependent variables!



| |image26|

*Several sets of (x, y) points, with the correlation coefficient of x and y for each set.
Note that the correlation reflects the non-linearity and direction of a linear relationship (top
row), but not the slope of that relationship (middle), nor many aspects of nonlinear relationships
(bottom). N.B.: the Fiure in the center has a slope of 0 but in that case the correlation
coefficient is undefied because the variance of Y is zero. (From: Wikipedia)*

Rank correlation 
~~~~~~~~~~~~~~~~~~

If the data distribution is not normal, a different approach is
necessary. In that case one can rank the set of subjects for each
variable and compare the orderings. There are two commonly used methods
of calculating the rank correlation.

- *Spearman's* :math:`\rho`, which is exactly the same as the Pearson
   correlation coefficient :math:`r` calculated on the ranks of the
   observations.

- *Kendall's* :math:`\tau`. is also a rank correlation coefficient,
  measuring the association between two measured quantities. It is
  harder to calculate than Spearman's rho, but it has been argued that
  confidence intervals for Spearman's rho are less reliable and less
  interpretable than confidence intervals for Kendall's tau-parameters.

Regression
----------

We can use the method of *regression* when we want to predict the value
of one variable from the other.

| |image27|

*Linear regression. (From Wikipedia)*

When we search for the best-fit line to a given :math:`(x_i,y_i)`
dataset, we are looking for the parameters :math:`(k,d)` which minimize
the sum of the squared *residuals* :math:`\epsilon_i` in

.. math::

   \label{eq:simpleRegression}
     y_i = k * x_i + d + \epsilon_i

where :math:`k` is the *slope* or *inclination* of the line, and
:math:`d` the *intercept*. This is in fact just the one-dimensional
example of the more general technique, which is described in the next
section. Note that in contrast to the correlation, this relationship
between :math:`x` and :math:`y` is no more symmetrical: it is assumed
that the :math:`x-`\ values are known exactly, and that all the
variability lies in the residuals.

| |image28|

*Best-fit linear regression line (red) and residuals (black).*

Introduction
~~~~~~~~~~~~

Given a data set :math:`\{y_i,\, x_{i1}, \ldots, x_{ip}\}_{i=1}^n`
of :math:`n` statistical units, a linear regression model assumes that
the relationship between the dependent variable :math:`y_i` and the
:math:`p`-vector of regressors :math:`x_i` is linear. This relationship
is modelled through a *disturbance term* or *error variable*
:math:`\epsilon_i`, an unobserved random variable that adds noise to the
linear relationship between the dependent variable and regressors. Thus
the model takes the form

.. math::

   \label{eq:regression}
      y_i = \beta_1   x_{i1} + \cdots + \beta_p x_{ip} + \varepsilon_i
      = \mathbf{x}^{\rm T}_i \beta + \varepsilon_i,
      \qquad i = 1, \ldots, n,

where :math:`^T` denotes the transpose, so that :math:`x_i^T\beta` is
the inner product between vectors :math:`x_i` :math:`\beta`.

Often these :math:`n` equations are stacked together and written in
vector form as

.. math:: \mathbf{y} = \mathbf{X}\beta + \varepsilon, \,

where

.. math::

   \mathbf{y} = \begin{pmatrix} y_1 \\ y_2 \\ \vdots \\ y_n \end{pmatrix}, \quad
      \mathbf{X} = \begin{pmatrix} \mathbf{x}^{\rm T}_1 \\ \mathbf{x}^{\rm T}_2 \\ \vdots \\ \mathbf{x}^{\rm T}_n \end{pmatrix}
      = \begin{pmatrix} x_{11} & \cdots & x_{1p} \\
      x_{21} & \cdots & x_{2p} \\
      \vdots & \ddots & \vdots \\
      x_{n1} & \cdots & x_{np}
      \end{pmatrix}, \quad
      \beta = \begin{pmatrix} \beta_1 \\ \vdots \\ \beta_p \end{pmatrix}, \quad
      \varepsilon = \begin{pmatrix} \varepsilon_1 \\ \varepsilon_2 \\ \vdots \\ \varepsilon_n \end{pmatrix}.

Some remarks on terminology and general use:

-  :math:`y_i` is called the *regressand*, *endogenous variable*,
   *response variable*, *measured variable*, or *dependent variable*.
   The decision as to which variable in a data set is modeled as the
   dependent variable and which are modeled as the independent variables
   may be based on a presumption that the value of one of the variables
   is caused by, or directly influenced by the other variables.
   Alternatively, there may be an operational reason to model one of the
   variables in terms of the others, in which case there need be no
   presumption of causality.

-  :math:`\mathbf{x}_i` are called *regressors*, *exogenous variables*,
   *explanatory variables*, *covariates*, *input variables*, *predictor
   variables*, or *independent variables*, but not to be confused with
   *independent random variables*. The matrix :math:`\mathbf{X}` is
   sometimes called the *design matrix*.

   -  Usually a constant is included as one of the regressors. For
      example we can take :math:`x_{i1}=1` for :math:`i=1,...,n`. The
      corresponding element of :math:`\beta` is called the *intercept*.
      Many statistical inference procedures for linear models require an
      intercept to be present, so it is often included even if
      theoretical considerations suggest that its value should be zero.

   -  Sometimes one of the regressors can be a non-linear function of
      another regressor or of the data, as in polynomial regression and
      segmented regression. The model remains linear as long as it is
      linear in the parameter vector :math:`\beta`.

   -  The regressors :math:`x_{ij}` may be viewed either as random
      variables, which we simply observe, or they can be considered as
      predetermined fixed values which we can choose. Both
      interpretations may be appropriate in different cases, and they
      generally lead to the same estimation procedures; however
      different approaches to asymptotic analysis are used in these two
      situations.

-  :math:`\beta\,` is a :math:`p`-dimensional *parameter
   vector*. Its elements are also called *effects*, or *regression
   coefficients*. Statistical estimation and inference in linear
   regression focuses on :math:`\beta`.

-  :math:`\varepsilon_i\,` is called the *residuals*, *error term*, *disturbance
   term*, or *noise*. This variable captures all other factors which
   influence the dependent variable :math:`y_i` other than the
   regressors :math:`x_i`. The relationship between the error term and
   the regressors, for example whether they are correlated, is a crucial
   step in formulating a linear regression model, as it will determine
   the method to use for estimation.

-  If :math:`i=1` and :math:`p=1` in the equation above, we have a *simple linear regression*, corresponding to :math:`y = k*x + d + \epsilon` . If :math:`i>1` we talk about *multilinear regression* or *multiple linear regression* .

*Example*. Consider a situation where a small ball is being tossed up in
the air and then we measure its heights of ascent :math:`h_i` at various
moments in time :math:`t_i`. Physics tells us that, ignoring the drag,
the relationship can be modelled as :

.. math:: h_i = \beta_1 t_i + \beta_2 t_i^2 + \varepsilon_i,

where :math:`\beta_1` determines the initial velocity of the ball,
:math:`\beta_2` is proportional to the standard gravity, and
:math:`\epsilon_i` is due to measurement errors. Linear regression can
be used to estimate the values of :math:`\beta_1` and :math:`\beta_2`
from the measured data. This model is non-linear in the time variable,
but it is linear in the parameters :math:`\beta_1` and :math:`\beta_2`;
if we take regressors
:math:`\mathbf{x}_i = (x_{i1},x_{i2}) = (t_i,t_i^2)`, the model takes on
the standard form :
:math:`h_i = \mathbf{x}^{\rm T}_i\beta + \varepsilon_i.`

Assumptions
~~~~~~~~~~~

To use the technique of linear regression, the following assumptions should be
fulfilled:

1. The *independent variables* (i.e. *x*) are exactly known.
2. Validity. Most importantly, the data you are analyzing should map to the research question you are trying to answer. This sounds obvious but is often overlooked or ignored because it can be inconvenient. For example, a linear regression does not properly describe a quadratic curve.
3. Additivity and linearity. The most important mathematical assumption of the regression model is that its deterministic component is a linear function of the separate predictors.
4. Independence of errors.
5. Equal variance of errors.
6. Normality of errors.


|ipynb| `80_multivariate.ipynb <http://nbviewer.ipython.org/url/raw.github.com/thomas-haslwanter/statsintro/master/ipynb/80_multivariate.ipynb>`_

|python| `multivariate.py <https://github.com/thomas-haslwanter/statsintro/blob/master/Code3/multivariate.py>`_

| |image29|

*The sets in the Anscombe's quartet have the same linear regression line but are
themselves very different.*

| |image30|

*Regression, with confidence intervals for the mean, as well as for the
predicted data. The red dotted line shows the confidence interval for the mean;
and the green dotted line the confidence interval for predicted data. (This can
be compared to the standard error and the standard deviation for a population.)*

Since to my knowledge there exists no program in the Python standard
library (or numpy, scipy) to calculate the confidence intervals for a
regression line, I include my corresponding program *fitLine.py*.
The output of this program is shown in the figure below.
This program also shows how Python programs intended for
distribution should be documented.

Exercises
---------

#. **Correlation**

    Read in the data for the average yearly temperature at the Sonnblick, from     *https://github.com/thomas-haslwanter/statsintro/blob/master/Data/data\_others/AvgTemp.xls*
    Calculate the Pearson and Spearman correlation, and Kendall's tau, for the temperature vs. year.

#. **Regression**

    For the same data, calculate the yearly increase in temperature, assuming a linear increase with time.
    Is this increase significant?

#. **Normality Check**

    For the data from the regression model, check if the model is ok by testing if the residuals are normally distributed (e.g. by using the Komogorov-Smirnov test)


|python| `fitLine.py <https://github.com/thomas-haslwanter/statsintro/blob/master/Code3/fitLine.py>`_

.. [4]
   This section has been taken from Wikipedia

.. |image24| image:: ../Images/Sensitivity_Specificity.png
    :scale: 50 %
.. |image25| image:: ../Images/Sensitivity_Specificity_Example.png
    :scale: 50 %
.. |image26| image:: ../Images/Correlation_examples2.png
    :scale: 50 %
.. |image27| image:: ../Images/Linear_regression.png
    :scale: 50 %
.. |image28| image:: ../Images/residuals_linreg.png
    :scale: 50 %
.. |image29| image:: ../Images/Anscombes_quartet.png
    :scale: 50 %
.. |image30| image:: ../Images/regression_wLegend.png
    :scale: 50 %

.. |ipynb| image:: ../Images/IPython.jpg
    :scale: 50 % 
.. |python| image:: ../Images/python.jpg
    :scale: 50 % 

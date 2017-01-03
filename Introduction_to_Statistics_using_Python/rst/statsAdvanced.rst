.. image:: ../Images/title_advanced.png
    :height: 100 px

In this course we have presented the basic statistical data analysis with Python. However, Python has much more to offer: a number of Python packages allow you to significantly extend your statistical data analysis and modeling. In the following, I want to give a very brief overview of most interesting and powerful ones that I have found so far:

  * statsmodels
  * PyMC
  * scikit-learn
  * A.Dobson: "An Introduction to Generalized Linear Models"

statsmodels
===========

`statsmodels <http://statsmodels.sourceforge.net/>`_ is a Python module that allows users to explore data, estimate statistical models, and perform statistical tests. An extensive list of descriptive statistics, statistical tests, plotting functions, and result statistics are available for different types of data and each estimator. Researchers across fields may find that statsmodels fully meets their needs for statistical computing and data analysis in Python. Features include:

  * Linear regression models
  * Generalized linear models
  * Discrete choice models
  * Robust linear models
  * Many models and functions for time series analysis
  * Nonparametric estimators
  * A collection of datasets for examples
  * A wide range of statistical tests
  * Input-output tools for producing tables in a number of formats (Text, LaTex, HTML) and for reading Stata files into NumPy and Pandas.
  * Plotting functions
  * Extensive unit tests to ensure correctness of results
  * Many more models and extensions in development

PyMC: Bayesian Statistics and Monte Carlo Markov Modeling
=========================================================

`PyMC <http://pymc-devs.github.io/pymc/>`_ is a python module that implements Bayesian statistical models and fitting algorithms, including Markov chain Monte Carlo. Its flexibility and extensibility make it applicable to a large suite of problems. Along with core sampling functionality, PyMC includes methods for summarizing output, plotting, goodness-of-fit and convergence diagnostics.

PyMC provides functionalities to make Bayesian analysis as painless as possible. Here is a short list of some of its features:

    * Fits Bayesian statistical models with Markov chain Monte Carlo and other algorithms.
    * Includes a large suite of well-documented statistical distributions.
    * Uses NumPy for numerics wherever possible.
    * Includes a module for modeling Gaussian processes.
    * Sampling loops can be paused and tuned manually, or saved and restarted later.
    * Creates summaries including tables and plots.
    * Traces can be saved to the disk as plain text, Python pickles, SQLite or MySQL database, or hdf5 archives.
    * Several convergence diagnostics are available.
    * Extensible: easily incorporates custom step methods and unusual probability distributions.
    * MCMC loops can be embedded in larger programs, and results can be analyzed with the full power of Python.

A very recommendable, free ebook on Bayesian methods, which also provides a very good introduction to \emph{PyMC}, is \href{http://camdavidsonpilon.github.io/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/}{
Probabilistic Programming \& Bayesian Methods for Hackers}. Warmly recommended!

scikit-learn
============

`scikit-learn <http://scikit-learn.org>`_ is arguably the most advanced open source machine learning package available. It provides simple and efficient tools for data mining and data analysis, covering supervised as well as unsupervised learning.

It provides tools for

  * **Classification**    Identifying to which set of categories a new observation belongs to.
  * **Regression**    Predicting a continuous value for a new example.
  * **Clustering**    Automatic grouping of similar objects into sets.
  * **Dimensionality reduction**    Reducing the number of random variables to consider.
  * **Model selection**    Comparing, validating and choosing parameters and models.
  * **Preprocessing**    Feature extraction and normalization.

Generalized Linear Models
=========================

This is not really a Python package, but rather a book. However, this book that Annette Dobson has written has made *Generalized Linear Models (GLM)}*  understandable and accessible for me (A. Dobson, *An Introduction to Generalized Linear Models*, John Wiley & Sons, 2008). While the book presents solutions for the models for R and Stata, I have developed Python solutions for almost all examples in the book ( https://github.com/thomas-haslwanter/dobson ). 

.. |ipynb| image:: ../Images/IPython.jpg
    :scale: 50 % 
.. |python| image:: ../Images/python.jpg
    :scale: 50 % 

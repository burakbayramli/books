.. image:: ../Images/title_preface.png
    :height: 100 px

This is a free book. It should a) help you to get up to speed with
Python, for b) the statistical analysis of data.

For most of my coding life I have worked with Matlab, as my work was
confined to academic research. When I started to look at real life
applications (i.e. things that you might want to really sell), I
realized how expensive Matlab licenses are. At the same time, I
discovered the (coding) beauty of Python. Put the two together, and add
the fact that around 2010 Python and its documentation had matured to
the point where you could use it without being a hacker. And you get a
free, beautiful package that allows you to do all the statistics that at
least 95% of all researchers need to do in their lifetime. OK, for
really serious statistical modeling :math:`R` still sets the standard.
But most of us will be more than happy with the tools that the Python
ecosystem offers today.

The idea of this book is to give you all (or at least most of) the tools
that you will need for your statistical data analysis. Thereby I try to
provide all the background required to understand what you are doing. I
will not proof any theorems, and won’t indulge in mathematics where it
is unnecessary. This approach explains why so much code is included: in
principle, you have to define our problem, select the corresponding
program, and adapt it to your needs. This should allow you to get going
quickly, even if you have little Python experience. This is also the
reason why I have not provided the software as a Python module, since I
expect that you have to tailor each program to your specific setup (data
format, etc).

How to use this book
--------------------

-  If you just want to look something up, simply go to the `HTML-version
   of the book <http://work.thaslwanter.at/Stats/html>`__.

   In the online version, code samples are marked as follows

   |ipynb| ... refers to *IPython notebooks*

   |python| ... refers to plain Python code 


-  If you want to go through it systematically, or if you prefer to read
   printed material, you may want to download the `PDF-version of the
   book <http://work.thaslwanter.at/Stats/StatsIntro.pdf>`__.

-  If you want to get the whole package, and/or if you want to
   contribute to the book, clone the `github repository of the
   book <https://github.com/thomas-haslwanter/statsintro>`__, which
   includes all the Python programs, the sample data used in the book,
   the TEX-files, RST-files, and all the images.

   If you have never used github, you might want to check out `this
   introduction to
   github <https://help.github.com/articles/set-up-git>`__. But don’t be
   scared off, you can download individual files easily from your
   web-browser.

Contributor List
----------------

If you have a suggestion or correction, please send email to
thomas.haslwanter@fh-linz.at. If I make a change based on your feedback,
I will add you to the contributor list (unless you ask to be omitted).

If you include at least part of the sentence the error appears in, that
makes it easy for me to search. Page and section numbers are fine, too,
but not as easy to work with. Thanks!

- Connor Johnson wrote a very nice blog explaining the results of
  statsmodels OLS command, which formed the basis of a large part of the
  section on *Statistical Models*

.. |ipynb| image:: ../Images/IPython.jpg
    :scale: 50 % 
.. |python| image:: ../Images/python.jpg
    :scale: 50 % 

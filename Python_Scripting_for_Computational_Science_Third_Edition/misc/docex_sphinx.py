"""
This is an example on how to document Python modules using doc 
strings and the sphinx tool. The doc strings can make use of 
the reStructuredText format, see
http://docutils.sourceforge.net/docs/user/rst/quickstart.html

Simple formatting rules
=======================

Paragraphs are separated by blank lines. Words in running
text can be *emphasized*.  Furthermore, text in double backquotes,
as in ``s = sin(r)``, is typeset as code.  Bullet lists start with 
a dash (-) and are indented, with a blank line before and after:

  - *a* is the first parameter
  - *b* is the second parameter. An item can
    occupy multiple lines
  - *c* is the third parameter

Hyperlinks are also available, just write the URL.

Headings
--------

To make a heading, just write the heading and use equal
signs, on the line below the heading, for sections, and 
simple dashes for subsections (other choices of characters
are also possible).

Code snippets
-------------

To include parts of a code, end the preceeding paragraph
with a double colon, *indent* all the computer code, and
insert a blank line before and after the code block::

  if a == b:
     return 2+2

Interactive sessions and doctests can be inserted without
colon and indentation of the code, but a blank line is
needed before and after the interactive block.

>>> a = 1
>>> b = 2
>>> a + b
3


Running Sphinx
--------------

To make a documentation of the present ``docex_epydoc`` module, follow
the forthcoming recipe. First, for each module prepare a file
``modulename.txt`` containing::

  :mod:`modulename`
  =======================

  .. automodule:: modulename
     :members:
     :undoc-members:
     :show-inheritance:

Second, run::

  sphinx-quickstart

and answer the questions. Make sure the extension of sphinx file is
``.txt`` and not ``.rst``. If you make a fresh version of the
documentation, remember to first delete all existing files that
``sphinx-quickstart`` generates (``conf.py``, ``index.txt``,
``_build`` directory). Third, list the modules for which there exist
``.txt`` files in ``index.txt``. Fourth, edit ``conf.py`` such that
``sys.path`` contains the directory where the modules reside. Fifth,
run::

  make html

to generate the Sphinx documentation in HTML format.  Finally, invoke
``_build/html/index.html`` in a browser to see the result.

The file ``docex_sphinx.sh`` contains a Bash script that automates
all the steps above, i.e., it generates the ``docex_sphinx.txt``
file, runs ``sphinx-quickstart``, edits ``conf.py``, etc.

Paragraphs are separated by blank lines. Words in running
text can be *emphasized*.  Furthermore, we may
have "computer code" in double backquotes, as in ``s = sin(r)``.
Examples of lists are given in the ``func1`` function
in class ``MyClass``.

To make a documentation of the present ``docex_epydoc`` module, follow
the forthcoming recipe. First, for each module prepare a file
``modulename.txt`` containing::

  :mod:`modulename`
  =======================

  .. automodule:: modulename
     :members:
     :undoc-members:
     :show-inheritance:

Second, run ``sphinx-quickstart`` and answer the questions. Make sure
the extension of the sphinx file is ``.txt`` and not ``.rst``. If you
make a fresh version of the documentation, remember to first delete
all existing files that ``sphinx-quickstart`` generates (``conf.py``, 
``index.txt``, ``_build`` directory). Third, list the modules for
which there exist ``.txt`` files in ``index.txt``. Fourth, edit
``conf.py`` such that ``sys.path`` contains the directory where the
modules reside. Fifth, run ``make html`` to generate the Sphinx
documentation in HTML format. 
Finally, invoke ``_build/html/index.html`` in a browser to see the result.
"""

class MyClass:
    """
    The purpose of MyClass is just to show how
    multi-line doc strings can be used to
    document classes.

    Example of a usage::

       my = MyClass()
       newlist = my.func1(1, 8, -0.1)
       oldlist += newlist[:1]
       
    """

    def func1(self, a, b, c):
        """
        Demonstrate how to document a function
        using doc strings and reStructuredText formatting.

        ==========   ===========   ================================
         Argument        Type                Description
        ==========   ===========   ================================
        a            float/int     first parameter
        b            sequence      second parameter
        c            arbitrary     third parameter
        ==========   ===========   ================================

        Return value: a list of the three input parameteres 
        ``[2*a,b,c]``.
        """
        return [2*a,b,c]
    
    def func2(self, a, b, c):
        # no doc
        a = b
        return c

def standalone(x):
    """
    Stand-alone functions can also be documented.
    Parameter *x* is some input.
    Return value: ``2+2``.
    """
    return 2+2

def standalone2():
    """Single-line doc string."""
    return 1+1

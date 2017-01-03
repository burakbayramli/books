"""
This is an example on how to document Python modules
using doc strings and the epydoc tool. The doc strings 
can make use of the epytext format.

Paragraphs are separated by blank lines. Words in running
text can be I{emphasized} and X{indexed}.  Furthermore, we may
have computer code, like C{s = sin(r)}.
Bullet lists start with dash (-) and are indented,
with a blank line before and after:

  - *a* is the first parameter
  - *b* is the second parameter. An item can
    occupy multiple lines
  - *c* is the third parameter

To make a documentation of the C{docex_epydoc} module, run::

  epydoc --html -o tmp docex_epydoc.py

Then invoke C{tmp/index.html} in a browser to see the result.
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
        using doc strings and Epytext formatting.

        @param a: first parameter.
        @type a: float or int
        @param b: second parameter.
        @type b: arbitrary
        @param c: third parameter.
        @type c: arbitrary
        @return: a list of the three input parameteres C{[2*a,b,c]}.
        """
        return [2*a, b, c]
    
    def func2(self, a, b, c):
        # no doc
        a = b
        return c

def standalone(x):
    """
    Stand-alone functions can also be documented.
    @param x: some input.
    @return: 2+2.
    """
    return 2+2

def standalone2():
    """Single-line doc string."""
    return 1+1








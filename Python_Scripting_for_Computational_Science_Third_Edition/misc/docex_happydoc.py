"""
This is an example on how to document Python modules
using doc strings and the HappyDoc tool.
The doc strings can make use of the the StructuredText
format, which is documented in the StructuredText.py
file that comes with HappyDoc. 

Simple formatting rules

  Paragraphs are separated by blank lines. Words in running
  text can be *emphasized*.  Furthermore, text in single
  forward quotes, like 's = sin(r)', is typeset as code.
  Examples of lists are given in the 'func1' function
  in class 'MyClass' in the present module.
  Hyperlinks are also available, see the 'README.txt' file
  that comes with HappyDoc.

Headings

  To make a heading, just write the heading and
  indent the proceeding paragraph.

Code snippets

  To include parts of a code, end the preceeding paragraph
  with example:, examples:, or a double colon::

      if a == b:
          return 2+2

Running HappyDoc

  To make a documentation of the doc.py module, run::
    
     happydoc doc.py

  It's as simple as that! The HTML files are in the
  'doc' subdirectory.
"""

class MyClass:
    """
    The purpose of MyClass is just to show how
    multi-line doc strings can be used to
    document classes.
    """

    def func1(self, a, b, c):
        """\
        Demonstrate how to document a function
        using doc strings and StructuredText.
        The arguments are:

          a -- first parameter

          b -- second parameter

          c -- third parameter

        This was an example of a descriptive list.        
        Bullet lists can be created as follows:

          * a is the first parameter

          * b is the second parameter

          * c is the third parameter

        Notice the need for blank lines between list items.
        Numbered lists are generated like this:

          1. a is the first parameter

          2. b is the second parameter

          3. c is the third parameter

        Nested lists also work fine:

          **arguments** --

            * item1

            * item2
        
        """
        return [a,b,c]
    
    def func2(self, a, b, c):
        # no doc
        a = b
        return c

def standalone():
    """\
    Stand-alone functions can also be documented.
    """
    return 2+2








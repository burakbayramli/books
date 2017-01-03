"""
HTML form utility from Harms and McDonald (Manning, 1999).
(Used in scripts in src/py/cgi)
"""

from HTMLgen import *
from HTMLcolors import *


class InputTable:
    """InputTable(entries, [keyword=value]...)
    Entries is a list of 3-element tuples (name, input_object, note) where
    name will label the input object on the left column, the input_object
    can be anything, and the note is a string to provide optional notations
    to the right of the input widgets.
    """
    def __init__(self, entries, **kw):
        self.entries = entries
        self.leftcolor = WHITE      # color used for left label column
        self.centercolor = WHITE    # color used for center input column
        self.rightcolor = WHITE     # color used for right label column
        self.leftalign = 'right'    # text alignment for left label column
        for (item, value) in kw.items():
            setattr(self, item, value)

    def __str__(self):
        table = TableLite(border=0, cellspacing=4)
        for (label, input, note) in self.entries: # assume a 3-tuple 
            row = TR()
            row.append(TD(label, align=self.leftalign, bgcolor=self.leftcolor))
            row.append(TD(input, bgcolor=self.centercolor))
            if note:
                row.append(TD(note, bgcolor=self.rightcolor))
            table.append(row)
        return str(table)

class RadioButtons:
    widgettype = 'radio'
    def __init__(self, items, **kw):
        self.items = items
        self.name = 'radiochoice'
        self.orient = 'vertical'
        self.selected = []
        for (item, value) in kw.items():
            setattr(self, item, value)
    def __str__(self):
        if self.orient[0] == 'v':
            sep = '<BR>\n'
        else:
            sep = ', \n'
        if type(self.selected) is type(""):
            self.selected = [self.selected]
        s = []
        for item in self.items:
            if item in self.selected:
                s.append(str(Input(type=self.widgettype, name=self.name,
                                   checked=1)))
            else:
                s.append(str(Input(type=self.widgettype, name=self.name)))
            s.append(str(item))
            s.append(sep)
        return string.join(s[:-1], "")

class CheckBoxes(RadioButtons):
    widgettype = 'checkbox'


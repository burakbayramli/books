#!/usr/bin/env python
"""
Demonstrate event bindings in a text widget
(text_tag1.py but we use lambda functions instead of functools.partial).
"""
from Tkinter import *

class HelloWorld:
    
    def __init__(self, parent):

        self.hwtext = Text(parent, width=20, height=5, wrap='word')
        self.hwtext.pack(side='top', pady=20)
	self.hwtext.insert('end',  # insert text after end
                           'Hello, World!', # text
                           'tag1') # name of tag bound to this text)
	self.hwtext.insert('end','\n')
        self.hwtext.insert('end',"You haven't hit me yet!", 'tag2')

        # let tag1 be blue when the mouse is over the tag
        # use lambda functions to implement the feature:
        self.hwtext.tag_bind('tag1','<Enter>',
            lambda event=None, x=self.hwtext:
              x.tag_configure('tag1',background='blue'))
	self.hwtext.tag_bind('tag1','<Leave>',
             lambda event=None, x=self.hwtext:
              x.tag_configure('tag1',background='white'))

        # let tag2 be yellow when the mouse is over the tag
        # use run-time, string-based construction of functions
        # to implement the feature (this is more flexible than
        # lambda functions, but requires more code - consider
        # it as an alternative to the solution above)
        
        self.nhits_tag2 = 0 # count the no of mouse hits on tag2
                            # must appear before the func def below

        self.tag2_enter = self.genfunc('tag2', 'yellow',
             # add a string containing optional Python code:
             r"i=self.hwtext.index('tag2'+'.first'); "\
             "self.hwtext.delete(i,'tag2'+'.last'); "\
             "self.hwtext.insert(i,'You have hit me "\
             "%d times' % self.nhits_tag2, 'tag2'); "\
             "self.nhits_tag2 = self.nhits_tag2 + 1")

        self.hwtext.tag_bind('tag2', '<Enter>', self.tag2_enter)

        self.tag2_leave = self.genfunc('tag2', 'white')
        self.hwtext.tag_bind('tag2', '<Leave>', self.tag2_leave)

        # example on a call:
        # self.tag2_enter(self.hwtext)

    # the following function constructs a function (with name funcname)
    # and Python code in the string code (observe how the calling
    # program is allowed to insert desired code!)
    
    def genfunc(self, tag, bg, optional_code=''):
        funcname = '%(tag)s_%(bg)s_update' % vars()
        # note: funcname can be as simple as 'temp', no unique
        # name is needed
        code = "def %(funcname)s(event=None):\n"\
               "    self.hwtext.tag_configure("\
               "'%(tag)s', background='%(bg)s')\n"\
               "    %(optional_code)s\n" % vars()
               #"    print \"calling\",funcname " 
        # run function definition as a python script:
        # normally "exec code" suffices, but this creates a function that
        # cannot see self.hwtext, but with "in vars()" this is visible
        # (see 6.13 in the reference manual)
        exec code in vars()
        print '\n\ngenerated a function:\n',funcname,'with contents\n',code
        # return function from funcname string:
        return eval(funcname)

root = Tk()               # root (main) window
hello = HelloWorld(root)
root.mainloop()

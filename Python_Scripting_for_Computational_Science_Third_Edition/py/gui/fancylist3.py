#!/usr/bin/env python

"""
As class FancyList2, but the interface is more flexible
for using FancyList3 as a list widget in GUIs.
The list, consisting of items and help lines, can be given
to the constructor or (later) to the setlist function.
Moreover, there is a clear function for clearing the list
and a get function for getting the contents of the list.
The constructor supports some extra keyword arguments borrowed
from Pmw.ScrolledListBox widgets.
"""
import Tkinter, Pmw, re

class Fancylist3:
    def __init__(self, parent, items_with_help=None,
                 list_width=20, list_height=10,
                 list_label='', labelpos='n',
                 listbox_font=None, label_font=None,
                 selectioncommand=None):
        self.frame = Tkinter.Frame(parent, borderwidth=3)
        self.frame.pack()
        
        self.listbox = Pmw.ScrolledText(self.frame,
            vscrollmode='dynamic', hscrollmode='dynamic',
            labelpos=labelpos, label_text=list_label,
            text_width=list_width, text_height=list_height,
            text_wrap='none',  # do not break too long lines
            text_font=listbox_font
        )
        self.listbox.pack(pady=10)
        self.selectioncommand = selectioncommand

        self.helplabel = Tkinter.Label(self.frame, width=60)
        self.helplabel.pack(side='bottom', fill='x', expand=True)

        if items_with_help:
            self.setlist(items_with_help)

    def setlist(self, items_with_help):
        self.clear()
        self.list = items_with_help
        counter = 0
        for (item, help) in self.list:
            tag = 'tag' + str(counter)  # unique tag name for each item
            self.listbox.insert('end', item + '\n', tag)
            self.listbox.tag_configure(tag, background='white')
            self.listbox.tag_bind(tag, '<Enter>', self.enter)
            self.listbox.tag_bind(tag, '<Leave>', self.leave)
            self.listbox.tag_bind(tag, '<Button-1>', self.record)
            counter = counter + 1
        # make the text buffer read-only:
        self.listbox.configure(text_state='disabled')

    def get(self, first=None, last=None):
        items = []
        if not first: first = 0
        if not last: last = len(self.list)
        if type(last) == type('end'):
            if last == 'end': last = len(self.list)
        for i in range(first,last,1):
            items.append(self.list[i][0])
        return items

    def clear(self):
        self.listbox.configure(text_state='normal')  # can write text
        self.listbox.clear()
        self.selected = []  # list of info about selected items
        
    def getlineno(self):
        'return the list item number corresp. to current mouse pos.'
        # Tk text widgets define an index 'current' that reflects
        # the character closest to the mouse
        index = self.listbox.index('current') # f.ex.: 4.12
        # extract the line number:
        #line = re.sub(r'\..*$', '', index)  # 4.12 to 4
        line = index.split('.')[0]           # 4.12 to 4
        # lines start at 1, subtract 1 and return int:
        return int(line) - 1
        
    def enter(self, event):
        tag = self.listbox.tag_names(self.listbox.index('current'))[0]
        # change color if not selected (i.e. green) and update help info:
        if not self.listbox.tag_cget(tag, 'background') == 'green':
            self.listbox.tag_configure(tag, background='yellow')
            self.helplabel.configure(text=self.list[self.getlineno()][1])

    def leave(self, event):
        tag = self.listbox.tag_names(self.listbox.index('current'))[0]
        # change color if not selected (i.e. green):
        if not self.listbox.tag_cget(tag, 'background') == 'green':
            self.listbox.tag_configure(tag, background='white')
            self.helplabel.configure(text='')

    def record(self, event):
        tag = self.listbox.tag_names(self.listbox.index('current'))[0]
        # find start and stop index for the current tag:
        start, stop = self.listbox.tag_ranges(tag)
        # with the start and stop index, get the tag's text:
        text = self.listbox.get(start, stop)
        text = text[:-1]  # drop last newline in text
        line = self.getlineno()
        try:
            # deselect:
            # (see if current (lineno,text) has been recorded in
            # self.selected, and if so, remove that item
            # and reset the background color to white (non-selected))
            i = self.selected.index((line,text))
            del self.selected[i]
            self.listbox.tag_configure(tag, background='white')
        except:
            # select:
            # (append (line,text) to self.selected and mark
            # the background with green color (selected))
            self.selected.append((line,text))
            self.listbox.tag_configure(tag, background='green')
            #print 'curselection:', self.getcurselection()
            #print '   selection:', self.curselection()
        # call a function specified by the selectioncommand= keyword arg
        # to the constructor?
        if self.selectioncommand is not None:
            self.selectioncommand()

    def getcurselection(self):
        return [text for index, text in self.selected]

    def curselection(self):
        return [index for index, text in self.selected]


# create demo in root window for testing
if __name__ == '__main__':
    root = Tkinter.Tk()
#    Pmw.initialise(root,fontScheme='pmw1')
    Pmw.initialise(root)
    root.title('GUI for Script II')

    list = [('curve1',  'initial surface elevation'),
            ('curve2',  'bottom topography'),
            ('curve3',  'surface elevation at t=3.2')
           ]
    def printout():
        print widget.getcurselection()
    widget = Fancylist3(root, items_with_help=list,
                        selectioncommand=printout)
    list += [('curve4', 'some other curve')]
    widget.setlist(list)
    def dump():
        print "Chosen items:",
        print widget.getcurselection()
        print "Chosen indices:",
        print widget.curselection()
    Tkinter.Button(root, text="Show selections", command=dump).pack()
    root.mainloop()



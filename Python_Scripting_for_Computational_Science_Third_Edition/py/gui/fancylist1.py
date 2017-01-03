#!/usr/bin/env python
import Tkinter, Pmw

class Fancylist1:
    def __init__(self, parent, list, list_width=20, list_height=10,
                 list_label='list of items'):
        self.frame = Tkinter.Frame(parent, borderwidth=3)
        self.frame.pack()
        
        self.listbox = Pmw.ScrolledText(self.frame,
            vscrollmode='dynamic', hscrollmode='dynamic',
            labelpos='n', label_text=list_label,
            text_width=list_width, text_height=list_height,
            text_wrap='none',  # do not break too long lines
        )
        self.listbox.pack(pady=10)
        # use the font in the label also for the list items:
        #self.font = self.listbox.component('label').cget('font')

        self.helplabel = Tkinter.Label(self.frame, width=60)
        self.helplabel.pack(side='bottom', fill='x', expand=True)

        self.selected = []
        self.setlist(list)

    def setlist(self, items_with_help):
        self.listbox.configure(text_state='normal')  # can write text
        self.listbox.clear() # delete all text
        self.list = items_with_help
        counter = 0
        for (item, help) in list:
            tag = 'tag' + str(counter)  # unique tag name for each item
            self.listbox.insert('end', item + '\n', tag)
            self.listbox.tag_configure(tag, background='white')
            # bind mouse events:
            from functools import partial
            self.listbox.tag_bind(tag, '<Enter>',
               partial(self.configure, tag, 'yellow', help))
            self.listbox.tag_bind(tag, '<Leave>',
               partial(self.configure, tag, 'white', ''))
            self.listbox.tag_bind(tag, '<Button-1>',
               partial(self.record, tag, counter, item))

            # with lambda functions:
            #self.listbox.tag_bind(tag, '<Enter>',
            #   lambda event, f=self.configure, t=tag, bg='yellow', text=help:
            #   f(event, t, bg, text))
            #self.listbox.tag_bind(tag, '<Leave>',
            #   lambda event, f=self.configure, t=tag, bg='white', text='':
            #   f(event, t, bg, text))
            #self.listbox.tag_bind(tag, '<Button-1>',
            #   lambda event, f=self.record, t=tag, c=counter, text=item:
            #   f(event, t, c, text))

            counter = counter + 1
        # make the text buffer read-only:
        self.listbox.configure(text_state='disabled')

    def configure(self, tag, bg, text, event):
        # do not change background color if the tag is selected,
        # i.e. if the tag is green:
        if not self.listbox.tag_cget(tag,'background') == 'green':
            self.listbox.tag_configure(tag, background=bg)
            self.helplabel.configure(text=text)
            
    def record(self, event, tag, line, text):
        try:
            # deselect:
            # (see if current (lineno,text) has been recorded in
            # self.selected, and if so, remove that item
            # and reset the background color to white (non-selected))
            i = self.selected.index((line, text))
            del self.selected[i]  # remove this item
            self.listbox.tag_configure(tag, background='white')
        except:
            # select:
            # (append (line,text) to self.selected and mark
            # the background with green color (selected))
            self.selected.append((line,text))
            self.listbox.tag_configure(tag, background='green')
            print 'You clicked a new item'
            print 'curselection:', self.getcurselection()
            print '   selection:', self.curselection()

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
    widget = Fancylist1(root, list, list_width=20, list_height=6,
                        list_label='test of fancy list')
    def dump():
        print "Chosen items:",
        print widget.getcurselection()
        print "Chosen indices:",
        print widget.curselection()
    Tkinter.Button(root, text="Show selections", command=dump).pack()
    root.mainloop()





#!/usr/bin/env python
import glob, os, sys, shutil, Pmw
from Tkinter import *

class Browser:
    def __init__(self, parent, files=None, wildcard_specification='*.ps'):
        if files is None:
            self.psfiles = glob.glob(wildcard_specification)
        else:
            self.psfiles = files
        self.psfiles.sort()
        self.giffiles = []
        for file in self.psfiles:
            root, ext = os.path.splitext(file)
            giffile = root + '.gif'
            self.giffiles.append(giffile)
            if not os.path.isfile(giffile):
                cmd = 'convert %s gif:%s' % (file, giffile)
                print cmd
                os.system(cmd)
            else:
                pass
        self.giffiles.sort()

        # make user interface
        self.master = parent
        self.top = Pmw.ScrolledFrame(self.master,
                usehullsize=1, hull_height=700, hull_width=400)
        self.top.pack(side='top', expand=True, fill='both')
        self.buttons = Frame(self.top.interior())  # frame for navigation
        self.buttons.pack(side='top')
        Button(self.buttons, text='Backward', width=10,
               command=self.backward).grid(row=0,column=0)
        Button(self.buttons, text='Forward', width=10,
               command=self.forward).grid(row=1,column=0)
        Button(self.buttons, text='Save', width=10,
               command=self.save).grid(row=0,column=1)
        Button(self.buttons, text='Kill saved', width=10,
               command=self.kill_saved).grid(row=1,column=1)
        Button(self.buttons, text='Quit', width=10,
               command=self.master.destroy).grid(row=2,column=0,columnspan=2)

        self.main_image_frame = Frame(self.top.interior())
        self.main_image_frame.pack(side='top')

        self.image_gif = PhotoImage(file=self.giffiles[0])
        self.image_label = Label(self.main_image_frame,
                                 image=self.image_gif)
        self.image_label.pack(side='top',pady=10)
        self.images = [self.image_gif]
        self.image_filename_var = StringVar()
        self.image_filename_var.set(self.giffiles[0])
        self.image_filename = Entry(self.main_image_frame,
                                    textvariable=self.image_filename_var,
                                    width=len(self.image_filename_var.get()),
                                    relief='flat') #, state='disabled')
        self.image_filename.pack(side='top')
        self.saved_image_frame = Frame(self.top.interior())
        self.saved_image_frame.pack(side='top')
        self.saved_photos = []
        self.saved_image_label = []
        self.saved_filenames = []
        self.current = 0  # index in self.giffiles

    def backward(self):
        if self.current > 0:
            self.current -= 1
        file = self.giffiles[self.current]
        self.image_gif = PhotoImage(file=file)
        self.image_label.configure(image=self.image_gif)
        self.image_filename_var.set(file)
        self.image_filename.configure(width=len(file))
        
    def forward(self):
        if self.current < len(self.giffiles)-1:
            self.current += 1
        file = self.giffiles[self.current]
        self.image_gif = PhotoImage(file=file)
        self.image_label.configure(image=self.image_gif)
        self.image_filename_var.set(file)
        self.image_filename.configure(width=len(file))

    def save(self):
        self.saved_photos.append(self.image_gif)
        self.saved_image_label.append(Label(self.saved_image_frame,
                                       image=self.saved_photos[-1]))
        self.saved_image_label[-1].pack(side='top')
        #print 'name of saved file',self.image_filename_var.get()
        self.saved_filenames.append(Entry(self.saved_image_frame,
                       relief='flat', 
                       width=len(self.image_filename_var.get())))
        self.saved_filenames[-1].pack(side='top')
        self.saved_filenames[-1].insert('end',self.image_filename_var.get())
        #self.saved_filenames[-1].configure(state='disabled')

    def kill_saved(self):
        self.saved_image_label[-1].destroy()
        del self.saved_image_label[-1]
        self.saved_filenames[-1].destroy()
        del self.saved_filenames[-1]
        del self.saved_photos[-1]

if __name__ == '__main__':
    root = Tk()
    Pmw.initialise(root)
    if len(sys.argv) < 2:
        print 'Usage: psplot_browser.py tmp_*.ps'
        sys.exit(0)
    gui = Browser(root, files=sys.argv[1:])
    root.mainloop()

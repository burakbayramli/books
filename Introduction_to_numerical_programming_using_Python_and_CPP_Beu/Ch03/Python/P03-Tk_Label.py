from tkinter import *                                 # import Tkinter module

root = Tk()                                           # create Tk root widget
                                         # create label widget: child to root
w = Label(root, text="This is a Label widget")
w.pack()                          # resize label to fit text; make it visible

root.mainloop()                                    # enter Tkinter event loop

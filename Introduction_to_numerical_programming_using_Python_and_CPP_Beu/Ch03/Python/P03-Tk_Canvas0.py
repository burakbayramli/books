from tkinter import *                                 # import Tkinter module

root = Tk()                                           # create Tk root widget
root.title("Canvas")
                                               # create canvas: child to root
w = Canvas(root, width=300, height=200, bg = "white")
w.pack()                                     # resize and make canvas visible

root.mainloop()                                    # enter Tkinter event loop

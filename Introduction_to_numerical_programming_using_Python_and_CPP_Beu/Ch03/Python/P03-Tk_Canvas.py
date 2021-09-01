from tkinter import *                                 # import Tkinter module

root = Tk()                                           # create Tk root widget
root.title("Canvas")

nx = 300; ny = 300                                              # canvas size
w = Canvas(root, width=nx, height=ny, bg = "white")           # create canvas
w.pack()                                     # resize and make canvas visible

w.create_line(150, 150, 100, 100, fill="red" , arrow=LAST, width=2)  # arrows
w.create_line(150, 150, 150, 250, fill="blue", arrow=LAST)

w.create_rectangle(50, 50, 250, 250, width=2)                        # square
w.create_oval(50, 50, 250, 250)                                      # circle

w.create_text(150, 75, text="(10:30)")                                 # text

root.mainloop()                                    # enter Tkinter event loop

from Tkinter import *

class Application(Frame):
    def __init__(self,master=None):
        Frame.__init__(self,master)
        self.grid()
        self.createWidgets()
        
    def createWidgets(self):
        self.demo_menu = Menubutton(self,text="Demos",relief=RAISED)
        self.demo_menu.grid(row=0,column=0)
        self.demo_menu.menu = Menu(self.demo_menu)
        self.demo_menu["menu"] = self.demo_menu.menu

        self.demo_menu.menu.add_command(label='Cool',command=self.quit)

        self.lecture_menu = Menubutton(self,text="Lectures",relief=RAISED)
        self.lecture_menu.grid(row=0,column=1)
        self.lecture_menu.menu = Menu(self.lecture_menu)
        self.lecture_menu["menu"] = self.lecture_menu.menu

        for lectnum in range(1,20):
            self.lecture_menu.menu.add_command(label=str(lectnum),command=self.quit)


        self.quitButton = Button(self,text="Quit",command=self.quit)
        self.quitButton.grid()

app = Application() 
app.master.title("gPy")
app.mainloop()

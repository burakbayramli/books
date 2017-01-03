import sys
import cPickle

sample = cPickle.load(open(sys.argv[1]))

n=0

def next():
    global n
    s = sample[n]
    k = 0
    for i in range(10):
        for j in range(10):
            if s[k] == 1:
                widgets[i][j].config(fg='black',bg='black')
            else:
                widgets[i][j].config(fg='white',bg='white')
            k+=1
    n += 1



import Tkinter
root=Tkinter.Tk()
frame = Tkinter.Frame(root)
frame.pack()
widgets = []
for i in range(10):
    x = []
    widgets.append(x)
    for j in range(10):
        widget = Tkinter.Label(frame,text='0')
        x.append(widget)
        widget.grid(row=i,column=j)
button = Tkinter.Button(root,text='Press',command=next)
button.pack()
root.mainloop()




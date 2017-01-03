#!/usr/bin/env python
"""Demo of the most fundamental Tkinter and Pmw widgets"""
from Tkinter import *
import tkMessageBox, tkFileDialog, tkColorChooser
import Pmw
import string, sys, os

class InputFields:
    """
    Demonstrate typical widgets for input data: entry, slider,
    option menu, checkbutton, radiobuttons.
    """
    def __init__(self,
                 parent,         
                 status_line,    
                 balloon=None,   
                 scrolled=True): 
        """
        Create widgets.
        
        parent           parent widget
        status_line      Label with status of user actions
        balloon          balloon help widget
        scrolled         scrolled main frame or not
        """
        self.master = parent
        self.status_line = status_line
        self.balloon = balloon
            
        if scrolled:
            # use an intelligent Pmw.ScrolledFrame widget to hold the
            # whole window; scrollbars are automatically added if necessary
            self.topframe = Pmw.ScrolledFrame(self.master,
                 usehullsize=1, hull_height=210, hull_width=340)
            # (just Pmw.ScrolledFrame(self.master) gives a fixed-size
            # domain with scrollbars; height/weight should here be adjusted
            # pack self.topframe in a separate function)

            # create all other widgets inside the top frame:
            self.create(self.topframe.interior())
            # or: self.create(self.topframe.component('frame'))
        else:
            # use a standard Tkinter Frame with adaptive size:
            self.topframe = Frame(self.master, borderwidth=2, relief='groove')
            self.topframe.pack(expand=True, fill='both')
            # create all other widgets inside the top frame:
            self.create(self.topframe)

    def pack(self, **kwargs):
        """
        Pack the topframe. The location of the InputFields GUI in
        the parent widget can be controlled by the calling code.
        """
        self.topframe.pack(kwargs, expand=True, fill='both')
        
    def create(self, parent):
        """Create all widgets."""

        # simple Pmw.EntryField:
        self.case = StringVar()  # tie a variable to an entry
        self.case_widget = Pmw.EntryField(parent,
                        labelpos='w', 
                        label_text='case name',
                        entry_width=15,
                        entry_textvariable=self.case,
                        command=self.status_entries)

        # entry field with validate specification:
        self.mass = DoubleVar(); self.mass.set(1.0)
        self.mass_widget = Pmw.EntryField(parent,
                        labelpos='w',  # n, nw, ne, e, and so on
                        label_text='mass',
                        validate={'validator': 'real', 'min': 0},
                        entry_width=15,
                        entry_textvariable=self.mass,
                        command=self.status_entries)
        
        # skip tying the next entry to a variable, use value= and get()
        self.damping_widget = Pmw.EntryField(parent,
                        labelpos='w',
                        label_text='damping',
                        validate={'validator': 'real', 'min': 0},
                        entry_width=15,
                        value=0.0,
                        command=self.status_entries)

        # entry field with balloon help:
        self.A = DoubleVar(); self.A.set('2')
        self.A_widget = Pmw.EntryField(parent,
                        labelpos='w',
                        label_text='amplitude',
                        validate=None,
                        entry_width=15,
                        entry_textvariable=self.A,
                        command=self.status_entries)
        try:
            self.balloon.bind(self.A_widget,
                             'Pressing return updates the status line')
        except:
            # balloon help is not available
            pass


        # slider:
        self.y0 = DoubleVar();  self.y0.set(0.2)
        self.y0_widget = Scale(parent,
          orient='horizontal',
          from_=0, to=2,    # range of slider
          tickinterval=0.5, # tickmarks on the slider "axis"
          resolution=0.05,  # the steps of the counter above the slider
          label='initial value y(0)',  # label printed above the slider
          #font='helvetica 12 italic', # optional font
          length=300,                  # length of slider in pixels
          variable=self.y0,            # value is tied to self.y0
          command=self.status_slider)

        # option menu:
        self.func = StringVar(); self.func.set('y')
        self.func_widget = Pmw.OptionMenu(parent,
               labelpos='w',  # n, nw, ne, e, and so on
               label_text='spring',
               items=['y', 'y3', 'siny'],
               menubutton_textvariable=self.func,
               menubutton_width=6,
               command=self.status_option)

        # checkbutton:
        self.store_data = IntVar(); self.store_data.set(1)
        self.store_data_widget = Checkbutton(parent,
                        text='store data',
                        variable=self.store_data,
                        command=self.status_checkbutton)

        # pack all entries and option menus, and align them nicely:
        widgets = (self.case_widget, self.mass_widget,
                   self.damping_widget, self.A_widget,
                   self.func_widget)
        for w in widgets:
            w.pack(side='top', pady=3, anchor='w')
        Pmw.alignlabels(widgets)

        # pack slider and checkbutton at the end (these are
        # adjusted to the left):
        self.y0_widget.pack(side='top', anchor='w', pady=4)      
        self.store_data_widget.pack(side='top', anchor='w', pady=3) 

    def status_entries(self):
        """Read values from entry widgets or variables tied to them."""
        s = "entry fields: '" + self.case.get() + \
            "', " + str(self.mass.get()) + \
            ", " + self.damping_widget.get() + \
            ", " + str(self.A.get())
        self.status_line.configure(text=s)

    def status_slider(self, value):
        self.status_line.configure(text='slider value: ' + \
                                   str(self.y0.get()))
        # or
        self.status_line.configure(text='slider value: ' + value)

    def status_option(self, value):
        self.status_line.configure(text=self.func)
        # or
        self.status_line.configure(text=value)

    def status_checkbutton(self):
        self.status_line.configure(text='store data checkbutton: ' + \
                                   str(self.store_data.get()))
        

class InputLists:
    """
    Demonstrate various widgets that let the user choose
    items from some kind of list:
    standard listbox, combo boxes, radio buttons, collection of
    checkbuttons, option menu.
    """
    def __init__(self, parent, status_line, balloon=None):
        self.master = parent
        self.status_line = status_line
        self.balloon = balloon
        self.frame = Frame(self.master, borderwidth=3)
        # pack self.frame in a separate function
        self.create(self.frame)

    def pack(self, **kwargs):
        self.frame.pack(kwargs)
        
    def create(self, parent):

        header = Label(parent, text='Widgets for list data', 
                       font='courier 14 bold', foreground='blue',
                       background='#%02x%02x%02x' % (196,196,196))
        header.pack(side='top', pady=10, ipady=10, fill='x')

        # frame for left-to-right packing of single-selection
        # list-like widgets:
        frame = Frame(parent); frame.pack(side='top')
        # the various widgets are aligned with a common top line,
        # obtained by anchor='n'

        # create list:
        listitems = ['list item ' + str(i+1) for i in range(40)]

        # standard listbox:
        self.list1 = Pmw.ScrolledListBox(frame,
               listbox_selectmode='single', # 'multiple'
               vscrollmode='static', hscrollmode='dynamic',
               listbox_width=12, listbox_height=6,
               label_text='plain listbox\nsingle selection',
               # labelpos is needed if label_text is present,
               # choices: n (north), nw (north-west), s (south) ...
               labelpos='n',  
               selectioncommand=self.status_list1)
        self.list1.pack(side='left', padx=10, anchor='n')
        # insert items:
        for item in listitems:
            self.list1.insert('end', item)  # insert after end of list
        # could also say
        # self.list.configure(items=listitems)
        # or give the items value as keyword 'items='
        # at construction time

        # example on updating an option in, e.g., the Tkinter
        # Listbox part of a Pmw.ScrolledListBox:
        # self.list.component('listbox').configure(background='blue')
        # i.e. the parts (listbox, label, etc) are ordinary Tkinter
        # widgets that can be extracted by the component method

        # simple combo box with list and entry for chosen item:
        self.combo1 = Pmw.ComboBox(frame,
               label_text='simple combo box',
               labelpos='nw',
               scrolledlist_items=listitems,
               selectioncommand=self.status_combobox,
               listbox_height=6,
               dropdown=False)
        self.combo1.pack(side='left', padx=10, anchor='n')

        # dropdown combo box with entry for chosen item and
        # button for showing the list:
        self.combo2 = Pmw.ComboBox(frame,
               label_text='dropdown combo box',
               labelpos='nw',
               scrolledlist_items=listitems,
               selectioncommand=self.status_combobox,
               listbox_height=6,
               dropdown=True)  # the only difference from combo1
        self.combo2.pack(side='left', padx=10, anchor='n')
        if self.balloon is not None:
            self.balloon.bind(self.combo2, 'Click on arrow to display list')

        frame_left = Frame(parent); frame_left.pack(side='left')
        
        # standard listbox:
        self.list2 = Pmw.ScrolledListBox(frame_left,
               listbox_selectmode='multiple',
               vscrollmode='static', hscrollmode='dynamic',
               listbox_width=12, listbox_height=6,
               label_text='plain listbox\nmultiple selection',
               labelpos='n',
               items=listitems,
               selectioncommand=self.status_list2)
        self.list2.pack(side='left', anchor='n')

        # frame_right holds other widgets packed top-bottom:
        frame_right = Frame(parent); frame_right.pack(side='left')

        # option menu:
        self.option_var = StringVar(); self.option_var.set('item2')
        self.option1 = Pmw.OptionMenu(frame_right,
               labelpos='w',  # n, nw, ne, e, and so on
               label_text='Option Menu:',
               items=['item1', 'item2', 'item3', 'item4'],
               menubutton_textvariable=self.option_var,
               menubutton_width=6,
               command=self.status_option)
        self.option1.pack(side='top', anchor='w')
        
        # plain Tk radio buttons, tied to a variable:
        self.radio_var = StringVar() # common variable for radio buttons
        self.radio1 = Frame(frame_right)
        self.radio1.pack(side='top', pady=5)
        Label(self.radio1,
              text='Tk radio buttons').pack(side='left')
        for radio in ('radio1', 'radio2', 'radio3', 'radio4'):
            r = Radiobutton(self.radio1, text=radio, variable=self.radio_var,
                            value='radiobutton no. ' + radio[5],
                            command=self.status_radio1)
            r.pack(side='left')

        # Pmw radio buttons
        self.radio2 = Pmw.RadioSelect(frame_right,
               selectmode='single',
               buttontype='radiobutton', # 'button': plain button layout
               labelpos='w',
               label_text='Pmw radio buttons\nsingle selection',
               orient='horizontal',
               frame_relief='ridge', # try some decoration...
               command=self.status_radio2)
        self.radio2.pack(side='top', padx=10, anchor='w')
        # add items; radio buttons are only feasible for a few items:
        for text in ('item1', 'item2', 'item3', 'item4'):
            self.radio2.add(text)
        self.radio2.invoke('item2')  # 'item2' is pressed by default


        # check button list:
        self.radio3 = Pmw.RadioSelect(frame_right,
               selectmode='multiple',
               buttontype='checkbutton',
               labelpos='w',
               label_text='Pmw check buttons\nmultiple selection',
               orient='horizontal',
               frame_relief='ridge', # try some decoration...
               command=self.status_radio3)
        self.radio3.pack(side='top', padx=10, anchor='w')
        # add items; radio buttons are only feasible for a few items:
        for text in ('item1', 'item2', 'item3', 'item4'):
            self.radio3.add(text)
        # press 'item2' and 'item4' by default:
        self.radio3.invoke('item2');  self.radio3.invoke('item4')

    def status_list1(self):
        """Extract single list selection."""
        selected_item   = self.list1.getcurselection()[0]
        selected_index = self.list1.curselection()[0]
        text = 'selected list item=' + str(selected_item) + \
               ', index=' + str(selected_index)
        self.status_line.configure(text=text)

    def status_list2(self):
        """Extract multiple list selections."""
        selected_items   = self.list2.getcurselection() # tuple
        selected_indices = self.list2.curselection()    # tuple
        text = 'list items=' + str(selected_items) + \
               ', indices=' + str(selected_indices)
        self.status_line.configure(text=text)

    def status_combobox(self, value):
        text = 'combo box value = ' + str(value)
        self.status_line.configure(text=text)

    def status_radio1(self):
        text = 'radiobutton variable = ' + self.radio_var.get()
        self.status_line.configure(text=text)
        
    def status_radio2(self, value):
        text = 'Pmw check buttons: ' + value
        self.status_line.configure(text=text)
        
    def status_radio3(self, button_name, pressed):
        if pressed: action = 'pressed'
        else:       action = 'released'
        text = 'Pmw radio button ' + button_name + ' was ' + \
               action + '; pressed buttons: ' + \
               str(self.radio3.getcurselection())
        self.status_line.configure(text=text)

    def status_option(self, value):
        self.status_line.configure(text='option menu = ' + value)
        # or, since self.option_var is tied to the option menu,
        # ...configure(text='option menu ' + self.option_var)

        
class TkinterPmwDemo:
    def __init__(self, parent):
        self.master = parent
        self.balloon = Pmw.Balloon(self.master)  # used for all balloon helps

        # write messages about window actions in a common status label:
        frame = Frame(self.master)
        # pack frame with status label at the bottom:
        frame.pack(side='bottom', anchor='w', fill='x', expand=True)
        #Label(frame, text='Widget action response: ', 
        #      font='helvetica 8', anchor='w').pack(side='left')
        self.status_line = Label(frame, relief='groove', #relief='sunken',
                                 font='helvetica 8', anchor='w')
        # configure text later
        self.status_line.pack(side='left', fill='x', expand=True)

        self.pulldown_menus(self.master)
        fields = InputFields(self.master, self.status_line,
                             balloon=self.balloon, scrolled=False)
        fields.pack(side='top',padx=30,pady=20)

        Button(self.master, text='Display widgets for list data',
               command=self.list_dialog, width=29).pack(pady=2)
        
        Button(self.master, text='Display the source code',
               command=self.display_code, width=29).pack(pady=2)
        
        # type q to quit:
        self.master.bind('<q>', self.quit) # self.quit needs an event argument
        
    def display_code(self):
        self.display_file(sys.argv[0], self.master)

    def pulldown_menus(self, parent):
        self.menu_bar = Pmw.MenuBar(parent,
                                    hull_relief='raised',
                                    hull_borderwidth=1,
                                    balloon=self.balloon,
                                    hotkeys=True,  # define accelerators
                                    )
        self.menu_bar.pack(fill='x')

        self.menu_bar.addmenu('File', None, tearoff=True)
        self.menu_bar.addmenuitem('File', 'command',
             statusHelp='Open a file',
             label='Open...',
             command=self.file_read)
        self.menu_bar.addmenuitem('File', 'command',
             statusHelp='Save a file',
             label='Save as...',
             command=self.file_save)
        self.menu_bar.addmenuitem('File', 'command',
             statusHelp='Exit this application',
             label='Quit',
             command=self.quit)

        self.menu_bar.addmenu('Dialogs',
             'Demonstrate various Tk/Pmw dialog boxes', # balloon help
             tearoff=True)

        self.menu_bar.addmenuitem('Dialogs', 'command',
             label='Tk confirmation dialog',
             command=self.confirmation_dialog)

        self.menu_bar.addmenuitem('Dialogs', 'command',
             label='Tk message dialog',
             command=self.Tk_message_dialog)

        self.menu_bar.addmenuitem('Dialogs', 'command',
             label='Pmw message dialog',
             command=self.Pmw_message_dialog)

        self.menu_bar.addmenuitem('Dialogs', 'command',
             label='Pmw user-defined dialog',
             command=self.userdef_dialog)

        self.menu_bar.addcascademenu('Dialogs', 'Color dialogs',
             statusHelp='Exemplify different color dialogs')

        self.menu_bar.addmenuitem('Color dialogs', 'command',
             label='Tk Color Dialog',
             command=self.tk_color_dialog)

        self.menu_bar.addmenuitem('Color dialogs', 'command',
             label='Pynche color dialog',
             command=self.pynche_color_dialog)
        
        self.menu_bar.addmenu('Demo',
             'Demonstrate various widgets and effects',
             tearoff=True)

        self.menu_bar.addmenuitem('Demo', 'command',
             label='List data',
             command=self.list_dialog)

        self.menu_bar.addmenuitem('Demo', 'command',
             label='Relief/borderwidth',
             command=self.relief_dialog)

        self.menu_bar.addmenuitem('Demo', 'command',
             label='Bitmaps',
             command=self.bitmap_dialog)

        self.menu_bar.addmenu('Help', None, side='right')

        self.menu_bar.addmenuitem('Help', 'command',
             label='Tutorial',
             command=self.tutorial)

        self.balloon_on = IntVar(); self.balloon_on.set(1)
        self.menu_bar.addmenuitem('Help', 'checkbutton',
             label='Balloon help',
             variable=self.balloon_on,
             command=self.toggle_balloon)


    def confirmation_dialog(self):
        message = 'This is a demo of a Tk conformation dialog box'
        ok = tkMessageBox.askokcancel('OK', message)
        if ok:
            self.status_line.configure(text="'Quit' was pressed")
        else:
            self.status_line.configure(text="'Cancel' was pressed")

    def Tk_message_dialog(self):
        message = 'This is a demo of a Tk message dialog box'
        answer = tkMessageBox.Message(icon='info', type='ok',
                 message=message, title='About').show()
        self.status_line.configure(text="'%s' was pressed" % answer)

    def Pmw_message_dialog(self):
        # message is typeset as a label so we need explicit newlines:
        message = """\
This is a demo of the Pmw.MessageDialog box,
which is useful for writing longer text messages
to the user."""
        Pmw.MessageDialog(self.master, title='Description',
                          buttons=('Quit',), message_text=message,
                          message_justify='left',
                          message_font='helvetica 12',
                          icon_bitmap='info',
                          # must be present if icon_bitmap is:
                          iconpos='w')  

    def userdef_dialog(self):
        self.userdef_d = Pmw.Dialog(self.master,
                          title='Programmer-Defined Dialog',
                          buttons=('Apply', 'Cancel'),
                          #defaultbutton='Apply',
                          command=self.userdef_dialog_action)

        self.userdef_d_gui = InputFields(self.userdef_d.interior(),
                                         self.status_line,
                                         self.balloon, scrolled=True)
        self.userdef_d_gui.pack()

    def userdef_dialog_action(self, result):
        # result contains the name of the button that we clicked
        if result == 'Apply':
            # example on extracting dialog variables:
            case = self.userdef_d_gui.case.get()
            # (changing variables in self.gui are reflected in
            # the self.status_line)
        else:
            text = 'you just canceled the dialog'
            self.status_line.configure(text=text)
        # does not work: self.dialog.deactivate(result)
        self.userdef_d.destroy()  # destroy dialog window
        
    def file_read(self):
        fname = tkFileDialog.Open(filetypes=[('anyfile','*')]).show()
        text = 'chosen file to open: ' + os.path.basename(fname)
        self.status_line.configure(text=text)
        # the dialog checks the validity of the filename, but
        # pressing Cancel results in an empty return string
        if fname:
            self.display_file(fname, self.master)
        
    def display_file(self, filename, parent):
        """Read file into a text widget in a _separate_ window."""
        filewindow = Toplevel(parent) # new window

        f = open(filename, 'r');  filestr = f.read();  f.close()
        # determine the number of lines and the max linewidth:
        lines = filestr.split('\n')
        nlines = len(lines)
        maxwidth = max(map(lambda line: len(line), lines))
        
        filetext = Pmw.ScrolledText(filewindow,
             borderframe=5, # a bit space around the text
             vscrollmode='dynamic', hscrollmode='dynamic',
             labelpos='n', label_text='Contents of file '+filename,
             text_width=min(80,maxwidth),
             text_height=min(50,nlines),
             text_wrap='none',  # do not break lines
             )
        filetext.pack(expand=True, fill='both')

        filetext.insert('end', filestr)

        # add a quit button:
        Button(filewindow, text='Quit',
               command=filewindow.destroy).pack(pady=10)

        # force the new window to be in focus:
        filewindow.focus_set()

    def file_save(self):
        fname = tkFileDialog.SaveAs(
                filetypes=[('temporary files','*.tmp')],
                initialfile='myfile.tmp',
                title='Save a file').show()
        text = 'chosen file to save: "' + os.path.basename(fname) + '"'
        self.status_line.configure(text=text)

    def quit(self, event=None):
        self.master.destroy()

    def tk_color_dialog(self):
        # see python src, subdirectory Lib/lib-tk
        # and the tkColorChooser.py file
        color = tkColorChooser.Chooser(
            initialcolor='gray',title='Choose background color').show()
        # or:
        # color = tkColorChooser.askcolor()

        # color[0] is now an (r,g,b) tuple and
        # color[1] is a hexadecimal number; send the latter to
        # tk_setPalette to change the background color:
        # (when Cancel is pressed, color is (None,None))
        if color[0] is not None:
            self.master.tk_setPalette(color[1])
            text = 'new background color is ' + str(color[0]) + \
                   ' (rgb) or ' + str(color[1])
            self.status_line.configure(text=text)

        
    def pynche_color_dialog(self):
        #from pynche import pyColorChooser
        #color = pyColorChooser.askcolor(parent=self.master)
        # or
        import pynche.pyColorChooser
        color = pynche.pyColorChooser.askcolor(self.master)
        try:
            self.master.tk_setPalette(color[1])
            text = 'new background color is ' + str(color[0]) + \
                   ' (rgb) or ' + color[1]
            self.status_line.configure(text=text)
        except: pass
        
    def list_dialog(self):
        self.list_d = Pmw.Dialog(self.master,
                          title='Demo of widgets for list data',
                          buttons=('Quit',), defaultbutton='Quit')
        lists = InputLists(self.list_d.interior(), self.status_line,
                           balloon=self.balloon)
        lists.pack(side='left')

    def relief_dialog(self):
        self.relief_d = Pmw.Dialog(self.master,
                          title='Demo of relief and borderwidth',
                          buttons=('Quit',),   # (default)
                          defaultbutton='Quit')

        self.reliefs_borderwidth(self.relief_d.interior())

    def reliefs_borderwidth(self, parent):
        # use a frame to align examples on various relief values:
        frame = Frame(parent); frame.pack(side='top',pady=15)
        # will use the grid geometry manager to pack widgets in this frame

        reliefs = ('groove', 'raised', 'ridge', 'sunken', 'flat')
        row = 0
        for borderwidth in (0,2,4,6):
            label = Label(frame, text='reliefs with borderwidth=%d: ' \
                          % borderwidth)
            label.grid(row=row, column=0, sticky='w', pady=5)
            for i in range(len(reliefs)):
                l = Label(frame, text=reliefs[i], relief=reliefs[i],
                          borderwidth=borderwidth)
                l.grid(row=row, column=i+1, padx=5, pady=5)
            row += 1

    def bitmap_dialog(self):
        self.bitmap_d = Pmw.Dialog(self.master,
                          title='Demo of predefined bitmaps',
                          buttons=('Quit',),
                          defaultbutton='Quit',
                          command=self.bitmap_dialog_action)

        self.bitmap_demo(self.bitmap_d.interior())

    def bitmap_demo(self, parent):
        # predefined bitmaps:
        bitmaps = ('error', 'gray25', 'gray50', 'hourglass',
                   'info', 'questhead', 'question', 'warning')
        Label(parent, text="""\
Predefined bitmaps, which can be used to
label dialogs (questions, info, etc.)""",
              foreground='red').pack()
        frame = Frame(parent); frame.pack(side='top', pady=5)
        for i in range(len(bitmaps)):  # write name of bitmaps
            Label(frame, text=bitmaps[i]).grid(row=0, column=i+1)
        for i in range(len(bitmaps)):  # insert bitmaps
            Label(frame, bitmap=bitmaps[i]).grid(row=1, column=i+1)

    def bitmap_dialog_action(self, result):
        # result contains the name of the button that we clicked
        if result == 'Quit':
            if tkMessageBox.askyesno('Yes', 'Are you sure you want to quit?'):
                self.bitmap_d.destroy()

    def tutorial(self):
        self.tutorial_d = Pmw.Dialog(self.master,
                          title='Short explanation of this application',
                          buttons=('Quit',),
                          defaultbutton='Quit')
        text = """\
This application demonstrates many of the most common widgets in
graphical user interfaces (with exception of the canvas widget).
The typical usage is to (i) launch

   %s

(ii) find the desired widget, (iii) look up in the source code to
find the basic construction statements, (iv) try this in your own
GUI, (v) look up man page or textbook information to fine tune the
widget settings. In this way, the application might act as a
example-oriented quick reference.

Try these actions to test the widgets in the application:

  1. Change one of the entry fields ("case name", "mass", etc.),
     and press return to see the status field at the bottom of
     the window being updated.
     In the source code you can see how to call a function each
     time the contents of a widget is changed. Such a function
     also demonstrate how to extract the contents of a widget.

  2. Choose an item from the option menu and watch the status line
     at the bottom of the window.

  3. Drag the slider.

  4. Push the "Display widgets for list data" button to see
     a collection of widgets for list data.

  4. Visit the File, Dialogs, and Demo pull-down menus and try out
     the various submenus. 
""" % sys.argv[0]

        # determine the number of lines and the max linewidth:
        lines = text.split('\n');  nlines = len(lines)
        maxwidth = max(map(lambda line: len(line), lines))

        help = Pmw.ScrolledText(self.tutorial_d.interior(),
             borderframe=5, # a bit space around the text
             vscrollmode='dynamic', hscrollmode='dynamic',
             labelpos='n',
             label_text='How to make use of this application',
             text_width=min(80,maxwidth), text_height=min(50,nlines),
             text_wrap='none')
        help.pack()
        help.insert('end', text)

    def toggle_balloon(self):
        if self.balloon_on.get():
            self.balloon.configure(state='both')  # on
        else:
            self.balloon.configure(state='none')  # off
            
    # this one is not active; provides just an example:
    def Tk_pulldown(self, parent):
        """
        Demonstrate how to create a menu bar with basic Tk
        components. This is a lower-level alternative to
        Pmw.MenuBar.
        """
        # pull-down menu:
        self.pulldown = Menubutton(parent, text='Pulldown Menu',
                                   relief='groove', underline=False)
        self.pulldown.pack(side='left',padx=10)
    
        # add entries in the 'Pulldown Menu' menu:
        self.pulldown_menu = Menu(self.pulldown, tearoff=True)

        # first menu entry:
        self.pulldown_menu.add_command(label='Tk Confirmation Dialog',
             command=self.confirmation_dialog, underline=0, accelerator='Alt+C')

        self.pulldown_menu.add_command(label='Tk Message Dialog',
             command=self.about_dialog, underline=0, accelerator='Alt+T')

        self.pulldown_menu.add_command(label='Pmw Message Dialog',
             command=self.message_dialog, underline=4, accelerator='Alt+M')

        self.pulldown_menu.add_command(label='Pmw User-Defined Dialog',
             command=self.userdef_dialog, underline=4, accelerator='Alt+U')

        # add cascading menu, here an entry "File Dialogs"
        # with two submenus, "Open" and "Save As":
        self.file_menu = Menu(self.pulldown_menu, tearoff=True)
        self.pulldown_menu.add_cascade(label='File Dialogs',
             menu=self.file_menu, underline=0)
        self.file_menu.add_command(label='Open',
                                   command=self.file_read)
        self.file_menu.add_command(label='Save As',
                                   command=self.file_save)

        # continue with main menu:
        self.pulldown_menu.add('separator')  # horizontal line
        self.pulldown_menu.add_command(label='Tk Color Dialog',
                                       command=self.tk_color_dialog)
        self.pulldown_menu.add_command(label='Pynche Color Dialog',
                                       command=self.pynche_color_dialog)

        # set up a pointer from the menubutton back to the menu:
        # (required for the pull-down menu to be displayed!)
        self.pulldown['menu'] = self.pulldown_menu



# could be implemented:
# bind double-click to any widget to popping up a dialogbox with
# all configure options (cf. scrolledframe demo) in EntryFields
# with apply and quit buttons (apply: run configure and watch the effect)


def create_lists():
    """Launch a GUI consisting of class InputLists only."""
    root = Tk()
    Pmw.initialise(root)
    status = Label(root) 
    widget = InputLists(root, status)
    widget.pack()
    status.pack()  # get the status line below the widgets
    root.mainloop()

def create_fields():
    """Launch a GUI consisting of class InputFields only."""
    root = Tk()
    Pmw.initialise(root)
    status = Label(root) 
    widget = InputFields(root, status)
    widget.pack()
    status.pack()  # get the status line below the widgets
    root.mainloop()

def create_all():
    """Create the complete TkinterPmwDemo."""
    root = Tk()
    Pmw.initialise(root)
    root.title('Tkinter/Pmw Demo')
    # tk_strictMotif changes the file dialog in Tk's OpenFile etc.
    #root.tk_strictMotif(1)
    #Pmw.initialise(root,fontScheme='pmw1')
    #import scitools.misc; scitools.misc.fontscheme1(root)
    widget = TkinterPmwDemo(root)
    # this widget packs itself...
    root.mainloop()
    
# create demo in root window for testing
if __name__ == '__main__':
    try:
        if sys.argv[1] == 'lists':
            create_lists()
        elif sys.argv[1] == 'fields':
            create_fields()
    except:
        create_all()

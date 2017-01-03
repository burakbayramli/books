#!/bin/sh
# execute the wish in the path \
   exec itkwish "$0" ${1+"$@"}
#  exec wish -f "$0" ${1+"$@"}
package require Iwidgets 3.0

iwidgets::scrolledframe .top -width 530 -height 720 \
   -vscrollmode dynamic -hscrollmode none
pack .top -expand 1 -fill both
set win [.top childsite]

label $win.header -text "Widget Demo" \
      -font "courier 24 bold" -foreground blue \
      -background [format "#%02x%02x%02x" 196 196 196]
pack $win.header -side top -pady 10 -ipady 10 -fill x

button $win.quit -text Quit -width 10 -command exit
pack $win.quit -pady 10

# write messages about window actions in a common status label:
label $win.status_line -width 50 -relief sunken \
      -font "helvetica 12" -anchor w
# configure text later
pack $win.status_line -side top

# use a frame to align examples on various relief values:
frame $win.frame1; pack $win.frame1 -side top -pady 15

# reliefs:
set reliefs [list groove raised ridge sunken flat]
label $win.frame1.label -text "reliefs: " -pady 5
grid $win.frame1.label -row 0 -column 0 -sticky w
for {set i 0} {$i < [llength $reliefs]} {incr i} {
    label $win.frame1.l$i -text [lindex $reliefs $i] \
          -relief [lindex $reliefs $i]
    grid $win.frame1.l$i -row 0 -column [expr $i + 1] \
         -padx 5 -pady 5
}

# borderwidth:

label $win.frame1.label2 -text "width -borderwidth 4: " -pady 5
grid $win.frame1.label2 -row 1 -column 0 -sticky w
for {set i 0} {$i < [llength $reliefs]} {incr i} {
    label $win.frame1.l2$i -text [lindex $reliefs $i] \
          -relief [lindex $reliefs $i] -borderwidth 4
    grid $win.frame1.l2$i -row 1 -column [expr $i + 1] \
         -padx 5 -pady 5
}

# predefined bitmaps:
set bitmaps [list error gray25 gray50 hourglass info \
                  questhead question warning]
label $win.label2 -text "predefined bitmaps:" -foreground red
pack $win.label2
frame $win.frame2; pack $win.frame2 -pady 5
# write name of bitmaps
for {set i 0} {$i < [llength $bitmaps]} {incr i} {
    label $win.frame2.l$i -text [lindex $bitmaps $i]
    grid $win.frame2.l$i -row 0 -column [expr $i + 1] \
         -padx 5 -pady 5
}
# insert bitmaps:
for {set i 0} {$i < [llength $bitmaps]} {incr i} {
    label $win.frame2.l2$i -bitmap [lindex $bitmaps $i]
    grid $win.frame2.l2$i -row 1 -column [expr $i + 1] \
         -padx 5 -pady 5
}

# text entry:
frame $win.frame3; pack $win.frame3 -side top -pady 10
label $win.frame3.label -text "text entry:"
pack  $win.frame3.label -side left
set entry_var "some text"
entry $win.frame3.entry -textvariable entry_var -width 10
pack  $win.frame3.entry -side left
# entry has no command option so we let updates in status line
# appear when pressing return in the field:
bind $win.frame3.entry "<Return>" read_entry

proc read_entry { } {
    global entry_var; global win
    set text "text-entry variable = $entry_var"
    $win.status_line configure -text $text
}

# slider:
set slider_var 2.0
scale $win.scale -orient horizontal -from 0 -to 4 -tickinterval 1 \
-resolution 0.1 -label "slider" -font "helvetica 12 italic" \
-length 300 -variable slider_var -command read_slider
pack $win.scale -side top -pady 4

proc read_slider { value } {
    global slider_var; global win
    set text "slider variable = $slider_var"
    # alternative: set text "slider variable = $value"
    $win.status_line configure -text $text
}

# checkbuttons:
frame $win.frame4; pack $win.frame4 -side top -pady 5
checkbutton $win.frame4.c1 -text "checkbutton 1" \
     -variable checkbutton_var1 \
     -command status_check1
pack $win.frame4.c1 -side left -padx 4

proc status_check1 { } {
    global checkbutton_var1; global win
    set text "checkbutton 1: $checkbutton_var1"
    $win.status_line configure -text $text
}

checkbutton $win.frame4.c2 -text "checkbutton 2" \
     -variable checkbutton_var2 \
     -command status_check2
pack $win.frame4.c2 -side left -padx 4

proc status_check2 { } {
    global checkbutton_var2; global win
    set text "checkbutton 2: $checkbutton_var2"
    $win.status_line configure -text $text
}

# radiobuttons:
frame $win.frame5; pack $win.frame5 -side top -pady 5
foreach radio [list radio1 radio2 radio3 radio4] {
    radiobutton $win.frame5.r$radio -text $radio \
    -variable radio_var -command status_radio \
    -value "radiobutton no. [string index $radio 5]"
    pack $win.frame5.r$radio -side left
}

proc status_radio { } {
    global radio_var; global win
    set text "radiobutton variable = $radio_var"
    $win.status_line configure -text $text
}
    
set mf [frame $win.frame6]
pack $mf -side top -pady 10
# pull-down menu:
menubutton $mf.pulldown -text "Pulldown Menu" \
  -relief groove -underline 0 -menu $mf.pulldown.menu
pack $mf.pulldown -side left -padx 10

# add entries in the "Pulldown Menu" menu:
menu $mf.pulldown.menu -tearoff 1

$mf.pulldown.menu add command \
  -label "TkMessage Dialog" -command about_dialog \
  -underline 0 -accelerator "Alt+T"

proc about_dialog { } {
    set message "This is a demo of an \"About\" dialog box"
    tk_messageBox -message $message -icon info -type ok \
        -title "About"
}

$mf.pulldown.menu add command \
  -label "\[incr Widget\] Message Dialog" \
  -command message_dialog -underline 14 -accelerator "Alt+M"

set message \
"This is a demo of the \[incr Widgets\] messagedialog box
that is useful for writing longer text messages
to the user."
iwidgets::messagedialog .descr -title "Description" \
    -bitmap info -text $message -font "helvetica 12"
# justify??

# two buttons are default: ok and cancel, only ok is
# needed and we shall name it "Quit":
.descr hide Cancel
.descr buttonconfigure OK -text "Quit"

proc message_dialog { } { .descr activate }

$mf.pulldown.menu add command \
  -label "\[incr Widget\] User-Defined Dialog" \
  -command userdef_dialog -underline 14 -accelerator "Alt+U"

iwidgets::dialog .ud2 -title "Programmer-Defined Dialog"
.ud2 hide OK; .ud2 hide Help
.ud2 buttonconfigure Apply  -command read_dialog
.ud2 buttonconfigure Cancel -command { .ud2 deactivate }
# add various other widgets in the interior:
set dwin [.ud2 childsite]; # grab the frame in the interior
iwidgets::entryfield $dwin.entry1 -labelpos w \
    -labeltext "entry 1 (any text)" \
    -textvariable d_entry1_var
iwidgets::entryfield $dwin.entry2 -labelpos w \
    -labeltext "entry 2 (real in \[0,2\])" \
    -validate real
# pack and align all entryfield widgets:
pack $dwin.entry1 $dwin.entry2 -padx 10 -pady 5
iwidgets::Labeledwidget::alignlabels $dwin.entry1 $dwin.entry2

proc userdef_dialog { } { .ud2 activate }

proc read_dialog { } {
    global dwin.entry2 d_entry1_var dwin win
    set text "entry 1: $d_entry1_var, entry 2: [$dwin.entry2 get]"
    $win.status_line configure -text $text
    .ud2 deactivate
}
    
# add cascading menu, here an entry "File Dialogs"
# with two submenus, "Open" and "Save As":

$mf.pulldown.menu add cascade -label "File Dialogs" \
    -menu $mf.pulldown.menu.file -underline 0

menu $mf.pulldown.menu.file -tearoff 1
$mf.pulldown.menu.file add command -label Open \
    -command file_read

proc file_read { } {
    set types {{{anyfile} {*}}}
    set fname [tk_getOpenFile -filetypes $types]
    set text "chosen file to open: $fname"
    global win
    $win.status_line configure -text $text

    if {[string compare $fname ""] == 0 } { return }
    if [ file isfile $fname ] {
	# load file into a string
	set FILE [open $fname r]
	set filestr [read $FILE]
    } else {
	error "the file $fname does not exist"
	return
    }

    # read file into a text widget in a _separate_ window:
    toplevel .toplevel
    iwidgets::scrolledtext .toplevel.filetext \
	-vscrollmode dynamic -hscrollmode dynamic \
        -labelpos n \
        -labeltext "Contents of file [file tail $fname]" \
        -visibleitems 80x20 -wrap none
    pack .toplevel.filetext
    .toplevel.filetext insert end $filestr

    # add a quit button:
    button .toplevel.quit -text Quit \
       -command { destroy .toplevel.filetext }
    pack .toplevel.quit -pady 10

    # force the new window to be in focus:
    focus .toplevel.filetext
}

$mf.pulldown.menu.file add command -label "Save As" \
    -command {
        set types {{{temporary files} {*.tmp}}}
        set fname [tk_getSaveFile -filetypes $types \
                   -initialfile myfile.tmp \
                   -title "Save a file"]
        set text "chosen file to save: $fname"
        global win; $win.status_line configure -text $text
    }

iwidgets::fileselectiondialog .fsd -mask "*" \
    -modality application

$mf.pulldown.menu.file add command \
    -label "\[incr Widgets\] File Selector" \
    -command { 
        if {[.fsd activate]} {
            set fname [.fsd get]
            set text "chosen file to save: $fname"
            global win; $win.status_line configure -text $text
        }
    }

$mf.pulldown.menu add separator
$mf.pulldown.menu add command \
  -label "Tk Color Dialog" -underline 14 -accelerator "Alt+U" \
  -command { set color [tk_chooseColor -initialcolor gray \
	                 -title "Choose background color"]; \
 	    if {[string compare $color ""] != 0} { \
   	        tk_setPalette background $color } \
           }

# option menu:

iwidgets::optionmenu $mf.options -labeltext "Option Menu:" \
    -labelpos w -command status_option
$mf.options insert end "Option 1" "Option 2" "Option 3"
pack $mf.options -side left -padx 10
$mf.options select "Option 3"

proc status_option { } {
    global mf win
    set text "option menu = [$mf.options get]"
    $win.status_line configure -text $text
}

# frame for left-to-right packing of listbox and combo boxes
frame $win.frame7; pack $win.frame7 -side top
# the various widgets are aligned with a common top line,
# obtained by anchor='n'

# listbox:
iwidgets::scrolledlistbox $win.frame7.list \
    -labeltext "plain listbox" -labelpos n \
    -visibleitems 12x3 -selectmode multiple \
    -vscrollmode static -hscrollmode dynamic \
    -selectioncommand status_list
pack $win.frame7.list -side left -padx 10 -anchor n
# insert items:
for {set i 0} {$i < 40} {incr i} { 
    lappend listitems [format "list item %d" [expr $i+1]]
}
foreach item $listitems {
    $win.frame7.list insert end $item
}

proc status_list { } {
    global win
    set selected_items [$win.frame7.list getcurselection]
    set selected_indices [$win.frame7.list curselection]
    set text "list itmes=$selected_items, indices=$selected_indices"
    $win.status_line configure -text $text
}

iwidgets::combobox $win.frame7.combo1 \
    -labeltext "simple combo box" -labelpos nw \
    -selectioncommand status_combobox -listheight 50 \
    -dropdown 0

pack $win.frame7.combo1 -side left -padx 10 -anchor n
foreach item $listitems {
    $win.frame7.combo1 insert list end $item
}
# default chosen entry:
#$win.frame7.combo1 insert entry end [lindex $listitems 0]

proc status_combobox { } {
    global win
    set value [$win.frame7.combo1 getcurselection]
    set text "combo box value = $value"
    $win.status_line configure -text $text
}

# combo2 is as combo1, except for -dropdown 1:
iwidgets::combobox $win.frame7.combo2 \
    -labeltext "simple combo box" -labelpos nw \
    -selectioncommand {
        set value [$win.frame7.combo2 getcurselection]
        set text "combo box value = $value"
        $win.status_line configure -text $text
    } \
    -dropdown 1
pack $win.frame7.combo2 -side left -padx 10 -anchor n
foreach item $listitems {
    $win.frame7.combo2 insert list end $item
}
# default chosen entry:
#$win.frame7.combo2 insert entry end [lindex $listitems 0]


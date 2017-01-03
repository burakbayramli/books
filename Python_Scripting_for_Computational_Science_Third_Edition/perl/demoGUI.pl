: # *-*-perl-*-*
    eval 'exec perl -w -S  $0 ${1+"$@"}' 
    if 0;  # if running under some shell

use Tk;
$main_win = MainWindow->new(-title => "Demo of Some Tk Widgets");
# by Susinthiran and H. P. Langtangen

$top = $main_win->Scrolled('Frame', -scrollbars => 'oeos');
$top->pack(-expand => 1, -fill => 'both');
#$top = $main_win->Frame();
$header = $top->Label(-text => "Demo of Tk Widgets in Perl/Tk",
		      -font => "courier 24 bold", 
		      -foreground => 'blue',
		      -background => 
		      sprintf("#%02x%02x%02x", 196, 196, 196));
$header->pack(-side => 'top', -padx => 10, 
	      -ipady => 10, -fill => 'x');

$top->Button(-text => 'Quit', -width => 10, -command => \&exit)
    ->pack(-pady => 10);


# type q to quit:
$main_win->bind( '<q>' => sub {Tk::exit()});

# write messages about window actions in a common status label:
$status_line = $top->Label(-width => 50, -relief => 'sunken',
			   -font => "helvetica 12", 
			   -anchor => 'w');
# configure text later
$status_line->pack(side => 'top');

# use a frame to align examples on various relief values:
$frame = $top->Frame(); $frame->pack(-side => 'top', 
				     -pady => 15);

# reliefs:
@reliefs = ('groove', 'raised', 'ridge', 'sunken', 'flat');
$label = $frame->Label(-text => 'reliefs: ', -pady => 5);
$label->grid(-row => 0, -column => 0, -sticky => 'w');

for ($i = 0; $i < $#reliefs+1; $i++) {
    $l = $frame->Label( -text =>"$reliefs[$i]",
			-relief =>"$reliefs[$i]");
    $l->grid(-row => 0, -column => $i+1, 
	     -padx => "5", -pady => "5"); 
}

# borderwidth:
$label = $frame->Label(-text => 'with borderwidth=4: ',
		       -pady => 5);
$label->grid(-row => 1, -column => 0, -sticky => 'w',
	     -pady => 5);
for ($i = 0; $i < $#reliefs+1; $i++) {
    $l = $frame->Label( -text =>"$reliefs[$i]", 
			-relief =>"$reliefs[$i]", 
			-borderwidth => 4);
    $l->grid(-row => 1, -column => $i+1, -padx => "5", 
	     -pady => "5"); 

}
# predefined bitmaps:
@bitmaps = ('error', 'gray25', 'gray50', 'hourglass',
	    'info', 'questhead', 'question', 'warning');

$top->Label(-text => 'predefined bitmaps:', 
	    -foreground => 'red')->pack();
$frame = $top->Frame(); $frame->pack(-side => 'top', 
				     -pady => 5);
for ($i = 0; $i < $#bitmaps+1; $i++) {
    # write name of bitmaps
    $frame->Label(-text =>"$bitmaps[$i]")->grid(-row => 0,
						-column => $i+1); 
}

for ($i = 0; $i < $#bitmaps+1; $i++) { 
    # insert bitmaps
    $frame->Label(-bitmap =>"$bitmaps[$i]")->grid(-row => 1, 
						  -column => $i+1); 
}

# text entry:
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 10);
$frame->Label(-text =>"text entry")->pack(-side => 'left'); 
$entry_var = "some text"; 
$e = $frame->Entry(-textvariable => \$entry_var , -width => 10);
$e->pack();
# Entry has no command option so we let updates in
# self.status_line appear when pressing return in the field:
$e->bind('<Return>', \&read_entry);

sub read_entry {
   my $text = "text-entry variable = $entry_var";
    $status_line->configure(-text => $text);
}

# slider:
$slider_var = 2.0;
$slider = $top->Scale(-orient => 'horizontal', 
  -from => 0, -to => 4, # range of slider
  -tickinterval => 1, # tickmarks on the slider "axis"
  -resolution => 0.1, # the steps of the counter above the slider
  -label => 'slider', # label printed above the slider
  -font => "helvetica 12 italic",
  -length => 300,     # length of slider in pixels
  -variable => \$slider_var, # value is tied to slider_var
  -command => \&read_slider); # called when changing the slider
$slider->pack(-pady => 4);

sub read_slider {
    my $text = "slider variable = $slider_var";
    $status_line->configure(-text => $text);
}
		     
# checkbuttons:
$checkbutton_var1 = 0;
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
$c1 = $frame->Checkbutton(-text => "checkbutton 1",
			  -variable => \$checkbutton_var1,
			  -command => \&status_check1);
$c1->pack(-side => 'left', -padx => 4); 

sub status_check1 {
    my $text = "checkbutton 1: $checkbutton_var1";
     $status_line->configure(-text => $text);
}

$checkbutton_var2 = 0;
$c2 = $frame->Checkbutton(-text => "checkbutton 2",
			  -variable => \$checkbutton_var2,
			  -command => \&status_check2);
$c2->pack(-side => 'left', -padx => 4); 

sub status_check2 {
    my $text = "checkbutton 2: $checkbutton_var2";
     $status_line->configure(-text => $text);
}

$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);

# radiobuttons:
$radio_var = "radio1";
foreach ('radio1', 'radio2', 'radio3', 'radio4') {
    $r = $frame->Radiobutton(-text => $_, 
			     -variable => \$radio_var,
			     -value =>"radiobutton no. $_",
			     -command => \&status_radio);
    $r->pack(-side => 'left');
}
sub status_radio {
    my $text = "radiobutton variable = $radio_var";
    $status_line->configure(-text => $text);
}

$frame = $top->Frame(); $frame->pack(-side => 'top', 
				     -pady => 10);

# pull-down menu:
$pulldown = $frame->Menubutton(-text =>"Pulldown Menu", 
			       -relief => 'groove',
			       -underline =>0);
$pulldown->pack(-side => 'left', -padx => 10);			     

# add entries in the "Pulldown Menu" menu:
$pulldown_menu = $pulldown->Menu(-tearoff => 1);

# first menu entry:
$pulldown_menu->add("command",-label => 'Tk Message Dialog',
		    -command => \&about_dialog, -underline => 0, 
		    -accelerator => "Alt+T");

sub about_dialog {
    my $message = " This is a demo of an \"About\" dialog box" ;
    my $messagebox = $top->messageBox(-icon => 'info',-message => $message,
				      -title => 'About');
}

# message box for Perl/Tk was dropped here.....is there any suitable?

$pulldown_menu->add("command",-label => 'User-Defined Dialog',
		    -command => \&userdef_dialog, -underline => 0, 
		    -accelerator => "Alt+U");

sub userdef_dialog {
    $d_entry1_var = "";
    $d_entry2_var = 1.8;
    $dialog = $top->DialogBox(-title => "Programmer-Defined Dialog",
			      -buttons => ['Apply', 'Cancel'],
			      -default_button => 'Apply',
			      -command => \&read_dialog);
    
# add various other widgets in the DialogBox:
    use Tk::LabEntry;
    $dialog->add(LabEntry,-label => "entry 1 (any text)",
		 -labelPack => [ -side => 'left' ],
		 -textvariable => \$d_entry1_var)->pack(-side => 'top',
							-anchor => 'w',
							-pady => 10,
							-padx => 10);
    $dialog->add(LabEntry,-label => "entry 2 (real in [0,2])",
		 -labelPack => [ -side => 'left' ],
		 -textvariable => \$d_entry2_var)->pack(-side => 'top',
							-anchor => 'w',
							-padx => 10);
    $dialog->Show();
}
sub read_dialog {
    # result contains the name of the button that we clicked
    my $text = "";
    my ($arg) = @_;
    if ($arg eq "Apply") {
	$text = "entry 1:  $d_entry1_var , entry 2: $d_entry2_var";
	$status_line->configure(-text => $text)}
	else {
	    
	    $text = "you just cancelled the dialog";
	    $status_line->configure(-text => $text);
	    $dialog->destroy($arg);   #destroy dialog window
	}
}
# add cascading menu, here an entry "File Dialogs"
# with two submenus, "Open" and "Save As":
$file_menu = $pulldown_menu->Menu(-tearoff => 1);
$pulldown_menu->add("cascade",-label =>"File Dialogs",
		    -menu =>$file_menu, -underline => 0);
$file_menu->add("command",-label => "Open",
		-command => \&file_read);

sub file_read {
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    my @file_type =(["any files",  '*']);   
    $fname = $frame->getOpenFile(-filetypes => \@file_type);
    use File::Basename;
    if (defined $fname) {
	my $text = "chosen file to open: " . basename($fname);
	$status_line->configure(-text => $text);
	# load file into a string:
	
	open(FILE,"<$fname") or die "$!\n"; 
	my @file = <FILE>;
	my $filestr = join("", @file);
	$status_line->configure(-text => $text);
	# could also launch an error message box

	# read file into a text widget in a _separate_ window:
	$filewindow = $main_win->Toplevel(-title => $main_win->title);
	$scrolled_widget= $filewindow->Scrolled('Text', 
			  -scrollbars => 'se')->pack(); 
	$scrolled_widget->insert('end', $filestr);
	$scrolled_widget->configure(-label =>" Contents of file $fname",
				    -height => '20', -width => '80',
				    -wrap => 'none');

	# add a quit button:
	$filewindow->Button(-text => "Quit", 
		    -command => sub {
  		    $filewindow->destroy() })->pack(-pady =>10);;
    }
}

$file_menu->add("command",-label => "Save As",
		-command => \&file_save);
sub file_save {
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    my @file_type =(["temporary files","*.tmp"]);
    $fname =$frame->getSaveFile(-filetypes => \@file_type,
				-defaultextension => ".tmp",
				-initialfile=>"myfile.tmp");
    if (defined $fname) {
	my $text = "chosen file to save: " . basename($fname);
	$status_line->configure(-text => $text);
    }
} 
$pulldown_menu->add("separator"); # horizontal line
$pulldown_menu->add("command",-label => "Tk Color Dialog",
		    -command => \&tk_color_dialog);
sub tk_color_dialog {
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    my $color = $frame->chooseColor(-initialcolor => 'gray', 
				 -title => "Choose background color");

    # $color is a hexadecimal number; we send this  to
    # tk_setPalette to change the background color:

    if (defined $color) {
	$main_win->setPalette($color);
	my  $text = "new background color is $color ";
	$status_line->configure(-text => $text);
	
    }
}

$pulldown->configure(-menu => $pulldown_menu);

# add balloon help on the "Pulldown Menu" button:
$balloon = $top->Balloon();    # used for all balloon helps
$balloon->attach($pulldown, -balloonmsg=>'Demo of dialog boxes');
 
# option menu: we have to make a label explicitlly to our option menu..
$frame->Label(-text => 'Option Menu:')->pack(-side => 'left');
$option_menu = $frame->Optionmenu(-options =>['Option 1', 'Option 2', 'Option 3'], 
				  -command => \&status_option);
$option_menu->pack(-side => 'left', -padx => 1);

sub status_option {
    my $value = shift;
    $status_line->configure(-text => "option menu = $value");
}


 # frame for left-to-right packing of listbox and combo boxes:
$frame = $top->Frame(); $frame->pack(-side => 'top');

# the various widgets are aligned with a common top line,
# obtained by anchor='n'

$list = $frame->Scrolled('Listbox', -scrollbars => 'e');
$list->pack(-side => 'left', -padx=> 10,  -anchor => 'n');
$list->configure(-height => 3, -width => 12, -selectmode => 'multiple',
		 -label => "plain listbox",-exportselection => 0);  
$list->bind('<Button-1>',\&status_list);

sub status_list {
#   my $list = shift;
    my @selected_indices = $list->curselection(); 
    my @selected_items = $list->get($selected_indices[0],$#selected_indices); 
    my $text = "list items= @selected_items, indices= @selected_indices";
    $status_line->configure(-text => $text);

}

# insert items:
@listitems = ();
for ($i = 1; $i < 41 ; $i++) {
    push(@listitems, "list item $i");
}

foreach $item (@listitems) {
    $list->insert('end', $item); # insert after end of list
}

# simple combo box with list and entry for chosen item:

$simplecombo_frame = $frame->Frame();
$simplecombo_frame->pack(-side => 'left', -anchor => 'n');
$simplecombo_label = $simplecombo_frame->Label(-text => "simple combo box");
$simplecombo_label->pack(-side => 'top',-anchor => 'n');

#$simplecombo_var ="";
$simplecombo_entry = $simplecombo_frame->
       Entry(-textvariable => \$simplecombo_var,
	     -width => 15, -font => "helvetica 12");

$simplecombo_entry->pack(-side => 'top');
$simplecombo_list = $simplecombo_frame->
       Scrolled('Listbox', -scrollbars => 'e');
$simplecombo_list->pack(-side => 'left', -padx=> 10, -anchor => 'n');
$simplecombo_list->configure(-height => 3, -width => 14);

$simplecombo_list->bind('<Button-1>', \&status_simplecombo);

sub status_simplecombo {
    my $list = shift;
    my $selected_indices = $list->curselection(); 
    my $selected_items = $list->get($selected_indices); 
    $simplecombo_var = $selected_items ;
    my $text = "combo box value = $simplecombo_var";
    $status_line->configure(-text => $text);
}

foreach $item (@listitems) {
    $simplecombo_list->insert('end', $item); # insert after end of list
}

# dropdown combo box with entry for chosen item and
# button for showing the list:

use Tk::BrowseEntry;
$combo1 = $frame->BrowseEntry(-label => "dropdown combo box",
			      -variable => \$combo1_var,
			      -browsecmd => \&status_combobox, 
			      -labelPack => [-side => 'top' ], 
			      -listwidth => 60);
$combo1->configure( -choices => \@listitems);
$combo1->pack(-side => 'top', -anchor => 'n', fill => 'both');

sub status_combobox {
    my $text = "combo box value = $combo1_var";
    $status_line->configure(-text => $text);
}

# make some empty spcace at bottom of $top.
$top->Frame(); $frame->pack(-side => 'top', -pady => 20);

MainLoop();






=pod

Here follows some of our experiences during the "translation" from
Python-Tk(Pmw)/Tcl-Tk to Perl/Tk:

In Perl/Tk ver. 8.021 or 8.22, there is no real combo box, like in
Pmw.  For dropdown combo box, we have used the BrowseEntry widget.
One option in BrowseEntry that could be useful, which is not yet
available, is to tell the widget whether it is a dropdown list or not.

To make a simple combo box, we have simply used the combination of 
Label, Entry, and Listbox, all put together in one frame.

In Perl/Tk version 8.022, the BrowseEntry widget's option
"-anchor => n" does not work. It seems to be a bug.  

The plain listbox does not seem to work as it should. There is a
problem with the method "Listbox->get()". It is supposed to return the
contents of the listbox element indicated by the first selection, or
an empty string if first refers a non-existent element.  If last is
specified, the command returns a list whose elements are all of the
listbox elements be tween first and last, inclusive.(taken from
perldoc Tk::Listbox) In addition, when deselecting the last element
from several selection, an error appears.

In user-defined dialogs, there is a problem when aligning entry1 and
entry2. Here we use LabEntry widgets in a DialogBox to make the
entries. The problem here is that a LabEntry widget is build with a
label and an entry widget, and its "length" varies with the text we
write in the LabEntry. If we use Label and Entry widgets, we will be
able to pack the Label widget to the "left" and the Entry widget to
the "right", with appropriate "padx".  Perhaps another solution to
this problem could be to make use of the grid geometry mangager
instead of pack.

=cut

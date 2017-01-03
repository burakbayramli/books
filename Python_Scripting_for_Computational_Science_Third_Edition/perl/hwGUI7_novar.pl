: # *-*-perl-*-*
  eval 'exec perl -w -S  $0 ${1+"$@"}' 
    if 0;  # if running under some shell

use Tk;
$main_win = MainWindow->new();
$top = $main_win->Frame(); 
$top->pack(-side => 'top');

# first frame consists of the Hello World text:
$hwframe = $top->Frame();
$hwframe->pack(-side => 'top');
$font = "times 18 bold";
$hwtext = $hwframe->Label(-text => "Hello, World!", -font => $font);
$hwtext->pack(-side => 'left', -pady => 20);  

# second frame consists of the sine computations:
$rframe = $top->Frame();
$rframe->pack(-side => 'top', -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');

# do not use a variable tied to the entry, use get and insert:
$r_entry = $rframe->Entry(-width => 6, -relief => 'sunken');
$r_entry->pack(-side => 'left');
$r_entry->insert('end', '1.2');  # default
$r_entry->bind('<Return>', \&comp_s);

sub comp_s { 
    $r = $r_entry->get();
    $s = sin($r); 
    $s_label->configure(-text => $s);
}

$rframe->Button(-text => " equals ", -relief => 'flat', 
		command => \&comp_s)->pack(-side => 'left');
# must store the label in a variable to access it in comp_s
# (notice that in Perl s_label does not need to be defined
# prior to comp_s in the code, as we need to do in Python)
$s_label = $rframe->Label(-width => 18);
$s_label->pack(-side => 'left', -pady => 20, -padx => 10);

# third frame consists of a quit button (we drop the
# frame and add the button straight into the toplevel widget)
$top->Button(-text => 'Goodbye, GUI World!', -command => \&exit,
	     -background => 'yellow',  -foreground => 'blue')
             ->pack(-side => 'top', -pady => 5, -fill => 'x');

$main_win->bind('<q>', \&exit);

MainLoop();






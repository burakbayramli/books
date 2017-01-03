: # *-*-perl-*-*
  eval 'exec perl -w -S  $0 ${1+"$@"}' 
    if 0;  # if running under some shell

use Tk;
$main_win = MainWindow->new();
$top = $main_win->Frame(); 
$top->pack(-side => 'top');

# create a frame to hold the first "Hello, World!" widget:
$hwframe = $top->Frame();
# the row frames are packed from top to bottom:
$hwframe->pack(-side => 'top');
# create a label inside this frame:
$font = "times 18 bold";
$hwtext = $hwframe->Label(-text => "Hello, World!", -font => $font);
# pack the label inside the frame:
$hwtext->pack(-side => 'left', -pady => 20);  
# (side does not matter)

# create a frame to hold "The sine of [number] equals [result]":
$rframe = $top->Frame();
# pack row frames from top to bottom:
$rframe->pack(-side => 'top', , -pady => 20, -padx => 10);
# create labels and textentry, packed from the left, as childred
# of the $rframe frame:
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
						
$r = 1.2;  # default
$r_entry = $rframe->Entry(-width => 6, -relief => 'sunken',
                          -textvariable => \$r);
$r_entry->pack(-side => 'left');
$r_entry->bind('<Return>', \&comp_s);
sub comp_s { $s = sin($r); }
$rframe->Label(-text => " equals ")->pack(-side => 'left');
$rframe->Label(-textvariable => \$s, 
               -width => 18)->pack(-side => 'left');

# create a quit button, but drop the frame as this is only one widget:
$top->Button(-text => 'Goodbye, GUI World!', -command => \&exit,
             -background => 'yellow',  -foreground => 'blue')
             ->pack(-side => 'top', -pady => 5, -fill => 'x');

$main_win->bind('<q>', \&exit);

MainLoop();






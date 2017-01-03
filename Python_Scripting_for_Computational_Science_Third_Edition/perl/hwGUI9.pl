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
						
$r = 1.2;  # default
$r_entry = $rframe->Entry(-width => 6, -relief => 'sunken',
                          -textvariable => \$r);
$r_entry->pack(-side => 'left');
$r_entry->bind('<Return>', \&comp_s);
sub comp_s { $s = sin($r); }
$rframe->Button(-text => " equals ", -relief => 'flat', 
                command => \&comp_s)->pack(-side => 'left');
$rframe->Label(-textvariable => \$s, 
               -width => 18)
               ->pack(-side => 'left', -pady => 20, -padx => 10);

# third frame consists of a quit button (we drop the
# frame and add the button straight into the toplevel widget)
$top->Button(-text => 'Goodbye, GUI World!', -command => \&exit,
             -background => 'yellow',  -foreground => 'blue')
             ->pack(-side => 'top', -pady => 5, -fill => 'x');

$main_win->bind('<q>', \&exit);

MainLoop();






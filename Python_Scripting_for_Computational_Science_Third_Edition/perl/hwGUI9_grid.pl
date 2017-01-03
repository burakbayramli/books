: # *-*-perl-*-*
  eval 'exec perl -w -S  $0 ${1+"$@"}' 
    if 0;  # if running under some shell

use Tk;
$main_win = MainWindow->new();
$top = $main_win->Frame(); 
$top->pack(-side => 'top');

# use grid to place widgets in 3x4 cells:
$font = "times 18 bold";
$hwtext = $top->Label(-text => "Hello, World!", -font => $font);
$hwtext->grid(-row => 0, -column => 0, -columnspan => 4, -pady => 20);

$top->Label(-text => 'The sine of ')->grid(-row => 1, -column => 0);
$r = 1.2;  # default
$r_entry = $top->Entry(-width => 6, -relief => 'sunken',
                       -textvariable => \$r);
$r_entry->grid(-row => 1, -column => 1);
$r_entry->bind('<Return>', \&comp_s);
sub comp_s { $s = sin($r); }

$top->Button(-text => " equals ", -relief => 'flat', 
             -command => \&comp_s)->grid(-row => 1, -column => 2);
$top->Label(-textvariable => \$s, -width => 18)
            ->grid(-row => 1, -column => 3);

$top->Button(-text => 'Goodbye, GUI World!', -command => \&exit,
             -background => 'yellow',  -foreground => 'blue')
             ->grid(-row => 2, -column => 0, -columnspan => 4,
                    -pady => 5, -sticky => 'ew');

$main_win->bind('<q>', \&exit);

MainLoop();

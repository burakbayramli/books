: # *-*-perl-*-*
  eval 'exec perl -w -S  $0 ${1+"$@"}' 
    if 0;  # if running under some shell

use Tk;
# create main window perl variable ($main_win) to hold all widgets:
$main_win = MainWindow->new();
$top = $main_win->Frame();      # create frame
$top->pack(-side => 'top');     # pack frame in main window

$hwtext = $top->Label(-text => "Hello, World! The sine of");
$hwtext->pack(-side => 'left');

$r = 1.2;  # default
$r_entry = $top->Entry(-width => 6, -relief => 'sunken',
                       -textvariable => \$r);
$r_entry->pack(-side => 'left');

$compute = $top->Button(-text => " equals ", -command => \&comp_s);
$compute->pack(-side => 'left');

sub comp_s { $s = sin($r); }

$s_label = $top->Label(-textvariable => \$s, -width => 18);
$s_label->pack(-side => 'left');

MainLoop();






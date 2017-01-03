: # *-*-perl-*-*
  eval 'exec perl -w -S  $0 ${1+"$@"}' 
    if 0;  # if running under some shell

use Tk;
$main_win = MainWindow->new();
$top = $main_win->Frame();      
$top->pack(-side => 'top');    
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'left');
$r = 1.2;  # default
$r_entry = $top->Entry(-width => 6, -relief => 'sunken',
                       -textvariable => \$r);
$r_entry->pack(-side => 'left');
$r_entry->bind('<Return>', \&comp_s);
sub comp_s { $s = sin($r); }
$top->Label(-text => " equals ")->pack(-side => 'left');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'left');

MainLoop();






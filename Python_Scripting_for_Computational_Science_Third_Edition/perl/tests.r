./tests.verify: test performed on 2004.08.14 


#### Test: ./tests.verify running hw.pl 1.2
Hello, World! sin(1.2)=0.932039085967226
CPU time of hw.pl: 0.0 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running datatrans1.pl .datatrans_infile tmp1file
CPU time of datatrans1.pl: 0.0 seconds on hplx30 i686, Linux



----- appending file tmp1file ------
0.1   5.36092e-01
0.2   3.12343e+00
0.3   5.87269e+00
0.4   3.12343e+00

#### Test: ./tests.verify running datatrans2.pl .datatrans_infile tmp2file
CPU time of datatrans2.pl: 0.0 seconds on hplx30 i686, Linux



----- appending file tmp2file ------
0.1   5.36092e-01
0.2   3.12343e+00
0.3   5.87269e+00
0.4   3.12343e+00

#### Test: ./tests.verify running simviz1.pl -A 5.0 -tstop 2 -case tmp4
CPU time of simviz1.pl: 0.1 seconds on hplx30 i686, Linux



----- appending file tmp4/tmp4.i ------

        1
        0.7
        5
        y
        5.0
        6.28318
        0.2
        2
        0.05


----- appending file tmp4/tmp4.gnuplot ------

set title 'tmp4: m=1 b=0.7 c=5 f(y)=y A=5.0 w=6.28318 y0=0.2 dt=0.05';
plot 'sim.dat' title 'y(t)' with lines;
set size ratio 0.3 1.5, 1.0;  
# define the postscript output format:
set term postscript eps monochrome dashed 'Times-Roman' 28;
# output file containing the plot:
set output 'tmp4.ps';
# basic plot command:
plot 'sim.dat' title 'y(t)' with lines;
# make a plot in PNG format as well:
set term png small color;
set output 'tmp4.png';
plot 'sim.dat' title 'y(t)' with lines;


----- appending file tmp4/tmp4.ps (just 30 lines) ------
%!PS-Adobe-2.0 EPSF-2.0
%%Title: tmp4.ps
%%Creator: gnuplot 3.7 patchlevel 3
%%CreationDate: Sat Aug 14 08:26:00 2004
%%DocumentFonts: (atend)
%%BoundingBox: 50 50 590 302
%%Orientation: Portrait
%%EndComments
/gnudict 256 dict def
gnudict begin
/Color false def
/Solid false def
/gnulinewidth 5.000 def
/userlinewidth gnulinewidth def
/vshift -93 def
/dl {10 mul} def
/hpt_ 31.5 def
/vpt_ 31.5 def
/hpt hpt_ def
/vpt vpt_ def
/M {moveto} bind def
/L {lineto} bind def
/R {rmoveto} bind def
/V {rlineto} bind def
/vpt2 vpt 2 mul def
/hpt2 hpt 2 mul def
/Lshow { currentpoint stroke M
  0 vshift R show } def
/Rshow { currentpoint stroke M
  dup stringwidth pop neg vshift R show } def

#### Test: ./tests.verify running swap1.pl 
superLibFunc\s*\(\s*([^,]+)\s*,\s*([^,]+)\s*\)Treating ../py/regex/.test1.c...
CPU time of swap1.pl: 0.0 seconds on hplx30 i686, Linux



----- appending file ../py/regex/.test1.c.tmp ------
#include <something.h>
void superLibFunc(float x, char* method)
{ printf("method=%s with x=%g",method,x); }

void someFunc ()
{
  /* calling up the super superLibFunc function */
  superLibFunc(x, a);  superLibFunc(ppp, qqq);
  /* other calls of superLibFunc */
  superLibFunc(method2 , method1);
  superLibFunc(method2 , 3method /* illegal name! */) ;  
  superLibFunc(method_2, _method1) ;
  /* here is a more challenging call wrt regex construction: */
  superLibFunc(super_method4 /* a special method that
                                 deserves a two-line comment... */
               , method1 /* the first method we have */ ) ;
}


#### Test: ./tests.verify running swap2.pl 
Treating ../py/regex/.test1.c...
line: void superLibFunc (char* method, float x)
    match=superLibFunc (char* method, float x)  arg1=char* method  arg2=float x
line:   superLibFunc(a,x);  superLibFunc(qqq,ppp);
    match=superLibFunc(a,x)  arg1=a  arg2=x
line:   superLibFunc ( method1, method2 );
    match=superLibFunc ( method1, method2 )  arg1=method1  arg2=method2 
line:   superLibFunc(3method /* illegal name! */, method2 ) ;  
    match=superLibFunc(3method /* illegal name! */, method2 )  arg1=3method /* illegal name! */  arg2=method2 
line:   superLibFunc(  _method1,method_2) ;
    match=superLibFunc(  _method1,method_2)  arg1=_method1  arg2=method_2
CPU time of swap2.pl: 0.0 seconds on hplx30 i686, Linux



----- appending file ../py/regex/.test1.c.tmp ------
#include <something.h>
void superLibFunc(float x, char* method)
{ printf("method=%s with x=%g",method,x); }

void someFunc ()
{
  /* calling up the super superLibFunc function */
  superLibFunc(x, a);  superLibFunc(ppp, qqq);
  /* other calls of superLibFunc */
  superLibFunc(method2 , method1);
  superLibFunc(method2 , 3method /* illegal name! */) ;  
  superLibFunc(method_2, _method1) ;
  /* here is a more challenging call wrt regex construction: */
  superLibFunc(super_method4 /* a special method that
                                 deserves a two-line comment... */
               , method1 /* the first method we have */ ) ;
}


#### Test: ./tests.verify running swap3.pl 
Treating ../py/regex/.test1.c...
CPU time of swap3.pl: 0.0 seconds on hplx30 i686, Linux



----- appending file ../py/regex/.test1.c.tmp ------
#include <something.h>
void superLibFunc(float x, char* method)
{ printf("method=%s with x=%g",method,x); }

void someFunc ()
{
  /* calling up the super superLibFunc function */
  superLibFunc(x, a);  superLibFunc(ppp, qqq);
  /* other calls of superLibFunc */
  superLibFunc(method2 , method1);
  superLibFunc(method2 , 3method /* illegal name! */) ;  
  superLibFunc(method_2, _method1) ;
  /* here is a more challenging call wrt regex construction: */
  superLibFunc(super_method4 /* a special method that
                                 deserves a two-line comment... */
               , method1 /* the first method we have */ ) ;
}


#### Test: ./tests.verify running refs.pl 
CPU time of refs.pl: 0.0 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running grep1.pl \-side refs.pl swap1.pl swap2.pl swap3.pl hw.pl grep2e.pl grep2l.pl grep2p.pl grep2x.pl grep1.pl grep2.pl grep3.pl grep4.pl datatrans1.pl datatrans2.pl demoGUI.pl debugregex.pl hwGUI9_grid.pl commontasks.pl simviz1.pl MyMod_ex.pl hwGUI7_novar.pl hwGUI1.pl hwGUI2.pl hwGUI3.pl hwGUI4.pl hwGUI5.pl hwGUI6.pl hwGUI7.pl hwGUI9.pl
demoGUI.pl: $header->pack(-side => 'top', -padx => 10, 
demoGUI.pl: $frame = $top->Frame(); $frame->pack(-side => 'top', 
demoGUI.pl: $frame = $top->Frame(); $frame->pack(-side => 'top', 
demoGUI.pl: $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 10);
demoGUI.pl: $frame->Label(-text =>"text entry")->pack(-side => 'left'); 
demoGUI.pl: $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
demoGUI.pl: $c1->pack(-side => 'left', -padx => 4); 
demoGUI.pl: $c2->pack(-side => 'left', -padx => 4); 
demoGUI.pl: $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
demoGUI.pl:     $r->pack(-side => 'left');
demoGUI.pl: $frame = $top->Frame(); $frame->pack(-side => 'top', 
demoGUI.pl: $pulldown->pack(-side => 'left', -padx => 10);			     
demoGUI.pl: 		 -labelPack => [ -side => 'left' ],
demoGUI.pl: 		 -textvariable => \$d_entry1_var)->pack(-side => 'top',
demoGUI.pl: 		 -labelPack => [ -side => 'left' ],
demoGUI.pl: 		 -textvariable => \$d_entry2_var)->pack(-side => 'top',
demoGUI.pl:     my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
demoGUI.pl:     my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
demoGUI.pl:     my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
demoGUI.pl: $frame->Label(-text => 'Option Menu:')->pack(-side => 'left');
demoGUI.pl: $option_menu->pack(-side => 'left', -padx => 1);
demoGUI.pl: $frame = $top->Frame(); $frame->pack(-side => 'top');
demoGUI.pl: $list->pack(-side => 'left', -padx=> 10,  -anchor => 'n');
demoGUI.pl: $simplecombo_frame->pack(-side => 'left', -anchor => 'n');
demoGUI.pl: $simplecombo_label->pack(-side => 'top',-anchor => 'n');
demoGUI.pl: $simplecombo_entry->pack(-side => 'top');
demoGUI.pl: $simplecombo_list->pack(-side => 'left', -padx=> 10, -anchor => 'n');
demoGUI.pl: 			      -labelPack => [-side => 'top' ], 
demoGUI.pl: $combo1->pack(-side => 'top', -anchor => 'n', fill => 'both');
demoGUI.pl: $top->Frame(); $frame->pack(-side => 'top', -pady => 20);
hwGUI9_grid.pl: $top->pack(-side => 'top');
hwGUI7_novar.pl: $top->pack(-side => 'top');
hwGUI7_novar.pl: $hwframe->pack(-side => 'top');
hwGUI7_novar.pl: $hwtext->pack(-side => 'left', -pady => 20);  
hwGUI7_novar.pl: $rframe->pack(-side => 'top', -padx => 10);
hwGUI7_novar.pl: $rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
hwGUI7_novar.pl: $r_entry->pack(-side => 'left');
hwGUI7_novar.pl: 		command => \&comp_s)->pack(-side => 'left');
hwGUI7_novar.pl: $s_label->pack(-side => 'left', -pady => 20, -padx => 10);
hwGUI7_novar.pl:              ->pack(-side => 'top', -pady => 5, -fill => 'x');
hwGUI1.pl: $top->pack(-side => 'top');     # pack frame in main window
hwGUI1.pl: $hwtext->pack(-side => 'left');
hwGUI1.pl: $r_entry->pack(-side => 'left');
hwGUI1.pl: $compute->pack(-side => 'left');
hwGUI1.pl: $s_label->pack(-side => 'left');
hwGUI2.pl: $top->pack(-side => 'top');    
hwGUI2.pl: $top->Label(-text => "Hello, World! The sine of")->pack(-side => 'left');
hwGUI2.pl: $r_entry->pack(-side => 'left');
hwGUI2.pl: $top->Label(-text => " equals ")->pack(-side => 'left');
hwGUI2.pl: $top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'left');
hwGUI3.pl: $top->pack(-side => 'top');  
hwGUI3.pl: $top->Label(-text => "Hello, World! The sine of")->pack(-side => 'left');
hwGUI3.pl: $r_entry->pack(-side => 'left');
hwGUI3.pl: $top->Label(-text => " equals ")->pack(-side => 'left');
hwGUI3.pl: $top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'left');
hwGUI4.pl: $top->pack(-side => 'top');   
hwGUI4.pl: $top->Label(-text => "Hello, World! The sine of")->pack(-side => 'top');
hwGUI4.pl: $r_entry->pack(-side => 'top');
hwGUI4.pl: $top->Label(-text => " equals ")->pack(-side => 'top');
hwGUI4.pl: $top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'top');
hwGUI5.pl: $top->pack(-side => 'top');
hwGUI5.pl: $hwframe->pack(-side => 'top');
hwGUI5.pl: $hwtext->pack(-side => 'left'); # side does not matter
hwGUI5.pl: $rframe->pack(-side => 'top');
hwGUI5.pl: $rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
hwGUI5.pl: $r_entry->pack(-side => 'left');
hwGUI5.pl: $rframe->Label(-text => " equals ")->pack(-side => 'left');
hwGUI5.pl:                -width => 18)->pack(-side => 'left');
hwGUI5.pl:              ->pack(-side => 'top');
hwGUI6.pl: $top->pack(-side => 'top');
hwGUI6.pl: $hwframe->pack(-side => 'top');
hwGUI6.pl: $hwtext->pack(-side => 'left', -pady => 20);  
hwGUI6.pl: $rframe->pack(-side => 'top', -padx => 10, pady => 20);
hwGUI6.pl: $rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
hwGUI6.pl: $r_entry->pack(-side => 'left');
hwGUI6.pl: $rframe->Label(-text => " equals ")->pack(-side => 'left');
hwGUI6.pl:                -width => 18)->pack(-side => 'left');
hwGUI6.pl:              ->pack(-side => 'top', -pady => 5);
hwGUI7.pl: $top->pack(-side => 'top');
hwGUI7.pl: $hwframe->pack(-side => 'top');
hwGUI7.pl: $hwtext->pack(-side => 'left', -pady => 20);  
hwGUI7.pl: $rframe->pack(-side => 'top', , -pady => 20, -padx => 10);
hwGUI7.pl: $rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
hwGUI7.pl: $r_entry->pack(-side => 'left');
hwGUI7.pl: $rframe->Label(-text => " equals ")->pack(-side => 'left');
hwGUI7.pl:                -width => 18)->pack(-side => 'left');
hwGUI7.pl:              ->pack(-side => 'top', -pady => 5, -fill => 'x');
hwGUI9.pl: $top->pack(-side => 'top');
hwGUI9.pl: $hwframe->pack(-side => 'top');
hwGUI9.pl: $hwtext->pack(-side => 'left', -pady => 20);  
hwGUI9.pl: $rframe->pack(-side => 'top', -padx => 10);
hwGUI9.pl: $rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
hwGUI9.pl: $r_entry->pack(-side => 'left');
hwGUI9.pl:                 command => \&comp_s)->pack(-side => 'left');
hwGUI9.pl:                ->pack(-side => 'left', -pady => 20, -padx => 10);
hwGUI9.pl:              ->pack(-side => 'top', -pady => 5, -fill => 'x');
CPU time of grep1.pl: 0.0 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running grep2.pl \-side refs.pl swap1.pl swap2.pl swap3.pl hw.pl grep2e.pl grep2l.pl grep2p.pl grep2x.pl grep1.pl grep2.pl grep3.pl grep4.pl datatrans1.pl datatrans2.pl demoGUI.pl debugregex.pl hwGUI9_grid.pl commontasks.pl simviz1.pl MyMod_ex.pl hwGUI7_novar.pl hwGUI1.pl hwGUI2.pl hwGUI3.pl hwGUI4.pl hwGUI5.pl hwGUI6.pl hwGUI7.pl hwGUI9.pl
$header->pack(-side => 'top', -padx => 10, 
$frame = $top->Frame(); $frame->pack(-side => 'top', 
$frame = $top->Frame(); $frame->pack(-side => 'top', 
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 10);
$frame->Label(-text =>"text entry")->pack(-side => 'left'); 
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
$c1->pack(-side => 'left', -padx => 4); 
$c2->pack(-side => 'left', -padx => 4); 
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    $r->pack(-side => 'left');
$frame = $top->Frame(); $frame->pack(-side => 'top', 
$pulldown->pack(-side => 'left', -padx => 10);			     
		 -labelPack => [ -side => 'left' ],
		 -textvariable => \$d_entry1_var)->pack(-side => 'top',
		 -labelPack => [ -side => 'left' ],
		 -textvariable => \$d_entry2_var)->pack(-side => 'top',
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
$frame->Label(-text => 'Option Menu:')->pack(-side => 'left');
$option_menu->pack(-side => 'left', -padx => 1);
$frame = $top->Frame(); $frame->pack(-side => 'top');
$list->pack(-side => 'left', -padx=> 10,  -anchor => 'n');
$simplecombo_frame->pack(-side => 'left', -anchor => 'n');
$simplecombo_label->pack(-side => 'top',-anchor => 'n');
$simplecombo_entry->pack(-side => 'top');
$simplecombo_list->pack(-side => 'left', -padx=> 10, -anchor => 'n');
			      -labelPack => [-side => 'top' ], 
$combo1->pack(-side => 'top', -anchor => 'n', fill => 'both');
$top->Frame(); $frame->pack(-side => 'top', -pady => 20);
$top->pack(-side => 'top');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
		command => \&comp_s)->pack(-side => 'left');
$s_label->pack(-side => 'left', -pady => 20, -padx => 10);
             ->pack(-side => 'top', -pady => 5, -fill => 'x');
$top->pack(-side => 'top');     # pack frame in main window
$hwtext->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$compute->pack(-side => 'left');
$s_label->pack(-side => 'left');
$top->pack(-side => 'top');    
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$top->Label(-text => " equals ")->pack(-side => 'left');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'left');
$top->pack(-side => 'top');  
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$top->Label(-text => " equals ")->pack(-side => 'left');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'left');
$top->pack(-side => 'top');   
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'top');
$r_entry->pack(-side => 'top');
$top->Label(-text => " equals ")->pack(-side => 'top');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'top');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left'); # side does not matter
$rframe->pack(-side => 'top');
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$rframe->Label(-text => " equals ")->pack(-side => 'left');
               -width => 18)->pack(-side => 'left');
             ->pack(-side => 'top');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', -padx => 10, pady => 20);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$rframe->Label(-text => " equals ")->pack(-side => 'left');
               -width => 18)->pack(-side => 'left');
             ->pack(-side => 'top', -pady => 5);
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', , -pady => 20, -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$rframe->Label(-text => " equals ")->pack(-side => 'left');
               -width => 18)->pack(-side => 'left');
             ->pack(-side => 'top', -pady => 5, -fill => 'x');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
                command => \&comp_s)->pack(-side => 'left');
               ->pack(-side => 'left', -pady => 20, -padx => 10);
             ->pack(-side => 'top', -pady => 5, -fill => 'x');
CPU time of grep2.pl: 0.0 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running grep2e.pl \-side refs.pl swap1.pl swap2.pl swap3.pl hw.pl grep2e.pl grep2l.pl grep2p.pl grep2x.pl grep1.pl grep2.pl grep3.pl grep4.pl datatrans1.pl datatrans2.pl demoGUI.pl debugregex.pl hwGUI9_grid.pl commontasks.pl simviz1.pl MyMod_ex.pl hwGUI7_novar.pl hwGUI1.pl hwGUI2.pl hwGUI3.pl hwGUI4.pl hwGUI5.pl hwGUI6.pl hwGUI7.pl hwGUI9.pl
$header->pack(-side => 'top', -padx => 10, 
$frame = $top->Frame(); $frame->pack(-side => 'top', 
$frame = $top->Frame(); $frame->pack(-side => 'top', 
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 10);
$frame->Label(-text =>"text entry")->pack(-side => 'left'); 
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
$c1->pack(-side => 'left', -padx => 4); 
$c2->pack(-side => 'left', -padx => 4); 
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    $r->pack(-side => 'left');
$frame = $top->Frame(); $frame->pack(-side => 'top', 
$pulldown->pack(-side => 'left', -padx => 10);			     
		 -labelPack => [ -side => 'left' ],
		 -textvariable => \$d_entry1_var)->pack(-side => 'top',
		 -labelPack => [ -side => 'left' ],
		 -textvariable => \$d_entry2_var)->pack(-side => 'top',
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
$frame->Label(-text => 'Option Menu:')->pack(-side => 'left');
$option_menu->pack(-side => 'left', -padx => 1);
$frame = $top->Frame(); $frame->pack(-side => 'top');
$list->pack(-side => 'left', -padx=> 10,  -anchor => 'n');
$simplecombo_frame->pack(-side => 'left', -anchor => 'n');
$simplecombo_label->pack(-side => 'top',-anchor => 'n');
$simplecombo_entry->pack(-side => 'top');
$simplecombo_list->pack(-side => 'left', -padx=> 10, -anchor => 'n');
			      -labelPack => [-side => 'top' ], 
$combo1->pack(-side => 'top', -anchor => 'n', fill => 'both');
$top->Frame(); $frame->pack(-side => 'top', -pady => 20);
$top->pack(-side => 'top');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
		command => \&comp_s)->pack(-side => 'left');
$s_label->pack(-side => 'left', -pady => 20, -padx => 10);
             ->pack(-side => 'top', -pady => 5, -fill => 'x');
$top->pack(-side => 'top');     # pack frame in main window
$hwtext->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$compute->pack(-side => 'left');
$s_label->pack(-side => 'left');
$top->pack(-side => 'top');    
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$top->Label(-text => " equals ")->pack(-side => 'left');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'left');
$top->pack(-side => 'top');  
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$top->Label(-text => " equals ")->pack(-side => 'left');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'left');
$top->pack(-side => 'top');   
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'top');
$r_entry->pack(-side => 'top');
$top->Label(-text => " equals ")->pack(-side => 'top');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'top');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left'); # side does not matter
$rframe->pack(-side => 'top');
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$rframe->Label(-text => " equals ")->pack(-side => 'left');
               -width => 18)->pack(-side => 'left');
             ->pack(-side => 'top');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', -padx => 10, pady => 20);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$rframe->Label(-text => " equals ")->pack(-side => 'left');
               -width => 18)->pack(-side => 'left');
             ->pack(-side => 'top', -pady => 5);
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', , -pady => 20, -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$rframe->Label(-text => " equals ")->pack(-side => 'left');
               -width => 18)->pack(-side => 'left');
             ->pack(-side => 'top', -pady => 5, -fill => 'x');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
                command => \&comp_s)->pack(-side => 'left');
               ->pack(-side => 'left', -pady => 20, -padx => 10);
             ->pack(-side => 'top', -pady => 5, -fill => 'x');
CPU time of grep2e.pl: 0.0 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running grep2l.pl \-side refs.pl swap1.pl swap2.pl swap3.pl hw.pl grep2e.pl grep2l.pl grep2p.pl grep2x.pl grep1.pl grep2.pl grep3.pl grep4.pl datatrans1.pl datatrans2.pl demoGUI.pl debugregex.pl hwGUI9_grid.pl commontasks.pl simviz1.pl MyMod_ex.pl hwGUI7_novar.pl hwGUI1.pl hwGUI2.pl hwGUI3.pl hwGUI4.pl hwGUI5.pl hwGUI6.pl hwGUI7.pl hwGUI9.pl
$header->pack(-side => 'top', -padx => 10, 
$frame = $top->Frame(); $frame->pack(-side => 'top', 
$frame = $top->Frame(); $frame->pack(-side => 'top', 
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 10);
$frame->Label(-text =>"text entry")->pack(-side => 'left'); 
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
$c1->pack(-side => 'left', -padx => 4); 
$c2->pack(-side => 'left', -padx => 4); 
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    $r->pack(-side => 'left');
$frame = $top->Frame(); $frame->pack(-side => 'top', 
$pulldown->pack(-side => 'left', -padx => 10);			     
		 -labelPack => [ -side => 'left' ],
		 -textvariable => \$d_entry1_var)->pack(-side => 'top',
		 -labelPack => [ -side => 'left' ],
		 -textvariable => \$d_entry2_var)->pack(-side => 'top',
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
$frame->Label(-text => 'Option Menu:')->pack(-side => 'left');
$option_menu->pack(-side => 'left', -padx => 1);
$frame = $top->Frame(); $frame->pack(-side => 'top');
$list->pack(-side => 'left', -padx=> 10,  -anchor => 'n');
$simplecombo_frame->pack(-side => 'left', -anchor => 'n');
$simplecombo_label->pack(-side => 'top',-anchor => 'n');
$simplecombo_entry->pack(-side => 'top');
$simplecombo_list->pack(-side => 'left', -padx=> 10, -anchor => 'n');
			      -labelPack => [-side => 'top' ], 
$combo1->pack(-side => 'top', -anchor => 'n', fill => 'both');
$top->Frame(); $frame->pack(-side => 'top', -pady => 20);
$top->pack(-side => 'top');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
		command => \&comp_s)->pack(-side => 'left');
$s_label->pack(-side => 'left', -pady => 20, -padx => 10);
             ->pack(-side => 'top', -pady => 5, -fill => 'x');
$top->pack(-side => 'top');     # pack frame in main window
$hwtext->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$compute->pack(-side => 'left');
$s_label->pack(-side => 'left');
$top->pack(-side => 'top');    
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$top->Label(-text => " equals ")->pack(-side => 'left');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'left');
$top->pack(-side => 'top');  
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$top->Label(-text => " equals ")->pack(-side => 'left');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'left');
$top->pack(-side => 'top');   
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'top');
$r_entry->pack(-side => 'top');
$top->Label(-text => " equals ")->pack(-side => 'top');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'top');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left'); # side does not matter
$rframe->pack(-side => 'top');
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$rframe->Label(-text => " equals ")->pack(-side => 'left');
               -width => 18)->pack(-side => 'left');
             ->pack(-side => 'top');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', -padx => 10, pady => 20);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$rframe->Label(-text => " equals ")->pack(-side => 'left');
               -width => 18)->pack(-side => 'left');
             ->pack(-side => 'top', -pady => 5);
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', , -pady => 20, -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$rframe->Label(-text => " equals ")->pack(-side => 'left');
               -width => 18)->pack(-side => 'left');
             ->pack(-side => 'top', -pady => 5, -fill => 'x');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
                command => \&comp_s)->pack(-side => 'left');
               ->pack(-side => 'left', -pady => 20, -padx => 10);
             ->pack(-side => 'top', -pady => 5, -fill => 'x');
CPU time of grep2l.pl: 0.0 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running grep2p.pl \-side refs.pl swap1.pl swap2.pl swap3.pl hw.pl grep2e.pl grep2l.pl grep2p.pl grep2x.pl grep1.pl grep2.pl grep3.pl grep4.pl datatrans1.pl datatrans2.pl demoGUI.pl debugregex.pl hwGUI9_grid.pl commontasks.pl simviz1.pl MyMod_ex.pl hwGUI7_novar.pl hwGUI1.pl hwGUI2.pl hwGUI3.pl hwGUI4.pl hwGUI5.pl hwGUI6.pl hwGUI7.pl hwGUI9.pl
$header->pack(-side => 'top', -padx => 10, 
$frame = $top->Frame(); $frame->pack(-side => 'top', 
$frame = $top->Frame(); $frame->pack(-side => 'top', 
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 10);
$frame->Label(-text =>"text entry")->pack(-side => 'left'); 
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
$c1->pack(-side => 'left', -padx => 4); 
$c2->pack(-side => 'left', -padx => 4); 
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    $r->pack(-side => 'left');
$frame = $top->Frame(); $frame->pack(-side => 'top', 
$pulldown->pack(-side => 'left', -padx => 10);			     
		 -labelPack => [ -side => 'left' ],
		 -textvariable => \$d_entry1_var)->pack(-side => 'top',
		 -labelPack => [ -side => 'left' ],
		 -textvariable => \$d_entry2_var)->pack(-side => 'top',
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
$frame->Label(-text => 'Option Menu:')->pack(-side => 'left');
$option_menu->pack(-side => 'left', -padx => 1);
$frame = $top->Frame(); $frame->pack(-side => 'top');
$list->pack(-side => 'left', -padx=> 10,  -anchor => 'n');
$simplecombo_frame->pack(-side => 'left', -anchor => 'n');
$simplecombo_label->pack(-side => 'top',-anchor => 'n');
$simplecombo_entry->pack(-side => 'top');
$simplecombo_list->pack(-side => 'left', -padx=> 10, -anchor => 'n');
			      -labelPack => [-side => 'top' ], 
$combo1->pack(-side => 'top', -anchor => 'n', fill => 'both');
$top->Frame(); $frame->pack(-side => 'top', -pady => 20);
$top->pack(-side => 'top');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
		command => \&comp_s)->pack(-side => 'left');
$s_label->pack(-side => 'left', -pady => 20, -padx => 10);
             ->pack(-side => 'top', -pady => 5, -fill => 'x');
$top->pack(-side => 'top');     # pack frame in main window
$hwtext->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$compute->pack(-side => 'left');
$s_label->pack(-side => 'left');
$top->pack(-side => 'top');    
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$top->Label(-text => " equals ")->pack(-side => 'left');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'left');
$top->pack(-side => 'top');  
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$top->Label(-text => " equals ")->pack(-side => 'left');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'left');
$top->pack(-side => 'top');   
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'top');
$r_entry->pack(-side => 'top');
$top->Label(-text => " equals ")->pack(-side => 'top');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'top');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left'); # side does not matter
$rframe->pack(-side => 'top');
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$rframe->Label(-text => " equals ")->pack(-side => 'left');
               -width => 18)->pack(-side => 'left');
             ->pack(-side => 'top');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', -padx => 10, pady => 20);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$rframe->Label(-text => " equals ")->pack(-side => 'left');
               -width => 18)->pack(-side => 'left');
             ->pack(-side => 'top', -pady => 5);
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', , -pady => 20, -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$rframe->Label(-text => " equals ")->pack(-side => 'left');
               -width => 18)->pack(-side => 'left');
             ->pack(-side => 'top', -pady => 5, -fill => 'x');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
                command => \&comp_s)->pack(-side => 'left');
               ->pack(-side => 'left', -pady => 20, -padx => 10);
             ->pack(-side => 'top', -pady => 5, -fill => 'x');
CPU time of grep2p.pl: 0.0 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running grep2x.pl \-side refs.pl swap1.pl swap2.pl swap3.pl hw.pl grep2e.pl grep2l.pl grep2p.pl grep2x.pl grep1.pl grep2.pl grep3.pl grep4.pl datatrans1.pl datatrans2.pl demoGUI.pl debugregex.pl hwGUI9_grid.pl commontasks.pl simviz1.pl MyMod_ex.pl hwGUI7_novar.pl hwGUI1.pl hwGUI2.pl hwGUI3.pl hwGUI4.pl hwGUI5.pl hwGUI6.pl hwGUI7.pl hwGUI9.pl
$header->pack(-side => 'top', -padx => 10, 
$frame = $top->Frame(); $frame->pack(-side => 'top', 
$frame = $top->Frame(); $frame->pack(-side => 'top', 
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 10);
$frame->Label(-text =>"text entry")->pack(-side => 'left'); 
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
$c1->pack(-side => 'left', -padx => 4); 
$c2->pack(-side => 'left', -padx => 4); 
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    $r->pack(-side => 'left');
$frame = $top->Frame(); $frame->pack(-side => 'top', 
$pulldown->pack(-side => 'left', -padx => 10);			     
		 -labelPack => [ -side => 'left' ],
		 -textvariable => \$d_entry1_var)->pack(-side => 'top',
		 -labelPack => [ -side => 'left' ],
		 -textvariable => \$d_entry2_var)->pack(-side => 'top',
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
$frame->Label(-text => 'Option Menu:')->pack(-side => 'left');
$option_menu->pack(-side => 'left', -padx => 1);
$frame = $top->Frame(); $frame->pack(-side => 'top');
$list->pack(-side => 'left', -padx=> 10,  -anchor => 'n');
$simplecombo_frame->pack(-side => 'left', -anchor => 'n');
$simplecombo_label->pack(-side => 'top',-anchor => 'n');
$simplecombo_entry->pack(-side => 'top');
$simplecombo_list->pack(-side => 'left', -padx=> 10, -anchor => 'n');
			      -labelPack => [-side => 'top' ], 
$combo1->pack(-side => 'top', -anchor => 'n', fill => 'both');
$top->Frame(); $frame->pack(-side => 'top', -pady => 20);
$top->pack(-side => 'top');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
		command => \&comp_s)->pack(-side => 'left');
$s_label->pack(-side => 'left', -pady => 20, -padx => 10);
             ->pack(-side => 'top', -pady => 5, -fill => 'x');
$top->pack(-side => 'top');     # pack frame in main window
$hwtext->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$compute->pack(-side => 'left');
$s_label->pack(-side => 'left');
$top->pack(-side => 'top');    
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$top->Label(-text => " equals ")->pack(-side => 'left');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'left');
$top->pack(-side => 'top');  
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$top->Label(-text => " equals ")->pack(-side => 'left');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'left');
$top->pack(-side => 'top');   
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'top');
$r_entry->pack(-side => 'top');
$top->Label(-text => " equals ")->pack(-side => 'top');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'top');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left'); # side does not matter
$rframe->pack(-side => 'top');
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$rframe->Label(-text => " equals ")->pack(-side => 'left');
               -width => 18)->pack(-side => 'left');
             ->pack(-side => 'top');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', -padx => 10, pady => 20);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$rframe->Label(-text => " equals ")->pack(-side => 'left');
               -width => 18)->pack(-side => 'left');
             ->pack(-side => 'top', -pady => 5);
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', , -pady => 20, -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$rframe->Label(-text => " equals ")->pack(-side => 'left');
               -width => 18)->pack(-side => 'left');
             ->pack(-side => 'top', -pady => 5, -fill => 'x');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
                command => \&comp_s)->pack(-side => 'left');
               ->pack(-side => 'left', -pady => 20, -padx => 10);
             ->pack(-side => 'top', -pady => 5, -fill => 'x');
CPU time of grep2x.pl: 0.0 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running grep3.pl \-side refs.pl swap1.pl swap2.pl swap3.pl hw.pl grep2e.pl grep2l.pl grep2p.pl grep2x.pl grep1.pl grep2.pl grep3.pl grep4.pl datatrans1.pl datatrans2.pl demoGUI.pl debugregex.pl hwGUI9_grid.pl commontasks.pl simviz1.pl MyMod_ex.pl hwGUI7_novar.pl hwGUI1.pl hwGUI2.pl hwGUI3.pl hwGUI4.pl hwGUI5.pl hwGUI6.pl hwGUI7.pl hwGUI9.pl
refs.pl: swap1.pl: swap2.pl: swap3.pl: hw.pl: grep2e.pl: grep2l.pl: grep2p.pl: grep2x.pl: grep1.pl: grep2.pl: grep3.pl: grep4.pl: datatrans1.pl: datatrans2.pl: demoGUI.pl: $header->pack(-side => 'top', -padx => 10, 
 $frame = $top->Frame(); $frame->pack(-side => 'top', 
 $frame = $top->Frame(); $frame->pack(-side => 'top', 
 $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 10);
 $frame->Label(-text =>"text entry")->pack(-side => 'left'); 
 $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
 $c1->pack(-side => 'left', -padx => 4); 
 $c2->pack(-side => 'left', -padx => 4); 
 $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
     $r->pack(-side => 'left');
 $frame = $top->Frame(); $frame->pack(-side => 'top', 
 $pulldown->pack(-side => 'left', -padx => 10);			     
 		 -labelPack => [ -side => 'left' ],
 		 -textvariable => \$d_entry1_var)->pack(-side => 'top',
 		 -labelPack => [ -side => 'left' ],
 		 -textvariable => \$d_entry2_var)->pack(-side => 'top',
     my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
     my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
     my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
 $frame->Label(-text => 'Option Menu:')->pack(-side => 'left');
 $option_menu->pack(-side => 'left', -padx => 1);
 $frame = $top->Frame(); $frame->pack(-side => 'top');
 $list->pack(-side => 'left', -padx=> 10,  -anchor => 'n');
 $simplecombo_frame->pack(-side => 'left', -anchor => 'n');
 $simplecombo_label->pack(-side => 'top',-anchor => 'n');
 $simplecombo_entry->pack(-side => 'top');
 $simplecombo_list->pack(-side => 'left', -padx=> 10, -anchor => 'n');
 			      -labelPack => [-side => 'top' ], 
 $combo1->pack(-side => 'top', -anchor => 'n', fill => 'both');
 $top->Frame(); $frame->pack(-side => 'top', -pady => 20);
debugregex.pl: hwGUI9_grid.pl: $top->pack(-side => 'top');
commontasks.pl: simviz1.pl: MyMod_ex.pl: hwGUI7_novar.pl: $top->pack(-side => 'top');
 $hwframe->pack(-side => 'top');
 $hwtext->pack(-side => 'left', -pady => 20);  
 $rframe->pack(-side => 'top', -padx => 10);
 $rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
 $r_entry->pack(-side => 'left');
 		command => \&comp_s)->pack(-side => 'left');
 $s_label->pack(-side => 'left', -pady => 20, -padx => 10);
              ->pack(-side => 'top', -pady => 5, -fill => 'x');
hwGUI1.pl: $top->pack(-side => 'top');     # pack frame in main window
 $hwtext->pack(-side => 'left');
 $r_entry->pack(-side => 'left');
 $compute->pack(-side => 'left');
 $s_label->pack(-side => 'left');
hwGUI2.pl: $top->pack(-side => 'top');    
 $top->Label(-text => "Hello, World! The sine of")->pack(-side => 'left');
 $r_entry->pack(-side => 'left');
 $top->Label(-text => " equals ")->pack(-side => 'left');
 $top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'left');
hwGUI3.pl: $top->pack(-side => 'top');  
 $top->Label(-text => "Hello, World! The sine of")->pack(-side => 'left');
 $r_entry->pack(-side => 'left');
 $top->Label(-text => " equals ")->pack(-side => 'left');
 $top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'left');
hwGUI4.pl: $top->pack(-side => 'top');   
 $top->Label(-text => "Hello, World! The sine of")->pack(-side => 'top');
 $r_entry->pack(-side => 'top');
 $top->Label(-text => " equals ")->pack(-side => 'top');
 $top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'top');
hwGUI5.pl: $top->pack(-side => 'top');
 $hwframe->pack(-side => 'top');
 $hwtext->pack(-side => 'left'); # side does not matter
 $rframe->pack(-side => 'top');
 $rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
 $r_entry->pack(-side => 'left');
 $rframe->Label(-text => " equals ")->pack(-side => 'left');
                -width => 18)->pack(-side => 'left');
              ->pack(-side => 'top');
hwGUI6.pl: $top->pack(-side => 'top');
 $hwframe->pack(-side => 'top');
 $hwtext->pack(-side => 'left', -pady => 20);  
 $rframe->pack(-side => 'top', -padx => 10, pady => 20);
 $rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
 $r_entry->pack(-side => 'left');
 $rframe->Label(-text => " equals ")->pack(-side => 'left');
                -width => 18)->pack(-side => 'left');
              ->pack(-side => 'top', -pady => 5);
hwGUI7.pl: $top->pack(-side => 'top');
 $hwframe->pack(-side => 'top');
 $hwtext->pack(-side => 'left', -pady => 20);  
 $rframe->pack(-side => 'top', , -pady => 20, -padx => 10);
 $rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
 $r_entry->pack(-side => 'left');
 $rframe->Label(-text => " equals ")->pack(-side => 'left');
                -width => 18)->pack(-side => 'left');
              ->pack(-side => 'top', -pady => 5, -fill => 'x');
hwGUI9.pl: $top->pack(-side => 'top');
 $hwframe->pack(-side => 'top');
 $hwtext->pack(-side => 'left', -pady => 20);  
 $rframe->pack(-side => 'top', -padx => 10);
 $rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
 $r_entry->pack(-side => 'left');
                 command => \&comp_s)->pack(-side => 'left');
                ->pack(-side => 'left', -pady => 20, -padx => 10);
              ->pack(-side => 'top', -pady => 5, -fill => 'x');
CPU time of grep3.pl: 0.0 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running grep4.pl \-side refs.pl swap1.pl swap2.pl swap3.pl hw.pl grep2e.pl grep2l.pl grep2p.pl grep2x.pl grep1.pl grep2.pl grep3.pl grep4.pl datatrans1.pl datatrans2.pl demoGUI.pl debugregex.pl hwGUI9_grid.pl commontasks.pl simviz1.pl MyMod_ex.pl hwGUI7_novar.pl hwGUI1.pl hwGUI2.pl hwGUI3.pl hwGUI4.pl hwGUI5.pl hwGUI6.pl hwGUI7.pl hwGUI9.pl
$header->pack(-side => 'top', -padx => 10, 
$frame = $top->Frame(); $frame->pack(-side => 'top', 
$frame = $top->Frame(); $frame->pack(-side => 'top', 
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 10);
$frame->Label(-text =>"text entry")->pack(-side => 'left'); 
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
$c1->pack(-side => 'left', -padx => 4); 
$c2->pack(-side => 'left', -padx => 4); 
$frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    $r->pack(-side => 'left');
$frame = $top->Frame(); $frame->pack(-side => 'top', 
$pulldown->pack(-side => 'left', -padx => 10);			     
		 -labelPack => [ -side => 'left' ],
		 -textvariable => \$d_entry1_var)->pack(-side => 'top',
		 -labelPack => [ -side => 'left' ],
		 -textvariable => \$d_entry2_var)->pack(-side => 'top',
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
    my $frame = $top->Frame(); $frame->pack(-side => 'top', -pady => 5);
$frame->Label(-text => 'Option Menu:')->pack(-side => 'left');
$option_menu->pack(-side => 'left', -padx => 1);
$frame = $top->Frame(); $frame->pack(-side => 'top');
$list->pack(-side => 'left', -padx=> 10,  -anchor => 'n');
$simplecombo_frame->pack(-side => 'left', -anchor => 'n');
$simplecombo_label->pack(-side => 'top',-anchor => 'n');
$simplecombo_entry->pack(-side => 'top');
$simplecombo_list->pack(-side => 'left', -padx=> 10, -anchor => 'n');
			      -labelPack => [-side => 'top' ], 
$combo1->pack(-side => 'top', -anchor => 'n', fill => 'both');
$top->Frame(); $frame->pack(-side => 'top', -pady => 20);
$top->pack(-side => 'top');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
		command => \&comp_s)->pack(-side => 'left');
$s_label->pack(-side => 'left', -pady => 20, -padx => 10);
             ->pack(-side => 'top', -pady => 5, -fill => 'x');
$top->pack(-side => 'top');     # pack frame in main window
$hwtext->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$compute->pack(-side => 'left');
$s_label->pack(-side => 'left');
$top->pack(-side => 'top');    
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$top->Label(-text => " equals ")->pack(-side => 'left');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'left');
$top->pack(-side => 'top');  
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$top->Label(-text => " equals ")->pack(-side => 'left');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'left');
$top->pack(-side => 'top');   
$top->Label(-text => "Hello, World! The sine of")->pack(-side => 'top');
$r_entry->pack(-side => 'top');
$top->Label(-text => " equals ")->pack(-side => 'top');
$top->Label(-textvariable => \$s, -width => 18)->pack(-side => 'top');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left'); # side does not matter
$rframe->pack(-side => 'top');
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$rframe->Label(-text => " equals ")->pack(-side => 'left');
               -width => 18)->pack(-side => 'left');
             ->pack(-side => 'top');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', -padx => 10, pady => 20);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$rframe->Label(-text => " equals ")->pack(-side => 'left');
               -width => 18)->pack(-side => 'left');
             ->pack(-side => 'top', -pady => 5);
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', , -pady => 20, -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
$rframe->Label(-text => " equals ")->pack(-side => 'left');
               -width => 18)->pack(-side => 'left');
             ->pack(-side => 'top', -pady => 5, -fill => 'x');
$top->pack(-side => 'top');
$hwframe->pack(-side => 'top');
$hwtext->pack(-side => 'left', -pady => 20);  
$rframe->pack(-side => 'top', -padx => 10);
$rframe->Label(-text => 'The sine of ')->pack(-side => 'left');
$r_entry->pack(-side => 'left');
                command => \&comp_s)->pack(-side => 'left');
               ->pack(-side => 'left', -pady => 20, -padx => 10);
             ->pack(-side => 'top', -pady => 5, -fill => 'x');
CPU time of grep4.pl: 0.0 seconds on hplx30 i686, Linux


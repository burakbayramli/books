: # *-*-perl-*-*
  eval 'exec perl -S  $0 ${1+"$@"}' 
    if 0;  # if running under some shell

$infilename = ".myprog.cpp";
open(INFILE,"<$infilename") # open for reading
   or die "Cannot read file $infilename; $!\n";
@inlines = <INFILE>;          # load file into a list of lines
# alternative reading, line by line:
while (defined($line = <INFILE>)) {
  # process $line
}
close(INFILE);

$outfilename = ".myprog2.cpp";
open(OUTFILE,">$outfilename")  # open for writing
   or die "Cannot write to file $outfilename; $!\n";
$line_no = 0;  # count the line number in the output file
foreach $line (@inlines) {
  $line_no++;
  print OUTFILE "$line_no: $line"
}
close(OUTFILE);

$filename = $outfilename;
open(OUTFILE,">>$filename");  # open for appending
# print multiple lines at once, using a ``here document'',
# e.g., embedded with Perl variables:
print OUTFILE <<EOF;
/*
  This file, "$outfilename", is a version
  of "$infilename" where each line is numbered.
*/
EOF

# equivalent output using a string instead:
print OUTFILE \
"/*
  This file, \"$outfilename\", is a version
  of \"$infilename\" where each line is numbered.
*/";

close(OUTFILE);

$w = 'World';
$s1 = "Hello, $w!";  # becomes Hello, World!

# single quotes preserve $, @ and other special Perl characters:
$s2 = 'Hello, $w!';  # becomes Hello, $w!
$s3 = "
   multi-
   line strings
are also possible";
print "String test:\ns1=$s1\ns2=$s2\ns3=$s3\n";

#------------------------------------------------------------
open(PIPE, "| sh");
print PIPE "echo Hello Pipe World";
close(PIPE);

#------------------------------------------------------------
$myarg1 = "myfile.1";

# make a list of words:
@arglist = ("$myarg1","displacement","tmp.ps");

# extract elements from a list:
($filename,$plottitle,$psfile) = @arglist;

# push an item onto a list:
$myvar2 = "myvar2";
push(@arglist, $myvar2);

# traverse a list:
foreach $entry (@arglist) {
  print "entry is $entry\n";
}

print "In-place manipulation of array entries:\n";
@A = qw/ 1.2 -3.4 5.5 -9 100 /;
for ($i=0; $i<=$#A; $i++) { print "A[$i]=$A[$i]\n"; }

for ($i=0; $i<=$#A; $i++) {
    if ($A[$i] < 0.0) { $A[$i] = 0.0; }
}
# @A does not contain negative numbers
print "No negative numbers:\n";
for ($i=0; $i<=$#A; $i++) { print "A[$i]=$A[$i]\n"; }


@A = qw/ 1.2 -3.4 5.5 -9 100 /;
# this construction also works:
for $r (@A) {
    if ($r < 0.0) { $r = 0.0; }
}
print "Alternative construction (foreach):\n";
for ($i=0; $i<=$#A; $i++) { print "A[$i]=$A[$i]\n"; }

#  ----------------------------------------------------------

$line1 = "iteration 12:    eps= 1.245E-05";
@words1 = split(/\s+/, $line1); # split wrt white space \s+
# words1[0] is "iteration"
# words1[1] is "12:"
# words1[2] is "eps="
# words1[3] is "1.245E-05"
$i=0; foreach $word (@words1) 
     { printf "words[%d]=%s\n",$i,$words1[$i]; $i++; }

$newline1 = join("#", @words1);
# newline1 is "iteration#12:#eps#1.245E-05"
print "newline1 is now [$newline1]\n";

$line2 = ".myc_12\@displacement\@u(x,3.1415)\@  no upwinding";
# (@ is a special Perl character and must be quoted!)

@words2 = split(/\@/, $line2); # @ is special => quote!
# words2[0] is ".myc_12"
# words2[1] is "displacement"
# words2[2] is "u(x,3.1415)"
# words2[3] is "  no upwinding"
$i=0; foreach $word (@words2) 
     { printf "words[%d]=%s\n",$i,$words2[$i]; $i++; }
 
# --------------------------------------------------------------

# substitute Hello by Hi everywhere in a file:
$filename = "hw.pl";
open(FILE,"<$filename") or die "$0: couldn't open file; $!";
@lines = <FILE>;            # read the file into an array of lines
close(FILE);
$line = join("", @lines);   # join all lines into one string
$line =~ s/Hello/Hi/g;      # substitute
print "regex sub:\n$line";


# ---------------------------------------------

$myfile = "hw.pl";
if (-f $myfile) { print "$myfile is a plain file\n"; }
if (-d $myfile) { print "$myfile is a directory\n"; }
if (-x $myfile) { print "$myfile is executable\n"; }
if (-z $myfile) { print "$myfile is empty(zero size)\n"; }
if (-T $myfile) { print "$myfile is a text file\n"; }
if (-B $myfile) { print "$myfile is a binary file\n"; }

($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
 $atime,$mtime,$ctime,$blksize,$blocks) = stat($myfile);
print "$myfile has atime=$atime, ctime=$ctime","\nmtime=$mtime, size=$size bytes";

# -----------------------------------------------------------------

use Cwd; 
$thisdir = cwd();

$dir = "mynewdir";
if (! -d $dir) {
    mkdir($dir, 5557) or die "$0: couldn't create dir; $!\n";
    chdir($dir);
}
chdir; # move to your home directory($ENV{'HOME'})

chdir($thisdir);
print "\n\nMade $dir directory, now it is removed while standing in $thisdir\n";
rmdir($dir);

# -------------------------------------------------------
$program = "vtk";
$found = 0;
$path = $ENV{'PATH'};  # e.g. /usr/bin:/usr/local/bin:/usr/X11/bin
@paths = split(/:/, $path);
foreach $dir (@paths) {
  if (-d $dir) {
      if (-x "$dir/$program") { $found = 1; $path = $dir; last; }
  }
}
if ($found) { print "$program found in $path\n"; } 
else { print "$program: not found\n"; }

# -------------------------------------------------------

@filelist = glob("*.pl"); 
foreach $file (@filelist) { print "Perl file: $file\n"; }

# -------------------------------------------------------

$cmd = "perl -pe '' hw.pl";  system "$cmd";
# redirect into a variable:
@res = `$cmd`;
print "Output of the command \"$cmd\"; was\n@res\n";

# -------------------------------------------------------

$tmpfile = "tmp.pl";
use File::Copy;
copy("hw.pl", $tmpfile);
rename($tmpfile,"tmp2.pl");

# -------------------------------------------------------
copy("hw.pl", "tmp2.pl");
copy("hw.pl", "tmp3.pl");
unlink(<tmp?.pl>);
unlink("tmp.pl");

use File::Path;
mkpath("$ENV{'HOME'}/perl/projects/test1");
if (-d "$ENV{'HOME'}/perl/projects/test1") {
  print "\nYes, the directory \"$ENV{'HOME'}/perl/projects/test1\" exists!\n";
}
rmtree("$ENV{'HOME'}/perl");
$name = "/usr/home/hpl/scripting/perl/intro/hw.pl";
use File::Basename;
$basename = basename($name);
$dirname  = dirname($name);
print "\n     $name\nhas basename $basename and dirname $dirname\n";
# alternative:
($base, $dirname, $suffix) = fileparse($name,".pl");
print "\n     $name\nhas base $base, dirname $dirname and suffix $suffix\n";


# -------------------------------------------------------
# hashes:
%cmlargs = (
            '-tstop' => 6.0,
            '-c_in_H' => 1.2
           );
@myargv = ("-myopt", "1.2", "-c_in_H", "9.8", "-tstop", "6.1");
while (@myargv) { # run through all command-line arg
    $option = shift @myargv;
    if (defined($cmlargs{$option})) { 
	# next command-line argument is the value:
	$value = shift @myargv; $cmlargs{$option} = $value;
	print "hash test: found option $option and its value $value\n";
    } else {
        print "The option $option is not registered\n";
    }
}
print "\n\nprinting the hash structure, key by key:\n";
foreach $item (keys %cmlargs) 
  { print "cmlargs{'$item'}=$cmlargs{$item}\n"; }

# -------------------------------------------------------
use File::Find;

find(\&printsize, "$ENV{scripting}/doc");

sub printsize {
  $file = $_;     # more descriptive variable name...
  if (-f $file) { # is $file a plain file, not a directory?
      $size = (stat($file))[7];
      $size2 = -s $file;
      if ( $size2 != $size ) 
        { die "Major error\nsize=$size and size2=$size2"; }

     # size is in bytes, write out if > 500 Kb
     if ($size > 500000)  {
        # output format:  4.3Mb projects/perl/test1.out
        $Mb = sprintf("%.2fMb",$size/1000000.0);
        print "$Mb $file in $File::Find::dir\n";
     }
  }
}


# -------------------------------------------------------

$v1 = 1.1; $v2 = 5.8; $v3 = 9; $b = 1;

($avg, $min, $max) = statistics($v1, $v2, $v3, $b);
print "\n\nstatistics: avg=$avg, min=$min, max=$max\n";

sub statistics {
    # arguments are available in the array @_
    my $avg = 0;  my $n = 0; # local variables
    foreach $term (@_) { $n++; $avg += $term; }
    $avg = $avg / $n;
    my $min = $_[0]; my $max = $_[0];
    shift @_;  # swallow first arg., it's already treated
    foreach $term (@_) { 
	if ($term < $min) { $min = $term; }
	if ($term > $max) { $max = $term; }
    }
    return ($avg, $min, $max);
}

$v1 = 1.3; $v2 = "some text";

print "before swap: v1=[$v1] v2=[$v2]\n";
swap($v1, $v2); # swap the values of $v1 and $v2
print "after  swap: v1=[$v1] v2=[$v2]\n";

sub swap {
    my $tmp = $_[0];
    $_[0] = $_[1];
    $_[1] = $tmp;
}

$filename = "my.tmp";
print2file(message => "testing hash args", file => $filename);
print2file();

sub print2file {
  my %args = (message => "no message", # default
              file => "tmp.tmp",        # default
              @_);                      # assign and override
  open(FILE,">$args{file}");  
  print FILE "$args{message}\n\n"; 
  close(FILE);
}

open(FL,"<$filename"); $content = <FL>;
print "On the file $filename we found:\n  [$content]\n";
unlink($filename);



@curvelist = ('curve1', 'curve2', 'curve3');
@explanations = ('initial shape of u',
                 'initial shape of H',
                 'shape of u at time=2.5');

# send the two lists to displaylist, using references
# (\@list is a reference to @list):
print "displaylist:\n";
displaylist(list => \@curvelist, help => \@explanations);
print "end of displaylist:\n\n";

sub displaylist {
    my %args = (@_);
    # extract the two lists from the two references:
    my $list_ref = $args{'list'};  # extract reference
    my @list = @$list_ref;         # extract list from reference
    my $help_ref = $args{'help'};  # extract reference
    my @help = @$help_ref;         # extract list from reference

    my $index = 0; my $item;
    for $item (@list) {
	printf("item %d: %-20s  description: %s\n",
	       $index, $item, $help[$index]);
        $index++;
    }

    print "\nAlternative, without lots of local variables:\n";
    $index = 0;
    for $item (@{$args{'list'}}) {
	printf("item %d: %-20s  description: %s\n",
	       $index, $item, ${@{$args{'help'}}}[$index]);
        $index++;
    }
}


#----------------------------------------------------------------
use LWP::Simple;

print "\nTesting LWP::Simple:\n\n";

@lines = get("http://www.ifi.uio.no/~hpl/downloadme.dat");

if (!@lines) { print "No Internet connection\n"; }
else { print "http://www.ifi.uio.no/~hpl/downloadme.dat:\nlines=@lines\n"; }

$s = getstore("http://www.ifi.uio.no/~hpl/downloadme.dat",
    "downloadme.dat.pl");
if ($s) { print "No Internet connection\n"; }

$s = mirror("http://www.ifi.uio.no/~hpl/downloadme.dat",
    "downloadme.dat.pl");
if ($s) { print "No Internet connection\n"; }

if (-f "downloadme.dat.pl") { unlink("downloadme.dat.pl"); }

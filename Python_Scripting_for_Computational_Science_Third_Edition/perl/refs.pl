: # *-*-perl-*-*
  eval 'exec perl -S  $0 ${1+"$@"}' 
    if 0;  # if running under some shell

# references when sending more multiple arrays to a subroutine:

@curvelist = ('curve1', 'curve2', 'curve3');
@explanations = ('initial shape of u',
                 'initial shape of H',
                 'shape of u at time=2.5');

# send the two lists to displaylist, using references
# (\@list is a reference to @list):
displaylist(list => \@curvelist, help => \@explanations);

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


# references for constructing a nested, heterogeneous list:

@point1 = (0,0);
@point2 = (0.1,1.2);
@point3 = (0.3,0);
@point4 = (0.5,-1.9);
@points = (\@point1, \@point2, \@point3, \@point4);
@xy1 = (\@point3, \@point4);
@curves1 = ("u1.dat", \@points, "H1.dat", \@xy1);

use Data::Dumper;
print Dumper(@curves1);

$a = $curves1[1][1][0];
print "a=$a\n";

@curves1 = ("u1.dat", [[0,0], [0.1,1.2], [0.3,0], [0.5,-1.9]], 
            "H1.dat", \@xy1);
print "same list, more compactly defined:\n";
print Dumper(@curves1);


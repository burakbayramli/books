#!/usr/bin/perl

sub debugregex {
    my ($pattern, $str) = @_;
    $s = "does '" . $pattern . "' match '" . $str . "'?\n";

    if ($str =~ /$pattern/) {
        # obtain a list of groups (if present):
	@groups = $str =~ m/$pattern/g;

	# repeat string, but with match enclosed in square brackets:
	$match = $&;
	$str2 = $str; $str2 =~ s/$match/[$match]/g;
	$s = $s . $str2;

	if ($groups[0] == 1) {
	    # ordinary match, no groups (see perlop man page)
	} else {
	    for $group (@groups) {
		$s = $s . "\ngroup: " . $group;
	    }
	}
    } else {
        $s = $s . "No match";
    }
    return $s;
}

$teststr = "some numbers 2.3, 6.98, and 0.5 are here";
$pattern1 = "(\\d+\\.\\d+)";  # 3 groups (numbers)
$pattern2 = "^(\\w+)\\s+.*\\s+(\\w+)\$";  # 2 groups (some and here)

print debugregex($pattern1, $teststr), "\n";
print debugregex($pattern2, $teststr), "\n";

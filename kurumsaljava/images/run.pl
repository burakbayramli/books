$sessiz = "qwrtyplkjhgfdszxcvbnmçþðQWRTYPLKJHGFDSZXCVBNMÇÞÐ";
$sesli = "âýoueaöiüIOUEÂAÖÝÜ";
$tr = "âöçþýðüÂÖÇÞÐÜ";

#print tr_split("mikroiþlemci");
#print tr_split("ýslanmak");
#exit(0);

foreach $f(<chap*.tex>) {
    print "$f\n";
    open GIRDI, $f;
    open CIKTI, ">changed/$f";

    while (<GIRDI>) {
	
	if ($_ =~ /\\section/ ||
	    $_ =~ /\\chapter/ ||
	    $_ =~ /\\begin/ ||
	    $_ =~ /\\end/ ||
	    $_ =~ /\\LARGE/ ||
	    $_ =~ /\\vspace/ ||
	    $_ =~ /\\setlength/ ||
	    $_ =~ /\\footnotesize/ ||
	    $_ =~ /\\subsection/ ||
	    $_ =~ /\\caption/ ||
	    $_ =~ /\\subsubsection/ )
	{
	    print CIKTI $_;
	    next;
	}
	
	
	s/\\\$/\$/sg;
	$line = "";
	foreach $word(split(/\s|\t|\n/)) {
	    if ( $word =~ /\\versal/ )
	    {
		$line = $line . " " . $word;		
		next;
	    }
	    
	    if ($word =~ /[$tr]/) {
		## bazi kelimeleri bolmeye ugrasma (icinde TR karakter olsa bile)
		if  (
		     $word !~ /\'/ &&
		     $word !~ /,/ &&
		     $word !~ /\(/ &&
		     $word !~ /\)/ &&
		     $word !~ /\}/ &&
		     $word !~ /\{/ &&
		     $word !~ /\"/ &&
		     $word !~ /;/ &&
		     $word !~ /-/ &&
		     $word !~ /!/ &&
		     $word !~ />/ &&
		     $word !~ /\// &&
		     $word !~ /:/ &&
		     $word !~ /\?/ &&
		     $word !~ /\`\`/ &&
		     $word !~ /\`/ &&
		     $word !~ /\'\'/ &&
		     $word !~ /\./ )
		{
		    $add = tr_split($word); 
		} else {
		    $add = $word;
		}
	    } else {
		
		$add = $word;
		
	    }

	    $line = $line . " " . $add;
	}

	$line = $line . "\n";
	print CIKTI $line;	
    }
    
    close GIRDI;
    close CIKTI;
}

#
#
sub tr_split {
    my $word = $_[0];
    my $new = "";
#    print "<< $word >>";
    $DASH = "\\\\-";
    $len = length($word);
    while ($len > 3) {
#	print "--";
	if (substr($word, 0, 7) =~ /[$sesli][$sessiz][$sessiz][$sesli][$sessiz][$sessiz]/) { # ilginc
#	    print "3.1\n";
	    $new = $new . substr($word, 0, 2) ;
	    $new = $new . $DASH;
	    $word = substr($word, 2, length($word));
	}
	if (substr($word, 0, 7) =~ /[$sessiz][$sesli][$sessiz][$sesli][$sessiz][$sesli][$sessiz]/) {
#	    print "3\n";
	    $new = $new . substr($word, 0, 2) ;
	    $new = $new . $DASH;
	    $word = substr($word, 2, length($word));
	}
	if (substr($word, 0, 6) =~ /[$sessiz][$sesli][$sessiz][$sessiz][$sesli][$sessiz]/) {
#	    print "5\n";
	    $new = $new . substr($word, 0, 3);
	    $new = $new . $DASH;
	    $word = substr($word, 3, length($word));
	}
	if (substr($word, 0, 5) =~ /[$sesli][$sessiz][$sessiz][$sessiz][$sesli]/) { # ustte
#	    print "-1\n";
	    $new = $new . substr($word, 0, 3) ;
	    $new = $new . $DASH;
	    $word = substr($word, 3, length($word));			
	}	
	if (substr($word, 0, 5) =~ /[$sesli][$sessiz][$sessiz][$sesli][$sessiz]/) { #islem
#	    print "2\n";
	    $new = $new . substr($word, 0, 2) ;
	    $new = $new . $DASH;
	    $word = substr($word, 2, length($word));
	}
	if (substr($word, 0, 5) =~ /[$sessiz][$sesli][$sessiz][$sesli][$sessiz]/) {
#	    print "4\n";
	    $new = $new . substr($word, 0, 2);
	    $new = $new . $DASH;
	    $word = substr($word, 2, length($word));
	}
	if (substr($word, 0, 5) =~  /[$sessiz][$sesli][$sessiz][$sessiz][$sesli]/) { #mikro
#	    print "7\n";
	    $new = $new . substr($word, 0, 3);
	    $new = $new . $DASH;
	    $word = substr($word, 3, length($word));
	}
	if (substr($word, 0, 4) =~ /[$sessiz][$sessiz][$sesli][$sessiz]/) { 
#	    print "0\n";
	    $new = $new . substr($word, 0, 4) ;
	    $new = $new . $DASH;
	    $word = substr($word, 4, length($word));			
	}	
	if (substr($word, 0, 4) =~ /[$sessiz][$sesli][$sessiz][$sesli]/) {
#	    print "6\n";
	    $new = $new . substr($word, 0, 2) ;
	    $new = $new . $DASH;
	    $word = substr($word, 2, length($word));
	}
	if (substr($word, 0, 4) =~ /[$sesli][$sessiz][$sessiz][$sesli]/) {
#	    print "6.2\n";
	    $new = $new . substr($word, 0, 2) ;
	    $new = $new . $DASH;
	    $word = substr($word, 2, length($word));
	}
	if (substr($word, 0, 4) =~ /[$sessiz][$sesli][$sessiz][$sessiz]/) {
#	    print "6.2\n";
	    $new = $new . substr($word, 0, 4) ;
	    $new = $new . $DASH;
	    $word = substr($word, 4, length($word));
	}
	if (substr($word, 0, 4) =~ /[$sessiz][$sesli][$sesli][$sessiz]/) {
#	    print "6.2\n";
	    $new = $new . substr($word, 0, 2) ;
	    $new = $new . $DASH;
	    $word = substr($word, 2, length($word));
	}
	if (substr($word, 0, 3) =~ /[$sesli][$sessiz][$sesli]/) {
#	    print "1\n";
	    $new = $new . substr($word, 0, 1) ;
	    $new = $new . $DASH;
	    $word = substr($word, 1, length($word));			
	}
	
	$len = length($word);

    }

    $new = $new . $word;
#    print "|| $new ||";
    return $new;
}

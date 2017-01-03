#!/ifi/ganglot/k00/inf3330/www_docs/packages/SunOS/bin/perl
use CGI qw/:standard/;
use CGI::QuickForm;

show_form(
  -ACCEPT => \&on_valid_form,  # must be supplied
  -TITLE  => "Hello, Web World!",
  -FIELDS => [
              { -LABEL => 'Hello, World! The sine of ',
                -TYPE => 'textfield', -name => 'r', 
	        -default => 1.2, },
   ],
  -BUTTONS => [ {-name => 'compute'}, ], # "submit" button(s)
);

sub on_valid_form {
  my $r = param('r');
  my $s = sin($r);
  print header, $s;  # write new page with the answer
}

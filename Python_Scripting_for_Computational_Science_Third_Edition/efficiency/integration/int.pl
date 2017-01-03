: # *-*-perl-*-*
  eval 'exec perl -w -S  $0 ${1+"$@"}' 
    if 0;  # if running under some shell

sub f1 {
  my ($x) = @_;
  my $f = exp(-$x*$x)*log(1+$x*sin($x));
  return $f;
}

sub trapezoidal {
  my ($a, $b, $f, $n) = @_;
  my $h = ($b-$a)/$n;   # Perl does not use integer division
  my $s = 0;
  my $x = $a;
  for (my $i = 1; $i <= $n; $i++) {
    $x = $x + $h;
    $s = $s + &$f($x);
  }
  $s = 0.5*(&$f($a) + &$f($b)) + $s;
  return $h*$s;
}

$a = 0;
$b = 2;
$n = 1000;
for ($i = 1; $i <= 10000; $i++) {
  $result = trapezoidal($a, $b, \&f1, $n);
}
print "$result\n";



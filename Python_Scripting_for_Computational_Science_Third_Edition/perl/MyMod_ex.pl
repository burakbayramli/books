#!/usr/bin/perl

use lib "$ENV{scripting}/src/intro/perl";
use MyMod;

MyMod::set_logfile('tmp.log');
print "MyMod::logfile=$MyMod::logfile\n";
$p1 = "just some text";
MyMod::another_routine($p1, 'mytmp.tmp');

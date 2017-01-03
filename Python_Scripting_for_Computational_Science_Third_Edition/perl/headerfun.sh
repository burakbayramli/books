#!/bin/bash

function newpage () {
 echo -e "\ntype enter to proceed"
 read
}
  
printf "





--------------------------------------
Perl is more shell-ish than you think!
--------------------------------------

A story of Perl headers by
Åsmund Ødegård and Hans Petter Langtangen




"

newpage

printf "
The typical Perl header reads

#!/usr/bin/perl

which works fine if your desired Perl interpreter is found in
the directory /usr/bin. An invalid path results in an error
message. For example, the script

"

# write a short Perl script:
printf "\
#!/usr/some/local/perl
print \"Hello, World!\\\n\";
" > tmp.pl

cat tmp.pl

echo -e "\nstored in a file tmp.pl and being executed as ./tmp.pl results in\n"

chmod a+rx tmp.pl
./tmp.pl

newpage

printf "
A safe Perl header that guarantees running the first Perl
interpreter found in your path, reads

: # *-*-perl-*-*
  eval 'exec perl -w -S  \$0 \${1+\"\$@\"}' 
    if 0;  # if running under some shell
"

newpage

#The first : is actually a comment sign from the very first Unix
#shell. This sign is tells the Perl interpreter that the script is a
#Perl script, provided : is encountered on the first line; otherwise :
#is just ignored. However, when we run the script as an executable
#file (e.g. ./thisscript.pl), the script is interpreted by the shell.
#The shell recognizes : as a comment. The next line

# hpl: maybe we emphasize the : here, I don't think : is significant either in
# the shell or in perl interpretion...


# repeat the header on the next page...
printf "
: # *-*-perl-*-*
  eval 'exec perl -w -S  \$0 \${1+\"\$@\"}' 
    if 0;  # if running under some shell

The first : is actually a comment sign from the very first Unix
shell. If a colon is encountered by Perl on the first line of a
script, it is safely and simply ignored. If Perl encounters a :
somehere else, it is an error. However, when we run the script as an
executable file (e.g.  ./thisscript.pl), the script is interpreted by
the shell. In this case : is a builtin command which in (t)csh does
nothing, always with zero exit status, and in Bourne shell also does
nothing but expanding arguments. In the Bourne shell case, the exit
status is also zero. The next line

  eval 'exec perl -w -S  \$0 \${1+\"\$@\"}' 

runs the eval command in the shell, which just implies running

  exec perl -w -S  \$0 \${1+\"\$@\"}

as a shell command."

newpage

printf "
The exec function replaces the current shell by the process

  perl -w -S  \$0 \${1+\"\$@\"}

This means that we call up the first Perl interpreter found in the
path and supply the Perl options -w, for turning on warnings, and -S,
which tells Perl to take the code from the script \$0, i.e., the name
of the file we are now executing under the shell.  The Perl
interpreter meets the colon, which is just ignored. The next statement

  eval 'exec perl -w -S  \$0 \${1+\"\$@\"}' 

would run Perl's eval command, i.e., evaluate exec perl ..., but Perl
also sees the proceeding if-test, which always evaluates to
false. That is, the eval statement in Perl is never executed.  "

newpage

printf "
Now, let's have some fun with other versions of the header.  As we
said, leaving out the first : is okay,

eval 'exec perl -w -S  \$0 \${1+\"\$@\"}' 
  if 0;  # if running under some shell
print \"Hello, World!\\n\";

Running this script as ./tmp.pl results in the expected output
"

# write a short Perl script:
printf "\
eval 'exec perl -w -S  \$0 \${1+\"\$@\"}' 
  if 0;  # if running under some shell
print \"Hello, World!\\n\";
" > tmp.pl
chmod a+rx tmp.pl
./tmp.pl

newpage

printf "
Many would expect that the following header

#!/bin/sh
eval 'exec perl -w -S  \$0 \${1+\"\$@\"}' 
  if 0;  # if running under some shell

would also work. The shell uses the first line to see that the script
is to be interpreted as Bourne shell code. The eval statement then
works as explained above, Perl is started, but here is what happens
(we have used /bin/sh -x so that every line is printed prior to its
execution):

"

# write a short Perl script:
printf "\
#!/bin/sh -x
eval 'exec perl -w -S  \$0 \${1+\"\$@\"}' 
  if 0;  # if running under some shell
print \"Hello, World!\\n\";
" > tmp.pl
chmod a+rx tmp.pl
./tmp.pl
echo -e "\n\ntype enter to proceed"
read

printf "
Hey - what happens? We start Perl, but exec perl triggers an error
message from /bin/sh ???

Try this one out:

#!/bin/sh -x and then just astringwithperlinthemiddle
eval 'exec perl -w -S  \$0 \${1+\"\$@\"}' 
  if 0;  # if running under some shell
print \"Hello, World!\\n\";

Running the program as an executable file, ./tmp.pl, does not work,
but running it explicitly under Perl works:

perl tmp.pl

"
# write a short Perl script:
printf "\
#!/bin/sh -x and then astringwithperlinthemiddle
eval 'exec perl -w -S  \$0 \${1+\"\$@\"}' 
  if 0;  # if running under some shell
print \"Hello, World!\\n\";
" > tmp.pl
chmod a+rx tmp.pl
perl ./tmp.pl

echo -e "\n\ntype enter to proceed"
read

printf "
Yes, it went fine... Now, remove the first l from the first line:

#!/bin/sh -x and then astringwithperinthemiddle

and run perl tmp.pl ...
"

# write a short Perl script:
printf "\
#!/bin/sh -x and then astringwithperinthemiddle
eval 'exec perl -w -S  \$0 \${1+\"\$@\"}' 
  if 0;  # if running under some shell
print \"Hello, World!\\n\";
" > tmp.pl
chmod a+rx tmp.pl
perl ./tmp.pl

echo -e "\n\ntype enter to proceed"
read

printf "

This is really interesting. Something strange is going on... It
appears that if Perl sees the word 'perl' on the first line, it
assumes that this is a Perl script, else sh is used. Lets study the
situation in more detail!
"

newpage

printf "
From the perlrun man-page it appears that if perl encounter a
shebang-line as the first line (a line staring with \#\!), perl
executes the given interpreter instead of the perl interpreter. But if
the shebang-line includes the word perl, perl is used. From the source
code for Perl, it appears that this is a simple substring search for
either 'perl' or 'Perl', hence that word can appear anywhere on the
line.

If the word 'perl' does not appear on the shebang-line, perl acts as if
the script was started from the shell, like ./thisscript.pl. The
shebang line is searched, and the perl-process is replaced by the
given interpreter, in this case /bin/sh with -x as an option. The sh
process will just read the first line as a comment. At the next line,
the eval-exec line, the shell will try to execute perl on the current
file as before, with -w and -S as options. Again perl will see the
shebang-line, and replace itself with the given /bin/sh
interpreter. But perl is even more sophisticated; it assumes that the
options given to itself was ment for the interpreter specified at the
shebang-line in the file, so -w and -S are given to /bin/sh together
with -x. But sh don't know these options, hence sh dies with an error.

If the -w and -S options meant for perl are omitted, this script will
start sh and perl in an endless loop!  
"


#!/usr/bin/env python
"""
An article about headers in Python scripts.
By H. P. Langtangen.
"""

text = '''
===============================
About Headers in Python Scripts
===============================


This article contains optional material, for the interested reader,
about headers in Python scripts.  All the material is limited to Unix
systems.  On Windows the header line is simply treated as a comment
and the contents of the line have no impact on the execution of the
script.

Many Python books teach you to start Python scripts with a header
containing the explicit path to your desired Python interpreter, e.g.,

    #!/usr/bin/python

When running the script by typing just the name of the script, this
header is used to find the program that is supposed to interpret the
rest of the script.  In case your Python interpreter is in
/usr/local/bin instead of /usr/bin , you have to write

#!/usr/local/bin/python

Sometimes you want to use a Python interpreter that you have compiled
yourself, e.g.~a more recent version of Python than what is installed
on your computer system, and this may lead to long paths like

#!/usr/home/me/scripting/python/install/linux/bin/python

When you move hundreds of scripts from one machine to another it is
likely that the header lines must be changed, and this is of course
inconvenient.  Therefore, you often want a more portable header.

Running the script by the command python proceeded by the name of the
script file, overrides the header line (it is just treated as a
comment), and the script is interpreted by the first python program
encountered in your path. That is, Unix will go through all
directories specified in your environment variable PATH and look for a
program named python . The first program that is found is used to run
the script.  Since you are in charge of defining PATH in set-up files,
the method gives you complete control over which Python version you
want to run. For example, you can run your own Python rather than the
official one on the computer system by just putting your local
directories with executables before the system directories in the PATH
variable.

Interpreting your script by the first Python interpreter found in your
path is also enabled by just writing the name of the script, provided
that the header reads

#!/usr/bin/env python

This construction works only if env is in /usr/bin .
Fortunately, this is normally the case on Unix systems.

The following heading is the safest and most general start of a Python
script:

#!/bin/sh
""":"
exec python $0 ${1+"$@"}
"""

Let us explain this header in detail.  When writing just the name of
the script, the script is executed under a Unix shell, i.e., the
script is interpreted as a shell script.  The first line now tells the
shell to interpret the script as a Bourne shell script (the
interpreter is the program /bin/sh ).  The Bourne shell interpreter
first meets an empty string, which is ok, followed by a new string
containing a colon. This colon acts as an inactive statement (it is
actually an old comment sign from ancient Unix shells).  Any other
valid shell statement could be contained in the second string, but the
point now is to have a simple statement that does not affect the
output of the script.  The next statement met by the shell is exec ,
which replaces the shell by the proceeding command, beginning with
python . This command implies running the first Python interpreter
found in the path, followed by the name of the script (the Bourne
shell variable \$0 ).  Thereafter we have the construction
\$\{1+"\$@"\ }, which is a reliable notation for all the command-line
arguments that were supplied to the script.  The variable \$@ is the
array of all command-line arguments in Unix shells, and enclosing it
in double quotes " ensures that the arguments are correctly
transferred to Python (e.g., it ensures that 1 2 3 '4 5 6' are
transferred as four arguments, not six).  The construction
\$\{1+"\$@"\ } is basically equivalent to "\$@" , but the former
syntax guarantees that no arguments are transferred to Python in case
of no command-line arguments (the simple construct "\$@" would then
send one argument: an empty string).

When invoking Python through the  exec python  statement, the same
file is run again, but this time it is interpreted as a Python code.
The first line is a comment line, while the next three lines define a
multi-line string. These lines do not affect the script, apart from
the fact that the triple-quoted string becomes a doc string if you
import the file as a module. This header is therefore not suited for
Python modules.

Finally, we remark that running the script under the first Python
interpreter in the user's path is not always what you want as a
programmer. System commands implemented as Python scripts normally
need to run a specific Python interpreter with specific associated
libraries.  To ensure that all users run the script under the same
interpreter, it is necessary to hardcode the path in the top of the
script.
'''

# print:
import sys
lines = text.split("\n")
line = 0
page = 14  # no of lines per print
repeat = 0  # no lines to repeat
while line+repeat < len(lines):
    for l in lines[line:line+page]:
        print l
    line += page-repeat  # repeat the last lines
    print "\nType return to continue: ",
    sys.stdin.readline()

"""
This script enables a primitive form of C-style macros with substitution
for Python programs.

There are two ways of defining macros:

  - Using a directive #macrofile "filename"
    in the source code

  - Placing macro definitions in the file

Macros are defined using the syntax::

  begin macro macroname (arg1, arg2, arg3, ...)
  <source code>
  end

In the file to be preprocessed, any occurence of macroname followed
by arguments enclosed in parenthesis::

  macroname(p1, p2, p3, ...)

enforces substitution of the macroname "call" by the <source code> lines
from the macro definition, with original arguments arg1, arg2, arg3, ...
replaced by new arguments p1, p2, p3, ... The macro call must appear
on a single line.
The replacement is performed by plain text substitution,
i.e., there is no parsing of the macro (as done by the C preprocessor).
Because of the plain text substitution scheme, macros must constructed
with care, as shown in the examples below.

Suppose we in the file to be preprocessed have::

  #macrofile "mymacros.def"

and that the "mymacros.def" file defines three macros::

  begin macro mymacro(a, b, c)
      a = some(a, b, a-b)
      c = 2
  end

  begin macro update(uold, unew, ic, jc, ip1, im1, jp1, jm1)
      unew[ic,jc] = uold[ic, jc] + dt*((uold[ip1,jc] - uold[ic,jc])/dx2 \ 
                                    -  (uold[ic,jc] - uold[im1,jc])/dx2) \ 
                                 + dt*((uold[ic,jp1] - uold[ic,jc])/dy2 \ 
                                    -  (uold[ic,jc) - uold[ic,jm1])/dy2)
  end

  BEGIN MACRO trouble (u, i, j, k, im1, jm1, km1)
  for (i=1; i<nx; i++)
    for (j=1; j<nx; j++)
      for (k=1; k<nx; k++)
        up[i][j][k] = u[im1][jm1][km1]
  END

Say we have the following code segment in the file to be preprocessed::

    if test:
        mymacro(u[j], u[j-1], quadratic)  # macro!
    i = 0  # boundary
    for j in xrange(ny):
        update(x, y, i, j, i+1, i-1, j+1, j+1)  # macro!

This code segment is then expanded to::

    if test:
        u[j] = some(u[j], u[j-1], u[j]-u[j-1])
        quadratic = 2
    i = 0  # boundary
    for j in xrange(ny):
        T[i,j] = Told[i, j] + dt*((Told[i+1,j] - Told[i,j])/dx2 \ 
                                      -  (Told[i,j] - Told[i-1,j])/dx2) \ 
                                   + dt*((Told[i,j+1] - Told[i,j])/dy2 \ 
                                      -  (Told[i,j) - Told[i,j+1])/dy2)

The "trouble" macro is an example on how easily plain text
substitution can go wrong, e.g.::

    trouble(Ti, p, q, r, p-1, q-1, r-1)

is expanded to::

    for (p=1; p<nx; p++)
      for (q=1; q<nx; q++)
        for (r=1; r<nx; r++)
          Tpp[p][q][r] = Tp[pm1][qm1][rm1]

The substitution scheme works with one argument at a time: first
"u" is replaced by "Ti" (causing "up" to become "Tip"), then "i"
is replaced by "p" (causing "Tip" to become "Tpp", for instance),
"j" is replaced by "r", and so on. Careful use should, however,
make these macros a valuable tool for parameterizing code segments
and minimize code duplication.
Using upper case descriptive names as macro arguments in definitions
and calls will often be successful.

Macros to be used with Fortran can have line lengths up to 138 chars,
but lines exceeding 72 chars are split into shorter lines.
Continuation of a line in the macro source is indicated by a backslash
at the end of the line (the preprocessor turns such source into
valid Fortran 77 code with continuation characters in column 6).

A very useful companion script is preprocess by Trent Mick
(http://trentm.com/projects/preprocess), which gives functionality
like a standard C preprocessor, except for macro substitution,
for programs in many languages: C, C++, Fortran, Python, Perl, Java,
Ruby, Bash, etc. macroreplace.py requires preprocess as a module.

"""
import sys, re, os

def read_macro_file(lines):
    """
    Read a list of lines holding a file where macro definitions may
    appear. Return a dictionary whose
    keys are macro names and whose values are a list [args,source],
    where args is a list of argument names of the macro and source
    is a string holding the source of the macro.
    """
    macro_pattern = r"(begin|BEGIN)\s+(macro|MACRO)\s+([^)]+)\s*\(([^)]*)\)"
    macro_pattern = re.compile(macro_pattern)
    inside_macro = False
    macros = {}
    output_lines = []  # lines minus macro definitions
    for line in lines:
        if not inside_macro:
            # outside, check if line is a macro definition:
            match = macro_pattern.search(line)
            if match:
                inside_macro = True
                source_lines = []  # collect lines of current macro
                macro_name = match.group(3).strip()
                args = match.group(4).strip()
                if args == "":
                    args = []
                else:
                    args = [arg.strip() for arg in args.split(",")]

                if 0:
                    # check that no arg is a substring of another arg,
                    # as this will destroy the substitution process
                    # (incomplete syntax check: the source may contain
                    # constructions that destroy the substitution process)
                    for arg in args:
                        for other_arg in args:
                            if arg != other_arg:
                                if arg.find(other_arg) >= 0:
                                    raise SyntaxError, \
                    "Argument '%s' in macro %s is a substring of "\
                    "another argument '%s'. This which will destroy the "\
                    "substitution process" % (arg, macro_name, other_arg)
            else:
                # just leave the line untouched
                output_lines.append(line)
                
        else:
            # inside macro, check if it is ended:
            sline = line.strip()
            if sline == 'end' or sline == 'END':
                inside_macro = False
                source = "".join(source_lines)
                macros[macro_name] = [args, source]
                log = '*** completed macro %s:\nargs=%s\nsource:\n%s' \
                      % (macro_name, macros[macro_name][0],
                         macros[macro_name][1])
                #print log
            else:
                # ordinary line inside macro, add to source_lines:
                source_lines.append(line)
    return macros, output_lines
                
                
            
def macro_replacement(sline, macros, language):
    """
    @param sline: If a line sline in a file contains a macro, replace the
    macro "call" by the source code of the macro,
    replacing arguments in the macro definition by the
    arguments in the call.
    @param macros: dictionary holding macro arguments and source code
    (first variable returned by read_macro_file).
    @param language: the language of the source file (special
    editing of the source may be needed for Fortran files since
    a line cannot exceed 72 characters).
    """
    for m_name in macros:
        m_args, m_source = macros[m_name]
        pattern = r"^\s*%s\s*\((.+)\)" % m_name
        match = re.search(pattern, sline)
        if match:
            args = match.group(1)
            if args == "":
                args = []
            else:
                args = [arg.strip() for arg in args.split(",")]
                
            source = m_source
            d = '*** The following line\n  %s\ncontains the macro %s\n   with arguments %s and source\n%s' % (sline, m_name, args, source)
            #print d
            # remove function definition:
            pattern = r"def\s+%s\s*\([^)]*\):" % m_name
            source = re.sub(pattern, "", source)
            for from_, to_ in zip(m_args, args):
                source = re.sub(from_, to_, source)

            # determine indent from sline: (tabs won't work here!)
            indent = len(sline) - len(sline.lstrip())
            source_lines = source.split("\n")
            # first remove leading indentation in source, defined as the
            # indentation of the first line:
            leading_source_indent = \
                    len(source_lines[0]) - len(source_lines[0].lstrip())
            source_lines = [line[leading_source_indent:] \
                            for line in source_lines]
            for i in range(len(source_lines)):
                source_lines[i] = " "*indent + source_lines[i] + '\n'

            if language == 'Fortran':
                # make sure lines do not exceed 72 chars, and remove
                # trailing backslashes at the end of lines and insert
                # continuation character in column 6:
                for i in range(len(source_lines)):
                    if source_lines[i][-2:] == "\\\n":  # cont on next line?
                        # insert continuation char in next line:
                        if i < len(source_lines)-1:
                            source_lines[i] = source_lines[i][:-2] + "\n"
                            source_lines[i+1] = '     >' + source_lines[i+1][7:]
                    if len(source_lines[i]) > 72:
                        if len(source_lines[i]) < 72+72-6:
                            source_lines[i] = source_lines[i][:73] + \
                                              '\n     >' + source_lines[i][73:]
                        else:
                            # preliminary implementation - future versions
                            # should introduce multiple breaks
                            raise ValueError, \
                            "too long (%d) macro source line\n%s\n"\
                            "(split into several lines)" % \
                            (len(source_lines[i]), source_lines[i])
            source = "".join(source_lines)
            return source

    return sline

def find_macros(filename):
    """
    @param filename: file to read; interpret and remove #macrofile definitions,
    and find macro definitions written directly in the file and remove these
    lines.
    @return: macros and new_lines. macros is a dictionary holding all the
    macro definitions (name is key, (arguments, source code) is the value),
    and new_lines is the set of lines with #macrofile commands and
    macro definitions removed.
    """
    macros = {}
    # first search for #macrofile "somefile" commands inside
    # the file, open somefile and process macro definitions
    regex = re.compile('#\s*(?P<op>macrofile)\s+"(?P<fname>.*?)"')
    f1 = open(filename, 'r')
    new_lines = []
    for line in f1:
        m = regex.search(line)
        if m:
            fname = m.group("fname")
            f2 = open(fname, 'r')
            lines = f2.readlines()
            f2.close()
            new_macros, dummy = read_macro_file(lines)
            macros.update(new_macros)
        else:
            # write out everything except the #macrofile lines
            new_lines.append(line)
    f1.close()
    # look for macro definition within the file
    # (in strings or comment lines)
    new_macros, new_lines = read_macro_file(new_lines)
    macros.update(new_macros)
    return macros, new_lines

def process(filename):
    macros, lines = find_macros(filename)
    import preprocess
    language = preprocess.getContentType(filename)
    for line in lines:
        line = macro_replacement(line, macros, language).rstrip()
        print line
        
if __name__ == '__main__':
    try:
        filename = sys.argv[1]
    except IndexError:
        print 'Usage: %s filename > newfile' % sys.argv[0]
        sys.exit(1)
    process(filename)

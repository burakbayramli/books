#!/usr/bin/env python
"""
This program investigates match and replacement involving
words starting with a backslash. Such words are highly relevant
when working with text in LaTeX format.
The program identifies problematic characters that need a
double backslash in the replacement string. For a right
substitution, all commands that start with a backslash need
an extra backslash, as two things may go wrong: \b will
indicate a word boundary and give a wrong match, while \c
will simply escape the c, and hence match the c and then
leave an extra undesired backslash in the substituted string.
"""
import string, re
letters = string.ascii_letters

print '\nInvestigating matching with re.search and re.sub\n'
escape4match = []
for letter in letters:
    pattern = r'(\%sword)' % letter
    text = r'Text with \%sword in it' % letter
    repl = 'xxxx'
    try:
        m = re.search(pattern, text)
        if not m:
            print 'could not match', pattern, 'in', text
            escape4match.append(letter)
        else:
            # correct match?
            if m.group(1) != r'\%sword' % letter:
                print m.group(1), 'is wrong match of', pattern

        # what about substitutions?
        new = re.sub(pattern, repl, text)
        if not ' xxxx ' in new:
            print 'wrong %s substitution:' % pattern, new
    except Exception, e:
        print 'problem with %s: ' % pattern, e
        escape4match.append(letter)

    # add double backslash:
    pattern = r'\\%sword' % letter
    try:
        m = re.search(pattern, text)
        if m:
            print 'could match', pattern, 'in', text
        new = re.sub(pattern, repl, text)
        if ' xxxx ' in new:
            print 'correct %s substitution:' % pattern, new
    except Exception, e:
        print 'problem with %s: ' % pattern,
        print e

print '\nInvestigating replacement string in re.sub\n'
escape4replacement = []
for letter in letters:
    pattern = 'xxxx'
    replacement = r'\%s' % letter
    text = r'Text with xxxx in it'
    try:
        new, n = re.subn(pattern, replacement, text)
        for i in new:
            if ord(i) == 32 or 65 <= ord(i) <= 90 or \
               ord(i) == 92 or 97 <= ord(i) <= 122:
                pass # fine
            else:
                print '\nunsuccessful replacement by %s' % replacement
                print 'output repr:', repr(new)
                print 'output str :', str(new)
                escape4replacement.append(letter)
    except Exception, e:
        print 'problem with letter %s: ' % letter,
        print e
        escape4replacement.append(letter)

print 'Always need an extra backslash for match'
print 'If char is any of %s, \\char has special meaning' % ''.join(escape4match)
print 'Need to escape %s for replacement pattern' % ''.join(escape4replacement)

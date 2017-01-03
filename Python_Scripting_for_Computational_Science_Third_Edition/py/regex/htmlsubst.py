#!/usr/bin/env python
import re

def edit(text, from_to):
    for from_, to_ in from_to:
        c = re.compile(from_, re.DOTALL)
        text = c.sub(to_, text)
    return text

# test:
filestr = """\
The <b>subst.py pattern replacement file1
file2 ...</b> script replaces a <b>pattern</b>,
specified as a regular expression, by a <b>
replacement</b> string in a series of files
<b>file1</b>, <b>file2</b>, and so forth.
"""
# list with (from, to) substitution:
htmledits = [('<b>(.*?)</b>', '<em>\g<1></em>'),]
print edit(filestr, htmledits)

# wrong regex (greedy):
htmledits = [('<b>(.*)</b>', '<em>\g<1></em>'),]
print edit(filestr, htmledits)

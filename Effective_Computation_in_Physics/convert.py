"""Converts to python files"""
import os
import re
import sys
import json
from copy import deepcopy
from hashlib import sha256
from binascii import hexlify

DIR = os.path.expanduser('~/comphys')

# http://pythex.org/?regex=%5C%5Bsource%2Cpython%5C%5D%5Cn(%3FP%3Cdashes%3E%5B-%5D%2B)%5Cn(.*%3F)(%3FP%3Ddashes)&test_string=Hitting%20Enter%20will%20execute%20what%20you%20type%20in%20and%20return%20a%20%2B%3E%3E%3E%2B%20prompt%3A%0A%0A%5Bsource%2Cpython%5D%0A---------------%0A%3E%3E%3E%20print(%22Hello%20Sir%20Newton.%22)%0AHello%20Sir%20Newton.%0A%3E%3E%3E%0A---------------%0A%0A(((%22help(%20)%20function%22)))(((%22exit(%20)%20function%22)))(((%22Python%22%2C%20%22getting%20help%22)))(((%22Python%22%0Ause%20the%20%2Bexit()%2B%20function.%20If%20this%20looks%20a%20lot%20like%20bash%2C%20it%20is%20because%20this%20%0Amethod%20of&ignorecase=0&multiline=0&dotall=1&verbose=0
CODE_RE = re.compile('\[source,python\]\n(?P<dashes>[-]+)\n'
                     '(.*?)\n(?P=dashes)\n', re.S)

IPYNB_STRUCT = {
 "metadata": {
  "name": "",
  "signature": "sha256:",
 },
 "nbformat": 3,
 "nbformat_minor": 0,
 "worksheets": [
  {
   "cells": [],
   "metadata": {}
  }
 ]
}

CELL_STRUCT = {
     "cell_type": "code",
     "collapsed": False,
     "input": [],
     "language": "python",
     "metadata": {},
     "outputs": [],
     "prompt_number": 0,
    }


def make_nb(text):
    nb = deepcopy(IPYNB_STRUCT)
    h = sha256(text.encode())
    nb['metadata']['signature'] += h.hexdigest()
    for i, m in enumerate(CODE_RE.finditer(text), 1):
        cell = deepcopy(CELL_STRUCT)
        cell['input'] = m.group(2).splitlines(True)
        cell['prompt_number'] = i
        nb['worksheets'][0]['cells'].append(cell)
    return nb


def main():
    files = [os.path.join(DIR, fname) for fname in os.listdir(DIR) 
             if fname.startswith('ch') and fname.endswith('.asciidoc')]
    for fname in files:
        with open(fname, 'r') as f:
            text = f.read()
        nb = make_nb(text)
        oname = os.path.splitext(os.path.basename(fname))[0] + '.ipynb'
        with open(oname, 'w') as f:
            json.dump(nb, f)

if __name__ == '__main__':
    main()
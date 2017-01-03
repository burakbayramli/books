from __future__ import print_function, division
__author__ = 'kevin.sheppard'

import subprocess
import glob
import os

files = glob.glob('*.py')


if os.name=='nt':
    python = 'python.exe'
else:
    python = 'python'


print('Current dir' + os.getcwd())

skip = ['custom_functions_and_modules_basic.py',
        'file_system_and_navigation_basic.py',
        'run_all_code.py']
for f in files:
    if f in skip:
        print("Skipping " + f)
        continue
    print("Running " + f)
    out = subprocess.check_output([python,f])
    if out.lower().find("traceback"):
        print(f)



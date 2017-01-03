"""Solutions for 'File System and Navigation' chapter.

Solutions file used IPython demo mode.  To play, run

from IPython.lib.demo import Demo
demo = Demo('optimization_solutions.py')
 
and then call 

demo()

to play through the code in steps.
"""
# <demo> auto
from __future__ import print_function, division
import glob
import os
import shutil
import zipfile as zf

chapter_name = 'chapter_21'
# Initial cleanup
try:
    shutil.rmtree(os.path.join(os.getcwd(),chapter_name))
except:
    pass

# <demo> --- stop ---
# Exercise 1
print('Creating directory')

os.mkdir(chapter_name)

# <demo> --- stop ---
# Exercise 2
print('Changing directory')
os.chdir(chapter_name)

# <demo> --- stop ---
# Exercise 3
print('Creating a file')
f = file('tobedeleted.py','w')
# Some content
f.writelines([(str(i) + '/n') for i in xrange(10)])
f.close()

# <demo> --- stop ---
# Exercise 4
print('Zipping file')
# Method 1
cwd = os.getcwd()
shutil.make_archive('tobedeleted','zip',cwd)

# Method 2
f = file('tobedeleted.py')
file_contents = f.read()

z = zf.ZipFile('tobedeleted-2.zip', mode='w')
z.writestr('tobedeleted.py', file_contents)
z.close()

# <demo> --- stop ---
# Exercise 5
print('Listing directory contents')
print("Directory: " + os.getcwd())
files = glob.glob('tobedeleted*')
for f in files:
    print(f)

# <demo> --- stop ---
# Exercise 6
raw_input("Press enter to delete the files and directory...")
for f in files:
    # Safety first!
    if f.find('tobedeleted')>=0:
        os.unlink(f)

os.chdir('..')
os.rmdir(chapter_name)
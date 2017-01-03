# Create file.csv.gz from file.csv
# Extract csv from gzipped csv
from __future__ import division
from __future__ import print_function
import glob
import gzip
import os
import shutil
import subprocess
import sys
import tarfile
import zipfile
from pylab import *
from numpy import *
# End Imports



pwd = os.getcwd()
os.chdir('c:\\temp')
os.chdir('c:/temp')  # Identical
os.chdir('..')
os.getcwd()          # Now in 'c:\\'

os.mkdir('c:\\temp\\test')
os.makedirs('c:/temp/test/level2/level3')  # mkdir will fail
os.rmdir('c:\\temp\\test\\level2\\level3')
shutil.rmtree('c:\\temp\\test')  # rmdir fails, since not empty

os.chdir('c:\\temp')
files = os.listdir('.')
for f in files:
    if os.path.isdir(f):
        print(f, ' is a directory.')
    elif os.path.isfile(f):
        print(f, ' is a file.')
    else:
        print(f, ' is a something else.')

files = glob.glob('c:\\temp\\*.txt')
for file in files:
    print(file)

os.chdir('c:\\temp\\python')
# Make an empty file
f = file('file.ext','w')
f.close()
# Copies file.ext to 'c:\temp\'
shutil.copy('file.ext','c:\\temp\\')
# Copies file.ext to 'c:\temp\\python\file2.ext'
shutil.copy('file.ext','file2.ext')
# Copies file.ext to 'c:\\temp\\file3.ext', plus metadata
shutil.copy2('file.ext','file3.ext')
shutil.copytree('c:\\temp\\python\\','c:\\temp\\newdir\\')
shutil.move('c:\\temp\\newdir\\','c:\\temp\\newdir2\\')

# Copy using xcopy
os.system('xcopy /S /I c:\\temp c:\\temp4')
subprocess.call('xcopy /S /I c:\\temp c:\\temp5',shell=True)
# Extract using 7-zip
subprocess.call('"C:\\Program Files\\7-Zip\\7z.exe" e -y c:\\temp\\zip.7z')

# Creates files.zip
shutil.make_archive('files','zip','c:\\temp\\folder_to_archive')
# Creates files.tar.gz
shutil.make_archive('files','gztar','c:\\temp\\folder_to_archive')

csvin = file('file.csv','rb')
gz = gzip.GzipFile('file.csv.gz','wb')
gz.writelines(csvin.read())
gz.close()
csvin.close()

# Extract zip
zip = zipfile.ZipFile('files.zip')
zip.extractall('c:\\temp\\zip\\')
zip.close()
# Extract gzip tar 'r:gz' indicates read gzipped
gztar = tarfile.open('file.tar.gz', 'r:gz')
gztar.extractall('c:\\temp\\gztar\\')
gztar.close()
gz = gzip.GzipFile('file.csv.gz','rb')
csvout = file('file.csv','wb')
csvout.writelines(gz.read())
csvout.close()
gz.close()

# Read all lines using readlines()
f = file('file.csv','r')
lines = f.readlines()
for line in lines:
    print(line)
f.close()
# Using blocking via readline()
f = file('file.csv','r')
line = f.readline()
while line != '':
    print(line)
    line = f.readline()
f.close()
# Using larger blocks via readlines(n)
f = file('file.csv','r')
lines = f.readlines(2)
while lines != '':
    for line in lines:
        print(line)
    lines = f.readline(2)
f.close()


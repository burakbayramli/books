#!/usr/bin/env python
import os, shutil, glob
os.chdir(os.pardir)
shutil.copy('main.f.orig', 'main.f')
shutil.copy('F77WAVE.fcp.orig', 'F77WAVE.fcp')

# edit main.f such that solutions are dumped,
# also use a small grid
os.system("perl -pi.old~ -e 's#^C(\s+)call dump# $1call dump#' main.f")
os.system("perl -pi.old~ -e 's#^[^C]\s+PARAMETER \(n=(\d+)\)#      PARAMETER (n=31)#' main.f")
os.system("./make.sh")
os.chdir("Verify")
tmpfiles = glob.glob("tmp_*.mtv")
for file in tmpfiles: os.remove(file)
f = open('tmp.input', 'w')
f.write('20\n') # no of time steps
f.close()
os.system("../app < tmp.input")

# show on the screen:
from scitools.misc import findprograms
if findprograms(['plotmtv'], write_message=1):
    os.system("plotmtv -geometry 600x700 -nodate -3d tmp_*.mtv")
    


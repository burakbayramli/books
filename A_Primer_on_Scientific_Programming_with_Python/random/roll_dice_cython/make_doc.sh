#!/bin/sh
doconce format sphinx MC_Cython.do.txt

if [ ! -d sphinx-rootdir]; then
  doconce sphinx_dir MC_Cython.do.txt
  mkdir sphinx-rootdir/figs
fi
cp MC_Cython.rst sphinx-rootdir
cp figs/*.png sphinx-rootdir/figs
cd sphinx-rootdir
make clean
make html
#cp ../../src/ode/cython/ode0_cy2.html ../../src/ode/cython/ode0_cy5.html _build/html
echo
echo "firefox sphinx-rootdir/_build/html/index.html"

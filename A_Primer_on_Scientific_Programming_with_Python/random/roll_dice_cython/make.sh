#!/bin/sh
python setup.py build_ext --inplace
# Test if module works
python -c "import roll_dice_cy"
if [ $? -eq 0 ]; then
  echo "Cython module successfully built"
fi
# Compile and view C code
cython -a roll_dice.pyx
#firefox roll_dice.html

exit 0
# Profile
python -m cProfile -o .prof roll_dice.py 300000
#python -c 'import pstats; pstats.Stats(".prof").sort_stats("time").print_stats(30)'
# Or
python -c "
import pstats
s = pstats.Stats('.prof')
s.strip_dirs()
s.sort_stats('time')
s.print_stats(30)
"




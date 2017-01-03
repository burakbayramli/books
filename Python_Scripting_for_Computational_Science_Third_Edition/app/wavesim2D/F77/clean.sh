#!/bin/sh
files="*.o *mon.out *~ app results versions/*.o versions/*~ doc/*.log doc/*.aux doc/*.dvi doc/*.toc Verify/*mon.out versions/F77WAVE*.f wave2D*"
echo "removing $files"
rm -f $files


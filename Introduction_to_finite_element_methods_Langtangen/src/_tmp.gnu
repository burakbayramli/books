unset border
unset xtics
unset ytics
unset ztics
set parametric
set xrange [0:1]
set yrange [0:1]
set zrange [0:1]
set view 72,345
splot "_tmp_0000.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0001.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0002.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0003.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0004.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0005.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0006.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0007.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0008.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0009.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0010.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0011.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0012.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0013.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0014.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0015.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0016.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0017.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0018.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0019.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0020.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0021.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0022.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0023.dat" using 1:2:3 with linespoints lt 2 lw 2 title "", "_tmp_0024.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0025.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0026.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0027.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0028.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0029.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0030.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0031.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0032.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0033.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0034.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0035.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0036.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0037.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0038.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0039.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0040.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0041.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0042.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0043.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0044.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0045.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0046.dat" using 1:2:3 with lines lt 1 lw 2 title "", "_tmp_0047.dat" using 1:2:3 with lines lt 1 lw 2 title ""
set output "tmp.eps"
set terminal postscript eps enhanced monochrome
replot
set output "tmp.png"
set terminal png
replot
replot
pause 3

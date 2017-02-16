% mainline.m
Px = [1:10]'
Py = [ 0.2 1.0 2.6 3.6 4.9 5.3 6.5 7.8 8.0 9.0]'
A = [ones(size(Px)) Px Py]
[c, n] = clsq(A,2)

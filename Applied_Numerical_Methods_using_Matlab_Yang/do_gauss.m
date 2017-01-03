%do_gauss
A=[0 1 1;2 -1 -1;1 1 -1];
b=[2 0 1]';
x=gauss(A,b)
x1=A\b

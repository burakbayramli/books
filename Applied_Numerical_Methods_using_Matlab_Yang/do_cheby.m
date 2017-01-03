%do_cheby.m
N=2; a=-2; b=2;
[c,x1,y1]=cheby('f31',N,a,b) %Chebyshev polynomial ftn
%for comparison with Lagrange/Newton polynomial ftn
k=[0: N]; xn=cos((2*N+1-2*k)*pi/2/(N+1));%Eq.(3.12-1):Chebyshev nodes
x=((b-a)*xn +a+b)/2  %Eq.(3.12-2) 
y=f31(x)
l=lagranp(x,y), n=newtonp(x,y) %do_cheby.m
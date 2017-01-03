%nm711.m  to perform the golden search method
f711=inline('(x.*x-4).^2/8-1','x');
a=0; b=3; r=(sqrt(5)-1)/2; TolX=1e-4; TolFun=1e-4; MaxIter=100;
[xo,fo]=opt_gs(f711,a,b,r,TolX,TolFun,MaxIter)

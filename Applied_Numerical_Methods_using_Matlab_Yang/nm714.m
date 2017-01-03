%nm714  
f713= inline('x(1)*(x(1)-4-x(2)) +x(2)*(x(2)-1)','x');  
x0=[0 0], TolX=1e-4; TolFun=1e-9; alpha0=1; MaxIter=100;
[xo,fo]= opt_steep(f713,x0,TolX,TolFun,alpha0,MaxIter)
